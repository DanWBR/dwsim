'    Property Package Base Class
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

Imports System.Runtime.Serialization.Formatters.Binary
Imports System.Runtime.Serialization
Imports System.IO
Imports System.Math
Imports CapeOpen
Imports System.Runtime.InteropServices.ComTypes
Imports iop = System.Runtime.InteropServices

Imports System.Xml.Serialization
Imports System.Runtime.Serialization.Formatters
Imports System.Threading.Tasks
Imports DWSIM.MathOps.MathEx
Imports DWSIM.Thermodynamics.PropertyPackages.Auxiliary.FlashAlgorithms
Imports DWSIM.Interfaces
Imports DWSIM.Interfaces.Interfaces2
Imports System.Reflection
Imports System.Runtime.InteropServices
Imports DWSIM.Thermodynamics.Streams

Imports Newtonsoft.Json

Imports cv = DWSIM.SharedClasses.SystemsOfUnits.Converter
Imports Microsoft.Scripting.Hosting

Imports sui = DWSIM.UI.Shared.Common
Imports DWSIM.UI.Shared
Imports DWSIM.Interfaces.Enums
Imports DWSIM.SharedClasses

Imports props = DWSIM.Thermodynamics.PropertyPackages.Auxiliary.PROPS
Namespace PropertyPackages

#Region "    Global Enumerations"

    Public Enum Phase

        Liquid
        Liquid1
        Liquid2
        Liquid3
        Aqueous
        Vapor
        Solid
        Mixture

    End Enum

    Public Enum PhaseType

        SinglePhase
        LiquidMixture
        Overall

    End Enum

    Public Enum State
        Liquid = 0
        Vapor = 1
        Solid = 2
    End Enum

    Public Enum FlashSpec

        P = 0
        T = 1
        S = 2
        H = 3
        V = 4
        U = 5
        VAP = 6
        SF = 7

    End Enum

    Public Enum ThermoProperty
        ActivityCoefficient
        Fugacity
        FugacityCoefficient
    End Enum

    Public Enum PackageType
        EOS = 0
        ActivityCoefficient = 1
        ChaoSeader = 2
        VaporPressure = 3
        Miscelaneous = 4
        CorrespondingStates = 5
        CAPEOPEN = 6
        Electrolytes = 7
        Specialized = 8
    End Enum

    Public Enum FlashMethod
        GlobalSetting = 2
        DWSIMDefault = 0
        InsideOut = 1
        InsideOut3P = 3
        GibbsMin2P = 4
        GibbsMin3P = 5
        NestedLoops3P = 6
        NestedLoopsSLE = 7
        NestedLoopsImmiscible = 8
        SimpleLLE = 9
        NestedLoopsSLE_SS = 10
        NestedLoops3PV2 = 11
        NestedLoops3PV3 = 12
    End Enum

#End Region

    ''' <summary>
    ''' The Property Package Class contains methods to do thermodynamic calculations for all supported phases in DWSIM.
    ''' </summary>
    ''' <remarks>The base class is inherited by each implemented property package, which contains its own methods.</remarks>
    <System.Serializable()> Public MustInherit Class PropertyPackage

        'CAPE-OPEN 1.0 Interfaces
        Implements ICapeIdentification, ICapeThermoPropertyPackage, ICapeUtilities, ICapeThermoEquilibriumServer, ICapeThermoCalculationRoutine

        'CAPE-OPEN 1.1 Interfaces
        Implements ICapeThermoPhases, ICapeThermoPropertyRoutine, ICapeThermoCompounds, ICapeThermoUniversalConstant
        Implements ICapeThermoMaterialContext, ICapeThermoEquilibriumRoutine

        'CAPE-OPEN Persistence Interface
        Implements IPersistStreamInit

        'CAPE-OPEN Error Interfaces
        Implements ECapeUser, ECapeUnknown, ECapeRoot

        'IDisposable
        Implements IDisposable

        'DWSIM XML support
        Implements Interfaces.ICustomXMLSerialization

        'DWSIM IPropertyPackage
        Implements IPropertyPackage

#Region "   Caching"

        <XmlIgnore> Private CompoundPropCache As New Dictionary(Of Integer, ICompoundConstantProperties)

#End Region

#Region "   Enums"

        Public Enum LiquidDensityCalcMode
            Rackett = 0
            Rackett_and_ExpData = 1
            EOS = 2
            COSTALD = 3
        End Enum

        Public Enum LiquidViscosityCalcMode
            Letsou_Stiel = 0
            ExpData = 1
        End Enum

        Public Enum LiquidViscosityMixRule
            MoleAverage = 0
            LogMoleAverage = 1
            InvertedMassAverage = 2
            InvertedLogMassAverage = 3
        End Enum

        Public Enum EnthalpyEntropyCpCvCalcMode
            LeeKesler = 0
            Ideal = 1
            Excess = 2
            ExpData = 3
            LiqCp_Excess = 4
        End Enum

        Public Enum LiquidEnthalpyEntropyCpCvCalcMode_EOS
            EOS = 0
            ExpData = 1
        End Enum

        Public Enum VaporPhaseFugacityCalcMode
            Ideal = 0
            PengRobinson = 1
        End Enum

        Public Enum SolidPhaseFugacityCalcMode
            FromLiquidFugacity = 0
            Ideal = 1
        End Enum

        Public Enum FlashCalculationApproachType
            NestedLoops = 0
            InsideOut = 1
            GibbsMinimization = 2
        End Enum

#End Region

#Region "   Property Calculation Settings"

        Public Property LiquidDensityCalculationMode_Subcritical As LiquidDensityCalcMode = LiquidDensityCalcMode.Rackett_and_ExpData

        Public Property LiquidDensityCalculationMode_Supercritical As LiquidDensityCalcMode = LiquidDensityCalcMode.Rackett_and_ExpData

        Public Property LiquidDensity_UsePenelouxVolumeTranslation As Boolean = True

        Public Property LiquidDensity_CorrectExpDataForPressure As Boolean = True

        Public Property LiquidViscosityCalculationMode_Subcritical As LiquidViscosityCalcMode = LiquidViscosityCalcMode.ExpData

        Public Property LiquidViscosityCalculationMode_Supercritical As LiquidViscosityCalcMode = LiquidViscosityCalcMode.Letsou_Stiel

        Public Property LiquidViscosity_CorrectExpDataForPressure As Boolean = True

        Public Property LiquidViscosity_MixingRule As LiquidViscosityMixRule = LiquidViscosityMixRule.MoleAverage

        Public Property LiquidFugacity_UsePoyntingCorrectionFactor As Boolean = True

        Public Property ActivityCoefficientModels_IgnoreMissingInteractionParameters As Boolean = False

        Public Property VaporPhaseFugacityCalculationMode As VaporPhaseFugacityCalcMode = VaporPhaseFugacityCalcMode.Ideal

        Public Property SolidPhaseFugacityCalculationMethod As SolidPhaseFugacityCalcMode = SolidPhaseFugacityCalcMode.FromLiquidFugacity

        Public Property SolidPhaseFugacity_UseIdealLiquidPhaseFugacity As Boolean = False

        Public Property SolidPhaseEnthalpy_UsesCp As Boolean = False

        Public Property EnthalpyEntropyCpCvCalculationMode As EnthalpyEntropyCpCvCalcMode = EnthalpyEntropyCpCvCalcMode.LeeKesler

        Public Property LiquidEnthalpyEntropyCpCvCalculationMode_EOS As LiquidEnthalpyEntropyCpCvCalcMode_EOS = LiquidEnthalpyEntropyCpCvCalcMode_EOS.EOS

        Public Property IgnoreVaporFractionLimit As Boolean = False

        Public Property IgnoreSalinityLimit As Boolean = False

        Public Property CalculateAdditionalMaterialStreamProperties As Boolean = True

        Public Property FlashCalculationApproach As FlashCalculationApproachType = FlashCalculationApproachType.NestedLoops

        Public Property SingleCompoundCheckThreshold As Double = 0.99999

        Public Property UseHenryConstants As Boolean = True

        Public Overridable ReadOnly Property IsAmineModel As Boolean = False

        Public Property AutoEstimateMissingNRTLUNIQUACParameters As Boolean = True

        ''' <summary>
        ''' ' For mobile compatibility only.
        ''' </summary>
        ''' <returns></returns>
        Public Property ParametersXMLString = ""

        Public Property AreModelParametersDirty = True

#End Region

#Region "   Members"

        Public Const ClassId As String = ""

        <JsonIgnore> <System.NonSerialized()> Private m_ms As Interfaces.IMaterialStream = Nothing
        Private m_ss As New List(Of String)
        Private m_configurable As Boolean = False

        Public Shared m_Henry As New Dictionary(Of String, HenryParam)

        <NonSerialized> Private m_ip As DataTable

        Public _packagetype As PackageType

        <JsonIgnore> Public _phasemappings As New Dictionary(Of String, PhaseInfo)

        Public IsElectrolytePP As Boolean = False

        Private LoopVarF, LoopVarX As Double, LoopVarState As State
        <JsonIgnore> <System.NonSerialized()> Dim m_Flowsheet As IFlowsheet

        <JsonIgnore> <System.NonSerialized()> Private _como As Object 'CAPE-OPEN Material Object

        Private dummyipvec As Double(,)

#End Region

#Region "   Constructor"

        Sub New()

            Initialize()

        End Sub

        Sub New(ByVal capeopenmode As Boolean)

            Settings.CAPEOPENMode = capeopenmode

            If capeopenmode Then InitCO()

            Initialize()

        End Sub

        Sub InitCO()

            'initialize collections

            _selectedcomps = New Dictionary(Of String, BaseClasses.ConstantProperties)
            _availablecomps = New Dictionary(Of String, BaseClasses.ConstantProperties)

            'load chempsep database if existent

            Me.LoadCSDB()

            'load Electrolytes XML database

            Me.LoadEDB()

            'load Biodiesel XML database

            Me.LoadBDDB()

            'load CoolProp database

            Me.LoadCPDB()

            'load chedl database

            Me.LoadCheDLDB()

            'load User databases

            Me.LoadUserDBs()

            'load additional compounds

            Try
                Me.LoadAdditionalCompounds()
            Catch ex As Exception
            End Try

        End Sub

        Public Sub LoadAdditionalCompounds()
            Dim comps = Databases.UserDB.LoadAdditionalCompounds()
            For Each cp As BaseClasses.ConstantProperties In comps
                If Not _availablecomps.ContainsKey(cp.Name) Then _availablecomps.Add(cp.Name, cp)
            Next
        End Sub

        Public Sub LoadCheDLDB()

            Dim chedl As New Databases.ChEDL_Thermo
            Dim cpa() As BaseClasses.ConstantProperties
            chedl.Load()
            cpa = chedl.Transfer().ToArray()
            Dim addedcomps = _availablecomps.Keys.Select(Function(x) x.ToLower).ToList()
            For Each cp As BaseClasses.ConstantProperties In cpa
                If Not addedcomps.Contains(cp.Name.ToLower) AndAlso Not _availablecomps.ContainsKey(cp.Name) Then
                    If _availablecomps.Values.Where(Function(x) x.CAS_Number = cp.CAS_Number).Count = 0 Then
                        _availablecomps.Add(cp.Name, cp)
                    End If
                End If
            Next

        End Sub

        Public Sub LoadCSDB()
            Dim csdb As New Databases.ChemSep
            Dim cpa() As BaseClasses.ConstantProperties
            csdb.Load()
            cpa = csdb.Transfer()
            For Each cp As BaseClasses.ConstantProperties In cpa
                If Not _availablecomps.ContainsKey(cp.Name) Then _availablecomps.Add(cp.Name, cp)
            Next
        End Sub

        Public Sub LoadDWSIMDB()
            Dim dwdb As New Databases.DWSIM
            Dim cpa() As BaseClasses.ConstantProperties
            dwdb.Load()
            cpa = dwdb.Transfer()
            For Each cp As BaseClasses.ConstantProperties In cpa
                If Not _availablecomps.ContainsKey(cp.Name) Then _availablecomps.Add(cp.Name, cp)
            Next
        End Sub

        Public Sub LoadBDDB()
            Dim bddb As New Databases.Biodiesel
            Dim cpa() As BaseClasses.ConstantProperties
            bddb.Load()
            cpa = bddb.Transfer()
            For Each cp As BaseClasses.ConstantProperties In cpa
                If Not _availablecomps.ContainsKey(cp.Name) Then _availablecomps.Add(cp.Name, cp)
            Next
        End Sub

        Public Sub LoadEDB()
            Dim edb As New Databases.Electrolyte
            Dim cpa() As BaseClasses.ConstantProperties
            edb.Load()
            cpa = edb.Transfer()
            For Each cp As BaseClasses.ConstantProperties In cpa
                If Not _availablecomps.ContainsKey(cp.Name) Then _availablecomps.Add(cp.Name, cp)
            Next
        End Sub

        Public Sub LoadCPDB()
            Dim cpdb As New Databases.CoolProp
            Dim cpa() As BaseClasses.ConstantProperties
            cpdb.Load()
            Try
                cpa = cpdb.Transfer()
                For Each cp As BaseClasses.ConstantProperties In cpa
                    If _availablecomps.Values.Where(Function(x) x.CAS_Number = cp.CAS_Number).Count = 0 Then
                        _availablecomps.Add(cp.Name, cp)
                        _availablecomps(cp.Name).IsCOOLPROPSupported = True
                    End If
                Next
            Catch ex As Exception
            End Try
        End Sub

        Public Sub LoadUserDBs()

            If GlobalSettings.Settings.UserDatabases IsNot Nothing Then
                'load user databases
                For Each fpath As String In GlobalSettings.Settings.UserDatabases
                    Try
                        Dim componentes As BaseClasses.ConstantProperties()
                        componentes = Databases.UserDB.ReadComps(fpath)
                        If componentes.Length > 0 Then
                            For Each c As BaseClasses.ConstantProperties In componentes
                                If Not _availablecomps.ContainsKey(c.Name) Then
                                    _availablecomps.Add(c.Name, c)
                                End If
                            Next
                        End If
                    Catch ex As Exception
                    End Try
                Next
            End If

        End Sub

        Public Overridable Sub ConfigParameters()

        End Sub

        Public Sub CreatePhaseMappings()
            Me._phasemappings = New Dictionary(Of String, PhaseInfo)
            With Me._phasemappings
                .Add("Vapor", New PhaseInfo("", 2, Phase.Vapor))
                .Add("Liquid1", New PhaseInfo("", 3, Phase.Liquid1))
                .Add("Liquid2", New PhaseInfo("", 4, Phase.Liquid2))
                .Add("Liquid3", New PhaseInfo("", 5, Phase.Liquid3))
                .Add("Aqueous", New PhaseInfo("", 6, Phase.Aqueous))
                If Not GlobalSettings.Settings.HideSolidPhaseFromCAPEOPENComponents Then .Add("Solid", New PhaseInfo("", 7, Phase.Solid))
            End With
        End Sub

        Public Sub CreatePhaseMappingsDW()
            Me._phasemappings = New Dictionary(Of String, PhaseInfo)
            With Me._phasemappings
                .Add("Vapor", New PhaseInfo("Vapor", 2, Phase.Vapor))
                .Add("Liquid1", New PhaseInfo("Liquid", 3, Phase.Liquid1))
                .Add("Liquid2", New PhaseInfo("Liquid2", 4, Phase.Liquid2))
                If Not GlobalSettings.Settings.HideSolidPhaseFromCAPEOPENComponents Then .Add("Solid", New PhaseInfo("Solid", 7, Phase.Solid))
            End With
        End Sub

#End Region

#Region "   Properties"

        Public Overridable ReadOnly Property Popular As Boolean = False

        Public Property PropertyMethodsInfo As New PropertyPackageMethods

        Public Overridable ReadOnly Property DisplayName As String = "" Implements IPropertyPackage.DisplayName

        Public Overridable ReadOnly Property DisplayDescription As String = "A Property Package consists in a set of property methods and procedures used to calculate chemical phase equilibria and physical properties." Implements IPropertyPackage.DisplayDescription

        Public Property OverrideKvalFugCoeff As Boolean = False

        Public KvalFugacityCoefficientOverride As Func(Of Double(), Double, Double, State, PropertyPackage, Double())

        Public Property OverrideEnthalpyCalculation As Boolean = False

        Public EnthalpyCalculationOverride As Func(Of Double(), Double, Double, State, PropertyPackage, Double)

        Public Property OverrideEntropyCalculation As Boolean = False

        Public EntropyCalculationOverride As Func(Of Double(), Double, Double, State, PropertyPackage, Double)

        Public Property ForceNewFlashAlgorithmInstance As Boolean = False

        Property FlashSettings As New Dictionary(Of Interfaces.Enums.FlashSetting, String)

        Public Property ExceptionLog As String = ""

        Public Overridable ReadOnly Property ImplementsAnalyticalDerivatives As Boolean = False

        Public Overridable ReadOnly Property IsFunctional As Boolean = True Implements IPropertyPackage.IsFunctional

        Public Overridable ReadOnly Property ShouldUseKvalueMethod2 As Boolean = False Implements IPropertyPackage.ShouldUseKvalueMethod2

        'forced solids list
        Public Property ForcedSolids As New List(Of String)

        'overriden phase properties' calculation routines
        Public Property PropertyOverrides As New Dictionary(Of String, String)

        Public ReadOnly Property PhaseMappings() As Dictionary(Of String, PhaseInfo)
            Get
                If Me._phasemappings Is Nothing Then
                    CreatePhaseMappingsDW()
                ElseIf Me._phasemappings.Count = 0 Then
                    CreatePhaseMappingsDW()
                End If
                Return _phasemappings
            End Get
        End Property

        Private Function GetFlash() As Auxiliary.FlashAlgorithms.FlashAlgorithm
            If CurrentMaterialStream IsNot Nothing Then
                If AUX_IS_SINGLECOMP(Phase.Mixture) Then
                    Return New SingleCompFlash() With {.FlashSettings = FlashSettings}
                Else
                    If FlashCalculationApproach = FlashCalculationApproachType.GibbsMinimization Then
                        Return New GibbsMinimizationMulti() With {.FlashSettings = FlashSettings}
                    Else
                        If FlashCalculationApproach = FlashCalculationApproachType.InsideOut Then
                            FlashSettings(FlashSetting.UseIOFlash) = True
                        Else
                            FlashSettings(FlashSetting.UseIOFlash) = False
                        End If
                        Return New UniversalFlash() With {.FlashSettings = FlashSettings}
                    End If
                End If
            Else
                If FlashCalculationApproach = FlashCalculationApproachType.GibbsMinimization Then
                    Return New GibbsMinimizationMulti() With {.FlashSettings = FlashSettings}
                Else
                    If FlashCalculationApproach = FlashCalculationApproachType.InsideOut Then
                        FlashSettings(FlashSetting.UseIOFlash) = True
                    Else
                        FlashSettings(FlashSetting.UseIOFlash) = False
                    End If
                    Return New UniversalFlash() With {.FlashSettings = FlashSettings}
                End If
            End If
        End Function


        ''' <summary>
        ''' Returns the FlashAlgorithm object instance for this property package.
        ''' </summary>
        ''' <value></value>
        ''' <returns>A FlashAlgorithm object to be used in flash calculations.</returns>
        ''' <remarks></remarks>
        <XmlIgnore> Public Overridable ReadOnly Property FlashBase() As Auxiliary.FlashAlgorithms.FlashAlgorithm

            Get
                If GlobalSettings.Settings.CAPEOPENMode Then
                    Return GetFlash()
                Else
                    If CurrentMaterialStream IsNot Nothing Then
                        Select Case CurrentMaterialStream.ForcePhase
                            Case ForcedPhase.None
                                Return GetFlash()
                            Case ForcedPhase.GlobalDef
                                If CurrentMaterialStream.Flowsheet IsNot Nothing Then
                                    If CurrentMaterialStream.Flowsheet.FlowsheetOptions.ForceStreamPhase = ForcedPhase.None Then
                                        Return GetFlash()
                                    Else
                                        Return New ForcedPhaseFlash() With {.FlashSettings = FlashSettings, .ForcePhase = CurrentMaterialStream.Flowsheet.FlowsheetOptions.ForceStreamPhase}
                                    End If
                                Else
                                    Return GetFlash()
                                End If
                            Case Else
                                Return New ForcedPhaseFlash() With {.FlashSettings = FlashSettings, .ForcePhase = CurrentMaterialStream.ForcePhase}
                        End Select
                    Else
                        Return GetFlash()
                    End If
                End If
            End Get

        End Property

        Public Function ShouldBypassEquilibriumCalculation() As Boolean

            If CurrentMaterialStream IsNot Nothing Then
                Select Case CurrentMaterialStream.ForcePhase
                    Case ForcedPhase.None
                        Return False
                    Case ForcedPhase.GlobalDef
                        If CurrentMaterialStream.Flowsheet.FlowsheetOptions.ForceStreamPhase = ForcedPhase.None Then
                            Return False
                        Else
                            Return True
                        End If
                    Case Else
                        Return True
                End Select
            Else
                Return False
            End If

        End Function

        Public Property UniqueID() As String = "" Implements IPropertyPackage.UniqueID

        Public Property Tag() As String Implements IPropertyPackage.Tag

        Public ReadOnly Property PackageType() As PackageType
            Get
                Return _packagetype
            End Get
        End Property

        Public Property IsConfigurable() As Boolean
            Get
                Return m_configurable
            End Get
            Set(ByVal value As Boolean)
                m_configurable = value
            End Set
        End Property

        ''' <summary>
        ''' Gets or sets the current material stream for this property package.
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        <XmlIgnore> <JsonIgnore> Public Property CurrentMaterialStream() As Interfaces.IMaterialStream Implements Interfaces.IPropertyPackage.CurrentMaterialStream
            Get
                Return m_ms
            End Get
            Set(ByVal MatStr As Interfaces.IMaterialStream)
                m_ms = Nothing
                m_ms = MatStr
                If MatStr IsNot Nothing Then
                    Dim comps = MatStr.Phases(0).Compounds.Values
                    For Each c In comps
                        If Not CompoundPropCache.ContainsKey(c.ConstantProperties.ID) Then
                            CompoundPropCache.Add(c.ConstantProperties.ID, c.ConstantProperties)
                        End If
                    Next
                    RunPostMaterialStreamSetRoutine()
                End If
            End Set
        End Property

        Public Overridable Sub RunPostMaterialStreamSetRoutine()

        End Sub

        Public ReadOnly Property SupportedComponents() As System.Collections.Generic.List(Of String)
            Get
                Return m_ss
            End Get
        End Property

        Public Overrides Function ToString() As String

            If ComponentName <> "" Then
                Return ComponentName
            Else
                Return MyBase.ToString
            End If

        End Function

#End Region

#Region "   Cloning Support"

        Public Overridable Function Clone() As PropertyPackage

            Dim data = SaveData()

            Dim pp As PropertyPackage = Activator.CreateInstance(Me.GetType())

            pp.LoadData(data)

            pp.UniqueID = "PP-" + Guid.NewGuid().ToString()

            If EnthalpyCalculationOverride IsNot Nothing Then
                pp.EnthalpyCalculationOverride = EnthalpyCalculationOverride.Clone()
            End If
            If EntropyCalculationOverride IsNot Nothing Then
                pp.EntropyCalculationOverride = EntropyCalculationOverride.Clone()
            End If
            If KvalFugacityCoefficientOverride IsNot Nothing Then
                pp.KvalFugacityCoefficientOverride = KvalFugacityCoefficientOverride.Clone()
            End If

            Return pp

        End Function

        Public Overridable Function DeepClone() As PropertyPackage

            Return ObjectCopy(Me)

        End Function

        Function ObjectCopy(ByVal obj As Object) As Object

            Dim objMemStream As New MemoryStream()
            Dim objBinaryFormatter As New BinaryFormatter(Nothing, New StreamingContext(StreamingContextStates.Clone))

            objBinaryFormatter.Serialize(objMemStream, obj)

            objMemStream.Seek(0, SeekOrigin.Begin)

            objBinaryFormatter.Binder = New DeserializationBinder()

            ObjectCopy = objBinaryFormatter.Deserialize(objMemStream)

            objMemStream.Close()

        End Function

#End Region

#Region "   Must Override or Overridable Functions"

        Public Overridable Function GetModel() As Object

            Return Nothing

        End Function

        Public Overridable Sub AddDefaultCompounds(compnames As String())

            If Not Settings.ExcelMode Then

                For Each comp In compnames

                    If _availablecomps.ContainsKey(comp) Then

                        Dim tmpcomp As New BaseClasses.ConstantProperties
                        tmpcomp = _availablecomps(comp)
                        _selectedcomps.Add(tmpcomp.Name, tmpcomp)
                        _availablecomps.Remove(tmpcomp.Name)

                    End If

                Next

            End If

        End Sub

        Public Sub CalcAdditionalEnergyTerms()

            For Each p As IPhase In Me.CurrentMaterialStream.Phases.Values
                CalcInternalEnergy(p)
                CalcGibbsFreeEnergy(p)
                CalcHelmholtzEnergy(p)
                CalcDiffusionCoefficients(p)
            Next

        End Sub

        Public Sub CalcAdditionalPhaseProperties() Implements IPropertyPackage.CalcAdditionalPhaseProperties

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "CalcAdditionalPhaseProperties", ComponentName & " (Additional Properties)", "Property Package Additional Phase Properties Calculation Routine")

            IObj?.Paragraphs.Add("<h3>Isothermal Compressibility</h3>")

            IObj?.Paragraphs.Add("Isothermal compressibility of a given phase is calculated 
                                following the thermodynamic definition:")

            IObj?.Paragraphs.Add("<m>\beta_T =-\frac{1}{V}\frac{\partial V}{\partial P}<m>")

            IObj?.Paragraphs.Add("The above expression is calculated rigorously by the PR and SRK 
                                equations of state. For the other models, a numerical derivative 
                                approximation is used.")

            IObj?.Paragraphs.Add("<h3>Bulk Modulus</h3>")

            IObj?.Paragraphs.Add("The Bulk Modulus of a phase is defined as the inverse of the 
                                isothermal compressibility:")

            IObj?.Paragraphs.Add("<m>K=\frac{1}{\beta_T}<m>")

            IObj?.Paragraphs.Add("<h3>Speed of Sound</h3><p>")

            IObj?.Paragraphs.Add("The speed of sound (isentropic) in a given phase is calculated by the 
                                following equations:")

            IObj?.Paragraphs.Add("<m>c=\sqrt{\frac{C_p}{C_v}\frac{K}{\rho}},</m>")

            IObj?.Paragraphs.Add("where:")

            IObj?.Paragraphs.Add("<mi>c</mi> Speed of sound (m/s)")

            IObj?.Paragraphs.Add("<mi>K</mi> Bulk Modulus (Pa)")

            IObj?.Paragraphs.Add("<mi>\rho</mi> Phase Density (kg/m³)")

            IObj?.Paragraphs.Add("<mi>C_p</mi> Constant pressure Heat capacity (kJ/kg K)")

            IObj?.Paragraphs.Add("<mi>C_v</mi> Constant volume Heat capacity (kJ/kg K)")

            IObj?.Paragraphs.Add("The ratio of heat capacities in above equation is equal to the ratio of 
                                  the isothermal to the isentropic compressibility in order to shift the basis
                                   from isothermal to isentropic.")


            IObj?.Paragraphs.Add("<h3>Joule-Thomson Coefficient</h3>")

            IObj?.Paragraphs.Add("In thermodynamics, the Joule–Thomson effect (also known as the 
                                Joule–Kelvin effect, Kelvin–Joule effect, or Joule–Thomson 
                                expansion) describes the temperature change of a real gas or 
                                liquid when it is forced through a valve or porous plug while 
                                kept insulated so that no heat is exchanged with the environment. 
                                This procedure is called a throttling process or Joule–Thomson 
                                process. At room temperature, all gases except hydrogen, helium 
                                and neon cool upon expansion by the Joule–Thomson process. The 
                                rate of change of temperature with respect to pressure in a 
                                Joule–Thomson process is the Joule–Thomson coefficient.")

            IObj?.Paragraphs.Add("The Joule-Thomson coefficient for a given phase is calculated by 
                                the following definition:")

            IObj?.Paragraphs.Add("<m>\mu=(\frac{\partial T}{\partial P})_{H},</m>")

            IObj?.Paragraphs.Add("The JT coefficient is calculated analytically by the PR and SRK 
                                equations of state, while the Goldzberg correlation is used for 
                                all other models,")

            IObj?.Paragraphs.Add("<m>\mu=\frac{0.0048823T_{pc}(18/T_{pr}^{2}-1)}{P_{pc}C_{p}\gamma},</m>
                                for gases, and")

            IObj?.Paragraphs.Add("<m>\mu=-\frac{1}{\rho C_{p}}</m>")

            IObj?.Paragraphs.Add("for liquids.")

            IObj?.Paragraphs.Add("<h2>Results</h2>")

            For Each p As IPhase In Me.CurrentMaterialStream.Phases.Values
                IObj?.Paragraphs.Add(String.Format("<h3>{0}</h3>", p.Name))
                If p.Name <> "Mixture" And p.Name <> "OverallLiquid" And p.Properties.molarfraction.HasValue Then

                    With p.Properties
                        IObj?.SetCurrent
                        .isothermal_compressibility = CalcIsothermalCompressibility(p)
                        If .isothermal_compressibility <> 0.0# Then .bulk_modulus = 1 / .isothermal_compressibility Else .bulk_modulus = 0.0#
                        IObj?.SetCurrent
                        .speedOfSound = CalcSpeedOfSound(p)
                        IObj?.SetCurrent
                        .jouleThomsonCoefficient = CalcJouleThomsonCoefficient(p)
                    End With

                    IObj?.SetCurrent
                    CalcInternalEnergy(p)
                    IObj?.SetCurrent
                    CalcGibbsFreeEnergy(p)
                    IObj?.SetCurrent
                    CalcHelmholtzEnergy(p)
                    IObj?.SetCurrent
                    CalcDiffusionCoefficients(p)

                    IObj?.Paragraphs.Add(String.Format("Isothermal Compressibility: {0} 1/Pa", p.Properties.isothermal_compressibility))
                    IObj?.Paragraphs.Add(String.Format("Bulk Modulus: {0} Pa", p.Properties.bulk_modulus))
                    IObj?.Paragraphs.Add(String.Format("Speed of Sound: {0} m/s", p.Properties.speedOfSound))
                    IObj?.Paragraphs.Add(String.Format("Joule-Thomson Coefficient: {0} K/Pa", p.Properties.jouleThomsonCoefficient))

                    CalcIdealGasCpCv(p, CurrentMaterialStream.Phases(0).Properties.temperature.GetValueOrDefault)
                    CalcEnthalpyEntropyF(p)

                End If
            Next

            IObj?.Close()

        End Sub

        Public Sub CalcAdditionalPhaseProperties(phaseID As Integer)

            Dim p = Me.CurrentMaterialStream.Phases(phaseID)

            Dim check As Boolean = False

            If p.Properties.molarfraction.HasValue Then
                check = True
            Else
                If Settings.CAPEOPENMode Then check = True
            End If

            If p.Name <> "Mixture" And p.Name <> "OverallLiquid" And check Then

                With p.Properties
                    .isothermal_compressibility = CalcIsothermalCompressibility(p)
                    If .isothermal_compressibility <> 0.0# Then .bulk_modulus = 1 / .isothermal_compressibility Else .bulk_modulus = 0.0#
                    .speedOfSound = CalcSpeedOfSound(p)
                    .jouleThomsonCoefficient = CalcJouleThomsonCoefficient(p)
                End With

                CalcInternalEnergy(p)
                CalcGibbsFreeEnergy(p)
                CalcHelmholtzEnergy(p)
                CalcDiffusionCoefficients(p)
                CalcIdealGasCpCv(p, CurrentMaterialStream.Phases(0).Properties.temperature.GetValueOrDefault)
                CalcEnthalpyEntropyF(p)

            End If


        End Sub

        Public Overridable Function CalcIsothermalCompressibility(p As IPhase) As Double

            Dim Z, P0, P1, T, Z1 As Double

            Dim check As Boolean = False

            If p.Properties.molarfraction.HasValue Then
                check = True
            Else
                If Settings.CAPEOPENMode Then check = True
            End If

            If Not check Then Return 0.0

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "CalcIsothermalCompressibility", ComponentName & " (Isothermal Compressibility)", "")

            T = CurrentMaterialStream.Phases(0).Properties.temperature.GetValueOrDefault
            P0 = CurrentMaterialStream.Phases(0).Properties.pressure.GetValueOrDefault
            Z = p.Properties.compressibilityFactor.GetValueOrDefault

            IObj?.SetCurrent

            P1 = P0 + 100

            Select Case p.Name
                Case "Mixture"
                    Return 0.0#
                Case "Vapor"
                    Z1 = AUX_Z(RET_VMOL(Phase.Vapor), T, P1, PhaseName.Vapor)
                Case "OverallLiquid"
                    Return 0.0#
                Case "Liquid1"
                    Z1 = AUX_Z(RET_VMOL(Phase.Liquid1), T, P1, PhaseName.Liquid)
                Case "Liquid2"
                    Z1 = AUX_Z(RET_VMOL(Phase.Liquid2), T, P1, PhaseName.Liquid)
                Case "Liquid3"
                    Z1 = AUX_Z(RET_VMOL(Phase.Liquid3), T, P1, PhaseName.Liquid)
                Case "Aqueous"
                    Z1 = AUX_Z(RET_VMOL(Phase.Aqueous), T, P1, PhaseName.Liquid)
                Case "Solid"
                    Return 0.0#
            End Select

            Dim K As Double = 1 / P0 - 1 / Z * (Z1 - Z) / (P1 - P0)

            If Double.IsNaN(K) Or Double.IsInfinity(K) Then K = 0.0#

            IObj?.Close()

            Return K

        End Function

        Public Overridable Function CalcIsothermalCompressibility(Vz As Double(), T As Double, P As Double, state As PhaseName) As Double

            Dim Z, P0, P1, Z1 As Double

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            P0 = P
            Z = AUX_Z(Vz, T, P, state)

            IObj?.SetCurrent

            P1 = P0 + 100

            Z1 = AUX_Z(Vz, T, P1, state)

            Dim K As Double = 1 / P0 - 1 / Z * (Z1 - Z) / (P1 - P0)

            If Double.IsNaN(K) Or Double.IsInfinity(K) Then K = 0.0#

            IObj?.Close()

            Return K

        End Function

        Public Overridable Function CalcSpeedOfSound(p As IPhase) As Double

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "CalcSpeedOfSound", ComponentName & " (Speed of Sound)", "")

            Dim K, rho As Double

            K = p.Properties.bulk_modulus.GetValueOrDefault
            rho = p.Properties.density.GetValueOrDefault

            IObj?.Close()

            Return (p.Properties.heatCapacityCp.GetValueOrDefault / p.Properties.heatCapacityCv.GetValueOrDefault * K / rho) ^ 0.5


        End Function

        Public Overridable Function CalcJouleThomsonCoefficient(p As IPhase) As Double

            Dim check As Boolean = False

            If p.Properties.molarfraction.HasValue Then
                check = True
            Else
                If Settings.CAPEOPENMode Then check = True
            End If

            If Not check Then Return 0.0

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "CalcJouleThomsonCoefficient", ComponentName & " (Joule-Thomson Coefficient)", "")

            Dim T As Double
            T = CurrentMaterialStream.Phases(0).Properties.temperature.GetValueOrDefault

            IObj?.Close()

            Select Case p.Name
                Case "Mixture"
                    Return 0.0#
                Case "Vapor"
                    Dim SG = p.Properties.molecularWeight.GetValueOrDefault / 28.97
                    If p.Properties.heatCapacityCp Is Nothing Then DW_CalcProp("heatCapacityCp", Phase.Vapor)
                    Return Auxiliary.PROPS.JT_Goldzberg(T, AUX_TCM(Phase.Vapor), AUX_PCM(Phase.Vapor), p.Properties.heatCapacityCp.GetValueOrDefault,
                                                     "V", SG)
                Case "OverallLiquid"
                    Return 0.0#
                Case "Liquid1"
                    If p.Properties.heatCapacityCp Is Nothing Then DW_CalcProp("heatCapacityCp", Phase.Liquid1)
                    Return Auxiliary.PROPS.JT_Goldzberg(T, AUX_TCM(Phase.Liquid1), AUX_PCM(Phase.Liquid1), p.Properties.heatCapacityCp.GetValueOrDefault,
                                                     "L", p.Properties.density.GetValueOrDefault)
                Case "Liquid2"
                    If p.Properties.heatCapacityCp Is Nothing Then DW_CalcProp("heatCapacityCp", Phase.Liquid2)
                    Return Auxiliary.PROPS.JT_Goldzberg(T, AUX_TCM(Phase.Liquid2), AUX_PCM(Phase.Liquid2), p.Properties.heatCapacityCp.GetValueOrDefault,
                                                     "L", p.Properties.density.GetValueOrDefault)
                Case "Liquid3"
                    If p.Properties.heatCapacityCp Is Nothing Then DW_CalcProp("heatCapacityCp", Phase.Liquid3)
                    Return Auxiliary.PROPS.JT_Goldzberg(T, AUX_TCM(Phase.Liquid3), AUX_PCM(Phase.Liquid3), p.Properties.heatCapacityCp.GetValueOrDefault,
                                                     "L", p.Properties.density.GetValueOrDefault)
                Case "Aqueous"
                    If p.Properties.heatCapacityCp Is Nothing Then DW_CalcProp("heatCapacityCp", Phase.Aqueous)
                    Return Auxiliary.PROPS.JT_Goldzberg(T, AUX_TCM(Phase.Aqueous), AUX_PCM(Phase.Aqueous), p.Properties.heatCapacityCp.GetValueOrDefault,
                                                     "L", p.Properties.density.GetValueOrDefault)
                Case "Solid"
                    Return 0.0#
            End Select

        End Function

        Public Sub CalcInternalEnergy(p As IPhase)

            Dim H, P0, V, T, Z, MW, U As Double

            H = p.Properties.enthalpy.GetValueOrDefault 'kJ/kg
            P0 = CurrentMaterialStream.Phases(0).Properties.pressure.GetValueOrDefault 'Pa
            T = CurrentMaterialStream.Phases(0).Properties.temperature.GetValueOrDefault 'K
            Z = p.Properties.compressibilityFactor.GetValueOrDefault
            MW = p.Properties.molecularWeight.GetValueOrDefault 'kg/kmol

            V = Z * 8.314 * T / P0 'm3/mol

            U = H - P0 * V / MW 'kJ/kg

            If Double.IsNaN(U) Then U = 0.0#

            p.Properties.internal_energy = U 'kJ/kg
            p.Properties.molar_internal_energy = U * MW 'kJ/kmol

        End Sub

        Public Sub CalcGibbsFreeEnergy(p As IPhase)

            Dim H, S, T, MW As Double

            H = p.Properties.enthalpy.GetValueOrDefault
            S = p.Properties.entropy.GetValueOrDefault
            T = CurrentMaterialStream.Phases(0).Properties.temperature.GetValueOrDefault
            MW = p.Properties.molecularWeight.GetValueOrDefault

            p.Properties.gibbs_free_energy = H - T * S
            p.Properties.molar_gibbs_free_energy = (H - T * S) * MW

        End Sub

        Public Sub CalcHelmholtzEnergy(p As IPhase)

            Dim U, S, T, MW As Double

            U = p.Properties.internal_energy.GetValueOrDefault
            S = p.Properties.entropy.GetValueOrDefault
            T = CurrentMaterialStream.Phases(0).Properties.temperature.GetValueOrDefault
            MW = p.Properties.molecularWeight.GetValueOrDefault

            p.Properties.helmholtz_energy = U - T * S
            p.Properties.molar_helmholtz_energy = (U - T * S) * MW

        End Sub

        Public Sub CalcDiffusionCoefficients(p As IPhase)

            Dim T, P0 As Double
            T = CurrentMaterialStream.Phases(0).Properties.temperature.GetValueOrDefault
            P0 = CurrentMaterialStream.Phases(0).Properties.pressure.GetValueOrDefault

            Dim type As String = "L", Vx As Double()

            Select Case p.Name
                Case "Vapor"
                    type = "V"
                    Vx = RET_VMOL(Phase.Vapor)
                Case "Liquid1"
                    type = "L"
                    Vx = RET_VMOL(Phase.Liquid1)
                Case "Liquid2"
                    type = "L"
                    Vx = RET_VMOL(Phase.Liquid2)
                Case "Liquid3"
                    type = "L"
                    Vx = RET_VMOL(Phase.Liquid3)
                Case "Aqueous"
                    type = "L"
                    Vx = RET_VMOL(Phase.Aqueous)
            End Select

            Dim xi, Mi, Mni, LNEni, LNDni, LNEi, LNDi, FVi, FVni, Dabv1, Dabv2, Dabl As Double

            For Each subst In p.Compounds.Values

                xi = subst.MoleFraction.GetValueOrDefault

                If xi = 0.0 Then xi = 1.0E-20

                Mi = subst.ConstantProperties.Molar_Weight
                LNEi = subst.ConstantProperties.LennardJonesEnergy
                LNDi = subst.ConstantProperties.LennardJonesDiameter
                FVi = subst.ConstantProperties.FullerDiffusionVolume

                Mni = 0.0#
                LNEni = 0.0#
                LNDni = 0.0#
                FVni = 0.0#
                For Each subst2 In p.Compounds.Values
                    If subst.Name <> subst2.Name Then
                        Mni += xi * subst.ConstantProperties.Molar_Weight
                        LNEni += xi * subst.ConstantProperties.LennardJonesEnergy
                        LNDni += xi * subst.ConstantProperties.LennardJonesDiameter
                        FVni += xi * subst.ConstantProperties.FullerDiffusionVolume
                    End If
                Next
                Mni /= xi
                LNEni /= xi
                LNDni /= xi
                FVni /= xi

                Dabv1 = Auxiliary.PROPS.CalcVaporDiffusivity_Fuller(T, P0, Mi, Mni, FVi, FVni)
                Dabv2 = Auxiliary.PROPS.CalcVaporDiffusivity_WilkeAndLee(T, P0, Mi, Mni, LNEi, LNEni, LNDi, LNDni)
                Dabl = Auxiliary.PROPS.CalcLiquidDiffusivity_WilkeAndChang(T, Mni, p.Properties.viscosity.GetValueOrDefault, subst.PartialVolume.GetValueOrDefault)

                If type = "V" Then
                    If FVi <> 0.0# And FVni <> 0.0# Then
                        subst.DiffusionCoefficient = Dabv1
                    Else
                        subst.DiffusionCoefficient = Dabv2
                    End If
                Else
                    subst.DiffusionCoefficient = Dabl
                End If

            Next

        End Sub

        Public Sub CalcIdealGasCpCv(p As IPhase, T As Double)

            Dim Cp = AUX_CPm(p, T)
            Dim Cv = Cp - 8.314 / p.Properties.molecularWeight.GetValueOrDefault()

            p.Properties.idealGasHeatCapacityCp = Cp
            p.Properties.idealGasHeatCapacityRatio = Cp / Cv

        End Sub

        Public Sub CalcEnthalpyEntropyF(p As IPhase)

            Dim Hf = Me.AUX_HFm25(p.Compounds.Select(Function(c) c.Value.MoleFraction.GetValueOrDefault).ToArray())
            Dim Sf = Me.AUX_SFm25(p.Compounds.Select(Function(c) c.Value.MoleFraction.GetValueOrDefault).ToArray())

            p.Properties.enthalpyF = p.Properties.enthalpy + Hf
            p.Properties.entropyF = p.Properties.entropy + Sf

        End Sub

        ''' <summary>
        ''' Provides a wrapper function for CAPE-OPEN CalcProp/CalcSingleProp functions.
        ''' </summary>
        ''' <param name="property">The property to be calculated.</param>
        ''' <param name="phase">The phase where the property must be calculated for.</param>
        ''' <remarks>This function is necessary since DWSIM's internal property calculation function calculates all properties at once,
        ''' while the CAPE-OPEN standards require that only the property that was asked for to be calculated, leaving the others unchanged.</remarks>
        Public MustOverride Sub DW_CalcProp(ByVal [property] As String, ByVal phase As Phase)

        ''' <summary>
        ''' Provides a default implementation for solid phase property calculations in CAPE-OPEN mode. Should be used by all derived propety packages.
        ''' </summary>
        ''' <remarks></remarks>
        Public Overridable Sub DW_CalcSolidPhaseProps()

            Dim phaseID As Integer = 7
            Dim result As Double = 0.0#

            Dim T, P As Double
            T = Me.CurrentMaterialStream.Phases(0).Properties.temperature.GetValueOrDefault
            P = Me.CurrentMaterialStream.Phases(0).Properties.pressure.GetValueOrDefault

            Dim phasemolarfrac As Double = Nothing
            Dim overallmolarflow As Double = Nothing

            overallmolarflow = Me.CurrentMaterialStream.Phases(0).Properties.molarflow.GetValueOrDefault
            phasemolarfrac = Me.CurrentMaterialStream.Phases(phaseID).Properties.molarfraction.GetValueOrDefault
            result = overallmolarflow * phasemolarfrac
            Me.CurrentMaterialStream.Phases(phaseID).Properties.molarflow = result
            result = result * Me.AUX_MMM(Phase.Solid) / 1000
            Me.CurrentMaterialStream.Phases(phaseID).Properties.massflow = result
            result = phasemolarfrac * overallmolarflow * Me.AUX_MMM(Phase.Solid) / 1000 / Me.CurrentMaterialStream.Phases(0).Properties.massflow.GetValueOrDefault
            Me.CurrentMaterialStream.Phases(phaseID).Properties.massfraction = result

            result = Me.AUX_SOLIDDENS
            Me.CurrentMaterialStream.Phases(phaseID).Properties.density = result
            Me.CurrentMaterialStream.Phases(phaseID).Properties.volumetric_flow = Me.CurrentMaterialStream.Phases(phaseID).Properties.massflow / Me.CurrentMaterialStream.Phases(phaseID).Properties.density

            Dim constprops As New List(Of Interfaces.ICompoundConstantProperties)
            For Each su As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                constprops.Add(su.ConstantProperties)
            Next
            Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy = Me.DW_CalcEnthalpy(RET_VMOL(PropertyPackages.Phase.Solid), T, P, State.Solid)
            Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy = Me.DW_CalcEntropy(RET_VMOL(PropertyPackages.Phase.Solid), T, P, State.Solid)
            Me.CurrentMaterialStream.Phases(phaseID).Properties.compressibilityFactor = 0.0#
            result = Me.DW_CalcSolidHeatCapacityCp(T, RET_VMOL(PropertyPackages.Phase.Solid), constprops)
            Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCp = result
            Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCv = result
            result = Me.AUX_MMM(Phase.Solid)
            Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight = result
            result = Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
            Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_enthalpy = result
            result = Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
            Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_entropy = result
            Me.CurrentMaterialStream.Phases(phaseID).Properties.thermalConductivity = 0.0#
            Me.CurrentMaterialStream.Phases(phaseID).Properties.viscosity = 1.0E+20
            Me.CurrentMaterialStream.Phases(phaseID).Properties.kinematic_viscosity = 1.0E+20

        End Sub

        ''' <summary>
        ''' Calculates the enthalpy of a mixture.
        ''' </summary>
        ''' <param name="Vx">Vector of doubles containing the molar composition of the mixture.</param>
        ''' <param name="T">Temperature (K)</param>
        ''' <param name="P">Pressure (Pa)</param>
        ''' <param name="st">State enum indicating the state of the mixture (liquid or vapor).</param>
        ''' <returns>The enthalpy of the mixture in kJ/kg.</returns>
        ''' <remarks>The basis for the calculated enthalpy/entropy in DWSIM is zero at 25 C and 1 atm.</remarks>
        Public MustOverride Function DW_CalcEnthalpy(ByVal Vx As Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double

        ''' <summary>
        ''' Calculates the enthalpy departure of a mixture.
        ''' </summary>
        ''' <param name="Vx">Vector of doubles containing the molar composition of the mixture.</param>
        ''' <param name="T">Temperature (K)</param>
        ''' <param name="P">Pressure (Pa)</param>
        ''' <param name="st">State enum indicating the state of the mixture (liquid or vapor).</param>
        ''' <returns>The enthalpy departure of the mixture in kJ/kg.</returns>
        ''' <remarks>The basis for the calculated enthalpy/entropy in DWSIM is zero at 25 C and 1 atm.</remarks>
        Public MustOverride Function DW_CalcEnthalpyDeparture(ByVal Vx As Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double

        ''' <summary>
        ''' Calculates the entropy of a mixture.
        ''' </summary>
        ''' <param name="Vx">Vector of doubles containing the molar composition of the mixture.</param>
        ''' <param name="T">Temperature (K)</param>
        ''' <param name="P">Pressure (Pa)</param>
        ''' <param name="st">State enum indicating the state of the mixture (liquid or vapor).</param>
        ''' <returns>The entropy of the mixture in kJ/kg.K.</returns>
        ''' <remarks>The basis for the calculated enthalpy/entropy in DWSIM is zero at 25 C and 1 atm.</remarks>
        Public MustOverride Function DW_CalcEntropy(ByVal Vx As Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double

        ''' <summary>
        ''' Calculates the entropy departure of a mixture.
        ''' </summary>
        ''' <param name="Vx">Vector of doubles containing the molar composition of the mixture.</param>
        ''' <param name="T">Temperature (K)</param>
        ''' <param name="P">Pressure (Pa)</param>
        ''' <param name="st">State enum indicating the state of the mixture (liquid or vapor).</param>
        ''' <returns>The entropy departure of the mixture in kJ/kg.K.</returns>
        ''' <remarks>The basis for the calculated enthalpy/entropy in DWSIM is zero at 25 C and 1 atm.</remarks>
        Public MustOverride Function DW_CalcEntropyDeparture(ByVal Vx As Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double

        ''' <summary>
        ''' Calculates K-values of components in a mixture.
        ''' </summary>
        ''' <param name="Vx">Vector of doubles containing the molar composition of the liquid phase.</param>
        ''' <param name="Vy">Vector of doubles containing the molar composition of the vapor phase.</param>
        ''' <param name="T">Temperature of the system.</param>
        ''' <param name="P">Pressure of the system.</param>
        ''' <returns>An array containing K-values for all components in the mixture.</returns>
        ''' <remarks>The composition vector must follow the same sequence as the components which were added in the material stream.</remarks>
        Public Overridable Overloads Function DW_CalcKvalue(ByVal Vx As Double(), ByVal Vy As Double(), ByVal T As Double, ByVal P As Double, Optional ByVal type As String = "LV") As Double()

            'If Vx.HasNaN() Or Vx.HasInf() Then
            '    Throw New Exception(String.Format("Tried to calculate K-values with invalid liquid composition: {0}", Vx.ToArrayString()))
            'End If

            'If Vy.HasNaN() Or Vy.HasInf() Then
            '    Throw New Exception(String.Format("Tried to calculate K-values with invalid vapor composition: {0}", Vy.ToArrayString()))
            'End If

            'If Double.IsNaN(T) Or Double.IsInfinity(T) Then
            '    Throw New Exception(String.Format("Tried to calculate K-values with invalid temperature: {0} K", T))
            'End If

            'If Double.IsNaN(P) Or Double.IsInfinity(P) Then
            '    Throw New Exception(String.Format("Tried to calculate K-values with invalid pressure: {0} Pa", P))
            'End If

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "DW_CalcKvalue", ComponentName & " K-value calculation (Property Package)", "Property Package K-value Calculation Routine")

            IObj?.Paragraphs.Add("This is the K-value calculation routine which is called back from the Flash Algorithm during an equilibrium calculation. It is calculated for each compound as")

            IObj?.Paragraphs.Add("<math>K = \frac{y}{x} = \frac{\phi_{L} }{\phi_{V} }</math>")

            IObj?.Paragraphs.Add("where <math_inline>\phi_{L}</math_inline> and <math_inline>\phi_{V}</math_inline> are the fugacity coefficients of the compound on the liquid and vapor phase, respectively.")

            IObj?.Paragraphs.Add("The fugacity coefficients also depend on x and y, and are used to calculate updated K-values, which is why this routine is always called from a successive substitution procedure.")

            IObj?.Paragraphs.Add(String.Format("<h2>Input Parameters</h2>"))

            IObj?.Paragraphs.Add(String.Format("Temperature: {0} K", T))
            IObj?.Paragraphs.Add(String.Format("Pressure: {0} Pa", P))
            IObj?.Paragraphs.Add(String.Format("Phase 1 composition: {0}", Vx.ToMathArrayString()))
            IObj?.Paragraphs.Add(String.Format("Phase 2 composition: {0}", Vy.ToMathArrayString()))
            IObj?.Paragraphs.Add(String.Format("Calculation Type: {0}", type))

            Dim fugvap As Double() = Nothing
            Dim fugliq As Double() = Nothing

            If OverrideKvalFugCoeff Then

                IObj?.Paragraphs.Add(String.Format("Fugacity coefficient calculation overriden by user. Calling user-defined functions..."))

                fugliq = KvalFugacityCoefficientOverride.Invoke(Vx, T, P, State.Liquid, Me)
                If type = "LV" Then
                    fugvap = KvalFugacityCoefficientOverride.Invoke(Vy, T, P, State.Vapor, Me)
                Else ' LL
                    fugvap = KvalFugacityCoefficientOverride.Invoke(Vy, T, P, State.Liquid, Me)
                End If

            Else

                IObj?.SetCurrent()

                If Settings.EnableParallelProcessing Then

                    Dim t1 = Task.Run(Sub() fugliq = Me.DW_CalcFugCoeff(Vx, T, P, State.Liquid))

                    Dim t2 = Task.Run(Sub()
                                          If type = "LV" Then
                                              fugvap = Me.DW_CalcFugCoeff(Vy, T, P, State.Vapor)
                                          Else ' LL
                                              fugvap = Me.DW_CalcFugCoeff(Vy, T, P, State.Liquid)
                                          End If
                                      End Sub)

                    Task.WaitAll(t1, t2)

                Else

                    fugliq = Me.DW_CalcFugCoeff(Vx, T, P, State.Liquid)

                    IObj?.SetCurrent()

                    If type = "LV" Then
                        fugvap = Me.DW_CalcFugCoeff(Vy, T, P, State.Vapor)
                    Else ' LL
                        fugvap = Me.DW_CalcFugCoeff(Vy, T, P, State.Liquid)
                    End If

                End If

            End If

            IObj?.Paragraphs.Add(String.Format("<h2>Intermediate Calculated Parameters</h2>"))

            IObj?.Paragraphs.Add(String.Format("Phase 1 fugacity coefficients: {0}", fugliq.ToMathArrayString()))
            IObj?.Paragraphs.Add(String.Format("Phase 2 fugacity coefficients: {0}", fugvap.ToMathArrayString()))

            Dim n As Integer = fugvap.Length - 1
            Dim i As Integer
            Dim K(n) As Double

            K = fugliq.DivideY(fugvap)

            If Double.IsNaN(K.SumY) Or Double.IsInfinity(K.SumY) Or K.SumY = 0.0# Then
                Dim cprops = DW_GetConstantProperties()
                Dim Pc, Tc, w As Double
                For i = 0 To n
                    If K(i) = 0.0# Or Double.IsInfinity(K(i)) Or Double.IsNaN(K(i)) Then
                        Pc = cprops(i).Critical_Pressure
                        Tc = cprops(i).Critical_Temperature
                        w = cprops(i).Acentric_Factor
                        If type = "LV" Then
                            K(i) = Pc / P * Math.Exp(5.373 * (1 + w) * (1 - Tc / T))
                        Else
                            K(i) = 1.0#
                        End If
                    End If
                Next
            End If

            IObj?.Paragraphs.Add(String.Format("Calculated K-values: {0}", K.ToMathArrayString()))

            If Me.AUX_CheckTrivial(K) Then

                IObj?.Paragraphs.Add(String.Format("Trivial solution detected! Recalculating K-values..."))

                Dim Pc, Tc, w As Double

                Dim cprops = DW_GetConstantProperties()

                For i = 0 To n
                    Pc = cprops(i).Critical_Pressure
                    Tc = cprops(i).Critical_Temperature
                    w = cprops(i).Acentric_Factor
                    If type = "LV" Then
                        K(i) = Pc / P * Math.Exp(5.373 * (1 + w) * (1 - Tc / T))
                    Else
                        K(i) = 1.0
                    End If
                Next

            End If

            For i = 0 To n
                If K(i) = 0.0 Then K(i) = 1.0E-20
            Next

            IObj?.Paragraphs.Add(String.Format("<h2>Results</h2>"))

            IObj?.Paragraphs.Add(String.Format("Calculated K-values: {0}", K.ToMathArrayString()))

            IObj?.Close()

            Return K

        End Function

        ''' <summary>
        ''' Calculates K-values of components in a mixture.
        ''' </summary>
        ''' <param name="Vx">Vector of doubles containing the molar composition of the mixture.</param>
        ''' <param name="T">Temperature of the system, in K.</param>
        ''' <param name="P">Pressure of the system, in Pa.</param>
        ''' <returns>An array containing K-values for all components in the mixture.</returns>
        ''' <remarks>The composition vector must follow the same sequence as the components which were added in the material stream.</remarks>
        Public Overridable Overloads Function DW_CalcKvalue(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double) As Double()

            Dim i As Integer
            Dim result = Me.FlashBase.Flash_PT(Vx, P, T, Me)
            Dim n As Integer = Vx.Length - 1
            Dim K(n) As Double

            i = 0
            For Each subst As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(1).Compounds.Values
                K(i) = (result(3)(i) / result(2)(i))
                i += 1
            Next

            i = 0
            For Each subst As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(1).Compounds.Values
                If K(i) = 0 Then K(i) = Me.AUX_PVAPi(subst.Name, T) / P
                If Double.IsInfinity(K(i)) Or Double.IsNaN(K(i)) Then
                    Dim Pc, Tc, w As Double
                    Pc = subst.ConstantProperties.Critical_Pressure
                    Tc = subst.ConstantProperties.Critical_Temperature
                    w = subst.ConstantProperties.Acentric_Factor
                    K(i) = Pc / P * Math.Exp(5.373 * (1 + w) * (1 - Tc / T))
                End If
                i += 1
            Next

            If Me.AUX_CheckTrivial(K) Then
                For i = 0 To Vx.Length - 1
                    K(i) = Me.AUX_PVAPi(i, T) / P
                    i += 1
                Next
            End If

            Return K

        End Function

        ''' <summary>
        ''' Calculates Ideal K-values using Critical Properties.
        ''' </summary>
        ''' <param name="T"></param>
        ''' <param name="P"></param>
        ''' <returns></returns>
        Public Function DW_CalcKvalue_Ideal_Wilson(ByVal T As Double, ByVal P As Double) As Double()

            Dim n As Integer = CurrentMaterialStream.Phases(0).Compounds.Count - 1
            Dim i As Integer
            Dim K(n) As Double

            Dim cprops = DW_GetConstantProperties()

            Dim Pc, Tc, w As Double
            For i = 0 To n
                Pc = cprops(i).Critical_Pressure
                Tc = cprops(i).Critical_Temperature
                w = cprops(i).Acentric_Factor
                K(i) = Pc / P * Math.Exp(5.373 * (1 + w) * (1 - Tc / T))
            Next

            Return K

        End Function

        ''' <summary>
        ''' Calculates Ideal K-values using Vapor Pressures.
        ''' </summary>
        ''' <param name="T"></param>
        ''' <param name="P"></param>
        ''' <returns></returns>
        Public Function DW_CalcKvalue_Ideal_VP(ByVal T As Double, ByVal P As Double) As Double()

            Dim n As Integer = CurrentMaterialStream.Phases(0).Compounds.Count - 1
            Dim i As Integer
            Dim K(n) As Double

            For i = 0 To n
                K(i) = AUX_PVAPi(i, T) / P
            Next

            Return K

        End Function

        Public Function DW_CheckKvaluesConsistency(Vz As Double(), Ki As Double(), T As Double, P As Double) As Double()

            Dim Ki_id = DW_CalcKvalue_Ideal_Wilson(T, P)
            Dim Ki2 = RET_NullVector()

            Dim ratio = Ki.DivideY(Ki_id)

            For i = 0 To Vz.Length - 1
                If ratio(i) > 100000 Or ratio(i) < 0.001 And Vz(i) < 0.0001 Then
                    Ki2(i) = Ki_id(i)
                Else
                    Ki2(i) = Ki(i)
                End If
            Next

            Return Ki2

        End Function

        ''' <summary>
        ''' Does a Bubble Pressure calculation for the specified liquid composition at the specified temperature.
        ''' </summary>
        ''' <param name="Vx">Vector of doubles containing liquid phase molar composition for each component in the mixture.</param>
        ''' <param name="T">Temperature in K</param>
        ''' <param name="Pref">Initial estimate for Pressure, in Pa</param>
        ''' <param name="K">Vector with initial estimates for K-values</param>
        ''' <param name="ReuseK">Boolean indicating wether to use the initial estimates for K-values or not.</param>
        ''' <returns>Returns the object vector {L, V, Vx, Vy, P, ecount, Ki} where L is the liquid phase molar fraction, 
        ''' V is the vapor phase molar fraction, Vx is the liquid phase molar composition vector, Vy is the vapor phase molar 
        ''' composition vector, P is the calculated Pressure in Pa, ecount is the number of iterations and Ki is a vector containing 
        ''' the calculated K-values.</returns>
        ''' <remarks>The composition vector must follow the same sequence as the components which were added in the material stream.</remarks>
        Public Overridable Function DW_CalcBubP(ByVal Vx As System.Array, ByVal T As Double, Optional ByVal Pref As Double = 0, Optional ByVal K As System.Array = Nothing, Optional ByVal ReuseK As Boolean = False) As Object
            Return Me.FlashBase.Flash_TV(Vx, T, 0, Pref, Me, ReuseK, K)
        End Function

        ''' <summary>
        ''' Does a Bubble Temperature calculation for the specified liquid composition at the specified pressure.
        ''' </summary>
        ''' <param name="Vx">Vector of doubles containing liquid phase molar composition for each component in the mixture.</param>
        ''' <param name="P"></param>
        ''' <param name="Tref"></param>
        ''' <param name="K">Vector with initial estimates for K-values</param>
        ''' <param name="ReuseK">Boolean indicating wether to use the initial estimates for K-values or not.</param>
        ''' <returns>Returns the object vector {L, V, Vx, Vy, T, ecount, Ki} where L is the liquid phase molar fraction, 
        ''' V is the vapor phase molar fraction, Vx is the liquid phase molar composition vector, Vy is the vapor phase molar 
        ''' composition vector, T is the calculated Temperature in K, ecount is the number of iterations and Ki is a vector containing 
        ''' the calculated K-values.</returns>
        ''' <remarks>The composition vector must follow the same sequence as the components which were added in the material stream.</remarks>
        Public Overridable Function DW_CalcBubT(ByVal Vx As System.Array, ByVal P As Double, Optional ByVal Tref As Double = 0, Optional ByVal K As System.Array = Nothing, Optional ByVal ReuseK As Boolean = False) As Object
            Return Me.FlashBase.Flash_PV(Vx, P, 0, Tref, Me, ReuseK, K)
        End Function

        ''' <summary>
        ''' Does a Dew Pressure calculation for the specified vapor phase composition at the specified temperature.
        ''' </summary>
        ''' <param name="Vx">Vector of doubles containing vapor phase molar composition for each component in the mixture.</param>
        ''' <param name="T">Temperature in K</param>
        ''' <param name="Pref">Initial estimate for Pressure, in Pa</param>
        ''' <param name="K">Vector with initial estimates for K-values</param>
        ''' <param name="ReuseK">Boolean indicating wether to use the initial estimates for K-values or not.</param>
        ''' <returns>Returns the object vector {L, V, Vx, Vy, P, ecount, Ki} where L is the liquid phase molar fraction, 
        ''' V is the vapor phase molar fraction, Vx is the liquid phase molar composition vector, Vy is the vapor phase molar 
        ''' composition vector, P is the calculated Pressure in Pa, ecount is the number of iterations and Ki is a vector containing 
        ''' the calculated K-values.</returns>
        ''' <remarks>The composition vector must follow the same sequence as the components which were added in the material stream.</remarks>
        Public Overridable Function DW_CalcDewP(ByVal Vx As System.Array, ByVal T As Double, Optional ByVal Pref As Double = 0, Optional ByVal K As System.Array = Nothing, Optional ByVal ReuseK As Boolean = False) As Object
            Return Me.FlashBase.Flash_TV(Vx, T, 1, Pref, Me, ReuseK, K)
        End Function

        ''' <summary>
        ''' Does a Dew Temperature calculation for the specified vapor composition at the specified pressure.
        ''' </summary>
        ''' <param name="Vx">Vector of doubles containing vapor phase molar composition for each component in the mixture.</param>
        ''' <param name="P"></param>
        ''' <param name="Tref"></param>
        ''' <param name="K">Vector with initial estimates for K-values</param>
        ''' <param name="ReuseK">Boolean indicating wether to use the initial estimates for K-values or not.</param>
        ''' <returns>Returns the object vector {L, V, Vx, Vy, T, ecount, Ki} where L is the liquid phase molar fraction, 
        ''' V is the vapor phase molar fraction, Vx is the liquid phase molar composition vector, Vy is the vapor phase molar 
        ''' composition vector, T is the calculated Temperature in K, ecount is the number of iterations and Ki is a vector containing 
        ''' the calculated K-values.</returns>
        ''' <remarks>The composition vector must follow the same sequence as the components which were added in the material stream.</remarks>
        Public Overridable Function DW_CalcDewT(ByVal Vx As System.Array, ByVal P As Double, Optional ByVal Tref As Double = 0, Optional ByVal K As System.Array = Nothing, Optional ByVal ReuseK As Boolean = False) As Object
            Return Me.FlashBase.Flash_PV(Vx, P, 1, Tref, Me, ReuseK, K)
        End Function

        ''' <summary>
        ''' Calculates fugacity coefficients for the specified composition at the specified conditions.
        ''' </summary>
        ''' <param name="Vx">Vector of doubles containing the molar composition of the mixture.</param>
        ''' <param name="T">Temperature in K</param>
        ''' <param name="P">Pressure in Pa</param>
        ''' <param name="st">Mixture state (Liquid or Vapor)</param>
        ''' <returns>A vector of doubles containing fugacity coefficients for the components in the mixture.</returns>
        ''' <remarks>The composition vector must follow the same sequence as the components which were added in the material stream.</remarks>
        Public MustOverride Function DW_CalcFugCoeff(ByVal Vx As Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double()

        ''' <summary>
        ''' Calculates fugacity coefficients for the specified composition at the specified conditions.
        ''' </summary>
        ''' <param name="Vz">Vector of doubles containing the molar composition of the mixture.</param>
        ''' <param name="T">Temperature in K</param>
        ''' <param name="V">Volume in m3/mol</param>
        ''' <returns></returns>
        Public Overridable Function DW_CalcFugCoeff(Vz As Double(), T As Double, V As Double) As Double()

            Throw New NotImplementedException("Fugacity coefficient calculation with T and V is not implemented by this Property Package.")

        End Function

        ''' <summary>
        ''' Calculates system pressure for the specified temperature, volume and composition.
        ''' </summary>
        ''' <param name="Vz">Vector of doubles containing the molar composition of the mixture.</param>
        ''' <param name="T">Temperature in K</param>
        ''' <param name="V">Volume in m3/mol</param>
        ''' <returns></returns>
        Public Overridable Function DW_CalcP(Vz As Double(), T As Double, V As Double) As Double

            Throw New NotImplementedException("Pressure calculation with T and V is not implemented by this Property Package.")

        End Function

        ''' <summary>
        ''' Calculate the critical points of the current Material Stream
        ''' </summary>
        ''' <returns>List of critical points in the following order: T in K, P in Pa and V in m3/mol</returns>
        Public Overridable Function DW_CalculateCriticalPoints() As List(Of Double())

            'use generic method

            Dim gm As New Utilities.TCP.GenericMethod

            gm.CalcP = Function(T, V, Vzi)
                           Return DW_CalcP(Vzi, T, V)
                       End Function

            gm.FugacityTV = Function(T, V, Vzi)
                                Return DW_CalcFugCoeff(Vzi, T, V)
                            End Function

            Dim Vz = RET_VMOL(Phase.Mixture)

            Dim VTc = RET_VTC()
            Dim VPc = RET_VPC()

            Dim V0 = 0.08664 * 8.314 * VTc.DivideY(VPc).MultiplyY(Vz).SumY
            Dim T0 = VTc.MultiplyY(Vz).SumY

            Return gm.CriticalPoint(Vz, V0, T0)

        End Function

        Public MustOverride Function SupportsComponent(ByVal comp As Interfaces.ICompoundConstantProperties) As Boolean

        Public MustOverride Sub DW_CalcPhaseProps(ByVal Phase As Phase)

        Public Overridable Sub DW_CalcTwoPhaseProps(ByVal Phase1 As PropertyPackages.Phase, ByVal Phase2 As PropertyPackages.Phase)

            Dim T As Double

            T = Me.CurrentMaterialStream.Phases(0).Properties.temperature.GetValueOrDefault
            Me.CurrentMaterialStream.Phases(0).Properties.surfaceTension = Me.AUX_SURFTM(T)

            Me.CurrentMaterialStream.Phases(1).Properties.surfaceTension = Me.AUX_SURFTM(1, T)
            Me.CurrentMaterialStream.Phases(3).Properties.surfaceTension = Me.AUX_SURFTM(3, T)
            Me.CurrentMaterialStream.Phases(4).Properties.surfaceTension = Me.AUX_SURFTM(4, T)

        End Sub

        Public Function DW_CalcGibbsEnergy(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, Optional ByVal forcephase As String = "") As Double

            Dim fugvap As Object = Nothing
            Dim fugliq As Object = Nothing

            If Settings.EnableParallelProcessing Then

                Dim task1 As Task = TaskHelper.Run(Sub()
                                                       fugliq = Me.DW_CalcFugCoeff(Vx, T, P, State.Liquid)
                                                   End Sub)
                Dim task2 As Task = TaskHelper.Run(Sub()
                                                       fugvap = Me.DW_CalcFugCoeff(Vx, T, P, State.Vapor)
                                                   End Sub)
                Task.WaitAll(task1, task2)

            Else
                fugliq = Me.DW_CalcFugCoeff(Vx, T, P, State.Liquid)
                fugvap = Me.DW_CalcFugCoeff(Vx, T, P, State.Vapor)
            End If

            Dim n As Integer = Vx.Length - 1
            Dim i As Integer

            Dim g, gid, gexv, gexl As Double

            gid = 0.0#

            gexv = 0.0#
            gexl = 0.0#
            For i = 0 To n
                If Vx(i) <> 0.0# Then gexv += Vx(i) * Log(fugvap(i)) * 8.314 * T
                If Vx(i) <> 0.0# Then gexl += Vx(i) * Log(fugliq(i)) * 8.314 * T
            Next

            If forcephase = "L" Then
                g = gid + gexl
            ElseIf forcephase = "V" Then
                g = gid + gexv
            Else
                If gexv < gexl Then g = gid + gexv Else g = gid + gexl
            End If

            Return g 'kJ/kmol

        End Function
        Public Sub DW_CalcConcentrations()
            Dim CF, MF, VF As Double

            For PhaseID = 0 To Me.CurrentMaterialStream.Phases.Count - 1
                VF = Me.CurrentMaterialStream.Phases(PhaseID).Properties.volumetric_flow.GetValueOrDefault
                If Me.CurrentMaterialStream.Phases(PhaseID).Compounds.ContainsKey(Me.CurrentMaterialStream.ReferenceSolvent) Then
                    MF = Me.CurrentMaterialStream.Phases(PhaseID).Compounds(Me.CurrentMaterialStream.ReferenceSolvent).MassFraction.GetValueOrDefault * Me.CurrentMaterialStream.Phases(PhaseID).Properties.massflow.GetValueOrDefault
                End If

                For Each C In Me.CurrentMaterialStream.Phases(PhaseID).Compounds.Values
                    CF = C.MoleFraction.GetValueOrDefault * Me.CurrentMaterialStream.Phases(PhaseID).Properties.molarflow.GetValueOrDefault

                    'molarity = mol solute per liter in phase
                    If VF > 0 Then
                        C.Molarity = CF / VF
                    Else
                        C.Molarity = 0
                    End If

                    'Molality = mol solute per 1000 g of solvent
                    If MF > 0 Then
                        C.Molality = CF / MF
                    Else
                        C.Molality = 0
                    End If
                Next
            Next

        End Sub

        Public Overridable Sub DW_CalcOverallProps()

            Dim HL, HV, HS, SL, SV, SS, DL, DV, DS, CPL, CPV, CPS, KL, KV, KS, CVL, CVV, CSV As Nullable(Of Double)
            Dim UL, UV, US, GL, GV, GS, AL, AV, AS_ As Double
            Dim xl, xv, xs, wl, wv, ws, result As Double

            xl = Me.CurrentMaterialStream.Phases(1).Properties.molarfraction.GetValueOrDefault
            xv = Me.CurrentMaterialStream.Phases(2).Properties.molarfraction.GetValueOrDefault
            xs = Me.CurrentMaterialStream.Phases(7).Properties.molarfraction.GetValueOrDefault

            wl = Me.CurrentMaterialStream.Phases(1).Properties.massfraction.GetValueOrDefault
            wv = Me.CurrentMaterialStream.Phases(2).Properties.massfraction.GetValueOrDefault
            ws = Me.CurrentMaterialStream.Phases(7).Properties.massfraction.GetValueOrDefault

            DL = Me.CurrentMaterialStream.Phases(1).Properties.density.GetValueOrDefault
            DV = Me.CurrentMaterialStream.Phases(2).Properties.density.GetValueOrDefault
            DS = Me.CurrentMaterialStream.Phases(7).Properties.density.GetValueOrDefault

            If Double.IsNaN(DL) Then DL = 0.0#

            Dim tl As Double = 0.0#
            Dim tv As Double = 0.0#
            Dim ts As Double = 0.0#

            If DL <> 0.0# And Not Double.IsNaN(DL) Then tl = Me.CurrentMaterialStream.Phases(1).Properties.massfraction.GetValueOrDefault / DL.GetValueOrDefault
            If DV <> 0.0# And Not Double.IsNaN(DV) Then tv = Me.CurrentMaterialStream.Phases(2).Properties.massfraction.GetValueOrDefault / DV.GetValueOrDefault
            If DS <> 0.0# And Not Double.IsNaN(DS) Then ts = Me.CurrentMaterialStream.Phases(7).Properties.massfraction.GetValueOrDefault / DS.GetValueOrDefault

            result = 0.0
            If wl > 0.0 Then result += wl / DL.GetValueOrDefault()
            If wv > 0.0 Then result += wv / DV.GetValueOrDefault()
            If ws > 0.0 Then result += ws / DS.GetValueOrDefault()

            result = 1 / result
            Me.CurrentMaterialStream.Phases(0).Properties.density = result

            HL = Me.CurrentMaterialStream.Phases(1).Properties.enthalpy.GetValueOrDefault
            HV = Me.CurrentMaterialStream.Phases(2).Properties.enthalpy.GetValueOrDefault
            HS = Me.CurrentMaterialStream.Phases(7).Properties.enthalpy.GetValueOrDefault

            result = wl * HL.GetValueOrDefault + wv * HV.GetValueOrDefault + ws * HS.GetValueOrDefault
            Me.CurrentMaterialStream.Phases(0).Properties.enthalpy = result

            SL = Me.CurrentMaterialStream.Phases(1).Properties.entropy.GetValueOrDefault
            SV = Me.CurrentMaterialStream.Phases(2).Properties.entropy.GetValueOrDefault
            SS = Me.CurrentMaterialStream.Phases(7).Properties.entropy.GetValueOrDefault

            result = wl * SL.GetValueOrDefault + wv * SV.GetValueOrDefault + ws * SS.GetValueOrDefault
            Me.CurrentMaterialStream.Phases(0).Properties.entropy = result

            UL = Me.CurrentMaterialStream.Phases(1).Properties.internal_energy.GetValueOrDefault
            UV = Me.CurrentMaterialStream.Phases(2).Properties.internal_energy.GetValueOrDefault
            US = Me.CurrentMaterialStream.Phases(7).Properties.internal_energy.GetValueOrDefault

            result = wl * UL + wv * UV + ws * US
            Me.CurrentMaterialStream.Phases(0).Properties.internal_energy = result

            GL = Me.CurrentMaterialStream.Phases(1).Properties.gibbs_free_energy.GetValueOrDefault
            GV = Me.CurrentMaterialStream.Phases(2).Properties.gibbs_free_energy.GetValueOrDefault
            GS = Me.CurrentMaterialStream.Phases(7).Properties.gibbs_free_energy.GetValueOrDefault

            result = wl * GL + wv * GV + ws * GS
            Me.CurrentMaterialStream.Phases(0).Properties.gibbs_free_energy = result

            AL = Me.CurrentMaterialStream.Phases(1).Properties.helmholtz_energy.GetValueOrDefault
            AV = Me.CurrentMaterialStream.Phases(2).Properties.helmholtz_energy.GetValueOrDefault
            AS_ = Me.CurrentMaterialStream.Phases(7).Properties.helmholtz_energy.GetValueOrDefault

            result = wl * AL + wv * AV + ws * AS_
            Me.CurrentMaterialStream.Phases(0).Properties.helmholtz_energy = result

            Me.CurrentMaterialStream.Phases(0).Properties.compressibilityFactor = Nothing

            CPL = Me.CurrentMaterialStream.Phases(1).Properties.heatCapacityCp.GetValueOrDefault
            CPV = Me.CurrentMaterialStream.Phases(2).Properties.heatCapacityCp.GetValueOrDefault
            CPS = Me.CurrentMaterialStream.Phases(7).Properties.heatCapacityCp.GetValueOrDefault

            result = wl * CPL.GetValueOrDefault + wv * CPV.GetValueOrDefault + ws * CPS.GetValueOrDefault

            Me.CurrentMaterialStream.Phases(0).Properties.heatCapacityCp = result

            CVL = Me.CurrentMaterialStream.Phases(1).Properties.heatCapacityCv.GetValueOrDefault
            CVV = Me.CurrentMaterialStream.Phases(2).Properties.heatCapacityCv.GetValueOrDefault
            CSV = Me.CurrentMaterialStream.Phases(7).Properties.heatCapacityCv.GetValueOrDefault

            result = wl * CVL.GetValueOrDefault + wv * CVV.GetValueOrDefault + ws * CSV.GetValueOrDefault

            Me.CurrentMaterialStream.Phases(0).Properties.heatCapacityCv = result

            result = Me.AUX_MMM(Phase.Mixture)
            Me.CurrentMaterialStream.Phases(0).Properties.molecularWeight = result

            result = Me.CurrentMaterialStream.Phases(0).Properties.enthalpy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(0).Properties.molecularWeight.GetValueOrDefault
            Me.CurrentMaterialStream.Phases(0).Properties.molar_enthalpy = result
            result = Me.CurrentMaterialStream.Phases(0).Properties.entropy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(0).Properties.molecularWeight.GetValueOrDefault
            Me.CurrentMaterialStream.Phases(0).Properties.molar_entropy = result
            result = Me.CurrentMaterialStream.Phases(0).Properties.internal_energy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(0).Properties.molecularWeight.GetValueOrDefault
            Me.CurrentMaterialStream.Phases(0).Properties.molar_internal_energy = result
            result = Me.CurrentMaterialStream.Phases(0).Properties.gibbs_free_energy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(0).Properties.molecularWeight.GetValueOrDefault
            Me.CurrentMaterialStream.Phases(0).Properties.molar_gibbs_free_energy = result
            result = Me.CurrentMaterialStream.Phases(0).Properties.helmholtz_energy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(0).Properties.molecularWeight.GetValueOrDefault
            Me.CurrentMaterialStream.Phases(0).Properties.molar_helmholtz_energy = result

            KL = Me.CurrentMaterialStream.Phases(1).Properties.thermalConductivity.GetValueOrDefault
            KV = Me.CurrentMaterialStream.Phases(2).Properties.thermalConductivity.GetValueOrDefault
            KS = Me.CurrentMaterialStream.Phases(7).Properties.thermalConductivity.GetValueOrDefault

            result = xl * KL.GetValueOrDefault + xv * KV.GetValueOrDefault + xs * KS.GetValueOrDefault
            Me.CurrentMaterialStream.Phases(0).Properties.thermalConductivity = result

            Me.CurrentMaterialStream.Phases(0).Properties.viscosity = Nothing

            Me.CurrentMaterialStream.Phases(0).Properties.kinematic_viscosity = Nothing

            Dim P As Double = Me.CurrentMaterialStream.Phases(0).Properties.pressure.GetValueOrDefault
            Dim T As Double = Me.CurrentMaterialStream.Phases(0).Properties.temperature.GetValueOrDefault

            If Not Settings.CAPEOPENMode And Not TypeOf Me Is CAPEOPENPropertyPackage Then
                If Me.FlashBase.FlashSettings(Enums.FlashSetting.CalculateBubbleAndDewPoints) Then
                    Try
                        Dim Vz As Double() = Me.RET_VMOL(Phase.Mixture)
                        Dim myres As Object = Me.FlashBase.Flash_PV(Vz, P, 0, T, Me)
                        'check if liquid phase is stable.
                        Dim myres2 As Object = Me.FlashBase.Flash_PT(Vz, P, myres(4), Me)
                        If myres2(5) > 0.0# Then
                            If Abs(myres2(2)(0) - myres2(6)(0)) > 0.01 Then
                                Me.CurrentMaterialStream.Phases(0).Properties.bubbleTemperature = Me.FlashBase.BubbleTemperature_LLE(Vz, myres2(2), myres2(6), P, myres(4) - 40, myres(4) + 40, Me)
                            End If
                        Else
                            Me.CurrentMaterialStream.Phases(0).Properties.bubbleTemperature = myres(4)
                        End If
                    Catch ex As Exception
                        Me.CurrentMaterialStream.Flowsheet.ShowMessage("Bubble Temperature calculation error: " & ex.Message.ToString, Interfaces.IFlowsheet.MessageType.GeneralError)
                    End Try
                    Try
                        Dim Vz As Double() = Me.RET_VMOL(Phase.Mixture)
                        result = Me.FlashBase.Flash_PV(Vz, P, 1.0, T, Me)(4)
                        Me.CurrentMaterialStream.Phases(0).Properties.dewTemperature = result
                    Catch ex As Exception
                        Me.CurrentMaterialStream.Flowsheet.ShowMessage("Dew Temperature calculation error: " & ex.Message.ToString, Interfaces.IFlowsheet.MessageType.GeneralError)
                    End Try
                    Try
                        Dim Vz As Double() = Me.RET_VMOL(Phase.Mixture)
                        Dim myres As Object = Me.FlashBase.Flash_TV(Vz, T, 0, P, Me)
                        'check if liquid phase is stable.
                        Dim myres2 As Object = Me.FlashBase.Flash_PT(Vz, myres(4), T, Me)
                        If myres2(5) > 0.0# Then
                            If Abs(myres2(2)(0) - myres2(6)(0)) > 0.01 Then
                                Me.CurrentMaterialStream.Phases(0).Properties.bubblePressure = Me.FlashBase.BubblePressure_LLE(Vz, myres2(2), myres2(6), myres(4), T, Me)
                            End If
                        Else
                            Me.CurrentMaterialStream.Phases(0).Properties.bubblePressure = myres(4)
                        End If
                    Catch ex As Exception
                        Me.CurrentMaterialStream.Flowsheet.ShowMessage("Bubble Pressure calculation error: " & ex.Message.ToString, Interfaces.IFlowsheet.MessageType.GeneralError)
                    End Try
                    Try
                        Dim Vz As Double() = Me.RET_VMOL(Phase.Mixture)
                        result = Me.FlashBase.Flash_TV(Vz, T, 1.0, P, Me)(4)
                        Me.CurrentMaterialStream.Phases(0).Properties.dewPressure = result
                    Catch ex As Exception
                        Me.CurrentMaterialStream.Flowsheet.ShowMessage("Dew Pressure calculation error: " & ex.Message.ToString, Interfaces.IFlowsheet.MessageType.GeneralError)
                    End Try
                End If
            End If

        End Sub

        Public Overridable Sub DW_CalcOverallDensity()

            Dim DL1, DL2, DL3, DL4, DV, DS As Nullable(Of Double)
            Dim xl1, xl2, xl3, xl4, xv, xs, wl1, wl2, wl3, wl4, wv, ws, vl1, vl2, vl3, vl4, vv, vs, result As Double

            xl1 = Me.CurrentMaterialStream.Phases(3).Properties.molarfraction.GetValueOrDefault
            xl2 = Me.CurrentMaterialStream.Phases(4).Properties.molarfraction.GetValueOrDefault
            xl3 = Me.CurrentMaterialStream.Phases(5).Properties.molarfraction.GetValueOrDefault
            xl4 = Me.CurrentMaterialStream.Phases(6).Properties.molarfraction.GetValueOrDefault
            xv = Me.CurrentMaterialStream.Phases(2).Properties.molarfraction.GetValueOrDefault
            xs = Me.CurrentMaterialStream.Phases(7).Properties.molarfraction.GetValueOrDefault

            wl1 = Me.CurrentMaterialStream.Phases(3).Properties.massfraction.GetValueOrDefault
            wl2 = Me.CurrentMaterialStream.Phases(4).Properties.massfraction.GetValueOrDefault
            wl3 = Me.CurrentMaterialStream.Phases(5).Properties.massfraction.GetValueOrDefault
            wl4 = Me.CurrentMaterialStream.Phases(6).Properties.massfraction.GetValueOrDefault
            wv = Me.CurrentMaterialStream.Phases(2).Properties.massfraction.GetValueOrDefault
            ws = Me.CurrentMaterialStream.Phases(7).Properties.massfraction.GetValueOrDefault

            Me.DW_CalcProp("density", Phase.Liquid1)
            Me.DW_CalcProp("density", Phase.Liquid2)
            Me.DW_CalcProp("density", Phase.Liquid3)
            Me.DW_CalcProp("density", Phase.Aqueous)
            Me.DW_CalcProp("density", Phase.Vapor)

            DL1 = Me.CurrentMaterialStream.Phases(3).Properties.density.GetValueOrDefault
            DL2 = Me.CurrentMaterialStream.Phases(4).Properties.density.GetValueOrDefault
            DL3 = Me.CurrentMaterialStream.Phases(5).Properties.density.GetValueOrDefault
            DL4 = Me.CurrentMaterialStream.Phases(6).Properties.density.GetValueOrDefault
            DV = Me.CurrentMaterialStream.Phases(2).Properties.density.GetValueOrDefault
            DS = Me.AUX_SOLIDDENS()

            If Double.IsNaN(DL1) Or Double.IsInfinity(DL1) Then DL1 = 0.0#
            If Double.IsNaN(DL2) Or Double.IsInfinity(DL2) Then DL2 = 0.0#
            If Double.IsNaN(DL3) Or Double.IsInfinity(DL3) Then DL3 = 0.0#
            If Double.IsNaN(DL4) Or Double.IsInfinity(DL4) Then DL4 = 0.0#
            If Double.IsNaN(DV) Or Double.IsInfinity(DV) Then DV = 0.0#
            If Double.IsNaN(DS) Or Double.IsInfinity(DS) Then DS = 0.0#

            Dim tl1 As Double = 0.0#
            Dim tl2 As Double = 0.0#
            Dim tl3 As Double = 0.0#
            Dim tl4 As Double = 0.0#
            Dim tv As Double = 0.0#
            Dim ts As Double = 0.0#

            If DL1 <> 0.0# And Not Double.IsNaN(DL1) Then tl1 = Me.CurrentMaterialStream.Phases(3).Properties.massfraction.GetValueOrDefault / DL1.GetValueOrDefault
            If DL2 <> 0.0# And Not Double.IsNaN(DL2) Then tl2 = Me.CurrentMaterialStream.Phases(4).Properties.massfraction.GetValueOrDefault / DL2.GetValueOrDefault
            If DL3 <> 0.0# And Not Double.IsNaN(DL3) Then tl3 = Me.CurrentMaterialStream.Phases(5).Properties.massfraction.GetValueOrDefault / DL3.GetValueOrDefault
            If DL4 <> 0.0# And Not Double.IsNaN(DL4) Then tl4 = Me.CurrentMaterialStream.Phases(6).Properties.massfraction.GetValueOrDefault / DL4.GetValueOrDefault
            If DV <> 0.0# And Not Double.IsNaN(DV) Then tv = Me.CurrentMaterialStream.Phases(2).Properties.massfraction.GetValueOrDefault / DV.GetValueOrDefault
            If DS <> 0.0# And Not Double.IsNaN(DS) Then ts = Me.CurrentMaterialStream.Phases(7).Properties.massfraction.GetValueOrDefault / DS.GetValueOrDefault

            vl1 = tl1 / (tl1 + tl2 + tl3 + tl4 + tv + ts)
            vl2 = tl2 / (tl1 + tl2 + tl3 + tl4 + tv + ts)
            vl3 = tl3 / (tl1 + tl2 + tl3 + tl4 + tv + ts)
            vl4 = tl4 / (tl1 + tl2 + tl3 + tl4 + tv + ts)
            vv = tv / (tl1 + tl2 + tl3 + tl4 + tv + ts)
            vs = ts / (tl1 + tl2 + tl3 + tl4 + tv + ts)

            If xl1 = 1 Then
                vl1 = 1
                vl2 = 0
                vl3 = 0
                vl4 = 0
                vv = 0
                vs = 0
            ElseIf xl2 = 1 Then
                vl1 = 0
                vl2 = 1
                vl3 = 0
                vl4 = 0
                vv = 1
                vs = 0
            ElseIf xl3 = 1 Then
                vl1 = 0
                vl2 = 0
                vl3 = 1
                vl4 = 0
                vv = 1
                vs = 0
            ElseIf xl4 = 1 Then
                vl1 = 0
                vl2 = 0
                vl3 = 0
                vl4 = 1
                vv = 1
                vs = 0
            ElseIf xv = 1 Then
                vl1 = 0
                vl2 = 0
                vl3 = 0
                vl4 = 0
                vv = 1
                vs = 0
            ElseIf xs = 1 Then
                vl1 = 0
                vl2 = 0
                vl3 = 0
                vl4 = 0
                vv = 0
                vs = 1
            End If

            result = vl1 * DL1.GetValueOrDefault + vl2 * DL2.GetValueOrDefault + vl3 * DL3.GetValueOrDefault + vl4 * DL4.GetValueOrDefault + vv * DV.GetValueOrDefault + vs * DS.GetValueOrDefault
            Me.CurrentMaterialStream.Phases(0).Properties.density = result

        End Sub

        Public Overridable Sub DW_CalcLiqMixtureProps()

            Dim nullstream As Boolean = False

            Dim hl, hl1, hl2, hl3, hw, sl, sl1, sl2, sl3, sw, dl, dl1, dl2, dl3, dw As Double
            Dim ul, ul1, ul2, ul3, uw, gl, gl1, gl2, gl3, gw, al, al1, al2, al3, aw As Double
            Dim cpl, cpl1, cpl2, cpl3, cpw, cvl, cvl1, cvl2, cvl3, cvw As Double
            Dim kl, kl1, kl2, kl3, kw, vil, vil1, vil2, vil3, viw As Double
            Dim xl, xl1, xl2, xl3, xw, wl, wl1, wl2, wl3, ww As Double
            Dim xlf, xlf1, xlf2, xlf3, xwf, wlf, wlf1, wlf2, wlf3, wwf As Double
            Dim cml, cml1, cml2, cml3, cmw, cwl, cwl1, cwl2, cwl3, cww As Double
            Dim phi1, phi2, phi3, phiw, Ql As Double

            xl1 = Me.CurrentMaterialStream.Phases(3).Properties.molarfraction.GetValueOrDefault
            xl2 = Me.CurrentMaterialStream.Phases(4).Properties.molarfraction.GetValueOrDefault
            xl3 = Me.CurrentMaterialStream.Phases(5).Properties.molarfraction.GetValueOrDefault
            xw = Me.CurrentMaterialStream.Phases(6).Properties.molarfraction.GetValueOrDefault

            If Not xl1.IsValid Then xl1 = 0.0#
            If Not xl2.IsValid Then xl2 = 0.0#
            If Not xl3.IsValid Then xl3 = 0.0#
            If Not xw.IsValid Then xw = 0.0#

            xl = xl1 + xl2 + xl3 + xw
            Me.CurrentMaterialStream.Phases(1).Properties.molarfraction = xl

            wl1 = Me.CurrentMaterialStream.Phases(3).Properties.massfraction.GetValueOrDefault
            wl2 = Me.CurrentMaterialStream.Phases(4).Properties.massfraction.GetValueOrDefault
            wl3 = Me.CurrentMaterialStream.Phases(5).Properties.massfraction.GetValueOrDefault
            ww = Me.CurrentMaterialStream.Phases(6).Properties.massfraction.GetValueOrDefault

            If Not wl1.IsValid Then wl1 = 0.0#
            If Not wl2.IsValid Then wl2 = 0.0#
            If Not wl3.IsValid Then wl3 = 0.0#
            If Not ww.IsValid Then ww = 0.0#

            wl = wl1 + wl2 + wl3 + ww
            Me.CurrentMaterialStream.Phases(1).Properties.massfraction = wl

            xlf1 = Me.CurrentMaterialStream.Phases(3).Properties.molarflow.GetValueOrDefault
            xlf2 = Me.CurrentMaterialStream.Phases(4).Properties.molarflow.GetValueOrDefault
            xlf3 = Me.CurrentMaterialStream.Phases(5).Properties.molarflow.GetValueOrDefault
            xwf = Me.CurrentMaterialStream.Phases(6).Properties.molarflow.GetValueOrDefault

            If Not xlf1.IsValid Then xlf1 = 0.0#
            If Not xlf2.IsValid Then xlf2 = 0.0#
            If Not xlf3.IsValid Then xlf3 = 0.0#
            If Not xwf.IsValid Then xwf = 0.0#

            xlf = xlf1 + xlf2 + xlf3 + xwf

            Me.CurrentMaterialStream.Phases(1).Properties.molarflow = xlf

            If xlf = 0.0 Then
                xlf1 = 0.000001
                xlf2 = 0.000001
                xlf3 = 0.000001
                xwf = 0.000001
                xlf = 0.000001
                nullstream = True
            End If

            wlf1 = Me.CurrentMaterialStream.Phases(3).Properties.massflow.GetValueOrDefault
            wlf2 = Me.CurrentMaterialStream.Phases(4).Properties.massflow.GetValueOrDefault
            wlf3 = Me.CurrentMaterialStream.Phases(5).Properties.massflow.GetValueOrDefault
            wwf = Me.CurrentMaterialStream.Phases(6).Properties.massflow.GetValueOrDefault

            If Not wlf1.IsValid Then wlf1 = 0.0#
            If Not wlf2.IsValid Then wlf2 = 0.0#
            If Not wlf3.IsValid Then wlf3 = 0.0#
            If Not wwf.IsValid Then wwf = 0.0#

            wlf = wlf1 + wlf2 + wlf3 + wwf

            Me.CurrentMaterialStream.Phases(1).Properties.massflow = wlf

            If wlf = 0.0 Then
                wlf1 = 0.000001
                wlf2 = 0.000001
                wlf3 = 0.000001
                wwf = 0.000001
                wlf = 0.000001
                nullstream = True
            End If

            For Each c As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(1).Compounds.Values

                cml1 = Me.CurrentMaterialStream.Phases(3).Compounds(c.Name).MoleFraction.GetValueOrDefault
                cml2 = Me.CurrentMaterialStream.Phases(4).Compounds(c.Name).MoleFraction.GetValueOrDefault
                cml3 = Me.CurrentMaterialStream.Phases(5).Compounds(c.Name).MoleFraction.GetValueOrDefault
                cmw = Me.CurrentMaterialStream.Phases(6).Compounds(c.Name).MoleFraction.GetValueOrDefault

                If Not cml1.IsValid Then cml1 = 0.0#
                If Not cml2.IsValid Then cml2 = 0.0#
                If Not cml3.IsValid Then cml3 = 0.0#
                If Not cmw.IsValid Then cmw = 0.0#

                cwl1 = Me.CurrentMaterialStream.Phases(3).Compounds(c.Name).MassFraction.GetValueOrDefault
                cwl2 = Me.CurrentMaterialStream.Phases(4).Compounds(c.Name).MassFraction.GetValueOrDefault
                cwl3 = Me.CurrentMaterialStream.Phases(5).Compounds(c.Name).MassFraction.GetValueOrDefault
                cww = Me.CurrentMaterialStream.Phases(6).Compounds(c.Name).MassFraction.GetValueOrDefault

                If Not cwl1.IsValid Then cwl1 = 0.0#
                If Not cwl2.IsValid Then cwl2 = 0.0#
                If Not cwl3.IsValid Then cwl3 = 0.0#
                If Not cww.IsValid Then cww = 0.0#

                cml = (xl1 * cml1 + xl2 * cml2 + xl3 * cml3 + xw * cmw) / xl
                cwl = (wl1 * cwl1 + wl2 * cwl2 + wl3 * cwl3 + ww * cww) / wl

                c.MoleFraction = cml
                c.MassFraction = cwl

            Next

            If wl1 > 0 Then dl1 = Me.CurrentMaterialStream.Phases(3).Properties.density.GetValueOrDefault Else dl1 = 1
            If wl2 > 0 Then dl2 = Me.CurrentMaterialStream.Phases(4).Properties.density.GetValueOrDefault Else dl2 = 1
            If wl3 > 0 Then dl3 = Me.CurrentMaterialStream.Phases(5).Properties.density.GetValueOrDefault Else dl3 = 1
            If ww > 0 Then dw = Me.CurrentMaterialStream.Phases(6).Properties.density.GetValueOrDefault Else dw = 1

            If wl1 > 0.0 And wl2 + wl3 + ww = 0.0 Then
                CurrentMaterialStream.Phases(1).Properties.compressibilityFactor = CurrentMaterialStream.Phases(3).Properties.compressibilityFactor.GetValueOrDefault
            ElseIf wl2 > 0.0 And wl1 + wl3 + ww = 0.0 Then
                CurrentMaterialStream.Phases(1).Properties.compressibilityFactor = CurrentMaterialStream.Phases(4).Properties.compressibilityFactor.GetValueOrDefault
            ElseIf ww > 0.0 And wl1 + wl2 + wl3 = 0.0 Then
                CurrentMaterialStream.Phases(1).Properties.compressibilityFactor = CurrentMaterialStream.Phases(6).Properties.compressibilityFactor.GetValueOrDefault
            End If

            dl = wl1 / dl1 + wl2 / dl2 + wl3 / dl3 + ww / dw
            dl = wl / dl

            If nullstream Then
                dl = Me.CurrentMaterialStream.Phases(3).Properties.density.GetValueOrDefault
            End If

            Me.CurrentMaterialStream.Phases(1).Properties.density = dl

            If Double.IsNaN(wlf / dl) Then
                Me.CurrentMaterialStream.Phases(1).Properties.volumetric_flow = 0.0#
            Else
                If nullstream Then
                    Me.CurrentMaterialStream.Phases(1).Properties.volumetric_flow = 0.0
                Else
                    Me.CurrentMaterialStream.Phases(1).Properties.volumetric_flow = wlf / dl
                End If
            End If

            If wl = 0 Then wl = 1.0#

            hl1 = Me.CurrentMaterialStream.Phases(3).Properties.enthalpy.GetValueOrDefault
            hl2 = Me.CurrentMaterialStream.Phases(4).Properties.enthalpy.GetValueOrDefault
            hl3 = Me.CurrentMaterialStream.Phases(5).Properties.enthalpy.GetValueOrDefault
            hw = Me.CurrentMaterialStream.Phases(6).Properties.enthalpy.GetValueOrDefault

            hl = hl1 * wl1 + hl2 * wl2 + hl3 * wl3 + hw * ww

            If Double.IsNaN(hl) Then hl = 0.0

            If nullstream Then
                Me.CurrentMaterialStream.Phases(1).Properties.enthalpy = hl1
            Else
                Me.CurrentMaterialStream.Phases(1).Properties.enthalpy = hl / wl
            End If

            sl1 = Me.CurrentMaterialStream.Phases(3).Properties.entropy.GetValueOrDefault
            sl2 = Me.CurrentMaterialStream.Phases(4).Properties.entropy.GetValueOrDefault
            sl3 = Me.CurrentMaterialStream.Phases(5).Properties.entropy.GetValueOrDefault
            sw = Me.CurrentMaterialStream.Phases(6).Properties.entropy.GetValueOrDefault

            sl = sl1 * wl1 + sl2 * wl2 + sl3 * wl3 + sw * ww

            If Double.IsNaN(sl) Then sl = 0.0

            If nullstream Then
                Me.CurrentMaterialStream.Phases(1).Properties.entropy = sl1
            Else
                Me.CurrentMaterialStream.Phases(1).Properties.entropy = sl / wl
            End If

            ul1 = Me.CurrentMaterialStream.Phases(3).Properties.internal_energy.GetValueOrDefault
            ul2 = Me.CurrentMaterialStream.Phases(4).Properties.internal_energy.GetValueOrDefault
            ul3 = Me.CurrentMaterialStream.Phases(5).Properties.internal_energy.GetValueOrDefault
            uw = Me.CurrentMaterialStream.Phases(6).Properties.internal_energy.GetValueOrDefault

            ul = ul1 * wl1 + ul2 * wl2 + ul3 * wl3 + uw * ww

            If Double.IsNaN(ul) Then ul = 0.0

            If nullstream Then
                Me.CurrentMaterialStream.Phases(1).Properties.internal_energy = ul1
            Else
                Me.CurrentMaterialStream.Phases(1).Properties.internal_energy = ul / wl
            End If

            gl1 = Me.CurrentMaterialStream.Phases(3).Properties.gibbs_free_energy.GetValueOrDefault
            gl2 = Me.CurrentMaterialStream.Phases(4).Properties.gibbs_free_energy.GetValueOrDefault
            gl3 = Me.CurrentMaterialStream.Phases(5).Properties.gibbs_free_energy.GetValueOrDefault
            gw = Me.CurrentMaterialStream.Phases(6).Properties.gibbs_free_energy.GetValueOrDefault

            gl = gl1 * wl1 + gl2 * wl2 + gl3 * wl3 + gw * ww

            If Double.IsNaN(gl) Then gl = 0.0

            If nullstream Then
                Me.CurrentMaterialStream.Phases(1).Properties.gibbs_free_energy = gl1
            Else
                Me.CurrentMaterialStream.Phases(1).Properties.gibbs_free_energy = gl / wl
            End If

            al1 = Me.CurrentMaterialStream.Phases(3).Properties.helmholtz_energy.GetValueOrDefault
            al2 = Me.CurrentMaterialStream.Phases(4).Properties.helmholtz_energy.GetValueOrDefault
            al3 = Me.CurrentMaterialStream.Phases(5).Properties.helmholtz_energy.GetValueOrDefault
            aw = Me.CurrentMaterialStream.Phases(6).Properties.helmholtz_energy.GetValueOrDefault

            al = al1 * wl1 + al2 * wl2 + al3 * wl3 + aw * ww

            If Double.IsNaN(al) Then al = 0.0

            If nullstream Then
                Me.CurrentMaterialStream.Phases(1).Properties.helmholtz_energy = al1
            Else
                Me.CurrentMaterialStream.Phases(1).Properties.helmholtz_energy = al / wl
            End If

            cpl1 = Me.CurrentMaterialStream.Phases(3).Properties.heatCapacityCp.GetValueOrDefault
            cpl2 = Me.CurrentMaterialStream.Phases(4).Properties.heatCapacityCp.GetValueOrDefault
            cpl3 = Me.CurrentMaterialStream.Phases(5).Properties.heatCapacityCp.GetValueOrDefault
            cpw = Me.CurrentMaterialStream.Phases(6).Properties.heatCapacityCp.GetValueOrDefault

            cpl = cpl1 * wl1 + cpl2 * wl2 + cpl3 * wl3 + cpw * ww

            If Double.IsNaN(cpl) Then cpl = 0.0

            If nullstream Then
                Me.CurrentMaterialStream.Phases(1).Properties.heatCapacityCp = cpl1
            Else
                Me.CurrentMaterialStream.Phases(1).Properties.heatCapacityCp = cpl / wl
            End If

            cvl1 = Me.CurrentMaterialStream.Phases(3).Properties.heatCapacityCv.GetValueOrDefault
            cvl2 = Me.CurrentMaterialStream.Phases(4).Properties.heatCapacityCv.GetValueOrDefault
            cvl3 = Me.CurrentMaterialStream.Phases(5).Properties.heatCapacityCv.GetValueOrDefault
            cvw = Me.CurrentMaterialStream.Phases(6).Properties.heatCapacityCv.GetValueOrDefault

            cvl = cvl1 * wl1 + cvl2 * wl2 + cvl3 * wl3 + cvw * ww

            If Double.IsNaN(cvl) Then cvl = 0.0

            If nullstream Then
                Me.CurrentMaterialStream.Phases(1).Properties.heatCapacityCv = cvl1
            Else
                Me.CurrentMaterialStream.Phases(1).Properties.heatCapacityCv = cvl / wl
            End If

            Dim result As Double

            result = Me.AUX_MMM(Phase.Liquid)
            Me.CurrentMaterialStream.Phases(1).Properties.molecularWeight = result

            result = Me.CurrentMaterialStream.Phases(1).Properties.enthalpy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(1).Properties.molecularWeight.GetValueOrDefault
            Me.CurrentMaterialStream.Phases(1).Properties.molar_enthalpy = result

            result = Me.CurrentMaterialStream.Phases(1).Properties.entropy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(1).Properties.molecularWeight.GetValueOrDefault
            Me.CurrentMaterialStream.Phases(1).Properties.molar_entropy = result

            result = Me.CurrentMaterialStream.Phases(1).Properties.internal_energy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(1).Properties.molecularWeight.GetValueOrDefault
            Me.CurrentMaterialStream.Phases(1).Properties.molar_internal_energy = result

            result = Me.CurrentMaterialStream.Phases(1).Properties.gibbs_free_energy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(1).Properties.molecularWeight.GetValueOrDefault
            Me.CurrentMaterialStream.Phases(1).Properties.molar_gibbs_free_energy = result

            result = Me.CurrentMaterialStream.Phases(1).Properties.helmholtz_energy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(1).Properties.molecularWeight.GetValueOrDefault
            Me.CurrentMaterialStream.Phases(1).Properties.molar_helmholtz_energy = result

            Ql = Me.CurrentMaterialStream.Phases(3).Properties.volumetric_flow.GetValueOrDefault + Me.CurrentMaterialStream.Phases(4).Properties.volumetric_flow.GetValueOrDefault _
                + Me.CurrentMaterialStream.Phases(5).Properties.volumetric_flow.GetValueOrDefault + Me.CurrentMaterialStream.Phases(6).Properties.volumetric_flow.GetValueOrDefault

            phi1 = Me.CurrentMaterialStream.Phases(3).Properties.volumetric_flow.GetValueOrDefault / Ql
            phi2 = Me.CurrentMaterialStream.Phases(4).Properties.volumetric_flow.GetValueOrDefault / Ql
            phi3 = Me.CurrentMaterialStream.Phases(5).Properties.volumetric_flow.GetValueOrDefault / Ql
            phiw = Me.CurrentMaterialStream.Phases(6).Properties.volumetric_flow.GetValueOrDefault / Ql

            kl1 = Me.CurrentMaterialStream.Phases(3).Properties.thermalConductivity.GetValueOrDefault
            kl2 = Me.CurrentMaterialStream.Phases(4).Properties.thermalConductivity.GetValueOrDefault
            kl3 = Me.CurrentMaterialStream.Phases(5).Properties.thermalConductivity.GetValueOrDefault
            kw = Me.CurrentMaterialStream.Phases(6).Properties.thermalConductivity.GetValueOrDefault

            'kl = kl1 * wl1 + kl2 * wl2 + kl3 * wl3 + kw * ww
            kl = kl1 * phi1 + kl2 * phi2 + kl3 * phi3 + kw * phiw

            If Double.IsNaN(kl) Then kl = 0.0

            If nullstream Then
                Me.CurrentMaterialStream.Phases(1).Properties.thermalConductivity = kl1
            Else
                Me.CurrentMaterialStream.Phases(1).Properties.thermalConductivity = kl
            End If

            vil1 = Me.CurrentMaterialStream.Phases(3).Properties.viscosity.GetValueOrDefault
            vil2 = Me.CurrentMaterialStream.Phases(4).Properties.viscosity.GetValueOrDefault
            vil3 = Me.CurrentMaterialStream.Phases(5).Properties.viscosity.GetValueOrDefault
            viw = Me.CurrentMaterialStream.Phases(6).Properties.viscosity.GetValueOrDefault

            'vil = vil1 * wl1 + vil2 * wl2 + kl3 * vil3 + viw * ww
            vil = vil1 * phi1 + vil2 * phi2 + kl3 * phi3 + viw * phiw

            If Double.IsNaN(vil) Then vil = 0.0

            If nullstream Then
                Me.CurrentMaterialStream.Phases(1).Properties.viscosity = vil1
                Me.CurrentMaterialStream.Phases(1).Properties.kinematic_viscosity = vil1 / dl1
            Else
                Me.CurrentMaterialStream.Phases(1).Properties.viscosity = vil
                Me.CurrentMaterialStream.Phases(1).Properties.kinematic_viscosity = vil / dl
            End If

        End Sub

        Public Overridable Function CalculateEquilibrium(calctype As Interfaces.Enums.FlashCalculationType, val1 As Double, val2 As Double, mixmolefrac() As Double, initialKval() As Double, initialestimate As Double) As Interfaces.IFlashCalculationResult Implements Interfaces.IPropertyPackage.CalculateEquilibrium

            Dim result As IFlashCalculationResult

            Select Case calctype
                Case Interfaces.Enums.FlashCalculationType.PressureTemperature
                    result = Me.FlashBase.CalculateEquilibrium(FlashSpec.P, FlashSpec.T, val1, val2, Me, mixmolefrac, initialKval, initialestimate)
                Case Interfaces.Enums.FlashCalculationType.PressureEnthalpy
                    result = Me.FlashBase.CalculateEquilibrium(FlashSpec.P, FlashSpec.H, val1, val2, Me, mixmolefrac, initialKval, initialestimate)
                Case Interfaces.Enums.FlashCalculationType.PressureEntropy
                    result = Me.FlashBase.CalculateEquilibrium(FlashSpec.P, FlashSpec.S, val1, val2, Me, mixmolefrac, initialKval, initialestimate)
                Case Interfaces.Enums.FlashCalculationType.PressureSolidFraction
                    result = Me.FlashBase.CalculateEquilibrium(FlashSpec.P, FlashSpec.SF, val1, val2, Me, mixmolefrac, initialKval, initialestimate)
                Case Interfaces.Enums.FlashCalculationType.PressureVaporFraction
                    result = Me.FlashBase.CalculateEquilibrium(FlashSpec.P, FlashSpec.VAP, val1, val2, Me, mixmolefrac, initialKval, initialestimate)
                Case Interfaces.Enums.FlashCalculationType.TemperatureEnthalpy
                    result = Me.FlashBase.CalculateEquilibrium(FlashSpec.T, FlashSpec.H, val1, val2, Me, mixmolefrac, initialKval, initialestimate)
                Case Interfaces.Enums.FlashCalculationType.TemperatureEntropy
                    result = Me.FlashBase.CalculateEquilibrium(FlashSpec.T, FlashSpec.S, val1, val2, Me, mixmolefrac, initialKval, initialestimate)
                Case Interfaces.Enums.FlashCalculationType.TemperatureSolidFraction
                    result = Me.FlashBase.CalculateEquilibrium(FlashSpec.T, FlashSpec.SF, val1, val2, Me, mixmolefrac, initialKval, initialestimate)
                Case Interfaces.Enums.FlashCalculationType.TemperatureVaporFraction
                    result = Me.FlashBase.CalculateEquilibrium(FlashSpec.T, FlashSpec.VAP, val1, val2, Me, mixmolefrac, initialKval, initialestimate)
                Case Interfaces.Enums.FlashCalculationType.VolumeTemperature
                    result = Me.FlashBase.CalculateEquilibrium(FlashSpec.V, FlashSpec.T, val1, val2, Me, mixmolefrac, initialKval, initialestimate)
                Case Interfaces.Enums.FlashCalculationType.VolumePressure
                    result = Me.FlashBase.CalculateEquilibrium(FlashSpec.V, FlashSpec.P, val1, val2, Me, mixmolefrac, initialKval, initialestimate)
                Case Interfaces.Enums.FlashCalculationType.VolumeEnthalpy
                    result = Me.FlashBase.CalculateEquilibrium(FlashSpec.V, FlashSpec.H, val1, val2, Me, mixmolefrac, initialKval, initialestimate)
                Case Interfaces.Enums.FlashCalculationType.PressureEntropy
                    result = Me.FlashBase.CalculateEquilibrium(FlashSpec.V, FlashSpec.S, val1, val2, Me, mixmolefrac, initialKval, initialestimate)
                Case Else
                    Throw New NotImplementedException
            End Select

            If result.ResultException IsNot Nothing Then Throw result.ResultException

            Return result

        End Function

        Public Function CalculateEquilibrium2(calctype As Enums.FlashCalculationType, val1 As Double, val2 As Double, initialestimate As Double) As IFlashCalculationResult Implements IPropertyPackage.CalculateEquilibrium2
            Return CalculateEquilibrium(calctype, val1, val2, RET_VMOL(Phase.Mixture), Nothing, initialestimate)
        End Function

        Public Overridable Sub DW_CalcEquilibrium(ByVal spec1 As FlashSpec, ByVal spec2 As FlashSpec)

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "DW_CalcEquilibrium", ComponentName & " (Phase Equilibria)", "Property Package Equilibrium Calculation Routine")

            Me.CurrentMaterialStream.AtEquilibrium = False

            Dim P, T, H, S, xv, xl, xl2, xs As Double
            Dim result As Object = Nothing
            Dim subst As Interfaces.ICompound
            Dim n As Integer = Me.CurrentMaterialStream.Phases(0).Compounds.Count
            Dim i As Integer = 0

            'for TVF/PVF/PH/PS flashes
            xv = Me.CurrentMaterialStream.Phases(2).Properties.molarfraction.GetValueOrDefault
            H = Me.CurrentMaterialStream.Phases(0).Properties.enthalpy.GetValueOrDefault
            S = Me.CurrentMaterialStream.Phases(0).Properties.entropy.GetValueOrDefault

            IObj?.Paragraphs.Add("This is the routine responsible for the calculation of phase distribution in the currently associated Material Stream, using the specified Flash Algorithm.")

            IObj?.Paragraphs.Add("The first thing that the routine does is to erase all previously calculated phase distribution and properties on the stream, if they exist.")

            IObj?.Paragraphs.Add("Erasing properties...")

            Me.DW_ZerarPhaseProps(Phase.Vapor)
            Me.DW_ZerarPhaseProps(Phase.Liquid)
            Me.DW_ZerarPhaseProps(Phase.Liquid1)
            Me.DW_ZerarPhaseProps(Phase.Liquid2)
            Me.DW_ZerarPhaseProps(Phase.Liquid3)
            Me.DW_ZerarPhaseProps(Phase.Aqueous)
            Me.DW_ZerarPhaseProps(Phase.Solid)
            Me.DW_ZerarComposicoes(Phase.Vapor)
            Me.DW_ZerarComposicoes(Phase.Liquid)
            Me.DW_ZerarComposicoes(Phase.Liquid1)
            Me.DW_ZerarComposicoes(Phase.Liquid2)
            Me.DW_ZerarComposicoes(Phase.Liquid3)
            Me.DW_ZerarComposicoes(Phase.Aqueous)
            Me.DW_ZerarComposicoes(Phase.Solid)

            IObj?.Paragraphs.Add("It then checks for the Material Stream State Specification, in order to proceed with the correct flash (equilibrium) calculation.")

            Select Case spec1

                Case FlashSpec.T

                    Select Case spec2

                        Case FlashSpec.P

                            IObj?.Paragraphs.Add("The defined specification is TP (Temperature and Pressure). DWSIM will call the 'Flash_PT' routine from the currently associated Flash Algorithm instance.")

                            T = Me.CurrentMaterialStream.Phases(0).Properties.temperature.GetValueOrDefault
                            P = Me.CurrentMaterialStream.Phases(0).Properties.pressure.GetValueOrDefault

                            If Not T.IsValid Or Not P.IsValid Then Throw New ArgumentException("TP Flash: " & Calculator.GetLocalString("ErrorInvalidFlashSpecValue"))
                            If Not T.IsPositive Or Not P.IsPositive Then Throw New ArgumentException("TP Flash: " & Calculator.GetLocalString("ErrorInvalidFlashSpecValue"))

                            Dim ige As Double = 0
                            Dim fge As Double = 0
                            Dim dge As Double = 0

                            If Not Settings.CAPEOPENMode Then

                                If Me.FlashBase.FlashSettings(Enums.FlashSetting.ValidateEquilibriumCalc) = True Then

                                    IObj?.SetCurrent()
                                    IObj?.Paragraphs.Add("Calculating Mixture Initial Gibbs Energy...")

                                    ige = Me.DW_CalcGibbsEnergy(RET_VMOL(Phase.Mixture), T, P)

                                End If

                            End If

                            IObj?.SetCurrent()
                            IObj?.Paragraphs.Add("Calling PT Flash calculation routine...")

                            result = Me.FlashBase.Flash_PT(RET_VMOL(Phase.Mixture), P, T, Me)

                            xl = result(0)
                            xv = result(1)
                            xl2 = result(5)
                            xs = result(7)

                            Dim Vx As Double() = result(2)
                            Dim Vy As Double() = result(3)
                            Dim Vx2 As Double() = result(6)
                            Dim Vs As Double() = result(8)

                            If Not Settings.CAPEOPENMode Then

                                'identify phase
                                If Me.FlashBase.FlashSettings(Enums.FlashSetting.UsePhaseIdentificationAlgorithm) Then
                                    If Me.ComponentName.Contains("SRK") Or Me.ComponentName.Contains("PR") Then
                                        If Not Me.AUX_IS_SINGLECOMP(Phase.Mixture) Then
                                            IObj?.SetCurrent()
                                            Dim newphase, eos As String
                                            If Me.ComponentName.Contains("SRK") Then eos = "SRK" Else eos = "PR"
                                            If xv = 1.0# Or xl = 1.0# Then
                                                If xv = 1.0# Then
                                                    newphase = Auxiliary.FlashAlgorithms.FlashAlgorithm.IdentifyPhase(Vy, P, T, Me, eos)
                                                    If newphase = "L" Then
                                                        xv = 0.0#
                                                        xl = 1.0#
                                                        Vx = Vy
                                                    End If
                                                Else
                                                    newphase = Auxiliary.FlashAlgorithms.FlashAlgorithm.IdentifyPhase(Vx, P, T, Me, eos)
                                                    If newphase = "V" Then
                                                        xv = 1.0#
                                                        xl = 0.0#
                                                        Vy = Vx
                                                    End If
                                                End If
                                            Else
                                                If xl2 = 0.0# Then
                                                    newphase = Auxiliary.FlashAlgorithms.FlashAlgorithm.IdentifyPhase(Vy, P, T, Me, eos)
                                                    If newphase = "L" Then
                                                        xl2 = xv
                                                        xv = 0.0#
                                                        Vx2 = Vy
                                                    End If
                                                    newphase = Auxiliary.FlashAlgorithms.FlashAlgorithm.IdentifyPhase(Vx, P, T, Me, eos)
                                                    If newphase = "V" Then
                                                        xv = 1.0#
                                                        xl = 0.0#
                                                        xl2 = 0.0#
                                                        Vy = RET_VMOL(Phase.Mixture)
                                                    End If
                                                ElseIf xv = 0.0# Then
                                                    newphase = Auxiliary.FlashAlgorithms.FlashAlgorithm.IdentifyPhase(Vx2, P, T, Me, eos)
                                                    If newphase = "V" Then
                                                        xv = xl2
                                                        xl2 = 0.0#
                                                        Vy = Vx2
                                                    End If
                                                End If
                                            End If
                                        End If
                                    End If
                                End If

                                If Me.FlashBase.FlashSettings(Enums.FlashSetting.ValidateEquilibriumCalc) = True Then

                                    IObj?.Paragraphs.Add("Calculating  Mixture Final Gibbs Energy...")

                                    IObj?.SetCurrent()
                                    fge = xl * Me.DW_CalcGibbsEnergy(Vx, T, P)
                                    IObj?.SetCurrent()
                                    fge += xl2 * Me.DW_CalcGibbsEnergy(Vx2, T, P)
                                    IObj?.SetCurrent()
                                    fge += xv * Me.DW_CalcGibbsEnergy(Vy, T, P)

                                    dge = fge - ige

                                    Dim dgtol As Double = Me.FlashBase.FlashSettings(Enums.FlashSetting.ValidationGibbsTolerance).ToDoubleFromInvariant()

                                    If dge > 0.0# And Math.Abs(dge / ige * 100) > Math.Abs(dgtol) Then
                                        Dim ex As New Exception(Calculator.GetLocalString("InvalidFlashResult") & "(DGE = " & dge & " kJ/kg, " & Format(dge / ige * 100, "0.00") & "%)")
                                        ex.Data.Add("DetailedDescription", "The calculated phase and composition distribution is invalid (increased Gibbs Free Energy).")
                                        ex.Data.Add("UserAction", "Try another Property Package and/or Flash Algorithm.")
                                        Throw ex
                                    End If

                                End If

                            End If

                            'do a density calculation check to order liquid phases from lighter to heavier

                            If xl2 <> 0.0# And xl = 0.0# Then
                                xl = result(5)
                                xl2 = 0.0#
                                Vx = result(6)
                                Vx2 = result(2)
                            ElseIf xl2 <> 0.0# And xl <> 0.0# Then
                                IObj?.Paragraphs.Add("Ordering liquid phases by density from lower to higher...")
                                Dim dens1, dens2, xl0, xl20, Vx0(), Vx20() As Double
                                IObj?.SetCurrent()
                                dens1 = Me.AUX_LIQDENS(T, Vx, P, 0, False)
                                IObj?.SetCurrent()
                                dens2 = Me.AUX_LIQDENS(T, Vx2, P, 0, False)
                                If dens2 < dens1 Then
                                    xl0 = xl
                                    xl20 = xl2
                                    Vx0 = Vx
                                    Vx20 = Vx2
                                    xl = xl20
                                    xl2 = xl0
                                    Vx = Vx20
                                    Vx2 = Vx0
                                End If
                            End If

                            Me.CurrentMaterialStream.Phases(3).Properties.molarfraction = xl
                            Me.CurrentMaterialStream.Phases(4).Properties.molarfraction = xl2
                            Me.CurrentMaterialStream.Phases(2).Properties.molarfraction = xv
                            Me.CurrentMaterialStream.Phases(7).Properties.molarfraction = xs

                            IObj?.SetCurrent()
                            IObj?.Paragraphs.Add("Calculating fugacity coefficient for compounds in Liquid Phase 1...")
                            Dim FCL = Me.DW_CalcFugCoeff(Vx, T, P, State.Liquid)
                            IObj?.SetCurrent()
                            IObj?.Paragraphs.Add("Calculating fugacity coefficient for compounds in Liquid Phase 2...")
                            Dim FCL2 = Me.DW_CalcFugCoeff(Vx2, T, P, State.Liquid)
                            IObj?.SetCurrent()
                            IObj?.Paragraphs.Add("Calculating fugacity coefficient for compounds in Vapor Phase...")
                            Dim FCV = Me.DW_CalcFugCoeff(Vy, T, P, State.Vapor)
                            IObj?.SetCurrent()
                            IObj?.Paragraphs.Add("Calculating fugacity coefficient for compounds in Solid Phase...")
                            Dim FCS = Me.DW_CalcFugCoeff(Vs, T, P, State.Solid)

                            i = 0
                            For Each subst In Me.CurrentMaterialStream.Phases(3).Compounds.Values
                                subst.MoleFraction = Vx(i)
                                subst.FugacityCoeff = FCL(i)
                                subst.ActivityCoeff = 0
                                subst.PartialVolume = 0
                                subst.PartialPressure = Vx(i) * FCL(i) * P
                                i += 1
                            Next
                            For Each subst In Me.CurrentMaterialStream.Phases(3).Compounds.Values
                                subst.MassFraction = Me.AUX_CONVERT_MOL_TO_MASS(subst.Name, 3)
                            Next
                            i = 0
                            For Each subst In Me.CurrentMaterialStream.Phases(4).Compounds.Values
                                subst.MoleFraction = Vx2(i)
                                subst.FugacityCoeff = FCL2(i)
                                subst.ActivityCoeff = 0
                                subst.PartialVolume = 0
                                subst.PartialPressure = Vx2(i) * FCL2(i) * P
                                i += 1
                            Next
                            For Each subst In Me.CurrentMaterialStream.Phases(4).Compounds.Values
                                subst.MassFraction = Me.AUX_CONVERT_MOL_TO_MASS(subst.Name, 4)
                            Next
                            i = 0
                            For Each subst In Me.CurrentMaterialStream.Phases(2).Compounds.Values
                                subst.MoleFraction = Vy(i)
                                subst.FugacityCoeff = FCV(i)
                                subst.ActivityCoeff = 0
                                subst.PartialVolume = 0
                                subst.PartialPressure = Vy(i) * FCV(i) * P
                                i += 1
                            Next
                            For Each subst In Me.CurrentMaterialStream.Phases(2).Compounds.Values
                                subst.MassFraction = Me.AUX_CONVERT_MOL_TO_MASS(subst.Name, 2)
                            Next
                            i = 0
                            For Each subst In Me.CurrentMaterialStream.Phases(7).Compounds.Values
                                subst.MoleFraction = Vs(i)
                                subst.FugacityCoeff = FCS(i)
                                subst.ActivityCoeff = 0
                                subst.PartialVolume = 0
                                subst.PartialPressure = 0
                                i += 1
                            Next
                            For Each subst In Me.CurrentMaterialStream.Phases(7).Compounds.Values
                                subst.MassFraction = Me.AUX_CONVERT_MOL_TO_MASS(subst.Name, 7)
                            Next

                            Me.CurrentMaterialStream.Phases(3).Properties.massfraction = xl * Me.AUX_MMM(Phase.Liquid1) / (xl * Me.AUX_MMM(Phase.Liquid1) + xl2 * Me.AUX_MMM(Phase.Liquid2) + xv * Me.AUX_MMM(Phase.Vapor) + xs * Me.AUX_MMM(Phase.Solid))
                            Me.CurrentMaterialStream.Phases(4).Properties.massfraction = xl2 * Me.AUX_MMM(Phase.Liquid2) / (xl * Me.AUX_MMM(Phase.Liquid1) + xl2 * Me.AUX_MMM(Phase.Liquid2) + xv * Me.AUX_MMM(Phase.Vapor) + xs * Me.AUX_MMM(Phase.Solid))
                            Me.CurrentMaterialStream.Phases(2).Properties.massfraction = xv * Me.AUX_MMM(Phase.Vapor) / (xl * Me.AUX_MMM(Phase.Liquid1) + xl2 * Me.AUX_MMM(Phase.Liquid2) + xv * Me.AUX_MMM(Phase.Vapor) + xs * Me.AUX_MMM(Phase.Solid))
                            Me.CurrentMaterialStream.Phases(7).Properties.massfraction = xs * Me.AUX_MMM(Phase.Solid) / (xl * Me.AUX_MMM(Phase.Liquid1) + xl2 * Me.AUX_MMM(Phase.Liquid2) + xv * Me.AUX_MMM(Phase.Vapor) + xs * Me.AUX_MMM(Phase.Solid))

                            Dim constprops As New List(Of Interfaces.ICompoundConstantProperties)
                            For Each su As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                                constprops.Add(su.ConstantProperties)
                            Next

                            Dim HM, HV, HL, HL2, HS As Double

                            IObj?.SetCurrent()
                            IObj?.Paragraphs.Add("Calculating Enthalpy for Liquid Phase 1...")
                            If xl <> 0 Then HL = Me.DW_CalcEnthalpy(Vx, T, P, State.Liquid)
                            IObj?.SetCurrent()
                            IObj?.Paragraphs.Add("Calculating Enthalpy for Liquid Phase 2...")
                            If xl2 <> 0 Then HL2 = Me.DW_CalcEnthalpy(Vx2, T, P, State.Liquid)
                            IObj?.SetCurrent()
                            IObj?.Paragraphs.Add("Calculating Enthalpy for Vapor Phase...")
                            If xv <> 0 Then HV = Me.DW_CalcEnthalpy(Vy, T, P, State.Vapor)
                            IObj?.SetCurrent()
                            IObj?.Paragraphs.Add("Calculating Enthalpy for Solid Phase...")
                            If xs <> 0 Then HS = Me.DW_CalcSolidEnthalpy(T, Vs, constprops)
                            HM = Me.CurrentMaterialStream.Phases(4).Properties.massfraction.GetValueOrDefault * HL2 + Me.CurrentMaterialStream.Phases(3).Properties.massfraction.GetValueOrDefault * HL + Me.CurrentMaterialStream.Phases(2).Properties.massfraction.GetValueOrDefault * HV + Me.CurrentMaterialStream.Phases(7).Properties.massfraction.GetValueOrDefault * HS

                            H = HM

                            Dim SM, SV, SL, SL2, SS As Double

                            IObj?.SetCurrent()
                            IObj?.Paragraphs.Add("Calculating Entropy for Liquid Phase 1...")
                            If xl <> 0 Then SL = Me.DW_CalcEntropy(Vx, T, P, State.Liquid)
                            IObj?.SetCurrent()
                            IObj?.Paragraphs.Add("Calculating Entropy for Liquid Phase 2...")
                            If xl2 <> 0 Then SL2 = Me.DW_CalcEntropy(Vx2, T, P, State.Liquid)
                            IObj?.SetCurrent()
                            IObj?.Paragraphs.Add("Calculating Entropy for Vapor Phase...")
                            If xv <> 0 Then SV = Me.DW_CalcEntropy(Vy, T, P, State.Vapor)
                            IObj?.SetCurrent()
                            IObj?.Paragraphs.Add("Calculating Entropy for Solid Phase...")
                            If xs <> 0 And T <> 298.15 Then SS = Me.DW_CalcSolidEnthalpy(T, Vs, constprops) / (T - 298.15)

                            SM = Me.CurrentMaterialStream.Phases(4).Properties.massfraction.GetValueOrDefault * SL2 + Me.CurrentMaterialStream.Phases(3).Properties.massfraction.GetValueOrDefault * SL + Me.CurrentMaterialStream.Phases(2).Properties.massfraction.GetValueOrDefault * SV + Me.CurrentMaterialStream.Phases(7).Properties.massfraction.GetValueOrDefault * SS

                            S = SM

                        Case FlashSpec.H

                            Throw New Exception(Calculator.GetLocalString("PropPack_FlashTHNotSupported"))

                        Case FlashSpec.S

                            Throw New Exception(Calculator.GetLocalString("PropPack_FlashTSNotSupported"))

                        Case FlashSpec.VAP

                            IObj?.Paragraphs.Add("The defined specification is TVF (Temperature and Vapor Fraction). DWSIM will call the 'Flash_TV' routine from the currently associated Flash Algorithm instance.")

                            Dim KI(n) As Double
                            Dim HM, HV, HL, HL2, HS As Double
                            Dim SM, SV, SL, SL2, SS As Double

                            i = 0
                            Do
                                KI(i) = 0
                                i = i + 1
                            Loop Until i = n + 1

                            T = Me.CurrentMaterialStream.Phases(0).Properties.temperature.GetValueOrDefault
                            P = Me.CurrentMaterialStream.Phases(0).Properties.pressure.GetValueOrDefault

                            If Not T.IsValid Or Not xv.IsValid Then Throw New ArgumentException("TVF Flash: " & Calculator.GetLocalString("ErrorInvalidFlashSpecValue"))
                            If Not T.IsPositive Or xv.IsNegative Then Throw New ArgumentException("TVF Flash: " & Calculator.GetLocalString("ErrorInvalidFlashSpecValue"))

                            If Double.IsNaN(P) Or Double.IsInfinity(P) Then P = 0.0#

                            Dim Vx, Vx2, Vy, Vs As Double()

                            IObj?.SetCurrent()
                            result = Me.FlashBase.Flash_TV(RET_VMOL(Phase.Mixture), T, xv, P, Me)

                            P = result(4)

                            xl = result(0)
                            xv = result(1)
                            xl2 = result(7)
                            xs = result(9)

                            Vx = result(2)
                            Vy = result(3)
                            Vx2 = result(8)
                            Vs = result(10)

                            If Settings.CAPEOPENMode Then
                                If xv = 1.0 Then
                                    xl = 1.0E-20
                                    xl2 = 0.0
                                    xs = 0.0
                                ElseIf xl = 1.0 Then
                                    xv = 1.0E-20
                                    xl2 = 0.0
                                    xs = 0.0
                                End If
                            End If

                            Me.CurrentMaterialStream.Phases(3).Properties.molarfraction = xl
                            Me.CurrentMaterialStream.Phases(4).Properties.molarfraction = xl2
                            Me.CurrentMaterialStream.Phases(2).Properties.molarfraction = xv
                            Me.CurrentMaterialStream.Phases(7).Properties.molarfraction = xs

                            IObj?.SetCurrent()
                            IObj?.Paragraphs.Add("Calculating fugacity coefficient for compounds in Liquid Phase 1...")
                            Dim FCL = Me.DW_CalcFugCoeff(Vx, T, P, State.Liquid)
                            IObj?.SetCurrent()
                            IObj?.Paragraphs.Add("Calculating fugacity coefficient for compounds in Liquid Phase 2...")
                            Dim FCL2 = Me.DW_CalcFugCoeff(Vx2, T, P, State.Liquid)
                            IObj?.SetCurrent()
                            IObj?.Paragraphs.Add("Calculating fugacity coefficient for compounds in Vapor Phase...")
                            Dim FCV = Me.DW_CalcFugCoeff(Vy, T, P, State.Vapor)

                            i = 0
                            For Each subst In Me.CurrentMaterialStream.Phases(3).Compounds.Values
                                subst.MoleFraction = Vx(i)
                                subst.FugacityCoeff = FCL(i)
                                subst.ActivityCoeff = 0
                                subst.PartialVolume = 0
                                subst.PartialPressure = Vx(i) * FCL(i) * P
                                i += 1
                            Next
                            For Each subst In Me.CurrentMaterialStream.Phases(3).Compounds.Values
                                subst.MassFraction = Me.AUX_CONVERT_MOL_TO_MASS(subst.Name, 3)
                            Next
                            i = 0
                            For Each subst In Me.CurrentMaterialStream.Phases(4).Compounds.Values
                                subst.MoleFraction = Vx2(i)
                                subst.FugacityCoeff = FCL2(i)
                                subst.ActivityCoeff = 0
                                subst.PartialVolume = 0
                                subst.PartialPressure = Vx2(i) * FCL2(i) * P
                                i += 1
                            Next
                            For Each subst In Me.CurrentMaterialStream.Phases(4).Compounds.Values
                                subst.MassFraction = Me.AUX_CONVERT_MOL_TO_MASS(subst.Name, 4)
                            Next
                            i = 0
                            For Each subst In Me.CurrentMaterialStream.Phases(2).Compounds.Values
                                subst.MoleFraction = Vy(i)
                                subst.FugacityCoeff = FCV(i)
                                subst.ActivityCoeff = 0
                                subst.PartialVolume = 0
                                subst.PartialPressure = Vy(i) * FCV(i) * P
                                i += 1
                            Next
                            For Each subst In Me.CurrentMaterialStream.Phases(2).Compounds.Values
                                subst.MassFraction = Me.AUX_CONVERT_MOL_TO_MASS(subst.Name, 2)
                            Next

                            i = 0
                            For Each subst In Me.CurrentMaterialStream.Phases(7).Compounds.Values
                                subst.MoleFraction = Vs(i)
                                subst.FugacityCoeff = 1.0#
                                subst.ActivityCoeff = 0
                                subst.PartialVolume = 0
                                subst.PartialPressure = 0
                                i += 1
                            Next
                            For Each subst In Me.CurrentMaterialStream.Phases(7).Compounds.Values
                                subst.MassFraction = Me.AUX_CONVERT_MOL_TO_MASS(subst.Name, 7)
                            Next

                            Me.CurrentMaterialStream.Phases(3).Properties.massfraction = xl * Me.AUX_MMM(Phase.Liquid1) / (xl * Me.AUX_MMM(Phase.Liquid1) + xl2 * Me.AUX_MMM(Phase.Liquid2) + xv * Me.AUX_MMM(Phase.Vapor) + xs * Me.AUX_MMM(Phase.Solid))
                            Me.CurrentMaterialStream.Phases(4).Properties.massfraction = xl2 * Me.AUX_MMM(Phase.Liquid2) / (xl * Me.AUX_MMM(Phase.Liquid1) + xl2 * Me.AUX_MMM(Phase.Liquid2) + xv * Me.AUX_MMM(Phase.Vapor) + xs * Me.AUX_MMM(Phase.Solid))
                            Me.CurrentMaterialStream.Phases(2).Properties.massfraction = xv * Me.AUX_MMM(Phase.Vapor) / (xl * Me.AUX_MMM(Phase.Liquid1) + xl2 * Me.AUX_MMM(Phase.Liquid2) + xv * Me.AUX_MMM(Phase.Vapor) + xs * Me.AUX_MMM(Phase.Solid))
                            Me.CurrentMaterialStream.Phases(7).Properties.massfraction = xs * Me.AUX_MMM(Phase.Solid) / (xl * Me.AUX_MMM(Phase.Liquid1) + xl2 * Me.AUX_MMM(Phase.Liquid2) + xv * Me.AUX_MMM(Phase.Vapor) + xs * Me.AUX_MMM(Phase.Solid))

                            Dim constprops As New List(Of Interfaces.ICompoundConstantProperties)
                            For Each su As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                                constprops.Add(su.ConstantProperties)
                            Next

                            IObj?.SetCurrent()
                            IObj?.Paragraphs.Add("Calculating Enthalpy for Liquid Phase 1...")
                            If xl <> 0 Then HL = Me.DW_CalcEnthalpy(Vx, T, P, State.Liquid)
                            IObj?.SetCurrent()
                            IObj?.Paragraphs.Add("Calculating Enthalpy for Liquid Phase 2...")
                            If xl2 <> 0 Then HL2 = Me.DW_CalcEnthalpy(Vx2, T, P, State.Liquid)
                            IObj?.SetCurrent()
                            IObj?.Paragraphs.Add("Calculating Enthalpy for Vapor Phase...")
                            If xv <> 0 Then HV = Me.DW_CalcEnthalpy(Vy, T, P, State.Vapor)
                            IObj?.SetCurrent()
                            IObj?.Paragraphs.Add("Calculating Enthalpy for Solid Phase...")
                            If xs <> 0 Then HS = Me.DW_CalcSolidEnthalpy(T, Vs, constprops)
                            HM = Me.CurrentMaterialStream.Phases(4).Properties.massfraction.GetValueOrDefault * HL2 + Me.CurrentMaterialStream.Phases(3).Properties.massfraction.GetValueOrDefault * HL + Me.CurrentMaterialStream.Phases(2).Properties.massfraction.GetValueOrDefault * HV + Me.CurrentMaterialStream.Phases(7).Properties.massfraction.GetValueOrDefault * HS

                            H = HM

                            IObj?.SetCurrent()
                            IObj?.Paragraphs.Add("Calculating Entropy for Liquid Phase 1...")
                            If xl <> 0 Then SL = Me.DW_CalcEntropy(Vx, T, P, State.Liquid)
                            IObj?.SetCurrent()
                            IObj?.Paragraphs.Add("Calculating Entropy for Liquid Phase 2...")
                            If xl2 <> 0 Then SL2 = Me.DW_CalcEntropy(Vx2, T, P, State.Liquid)
                            IObj?.SetCurrent()
                            IObj?.Paragraphs.Add("Calculating Entropy for Vapor Phase...")
                            If xv <> 0 Then SV = Me.DW_CalcEntropy(Vy, T, P, State.Vapor)
                            IObj?.SetCurrent()
                            IObj?.Paragraphs.Add("Calculating Entropy for Solid Phase...")
                            If xs <> 0 And T <> 298.15 Then SS = Me.DW_CalcSolidEnthalpy(T, Vs, constprops) / (T - 298.15)

                            SM = Me.CurrentMaterialStream.Phases(4).Properties.massfraction.GetValueOrDefault * SL2 + Me.CurrentMaterialStream.Phases(3).Properties.massfraction.GetValueOrDefault * SL + Me.CurrentMaterialStream.Phases(2).Properties.massfraction.GetValueOrDefault * SV + Me.CurrentMaterialStream.Phases(7).Properties.massfraction.GetValueOrDefault * SS

                            S = SM

                    End Select

                Case FlashSpec.P

                    Select Case spec2

                        Case FlashSpec.H

                            IObj?.Paragraphs.Add("The defined specification is PH (Pressure and Enthalpy). DWSIM will call the 'Flash_PH' routine from the currently associated Flash Algorithm instance.")

                            T = Me.CurrentMaterialStream.Phases(0).Properties.temperature.GetValueOrDefault
                            P = Me.CurrentMaterialStream.Phases(0).Properties.pressure.GetValueOrDefault

                            If Double.IsNaN(H) Or Double.IsInfinity(H) Then H = Me.CurrentMaterialStream.Phases(0).Properties.molar_enthalpy.GetValueOrDefault / Me.CurrentMaterialStream.Phases(0).Properties.molecularWeight.GetValueOrDefault

                            If Not T.IsValid Or Not H.IsValid Then Throw New ArgumentException("PH Flash: " & Calculator.GetLocalString("ErrorInvalidFlashSpecValue"))
                            'If Not T.IsPositive Then Throw New ArgumentException("PH Flash: " & Calculator.GetLocalString("ErrorInvalidFlashSpecValue"))

redirect:                   IObj?.SetCurrent()
                            result = Me.FlashBase.Flash_PH(RET_VMOL(Phase.Mixture), P, H, T, Me)

                            T = result(4)

                            xl = result(0)
                            xv = result(1)
                            xl2 = result(7)
                            xs = result(9)

                            Me.CurrentMaterialStream.Phases(3).Properties.molarfraction = xl
                            Me.CurrentMaterialStream.Phases(4).Properties.molarfraction = xl2
                            Me.CurrentMaterialStream.Phases(2).Properties.molarfraction = xv
                            Me.CurrentMaterialStream.Phases(7).Properties.molarfraction = xs

                            Dim Vx = result(2)
                            Dim Vy = result(3)
                            Dim Vx2 = result(8)
                            Dim Vs = result(10)

                            IObj?.SetCurrent()
                            IObj?.Paragraphs.Add("Calculating fugacity coefficient for compounds in Liquid Phase 1...")
                            Dim FCL = Me.DW_CalcFugCoeff(Vx, T, P, State.Liquid)
                            IObj?.SetCurrent()
                            IObj?.Paragraphs.Add("Calculating fugacity coefficient for compounds in Liquid Phase 1...")
                            Dim FCL2 = Me.DW_CalcFugCoeff(Vx2, T, P, State.Liquid)
                            IObj?.SetCurrent()
                            IObj?.Paragraphs.Add("Calculating fugacity coefficient for compounds in Vapor Phase...")
                            Dim FCV = Me.DW_CalcFugCoeff(Vy, T, P, State.Vapor)
                            IObj?.SetCurrent()
                            IObj?.Paragraphs.Add("Calculating fugacity coefficient for compounds in Solid Phase...")
                            Dim FCS = Me.DW_CalcFugCoeff(Vs, T, P, State.Solid)

                            i = 0
                            For Each subst In Me.CurrentMaterialStream.Phases(3).Compounds.Values
                                subst.MoleFraction = Vx(i)
                                subst.FugacityCoeff = FCL(i)
                                subst.ActivityCoeff = 0
                                subst.PartialVolume = 0
                                subst.PartialPressure = Vx(i) * FCL(i) * P
                                i += 1
                            Next
                            i = 1
                            For Each subst In Me.CurrentMaterialStream.Phases(3).Compounds.Values
                                subst.MassFraction = Me.AUX_CONVERT_MOL_TO_MASS(subst.Name, 3)
                                i += 1
                            Next
                            i = 0
                            For Each subst In Me.CurrentMaterialStream.Phases(4).Compounds.Values
                                subst.MoleFraction = Vx2(i)
                                subst.FugacityCoeff = FCL2(i)
                                subst.ActivityCoeff = 0
                                subst.PartialVolume = 0
                                subst.PartialPressure = Vx2(i) * FCL2(i) * P
                                i += 1
                            Next
                            i = 1
                            For Each subst In Me.CurrentMaterialStream.Phases(4).Compounds.Values
                                subst.MassFraction = Me.AUX_CONVERT_MOL_TO_MASS(subst.Name, 4)
                                i += 1
                            Next
                            i = 0
                            For Each subst In Me.CurrentMaterialStream.Phases(2).Compounds.Values
                                subst.MoleFraction = Vy(i)
                                subst.FugacityCoeff = FCV(i)
                                subst.ActivityCoeff = 0
                                subst.PartialVolume = 0
                                subst.PartialPressure = Vy(i) * FCV(i) * P
                                i += 1
                            Next
                            i = 1
                            For Each subst In Me.CurrentMaterialStream.Phases(2).Compounds.Values
                                subst.MassFraction = Me.AUX_CONVERT_MOL_TO_MASS(subst.Name, 2)
                                i += 1
                            Next
                            i = 0
                            For Each subst In Me.CurrentMaterialStream.Phases(7).Compounds.Values
                                subst.MoleFraction = Vs(i)
                                subst.FugacityCoeff = FCS(i)
                                subst.ActivityCoeff = 0
                                subst.PartialVolume = 0
                                subst.PartialPressure = 0
                                i += 1
                            Next
                            For Each subst In Me.CurrentMaterialStream.Phases(7).Compounds.Values
                                subst.MassFraction = Me.AUX_CONVERT_MOL_TO_MASS(subst.Name, 7)
                            Next

                            Me.CurrentMaterialStream.Phases(3).Properties.massfraction = xl * Me.AUX_MMM(Phase.Liquid1) / (xl * Me.AUX_MMM(Phase.Liquid1) + xl2 * Me.AUX_MMM(Phase.Liquid2) + xv * Me.AUX_MMM(Phase.Vapor) + xs * Me.AUX_MMM(Phase.Solid))
                            Me.CurrentMaterialStream.Phases(4).Properties.massfraction = xl2 * Me.AUX_MMM(Phase.Liquid2) / (xl * Me.AUX_MMM(Phase.Liquid1) + xl2 * Me.AUX_MMM(Phase.Liquid2) + xv * Me.AUX_MMM(Phase.Vapor) + xs * Me.AUX_MMM(Phase.Solid))
                            Me.CurrentMaterialStream.Phases(2).Properties.massfraction = xv * Me.AUX_MMM(Phase.Vapor) / (xl * Me.AUX_MMM(Phase.Liquid1) + xl2 * Me.AUX_MMM(Phase.Liquid2) + xv * Me.AUX_MMM(Phase.Vapor) + xs * Me.AUX_MMM(Phase.Solid))
                            Me.CurrentMaterialStream.Phases(7).Properties.massfraction = xs * Me.AUX_MMM(Phase.Solid) / (xl * Me.AUX_MMM(Phase.Liquid1) + xl2 * Me.AUX_MMM(Phase.Liquid2) + xv * Me.AUX_MMM(Phase.Vapor) + xs * Me.AUX_MMM(Phase.Solid))

                            Dim constprops As New List(Of Interfaces.ICompoundConstantProperties)
                            For Each su As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                                constprops.Add(su.ConstantProperties)
                            Next

                            Dim SM, SV, SL, SL2, SS As Double

                            IObj?.SetCurrent()
                            IObj?.Paragraphs.Add("Calculating Entropy for Liquid Phase 1...")
                            If xl <> 0 Then SL = Me.DW_CalcEntropy(Vx, T, P, State.Liquid)
                            IObj?.SetCurrent()
                            IObj?.Paragraphs.Add("Calculating Entropy for Liquid Phase 2...")
                            If xl2 <> 0 Then SL2 = Me.DW_CalcEntropy(Vx2, T, P, State.Liquid)
                            IObj?.SetCurrent()
                            IObj?.Paragraphs.Add("Calculating Entropy for Vapor Phase...")
                            If xv <> 0 Then SV = Me.DW_CalcEntropy(Vy, T, P, State.Vapor)
                            IObj?.SetCurrent()
                            If xs <> 0 And T <> 298.15 Then SS = Me.DW_CalcSolidEnthalpy(T, Vs, constprops) / (T - 298.15)
                            SM = Me.CurrentMaterialStream.Phases(4).Properties.massfraction.GetValueOrDefault * SL2 + Me.CurrentMaterialStream.Phases(3).Properties.massfraction.GetValueOrDefault * SL + Me.CurrentMaterialStream.Phases(2).Properties.massfraction.GetValueOrDefault * SV + Me.CurrentMaterialStream.Phases(7).Properties.massfraction.GetValueOrDefault * SS

                            S = SM

                        Case FlashSpec.S

                            IObj?.Paragraphs.Add("The defined specification is PS (Pressure and Entropy). DWSIM will call the 'Flash_PS' routine from the currently associated Flash Algorithm instance.")

                            T = Me.CurrentMaterialStream.Phases(0).Properties.temperature.GetValueOrDefault
                            P = Me.CurrentMaterialStream.Phases(0).Properties.pressure.GetValueOrDefault

                            If Double.IsNaN(S) Or Double.IsInfinity(S) Then S = Me.CurrentMaterialStream.Phases(0).Properties.molar_entropy.GetValueOrDefault / Me.CurrentMaterialStream.Phases(0).Properties.molecularWeight.GetValueOrDefault

                            If Not T.IsValid Or Not S.IsValid Then Throw New ArgumentException("PS Flash: " & Calculator.GetLocalString("ErrorInvalidFlashSpecValue"))
                            'If Not T.IsPositive Then Throw New ArgumentException("PS Flash: " & Calculator.GetLocalString("ErrorInvalidFlashSpecValue"))

redirect2:                  IObj?.SetCurrent()
                            result = Me.FlashBase.Flash_PS(RET_VMOL(Phase.Mixture), P, S, T, Me)

                            T = result(4)

                            xl = result(0)
                            xv = result(1)
                            xl2 = result(7)
                            xs = result(9)

                            Me.CurrentMaterialStream.Phases(3).Properties.molarfraction = xl
                            Me.CurrentMaterialStream.Phases(4).Properties.molarfraction = xl2
                            Me.CurrentMaterialStream.Phases(2).Properties.molarfraction = xv
                            Me.CurrentMaterialStream.Phases(7).Properties.molarfraction = xs

                            Dim Vx = result(2)
                            Dim Vy = result(3)
                            Dim Vx2 = result(8)
                            Dim Vs = result(10)

                            IObj?.SetCurrent()
                            IObj?.Paragraphs.Add("Calculating fugacity coefficient for compounds in Liquid Phase 1...")
                            Dim FCL = Me.DW_CalcFugCoeff(Vx, T, P, State.Liquid)
                            IObj?.SetCurrent()
                            IObj?.Paragraphs.Add("Calculating fugacity coefficient for compounds in Liquid Phase 1...")
                            Dim FCL2 = Me.DW_CalcFugCoeff(Vx2, T, P, State.Liquid)
                            IObj?.SetCurrent()
                            IObj?.Paragraphs.Add("Calculating fugacity coefficient for compounds in Vapor Phase...")
                            Dim FCV = Me.DW_CalcFugCoeff(Vy, T, P, State.Vapor)

                            i = 0
                            For Each subst In Me.CurrentMaterialStream.Phases(3).Compounds.Values
                                subst.MoleFraction = Vx(i)
                                subst.FugacityCoeff = FCL(i)
                                subst.ActivityCoeff = 0
                                subst.PartialVolume = 0
                                subst.PartialPressure = Vx(i) * FCL(i) * P
                                i += 1
                            Next
                            i = 1
                            For Each subst In Me.CurrentMaterialStream.Phases(3).Compounds.Values
                                subst.MassFraction = Me.AUX_CONVERT_MOL_TO_MASS(subst.Name, 3)
                                i += 1
                            Next
                            i = 0
                            For Each subst In Me.CurrentMaterialStream.Phases(4).Compounds.Values
                                subst.MoleFraction = Vx2(i)
                                subst.FugacityCoeff = FCL2(i)
                                subst.ActivityCoeff = 0
                                subst.PartialVolume = 0
                                subst.PartialPressure = Vx2(i) * FCL2(i) * P
                                i += 1
                            Next
                            i = 1
                            For Each subst In Me.CurrentMaterialStream.Phases(4).Compounds.Values
                                subst.MassFraction = Me.AUX_CONVERT_MOL_TO_MASS(subst.Name, 4)
                                i += 1
                            Next
                            i = 0
                            For Each subst In Me.CurrentMaterialStream.Phases(2).Compounds.Values
                                subst.MoleFraction = Vy(i)
                                subst.FugacityCoeff = FCV(i)
                                subst.ActivityCoeff = 0
                                subst.PartialVolume = 0
                                subst.PartialPressure = Vy(i) * FCV(i) * P
                                i += 1
                            Next

                            i = 1
                            For Each subst In Me.CurrentMaterialStream.Phases(2).Compounds.Values
                                subst.MassFraction = Me.AUX_CONVERT_MOL_TO_MASS(subst.Name, 2)
                                i += 1
                            Next

                            i = 0
                            For Each subst In Me.CurrentMaterialStream.Phases(7).Compounds.Values
                                subst.MoleFraction = Vs(i)
                                subst.FugacityCoeff = 1.0#
                                subst.ActivityCoeff = 0
                                subst.PartialVolume = 0
                                subst.PartialPressure = 0
                                i += 1
                            Next
                            For Each subst In Me.CurrentMaterialStream.Phases(7).Compounds.Values
                                subst.MassFraction = Me.AUX_CONVERT_MOL_TO_MASS(subst.Name, 7)
                            Next

                            Me.CurrentMaterialStream.Phases(3).Properties.massfraction = xl * Me.AUX_MMM(Phase.Liquid1) / (xl * Me.AUX_MMM(Phase.Liquid1) + xl2 * Me.AUX_MMM(Phase.Liquid2) + xv * Me.AUX_MMM(Phase.Vapor) + xs * Me.AUX_MMM(Phase.Solid))
                            Me.CurrentMaterialStream.Phases(4).Properties.massfraction = xl2 * Me.AUX_MMM(Phase.Liquid2) / (xl * Me.AUX_MMM(Phase.Liquid1) + xl2 * Me.AUX_MMM(Phase.Liquid2) + xv * Me.AUX_MMM(Phase.Vapor) + xs * Me.AUX_MMM(Phase.Solid))
                            Me.CurrentMaterialStream.Phases(2).Properties.massfraction = xv * Me.AUX_MMM(Phase.Vapor) / (xl * Me.AUX_MMM(Phase.Liquid1) + xl2 * Me.AUX_MMM(Phase.Liquid2) + xv * Me.AUX_MMM(Phase.Vapor) + xs * Me.AUX_MMM(Phase.Solid))
                            Me.CurrentMaterialStream.Phases(7).Properties.massfraction = xs * Me.AUX_MMM(Phase.Solid) / (xl * Me.AUX_MMM(Phase.Liquid1) + xl2 * Me.AUX_MMM(Phase.Liquid2) + xv * Me.AUX_MMM(Phase.Vapor) + xs * Me.AUX_MMM(Phase.Solid))

                            Dim constprops As New List(Of Interfaces.ICompoundConstantProperties)
                            For Each su As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                                constprops.Add(su.ConstantProperties)
                            Next

                            Dim HM, HV, HL, HL2, HS As Double

                            IObj?.SetCurrent()
                            IObj?.Paragraphs.Add("Calculating Enthalpy for Liquid Phase 1...")
                            If xl <> 0 Then HL = Me.DW_CalcEnthalpy(Vx, T, P, State.Liquid)
                            IObj?.SetCurrent()
                            IObj?.Paragraphs.Add("Calculating Enthalpy for Liquid Phase 2...")
                            If xl2 <> 0 Then HL2 = Me.DW_CalcEnthalpy(Vx2, T, P, State.Liquid)
                            IObj?.SetCurrent()
                            IObj?.Paragraphs.Add("Calculating Enthalpy for Vapor Phase...")
                            If xv <> 0 Then HV = Me.DW_CalcEnthalpy(Vy, T, P, State.Vapor)
                            IObj?.SetCurrent()
                            If xs <> 0 Then HS = Me.DW_CalcSolidEnthalpy(T, Vs, constprops)
                            HM = Me.CurrentMaterialStream.Phases(4).Properties.massfraction.GetValueOrDefault * HL2 + Me.CurrentMaterialStream.Phases(3).Properties.massfraction.GetValueOrDefault * HL + Me.CurrentMaterialStream.Phases(2).Properties.massfraction.GetValueOrDefault * HV + Me.CurrentMaterialStream.Phases(7).Properties.massfraction.GetValueOrDefault * HS

                            H = HM

                        Case FlashSpec.VAP

                            IObj?.Paragraphs.Add("The defined specification is PVF (Pressure and Vapor Fraction). DWSIM will call the 'Flash_PV' routine from the currently associated Flash Algorithm instance.")

                            Dim KI(n) As Double
                            Dim HM, HV, HL, HL2, HS As Double
                            Dim SM, SV, SL, SL2, SS As Double

                            i = 0
                            Do
                                KI(i) = 0
                                i = i + 1
                            Loop Until i = n + 1

                            T = Me.CurrentMaterialStream.Phases(0).Properties.temperature.GetValueOrDefault
                            P = Me.CurrentMaterialStream.Phases(0).Properties.pressure.GetValueOrDefault

                            If Not P.IsValid Or Not xv.IsValid Then Throw New ArgumentException("PVF Flash: " & Calculator.GetLocalString("ErrorInvalidFlashSpecValue"))
                            If Not P.IsPositive Or xv.IsNegative Then Throw New ArgumentException("PVF Flash: " & Calculator.GetLocalString("ErrorInvalidFlashSpecValue"))

                            Dim Vx, Vx2, Vy, Vs As Double()

                            IObj?.SetCurrent()

                            result = Me.FlashBase.Flash_PV(RET_VMOL(Phase.Mixture), P, xv, 0.0, Me)

                            T = result(4)

                            xl = result(0)
                            xv = result(1)
                            xl2 = result(7)
                            xs = result(9)

                            Vx = result(2)
                            Vy = result(3)
                            Vx2 = result(8)
                            Vs = result(10)

                            If Settings.CAPEOPENMode Then
                                If xv = 1.0 Then
                                    xl = 1.0E-20
                                    xl2 = 0.0
                                    xs = 0.0
                                ElseIf xl = 1.0 Then
                                    xv = 1.0E-20
                                    xl2 = 0.0
                                    xs = 0.0
                                End If
                            End If

                            Me.CurrentMaterialStream.Phases(3).Properties.molarfraction = xl
                            Me.CurrentMaterialStream.Phases(4).Properties.molarfraction = xl2
                            Me.CurrentMaterialStream.Phases(2).Properties.molarfraction = xv
                            Me.CurrentMaterialStream.Phases(7).Properties.molarfraction = xs

                            IObj?.SetCurrent()
                            IObj?.Paragraphs.Add("Calculating fugacity coefficient for compounds in Liquid Phase 1...")
                            Dim FCL = Me.DW_CalcFugCoeff(Vx, T, P, State.Liquid)
                            IObj?.SetCurrent()
                            IObj?.Paragraphs.Add("Calculating fugacity coefficient for compounds in Liquid Phase 2...")
                            Dim FCL2 = Me.DW_CalcFugCoeff(Vx2, T, P, State.Liquid)
                            IObj?.SetCurrent()
                            IObj?.Paragraphs.Add("Calculating fugacity coefficient for compounds in Vapor Phase...")
                            Dim FCV = Me.DW_CalcFugCoeff(Vy, T, P, State.Vapor)

                            i = 0
                            For Each subst In Me.CurrentMaterialStream.Phases(3).Compounds.Values
                                subst.MoleFraction = Vx(i)
                                subst.FugacityCoeff = FCL(i)
                                subst.ActivityCoeff = 0
                                subst.PartialVolume = 0
                                subst.PartialPressure = 0
                                i += 1
                            Next
                            For Each subst In Me.CurrentMaterialStream.Phases(3).Compounds.Values
                                subst.MassFraction = Me.AUX_CONVERT_MOL_TO_MASS(subst.Name, 3)
                            Next
                            i = 0
                            For Each subst In Me.CurrentMaterialStream.Phases(4).Compounds.Values
                                subst.MoleFraction = Vx2(i)
                                subst.FugacityCoeff = FCL2(i)
                                subst.ActivityCoeff = 0
                                subst.PartialVolume = 0
                                subst.PartialPressure = 0
                                i += 1
                            Next
                            For Each subst In Me.CurrentMaterialStream.Phases(4).Compounds.Values
                                subst.MassFraction = Me.AUX_CONVERT_MOL_TO_MASS(subst.Name, 4)
                            Next
                            i = 0
                            For Each subst In Me.CurrentMaterialStream.Phases(2).Compounds.Values
                                subst.MoleFraction = Vy(i)
                                subst.FugacityCoeff = FCV(i)
                                subst.ActivityCoeff = 0
                                subst.PartialVolume = 0
                                subst.PartialPressure = 0
                                i += 1
                            Next
                            For Each subst In Me.CurrentMaterialStream.Phases(2).Compounds.Values
                                subst.MassFraction = Me.AUX_CONVERT_MOL_TO_MASS(subst.Name, 2)
                            Next

                            i = 0
                            For Each subst In Me.CurrentMaterialStream.Phases(7).Compounds.Values
                                subst.MoleFraction = Vs(i)
                                subst.FugacityCoeff = 1.0#
                                subst.ActivityCoeff = 0
                                subst.PartialVolume = 0
                                subst.PartialPressure = 0
                                i += 1
                            Next
                            For Each subst In Me.CurrentMaterialStream.Phases(7).Compounds.Values
                                subst.MassFraction = Me.AUX_CONVERT_MOL_TO_MASS(subst.Name, 7)
                            Next

                            Me.CurrentMaterialStream.Phases(3).Properties.massfraction = xl * Me.AUX_MMM(Phase.Liquid1) / (xl * Me.AUX_MMM(Phase.Liquid1) + xl2 * Me.AUX_MMM(Phase.Liquid2) + xv * Me.AUX_MMM(Phase.Vapor) + xs * Me.AUX_MMM(Phase.Solid))
                            Me.CurrentMaterialStream.Phases(4).Properties.massfraction = xl2 * Me.AUX_MMM(Phase.Liquid2) / (xl * Me.AUX_MMM(Phase.Liquid1) + xl2 * Me.AUX_MMM(Phase.Liquid2) + xv * Me.AUX_MMM(Phase.Vapor) + xs * Me.AUX_MMM(Phase.Solid))
                            Me.CurrentMaterialStream.Phases(2).Properties.massfraction = xv * Me.AUX_MMM(Phase.Vapor) / (xl * Me.AUX_MMM(Phase.Liquid1) + xl2 * Me.AUX_MMM(Phase.Liquid2) + xv * Me.AUX_MMM(Phase.Vapor) + xs * Me.AUX_MMM(Phase.Solid))
                            Me.CurrentMaterialStream.Phases(7).Properties.massfraction = xs * Me.AUX_MMM(Phase.Solid) / (xl * Me.AUX_MMM(Phase.Liquid1) + xl2 * Me.AUX_MMM(Phase.Liquid2) + xv * Me.AUX_MMM(Phase.Vapor) + xs * Me.AUX_MMM(Phase.Solid))

                            Dim constprops As New List(Of Interfaces.ICompoundConstantProperties)
                            For Each su As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                                constprops.Add(su.ConstantProperties)
                            Next

                            IObj?.SetCurrent()
                            IObj?.Paragraphs.Add("Calculating Enthalpy for Liquid Phase 1...")
                            If xl <> 0 Then HL = Me.DW_CalcEnthalpy(Vx, T, P, State.Liquid)
                            IObj?.SetCurrent()
                            IObj?.Paragraphs.Add("Calculating Enthalpy for Liquid Phase 1...")
                            If xl2 <> 0 Then HL2 = Me.DW_CalcEnthalpy(Vx2, T, P, State.Liquid)
                            IObj?.SetCurrent()
                            IObj?.Paragraphs.Add("Calculating Enthalpy for Vapor Phase...")
                            If xv <> 0 Then HV = Me.DW_CalcEnthalpy(Vy, T, P, State.Vapor)
                            IObj?.SetCurrent()
                            IObj?.Paragraphs.Add("Calculating Enthalpy for Solid Phase...")
                            If xs <> 0 Then HS = Me.DW_CalcSolidEnthalpy(T, Vs, constprops)
                            HM = Me.CurrentMaterialStream.Phases(4).Properties.massfraction.GetValueOrDefault * HL2 + Me.CurrentMaterialStream.Phases(3).Properties.massfraction.GetValueOrDefault * HL + Me.CurrentMaterialStream.Phases(2).Properties.massfraction.GetValueOrDefault * HV + Me.CurrentMaterialStream.Phases(7).Properties.massfraction.GetValueOrDefault * HS

                            H = HM

                            IObj?.SetCurrent()
                            IObj?.Paragraphs.Add("Calculating Entropy for Liquid Phase 1...")
                            If xl <> 0 Then SL = Me.DW_CalcEntropy(Vx, T, P, State.Liquid)
                            IObj?.SetCurrent()
                            IObj?.Paragraphs.Add("Calculating Entropy for Liquid Phase 1...")
                            If xl2 <> 0 Then SL2 = Me.DW_CalcEntropy(Vx2, T, P, State.Liquid)
                            IObj?.SetCurrent()
                            IObj?.Paragraphs.Add("Calculating Entropy for Vapor Phase...")
                            If xv <> 0 Then SV = Me.DW_CalcEntropy(Vy, T, P, State.Vapor)
                            IObj?.SetCurrent()
                            IObj?.Paragraphs.Add("Calculating Entropy for Solid Phase...")
                            If xs <> 0 And T <> 298.15 Then SS = Me.DW_CalcSolidEnthalpy(T, Vs, constprops) / (T - 298.15)

                            SM = Me.CurrentMaterialStream.Phases(4).Properties.massfraction.GetValueOrDefault * SL2 + Me.CurrentMaterialStream.Phases(3).Properties.massfraction.GetValueOrDefault * SL + Me.CurrentMaterialStream.Phases(2).Properties.massfraction.GetValueOrDefault * SV + Me.CurrentMaterialStream.Phases(7).Properties.massfraction.GetValueOrDefault * SS

                            S = SM

                    End Select

            End Select

            If Settings.CAPEOPENMode Then
                Dim summf As Double = 0, sumwf As Double = 0
                For Each pi As PhaseInfo In Me.PhaseMappings.Values
                    If Not pi.PhaseLabel = "Disabled" Then
                        summf += Me.CurrentMaterialStream.Phases(pi.DWPhaseIndex).Properties.molarfraction.GetValueOrDefault
                        sumwf += Me.CurrentMaterialStream.Phases(pi.DWPhaseIndex).Properties.massfraction.GetValueOrDefault
                    End If
                Next
                If Abs(summf - 1) > 0.0001 Then
                    For Each pi As PhaseInfo In Me.PhaseMappings.Values
                        If Not pi.PhaseLabel = "Disabled" Then
                            If Not Me.CurrentMaterialStream.Phases(pi.DWPhaseIndex).Properties.molarfraction.HasValue Then
                                Me.CurrentMaterialStream.Phases(pi.DWPhaseIndex).Properties.molarfraction = 1 - summf
                                Me.CurrentMaterialStream.Phases(pi.DWPhaseIndex).Properties.massfraction = 1 - sumwf
                            End If
                        End If
                    Next
                End If
            End If

            With Me.CurrentMaterialStream

                .Phases(0).Properties.temperature = T
                .Phases(0).Properties.pressure = P
                .Phases(0).Properties.enthalpy = H
                .Phases(0).Properties.entropy = S

            End With

            IObj?.Paragraphs.Add("Phase Equilibrium for the currently associated Material Stream was calculated successfully.")

            IObj?.Close()

            Me.CurrentMaterialStream.AtEquilibrium = True

        End Sub

        Private Function EnthalpyTx(ByVal x As Double, ByVal otherargs As Object) As Double
            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()
            Inspector.Host.CheckAndAdd(IObj, "", "EnthalpyTx", "Single-Compound Enthalpy Calculation", "Temperature Loop", True)
            Dim er As Double = LoopVarF - Me.DW_CalcEnthalpy(Me.RET_VMOL(Phase.Mixture), x, LoopVarX, LoopVarState)
            IObj?.Close()
            Return er

        End Function

        Private Function EnthalpyPx(ByVal x As Double, ByVal otherargs As Object) As Double
            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()
            Inspector.Host.CheckAndAdd(IObj, "", "EnthalpyPx", "Single-Compound Enthalpy Calculation", "Pressure Loop", True)
            Dim er As Double = LoopVarF - Me.DW_CalcEnthalpy(Me.RET_VMOL(Phase.Mixture), LoopVarX, x, LoopVarState)
            IObj?.Close()
            Return er

        End Function

        Private Function EntropyTx(ByVal x As Double, ByVal otherargs As Object) As Double
            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()
            Inspector.Host.CheckAndAdd(IObj, "", "EntropyTx", "Single-Compound Enthalpy Calculation", "Temperature Loop", True)
            Dim er As Double = LoopVarF - Me.DW_CalcEntropy(Me.RET_VMOL(Phase.Mixture), x, LoopVarX, LoopVarState)
            IObj?.Close()
            Return er

        End Function

        Private Function EntropyPx(ByVal x As Double, ByVal otherargs As Object) As Double
            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()
            Inspector.Host.CheckAndAdd(IObj, "", "EntropyPx", "Single-Compound Entropy Calculation", "Pressure Loop", True)
            Dim er As Double = LoopVarF - Me.DW_CalcEntropy(Me.RET_VMOL(Phase.Mixture), LoopVarX, x, LoopVarState)
            IObj?.Close()
            Return er

        End Function

        Public Sub DW_CalcVazaoMassica()
            With Me.CurrentMaterialStream
                .Phases(0).Properties.massflow = .Phases(0).Properties.molarflow.GetValueOrDefault * Me.AUX_MMM(Phase.Mixture) / 1000
            End With
        End Sub

        Public Sub DW_CalcVazaoMolar()
            With Me.CurrentMaterialStream
                .Phases(0).Properties.molarflow = .Phases(0).Properties.massflow.GetValueOrDefault / Me.AUX_MMM(Phase.Mixture) * 1000
                If Double.IsNaN(.Phases(0).Properties.molarflow) Then .Phases(0).Properties.molarflow = 0.0#
            End With
        End Sub

        Public Sub DW_CalcVazaoVolumetrica()
            With Me.CurrentMaterialStream
                If .Phases(0).Properties.density.GetValueOrDefault > 0 Then
                    If .Phases(0).Properties.density.GetValueOrDefault > 0 Then
                        .Phases(0).Properties.volumetric_flow = .Phases(0).Properties.massflow.GetValueOrDefault / .Phases(0).Properties.density.GetValueOrDefault
                    Else
                        .Phases(0).Properties.volumetric_flow = 0.0#
                    End If
                Else
                    .Phases(0).Properties.volumetric_flow = 0.0#
                End If
            End With
        End Sub

        <System.Obsolete>
        Public Overridable Function DW_CalcMassaEspecifica_ISOL(ByVal Phase1 As Phase, ByVal T As Double, ByVal P As Double, Optional ByVal Pvp As Double = 0) As Double
            Throw New Exception("Obsolete function")
        End Function

        <System.Obsolete>
        Public Overridable Function DW_CalcViscosidadeDinamica_ISOL(ByVal Phase1 As Phase, ByVal T As Double, ByVal P As Double) As Double
            Throw New Exception("Obsolete function")
        End Function

        <System.Obsolete>
        Public Overridable Function DW_CalcTensaoSuperficial_ISOL(ByVal Phase1 As Phase, ByVal T As Double, ByVal P As Double) As Double
            Throw New Exception("Obsolete function")
        End Function

        <System.Obsolete>
        Public Overridable Function DW_CalcEquilibrio_ISOL(ByVal spec1 As FlashSpec, ByVal spec2 As FlashSpec, ByVal val1 As Double, ByVal val2 As Double, ByVal estimate As Double) As Object
            Throw New Exception("Obsolete function")
        End Function

        <System.Obsolete>
        Public Overridable Function DW_CalcEnergyFlowMistura_ISOL(ByVal T As Double, ByVal P As Double) As Double
            Throw New Exception("Obsolete function")
        End Function

        <System.Obsolete>
        Public Overridable Function DW_CalcCp_ISOL(ByVal Phase1 As PropertyPackages.Phase, ByVal T As Double, ByVal P As Double) As Double
            Throw New Exception("Obsolete function")
        End Function

        <System.Obsolete>
        Public Overridable Function DW_CalcCv_ISOL(ByVal Phase1 As PropertyPackages.Phase, ByVal T As Double, ByVal P As Double) As Double
            Throw New Exception("Obsolete function")
        End Function

        <System.Obsolete>
        Public Overridable Function DW_CalcK_ISOL(ByVal Phase1 As PropertyPackages.Phase, ByVal T As Double, ByVal P As Double) As Double
            Throw New Exception("Obsolete function")
        End Function

        <System.Obsolete>
        Public Overridable Function DW_CalcMM_ISOL(ByVal Phase1 As PropertyPackages.Phase, ByVal T As Double, ByVal P As Double) As Double
            Throw New Exception("Obsolete function")
        End Function

        <System.Obsolete>
        Public Overridable Function DW_CalcPVAP_ISOL(ByVal T As Double) As Double
            Throw New Exception("Obsolete function")
        End Function


        ''' <summary>
        ''' This function returns points to build the phase envelope.
        ''' </summary>
        ''' <param name="peoptions"></param>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Overridable Function DW_ReturnPhaseEnvelope(ByVal peoptions As PhaseEnvelopeOptions, Optional ByVal bw As System.ComponentModel.BackgroundWorker = Nothing) As Object

            If Settings.EnableGPUProcessing Then Calculator.InitComputeDevice()

            Dim i As Integer

            Dim n As Integer = Me.CurrentMaterialStream.Phases(0).Compounds.Count - 1

            Dim Vz(n) As Double
            Dim comp As Interfaces.ICompound

            Dim Vn = RET_VNAMES().ToList()

            i = 0
            For Each comp In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                Vz(i) = comp.MoleFraction.GetValueOrDefault
                i += 1
            Next

            If peoptions.ImmiscibleWater And Vn.Contains("Water") Then
                Vz(Vn.IndexOf("Water")) = 0.0
                Vz = Vz.NormalizeY()
            End If

            Dim j, k, l As Integer
            i = 0
            Do
                If Vz(i) = 0 Then j += 1
                i = i + 1
            Loop Until i = n + 1

            Dim VTc(n), Vpc(n), Vw(n), VVc(n), VKij(n, n), VKij3(n, n) As Double
            Dim Vm2(Vz.Length - 1 - j), VPc2(Vz.Length - 1 - j), VTc2(Vz.Length - 1 - j), VVc2(Vz.Length - 1 - j), Vw2(Vz.Length - 1 - j), VKij2(Vz.Length - 1 - j, Vz.Length - 1 - j)

            VTc = Me.RET_VTC()
            Vpc = Me.RET_VPC()
            VVc = Me.RET_VVC()
            Vw = Me.RET_VW()
            VKij = Me.RET_VKij

            i = 0
            k = 0
            Do
                If Vz(i) <> 0 Then
                    Vm2(k) = Vz(i)
                    VTc2(k) = VTc(i)
                    VPc2(k) = Vpc(i)
                    VVc2(k) = VVc(i)
                    Vw2(k) = Vw(i)
                    j = 0
                    l = 0
                    Do
                        If Vz(l) <> 0 Then
                            VKij2(k, j) = VKij(i, l)
                            j = j + 1
                        End If
                        l = l + 1
                    Loop Until l = n + 1
                    k = k + 1
                End If
                i = i + 1
            Loop Until i = n + 1

            Dim options As PhaseEnvelopeOptions = peoptions.Clone()

            With options
                If Not .BubbleUseCustomParameters Then
                    .BubbleCurveDeltaP = 101325
                    .BubbleCurveDeltaT = 1.0
                    .BubbleCurveInitialPressure = 0.0#
                    .BubbleCurveInitialTemperature = RET_VTB.Min * 0.3
                    .BubbleCurveInitialFlash = "TVF"
                    .BubbleCurveMaximumPoints = 500
                    .BubbleCurveMaximumTemperature = RET_VTC.Max * 1.2
                    .CheckLiquidInstability = False
                End If
                If Not .DewUseCustomParameters Then
                    .DewCurveDeltaP = 25000
                    .DewCurveDeltaT = 1.0
                    .DewCurveInitialPressure = 101325
                    .DewCurveInitialTemperature = 0.0#
                    .DewCurveInitialFlash = "PVF"
                    .DewCurveMaximumPoints = 500
                    .DewCurveMaximumTemperature = RET_VTC.Max * 1.5
                End If
            End With

            Dim T, P As Double

            Dim PB, PO, TVB, TVD, HB, HO, SB, SO, VB, VO, TE, PE, TH, PHsI, PHsII,
                PB1, TVB1, HB1, SB1, PB2, TVB2, HB2, SB2, VB1, VB2,
                TQ, PQ, TI, PI, TOWF, POWF, VOWF, HOWF, SOWF As New ArrayList

            Dim TCR, PCR, VCR As Double

            Dim CP As New ArrayList, recalcCP As Boolean = False, stopAtCP As Boolean = False

            If TypeOf Me Is PengRobinsonPropertyPackage Then
                If n > 0 Then
                    CP = New Utilities.TCP.Methods().CRITPT_PR(Vm2, VTc2, VPc2, VVc2, Vw2, VKij2)
                    If CP.Count = 0 Then CP = New Utilities.TCP.Methods().CRITPT_PR(Vm2, VTc2, VPc2, VVc2, Vw2, VKij3)
                    If CP.Count > 0 Then
                        Dim cp0 = CP(0)
                        TCR = cp0(0)
                        PCR = cp0(1)
                        VCR = cp0(2)
                        stopAtCP = True
                    Else
                        TCR = Me.AUX_TCM(Phase.Mixture)
                        PCR = Me.AUX_PCM(Phase.Mixture)
                        VCR = Me.AUX_VCM(Phase.Mixture)
                        recalcCP = True
                    End If
                Else
                    TCR = Me.AUX_TCM(Phase.Mixture)
                    PCR = Me.AUX_PCM(Phase.Mixture)
                    VCR = Me.AUX_VCM(Phase.Mixture)
                    CP.Add(New Object() {TCR, PCR, VCR})
                End If
            ElseIf TypeOf Me Is SRKPropertyPackage Then
                If n > 0 Then
                    CP = New Utilities.TCP.Methods_SRK().CRITPT_PR(Vm2, VTc2, VPc2, VVc2, Vw2, VKij2)
                    If CP.Count = 0 Then CP = New Utilities.TCP.Methods_SRK().CRITPT_PR(Vm2, VTc2, VPc2, VVc2, Vw2, VKij3)
                    If CP.Count > 0 Then
                        Dim cp0 = CP(0)
                        TCR = cp0(0)
                        PCR = cp0(1)
                        VCR = cp0(2)
                        stopAtCP = True
                    Else
                        TCR = Me.AUX_TCM(Phase.Mixture)
                        PCR = Me.AUX_PCM(Phase.Mixture)
                        VCR = Me.AUX_VCM(Phase.Mixture)
                        recalcCP = True
                    End If
                Else
                    TCR = Me.AUX_TCM(Phase.Mixture)
                    PCR = Me.AUX_PCM(Phase.Mixture)
                    VCR = Me.AUX_VCM(Phase.Mixture)
                    CP.Add(New Object() {TCR, PCR, VCR})
                End If
            Else
                If n > 0 Then
                    CP = New ArrayList(DW_CalculateCriticalPoints())
                    If CP.Count > 0 Then
                        Dim cp0 = CP(0)
                        TCR = cp0(0)
                        PCR = cp0(1)
                        VCR = cp0(2)
                        stopAtCP = True
                    Else
                        TCR = Me.AUX_TCM(Phase.Mixture)
                        PCR = Me.AUX_PCM(Phase.Mixture)
                        VCR = Me.AUX_VCM(Phase.Mixture)
                        recalcCP = True
                    End If
                Else
                    TCR = Me.AUX_TCM(Phase.Mixture)
                    PCR = Me.AUX_PCM(Phase.Mixture)
                    VCR = Me.AUX_VCM(Phase.Mixture)
                    CP.Add(New Object() {TCR, PCR, VCR})
                End If
            End If

            Dim beta As Double = 10.0#

            Dim tmp2 As Object
            Dim result As IFlashCalculationResult = Nothing
            Dim KI(n) As Double

            Dim tpflash As New NestedLoops3PV3() With {.FlashSettings = Me.FlashBase.FlashSettings}
            tpflash.FlashSettings(Enums.FlashSetting.ThreePhaseFlashStabTestSeverity) = 2

            j = 0
            Do
                KI(j) = 0
                j = j + 1
            Loop Until j = n + 1

            If peoptions.ImmiscibleWater And Vn.Contains("Water") Then

                Dim Twx As Double = 273.15
                Dim Pwx As Double = 0.0
                Dim st As New Auxiliary.IAPWS_IF97

                For i = 0 To 10000

                    TOWF.Add(Twx)
                    Pwx = AUX_PVAPi("Water", Twx)
                    POWF.Add(Pwx)
                    HOWF.Add(st.enthalpySatVapTW(Twx))
                    SOWF.Add(st.entropySatVapTW(Twx))
                    VOWF.Add(1 / st.densSatVapTW(Twx) * Me.AUX_MMM(Phase.Mixture))

                    Twx += 5.0

                    If Pwx > 217.7 * 101325 Then Exit For

                Next

            End If

            i = 0
            P = options.BubbleCurveInitialPressure
            T = options.BubbleCurveInitialTemperature
            Do

                If i < 2 Then

                    If options.BubbleCurveInitialFlash = "TVF" Then
                        tmp2 = Me.FlashBase.Flash_TV(Vz, T, 0, options.BubbleCurveInitialPressure, Me)
                        TVB.Add(T)
                        PB.Add(tmp2(4))
                        P = PB(PB.Count - 1)
                        HB.Add(Me.DW_CalcEnthalpy(Vz, T, P, State.Liquid))
                        SB.Add(Me.DW_CalcEntropy(Vz, T, P, State.Liquid))
                        VB.Add(1 / Me.AUX_LIQDENS(T, Vz, P, P) * Me.AUX_MMM(Phase.Mixture))
                        KI = tmp2(6)
                    Else
                        tmp2 = Me.FlashBase.Flash_PV(Vz, P, 0, options.BubbleCurveInitialTemperature, Me)
                        TVB.Add(tmp2(4))
                        PB.Add(P)
                        T = TVB(TVB.Count - 1)
                        HB.Add(Me.DW_CalcEnthalpy(Vz, T, P, State.Liquid))
                        SB.Add(Me.DW_CalcEntropy(Vz, T, P, State.Liquid))
                        VB.Add(1 / Me.AUX_LIQDENS(T, Vz, P, P) * Me.AUX_MMM(Phase.Mixture))
                        KI = tmp2(6)
                    End If

                    'check instability

                    If options.CheckLiquidInstability Then

                        result = tpflash.CalculateEquilibrium(FlashSpec.P, FlashSpec.T, P, T, Me, RET_VMOL(Phase.Mixture), Nothing, 0)

                        If result.ResultException Is Nothing Then
                            If result.GetLiquidPhase2MoleFraction > 0.0# Then
                                'liquid phase is unstable
                                'bubble line liquid phase 1
                                Try
                                    tmp2 = Me.FlashBase.Flash_TV(result.GetLiquidPhase1MoleFractions, T, 0.0#, P * 1.05, Me)
                                    TVB1.Add(T)
                                    PB1.Add(tmp2(4))
                                    HB1.Add(Me.DW_CalcEnthalpy(result.GetLiquidPhase1MoleFractions, T, tmp2(4), State.Liquid))
                                    SB1.Add(Me.DW_CalcEntropy(result.GetLiquidPhase1MoleFractions, T, tmp2(4), State.Liquid))
                                    VB1.Add(1 / Me.AUX_LIQDENS(T, result.GetLiquidPhase1MoleFractions) * Me.AUX_MMM(result.GetLiquidPhase1MoleFractions))
                                Catch ex As Exception
                                End Try
                                'bubble line liquid phase 2
                                Try
                                    tmp2 = Me.FlashBase.Flash_TV(result.GetLiquidPhase2MoleFractions, T, 0.0#, P * 1.05, Me)
                                    TVB2.Add(T)
                                    PB2.Add(tmp2(4))
                                    HB2.Add(Me.DW_CalcEnthalpy(result.GetLiquidPhase2MoleFractions, T, tmp2(4), State.Liquid))
                                    SB2.Add(Me.DW_CalcEntropy(result.GetLiquidPhase2MoleFractions, T, tmp2(4), State.Liquid))
                                    VB2.Add(1 / Me.AUX_LIQDENS(T, result.GetLiquidPhase2MoleFractions) * Me.AUX_MMM(result.GetLiquidPhase2MoleFractions))
                                Catch ex As Exception
                                End Try
                            End If
                        Else
                            Throw result.ResultException
                        End If

                    End If

                    If options.BubbleCurveInitialFlash = "TVF" Then
                        T = T + options.BubbleCurveDeltaT
                    Else
                        P = P + options.BubbleCurveDeltaP
                    End If

                Else

                    If beta < 20 Then
                        Try
                            tmp2 = Me.FlashBase.Flash_TV(Vz, T, 0, PB(PB.Count - 1), Me, True, KI)
                            TVB.Add(T)
                            PB.Add(tmp2(4))
                            P = PB(PB.Count - 1)
                            HB.Add(Me.DW_CalcEnthalpy(Vz, T, P, State.Liquid))
                            SB.Add(Me.DW_CalcEntropy(Vz, T, P, State.Liquid))
                            VB.Add(1 / Me.AUX_LIQDENS(T, Vz, P, P) * Me.AUX_MMM(Phase.Mixture))
                            KI = tmp2(6)
                            beta = (Math.Log(PB(PB.Count - 1) / 101325) - Math.Log(PB(PB.Count - 2) / 101325)) / (Math.Log(TVB(TVB.Count - 1)) - Math.Log(TVB(TVB.Count - 2)))
                        Catch ex As Exception
                        End Try
                    Else
                        Try
                            tmp2 = Me.FlashBase.Flash_PV(Vz, P, 0, TVB(TVB.Count - 1), Me, True, KI)
                            TVB.Add(tmp2(4))
                            PB.Add(P)
                            T = TVB(TVB.Count - 1)
                            HB.Add(Me.DW_CalcEnthalpy(Vz, T, P, State.Liquid))
                            SB.Add(Me.DW_CalcEntropy(Vz, T, P, State.Liquid))
                            VB.Add(1 / Me.AUX_LIQDENS(T, Vz, P, P) * Me.AUX_MMM(Phase.Mixture))
                            KI = tmp2(6)
                            beta = (Math.Log(PB(PB.Count - 1) / 101325) - Math.Log(PB(PB.Count - 2) / 101325)) / (Math.Log(TVB(TVB.Count - 1)) - Math.Log(TVB(TVB.Count - 2)))
                        Catch ex As Exception
                        End Try
                    End If


                    If options.CheckLiquidInstability Then

                        'check instability

                        result = tpflash.CalculateEquilibrium(FlashSpec.P, FlashSpec.T, P, T, Me, RET_VMOL(Phase.Mixture), Nothing, 0)

                        If result.ResultException Is Nothing Then
                            If result.GetLiquidPhase2MoleFraction > 0.0# Then
                                'liquid phase is unstable
                                'bubble line liquid phase 1
                                Try
                                    tmp2 = Me.FlashBase.Flash_TV(result.GetLiquidPhase1MoleFractions, T, 0.0#, P * 1.05, Me)
                                    TVB1.Add(T)
                                    PB1.Add(tmp2(4))
                                    HB1.Add(Me.DW_CalcEnthalpy(result.GetLiquidPhase1MoleFractions, T, tmp2(4), State.Liquid))
                                    SB1.Add(Me.DW_CalcEntropy(result.GetLiquidPhase1MoleFractions, T, tmp2(4), State.Liquid))
                                    VB1.Add(1 / Me.AUX_LIQDENS(T, result.GetLiquidPhase1MoleFractions) * Me.AUX_MMM(result.GetLiquidPhase1MoleFractions))
                                Catch ex As Exception
                                End Try
                                'bubble line liquid phase 2
                                Try
                                    tmp2 = Me.FlashBase.Flash_TV(result.GetLiquidPhase2MoleFractions, T, 0.0#, P * 1.05, Me)
                                    TVB2.Add(T)
                                    PB2.Add(tmp2(4))
                                    HB2.Add(Me.DW_CalcEnthalpy(result.GetLiquidPhase2MoleFractions, T, tmp2(4), State.Liquid))
                                    SB2.Add(Me.DW_CalcEntropy(result.GetLiquidPhase2MoleFractions, T, tmp2(4), State.Liquid))
                                    VB2.Add(1 / Me.AUX_LIQDENS(T, result.GetLiquidPhase2MoleFractions) * Me.AUX_MMM(result.GetLiquidPhase2MoleFractions))
                                Catch ex As Exception
                                End Try
                            End If
                        Else
                            Throw result.ResultException
                        End If

                    End If

                    If stopAtCP Then
                        If Math.Abs(T - TCR) < 2.0 And Math.Abs(P - PCR) < 10000 Or T > TCR Then
                            Exit Do
                        End If
                    End If

                    If beta < 20 Then
                        If Math.Abs(T - TCR) / TCR < 0.01 And Math.Abs(P - PCR) / PCR < 0.02 Then
                            T = T + options.BubbleCurveDeltaT * 0.5
                        Else
                            T = T + options.BubbleCurveDeltaT
                        End If
                    Else
                        If Math.Abs(T - TCR) / TCR < 0.01 And Math.Abs(P - PCR) / PCR < 0.01 Then
                            P = P + options.BubbleCurveDeltaP * 0.1
                        Else
                            P = P + options.BubbleCurveDeltaP
                        End If
                    End If

                End If

                If Double.IsNaN(beta) Or Double.IsInfinity(beta) Then beta = 0.0#

                If TypeOf Me Is PengRobinsonPropertyPackage Or TypeOf Me Is SRKPropertyPackage Then
                    If Math.Abs(T - TCR) / TCR < 0.002 And Math.Abs(P - PCR) / PCR < 0.002 Then
                        Exit Do
                    End If
                End If

                If bw IsNot Nothing Then If bw.CancellationPending Then Exit Do Else bw.ReportProgress(0, "Bubble Points... " & ((i + 1) / options.BubbleCurveMaximumPoints * 100).ToString("N1") & "%")

                i = i + 1

            Loop Until i >= options.BubbleCurveMaximumPoints Or PB(PB.Count - 1) = 0 Or PB(PB.Count - 1) < 0 Or TVB(TVB.Count - 1) < 0 Or
                        Double.IsNaN(PB(PB.Count - 1)) = True Or Double.IsNaN(TVB(TVB.Count - 1)) = True Or T >= options.BubbleCurveMaximumTemperature

            Dim Switch = False

            beta = 10

            j = 0
            Do
                KI(j) = 0
                j = j + 1
            Loop Until j = n + 1

            i = 0
            P = options.DewCurveInitialPressure
            T = options.DewCurveInitialTemperature
            Do

                If i < 2 Then

                    If options.DewCurveInitialFlash = "TVF" Then
                        tmp2 = Me.FlashBase.Flash_TV(Vz, T, 1, options.DewCurveInitialPressure, Me)
                        TVD.Add(T)
                        PO.Add(tmp2(4))
                        P = PO(PO.Count - 1)
                        HO.Add(Me.DW_CalcEnthalpy(Vz, T, P, State.Vapor))
                        SO.Add(Me.DW_CalcEntropy(Vz, T, P, State.Vapor))
                        VO.Add(1 / Me.AUX_VAPDENS(T, P) * Me.AUX_MMM(Phase.Mixture))
                        KI = tmp2(6)
                        T = T + options.DewCurveDeltaT
                    Else
                        tmp2 = Me.FlashBase.Flash_PV(Vz, P, 1, options.DewCurveInitialTemperature, Me)
                        TVD.Add(tmp2(4))
                        PO.Add(P)
                        T = TVD(TVD.Count - 1)
                        HO.Add(Me.DW_CalcEnthalpy(Vz, T, P, State.Vapor))
                        SO.Add(Me.DW_CalcEntropy(Vz, T, P, State.Vapor))
                        VO.Add(1 / Me.AUX_VAPDENS(T, P) * Me.AUX_MMM(Phase.Mixture))
                        KI = tmp2(6)
                        P = P + options.DewCurveDeltaP
                    End If

                Else

                    If beta < 0.0 Then
                        Try
                            tmp2 = Me.FlashBase.Flash_TV(Vz, T, 1, PO(PO.Count - 1), Me, False, KI)
                            TVD.Add(T)
                            PO.Add(tmp2(4))
                            P = PO(PO.Count - 1)
                            HO.Add(Me.DW_CalcEnthalpy(Vz, T, P, State.Vapor))
                            SO.Add(Me.DW_CalcEntropy(Vz, T, P, State.Vapor))
                            VO.Add(1 / Me.AUX_VAPDENS(T, P) * Me.AUX_MMM(Phase.Mixture))
                            KI = tmp2(6)
                        Catch ex As Exception
                        End Try
                    Else
                        Try
                            tmp2 = Me.FlashBase.Flash_PV(Vz, P, 1, TVD(TVD.Count - 1), Me, False, KI)
                            TVD.Add(tmp2(4))
                            PO.Add(P)
                            T = TVD(TVD.Count - 1)
                            HO.Add(Me.DW_CalcEnthalpy(Vz, T, P, State.Vapor))
                            SO.Add(Me.DW_CalcEntropy(Vz, T, P, State.Vapor))
                            VO.Add(1 / Me.AUX_VAPDENS(T, P) * Me.AUX_MMM(Phase.Mixture))
                            KI = tmp2(6)
                        Catch ex As Exception
                        End Try
                    End If

                    beta = (Math.Log(PO(PO.Count - 1) / 101325) - Math.Log(PO(PO.Count - 2) / 101325)) / (Math.Log(TVD(TVD.Count - 1)) - Math.Log(TVD(TVD.Count - 2)))

                    If PO.Count > 50 Then
                        Dim p1 As Double = PO(PO.Count - 1)
                        Dim p2 As Double = PO(PO.Count - 2)
                        Dim p3 As Double = PO(PO.Count - 3)
                        Dim t1 As Double = TVD(TVD.Count - 1)
                        Dim t2 As Double = TVD(TVD.Count - 2)
                        Dim t3 As Double = TVD(TVD.Count - 3)
                        Dim d1 = ((p2 - p1) ^ 2 + (t2 - t1) ^ 2) ^ 0.5
                        Dim d2 = ((p3 - p2) ^ 2 + (t3 - t2) ^ 2) ^ 0.5
                        If d2 > d1 * 50 And d1 > 0.0 Then
                            PO.RemoveAt(PO.Count - 1)
                            TVD.RemoveAt(TVD.Count - 1)
                            HO.RemoveAt(HO.Count - 1)
                            SO.RemoveAt(SO.Count - 1)
                            VO.RemoveAt(VO.Count - 1)
                            Exit Do
                        End If
                    End If

                    If stopAtCP Then
                        If Math.Abs(T - TCR) < 2.0 And Math.Abs(P - PCR) < 10000 Then
                            Exit Do
                        End If
                    End If

                    If TVD(TVD.Count - 1) - TVD(TVD.Count - 2) <= 0 Then
                        T = T - options.DewCurveDeltaT * 0.1
                    Else
                        If beta < 0.0 Then
                            If TVD(TVD.Count - 1) - TVD(TVD.Count - 2) <= 0 Then
                                If Math.Abs(T - TCR) / TCR < 0.02 And Math.Abs(P - PCR) / PCR < 0.02 Then
                                    T = T - options.DewCurveDeltaT * 0.1
                                Else
                                    T = T - options.DewCurveDeltaT
                                End If
                            Else
                                If Math.Abs(T - TCR) / TCR < 0.02 And Math.Abs(P - PCR) / PCR < 0.02 Then
                                    T = T + options.DewCurveDeltaT * 0.1
                                Else
                                    T = T + options.DewCurveDeltaT
                                End If
                            End If
                        Else
                            If Math.Abs(T - TCR) / TCR < 0.05 And Math.Abs(P - PCR) / PCR < 0.05 Then
                                P = P + options.DewCurveDeltaP * 0.25
                            Else
                                P = P + options.DewCurveDeltaP
                            End If
                        End If
                    End If

                    If i >= PO.Count Then
                        i = i - 1
                    End If

                    If Double.IsNaN(beta) Or Double.IsInfinity(beta) Then beta = 0.0#

                End If

                If bw IsNot Nothing Then If bw.CancellationPending Then Exit Do Else bw.ReportProgress(0, "Dew Points... " & ((i + 1) / options.DewCurveMaximumPoints * 100).ToString("N1") & "%")

                i = i + 1

            Loop Until i >= options.DewCurveMaximumPoints Or PO(PO.Count - 1) = 0 Or PO(PO.Count - 1) < 0 Or TVD(TVD.Count - 1) < 0 Or
                        Double.IsNaN(PO(PO.Count - 1)) = True Or Double.IsNaN(TVD(TVD.Count - 1)) = True Or T >= options.DewCurveMaximumTemperature

            If recalcCP OrElse (Not TypeOf Me Is PengRobinsonPropertyPackage And Not TypeOf Me Is SRKPropertyPackage) Then

                'calculate intersection point, if any

                Dim dist As New Dictionary(Of Integer, Dictionary(Of Integer, Double))

                Dim maxP, maxT As Double
                maxP = Max(PB.ToArray.Max, PO.ToArray.Max)
                maxT = Max(TVB.ToArray.Max, TVB.ToArray.Max)

                For i = 0 To PB.Count - 1
                    dist.Add(i, New Dictionary(Of Integer, Double))
                    For j = 0 To PO.Count - 1
                        dist(i).Add(j, Abs(PB(i) - PO(j)) / maxP + Abs(TVB(i) - TVD(j)) / maxT)
                    Next
                Next

                Dim mindist As Double, ib, id As Integer

                mindist = 1.0E+20

                i = 0
                For Each item In dist.Values
                    j = 0
                    For Each item2 In item.Values
                        j += 1
                        If item2 < mindist Then
                            mindist = item2
                            ib = i
                            id = j
                        End If
                    Next
                    i += 1
                Next

                If mindist < (Min(options.BubbleCurveDeltaP, options.DewCurveDeltaP) / maxP + Min(options.BubbleCurveDeltaT, options.DewCurveDeltaT) / maxT) Then

                    'there is an intersection, update critical point

                    Dim Tc, Pc, Vc As Double

                    Tc = (TVB(ib) + TVD(id)) / 2
                    Pc = (PB(ib) + PO(id)) / 2
                    Vc = 1 / Me.AUX_VAPDENS(Tc, Pc) * Me.AUX_MMM(Phase.Mixture) / 1000

                    CP.Clear()
                    CP.Add(New Object() {Tc, Pc, Vc})

                    'remove data beyond intersection point

                    TVB = New ArrayList(TVB.GetRange(0, ib))
                    PB = New ArrayList(PB.GetRange(0, ib))
                    VB = New ArrayList(VB.GetRange(0, ib))
                    HB = New ArrayList(HB.GetRange(0, ib))
                    SB = New ArrayList(SB.GetRange(0, ib))

                    TVB.Add(Tc)
                    PB.Add(Pc)
                    VB.Add(Vc)
                    HB.Add(Me.DW_CalcEnthalpy(Vz, Tc, Pc, State.Vapor))
                    SB.Add(Me.DW_CalcEntropy(Vz, Tc, Pc, State.Vapor))

                    TVB.Add(0.0#)
                    PB.Add(0.0#)
                    VB.Add(0.0#)
                    HB.Add(0.0#)
                    SB.Add(0.0#)

                    TVB.Add(0.0#)
                    PB.Add(0.0#)
                    VB.Add(0.0#)
                    HB.Add(0.0#)
                    SB.Add(0.0#)

                    TVD = New ArrayList(TVD.GetRange(0, id))
                    PO = New ArrayList(PO.GetRange(0, id))
                    VO = New ArrayList(VO.GetRange(0, id))
                    HO = New ArrayList(HO.GetRange(0, id))
                    SO = New ArrayList(SO.GetRange(0, id))

                    TVD.Add(Tc)
                    PO.Add(Pc)
                    VO.Add(Vc)
                    HO.Add(Me.DW_CalcEnthalpy(Vz, Tc, Pc, State.Vapor))
                    SO.Add(Me.DW_CalcEntropy(Vz, Tc, Pc, State.Vapor))

                    TVD.Add(0.0#)
                    PO.Add(0.0#)
                    VO.Add(0.0#)
                    HO.Add(0.0#)
                    SO.Add(0.0#)

                    TVD.Add(0.0#)
                    PO.Add(0.0#)
                    VO.Add(0.0#)
                    HO.Add(0.0#)
                    SO.Add(0.0#)

                End If

            End If

            'complete lines up to critical point

            Dim POL = PO(PO.Count - 1)
            Dim TOL = TVD(TVD.Count - 1)
            Dim PBL = PB(PB.Count - 1)
            Dim TBL = TVB(TVB.Count - 1)

            Dim DPO = (PCR - POL) / 10
            Dim DTO = (TCR - TOL) / 10
            Dim DPB = (PCR - PBL) / 10
            Dim DTB = (TCR - TBL) / 10

            'If Math.Abs(DPO * 10) > 101325 Or Math.Abs(DPB * 10) * 5 Or
            '        Math.Abs(DTO * 10) > 101325 Or Math.Abs(DTB * 10) * 5 Then

            '    Dim POlast = PO.ToDoubleList()
            '    Dim PBLast = PB.ToDoubleList()
            '    Dim TOlast = TVD.ToDoubleList()
            '    Dim TBlast = TVB.ToDoubleList()

            '    POlast.Add(PCR)
            '    PBLast.Add(PCR)
            '    TOlast.Add(TCR)
            '    TBlast.Add(TCR)

            '    POlast.Reverse()
            '    PBLast.Reverse()
            '    TOlast.Reverse()
            '    TBlast.Reverse()

            '    Dim POLn = POL + DPO
            '    Dim PBLn = PBL + DPB
            '    Dim TOLn = TOL + DTO
            '    Dim TBLn = TBL + DTB
            '    For i = 0 To 11
            '        If Math.Abs(DTB) > 0.2 Then
            '            Dim PBn = MathNet.Numerics.Interpolate.RationalWithPoles(TBlast.Take(10), PBLast.Take(10)).Interpolate(TBLn)
            '            PB.Add(PBn)
            '            TVB.Add(TBLn)
            '        Else
            '            Dim TBn = MathNet.Numerics.Interpolate.RationalWithPoles(PBLast.Take(10), TOlast.Take(10)).Interpolate(PBLn)
            '            PB.Add(PBLn)
            '            TVB.Add(TBn)
            '        End If
            '        If Math.Abs(DTO) > 0.2 Then
            '            Dim POn = MathNet.Numerics.Interpolate.RationalWithPoles(TOlast.Take(10), POlast.Take(10)).Interpolate(TOLn)
            '            PO.Add(POn)
            '            TVD.Add(TOLn)
            '        Else
            '            Dim TOn = MathNet.Numerics.Interpolate.RationalWithPoles(POlast.Take(10), TOlast.Take(10)).Interpolate(POLn)
            '            PO.Add(POLn)
            '            TVD.Add(TOn)
            '        End If
            '        POLn += DPO
            '        PBLn += DPB
            '        TOLn += DTO
            '        TBLn += DTB
            '    Next

            'End If

            'calculate quality curve

            beta = 10

            If options.QualityLine Then

                Dim npoints As Integer
                If options.QualityValue < 0.5 Then
                    npoints = options.BubbleCurveMaximumPoints
                Else
                    npoints = options.DewCurveMaximumPoints
                End If

                j = 0
                Do
                    KI(j) = 0
                    j = j + 1
                Loop Until j = n + 1

                i = 0
                If options.QualityValue < 0.5 Then
                    P = options.BubbleCurveInitialPressure
                    T = options.BubbleCurveInitialTemperature
                Else
                    P = options.DewCurveInitialPressure
                    T = options.DewCurveInitialTemperature
                End If
                Do
                    If i < 2 Then
                        Try
                            tmp2 = Me.FlashBase.Flash_PV(Vz, P, options.QualityValue, 0, Me, False, KI)
                            TQ.Add(tmp2(4))
                            PQ.Add(P)
                            T = TQ(TQ.Count - 1)
                            KI = tmp2(6)
                        Catch ex As Exception
                        Finally
                            If options.QualityValue < 0.5 Then
                                P = P + options.BubbleCurveDeltaP
                            Else
                                P = P + options.DewCurveDeltaP
                            End If
                        End Try
                    Else
                        If beta < 2 Then
                            Try
                                tmp2 = Me.FlashBase.Flash_TV(Vz, T, options.QualityValue, PQ(PQ.Count - 1), Me, True, KI)
                                TQ.Add(T)
                                PQ.Add(tmp2(4))
                                P = PQ(PQ.Count - 1)
                                KI = tmp2(6)
                                beta = (Math.Log(PQ(PQ.Count - 1) / 101325) - Math.Log(PQ(PQ.Count - 2) / 101325)) / (Math.Log(TQ(TQ.Count - 1)) - Math.Log(TQ(TQ.Count - 2)))
                            Catch ex As Exception
                            Finally
                                If Math.Abs(T - TCR) / TCR < 0.1 And Math.Abs(P - PCR) / PCR < 0.2 Then
                                    If options.QualityValue < 0.5 Then
                                        T = T + options.BubbleCurveDeltaT * 0.25
                                    Else
                                        T = T + options.DewCurveDeltaT * 0.25
                                    End If
                                Else
                                    If options.QualityValue < 0.5 Then
                                        T = T + options.BubbleCurveDeltaT
                                    Else
                                        T = T + options.DewCurveDeltaT
                                    End If
                                End If
                            End Try
                        Else
                            Try
                                tmp2 = Me.FlashBase.Flash_PV(Vz, P, options.QualityValue, TQ(TQ.Count - 1), Me, True, KI)
                                TQ.Add(tmp2(4))
                                PQ.Add(P)
                                T = TQ(TQ.Count - 1)
                                KI = tmp2(6)
                                beta = (Math.Log(PQ(PQ.Count - 1) / 101325) - Math.Log(PQ(PQ.Count - 2) / 101325)) / (Math.Log(TQ(TQ.Count - 1)) - Math.Log(TQ(TQ.Count - 2)))
                            Catch ex As Exception
                            Finally
                                If Math.Abs(T - TCR) / TCR < 0.1 And Math.Abs(P - PCR) / PCR < 0.1 Then
                                    If options.QualityValue < 0.5 Then
                                        P = P + options.BubbleCurveDeltaP * 0.1
                                    Else
                                        P = P + options.DewCurveDeltaP * 0.1
                                    End If
                                Else
                                    If options.QualityValue < 0.5 Then
                                        P = P + options.BubbleCurveDeltaP
                                    Else
                                        P = P + options.DewCurveDeltaP
                                    End If
                                End If
                            End Try
                        End If
                    End If

                    If bw IsNot Nothing Then If bw.CancellationPending Then Exit Do Else bw.ReportProgress(0, "Quality Line... " & ((i + 1) / npoints * 100).ToString("N1") & "%")

                    i = i + 1
                    If i > 2 Then
                        If PQ(PQ.Count - 1) = PQ(PQ.Count - 2) Or TQ(TQ.Count - 1) = TQ(TQ.Count - 2) Then Exit Do
                    End If

                Loop Until i >= npoints Or PQ(PQ.Count - 1) = 0 Or PQ(PQ.Count - 1) < 0 Or TQ(TQ.Count - 1) < 0 Or
                            Double.IsNaN(PQ(PQ.Count - 1)) = True Or Double.IsNaN(TQ(TQ.Count - 1)) = True Or
                            Math.Abs(T - TCR) / TCR < 0.02 And Math.Abs(P - PCR) / PCR < 0.02

            Else

                TQ.Add(0)
                PQ.Add(0)

            End If

            If n > 0 And options.StabilityCurve Then
                If TypeOf Me Is PengRobinsonPropertyPackage Then
                    If bw IsNot Nothing Then bw.ReportProgress(0, "Stability Line")
                    Dim res As ArrayList = New Utilities.TCP.Methods().STABILITY_CURVE(Vm2, VTc2, VPc2, VVc2, Vw2, VKij2)
                    i = 0
                    Do
                        TE.Add(res(i)(0))
                        PE.Add(res(i)(1))
                        i += 1
                    Loop Until i = res.Count
                ElseIf TypeOf Me Is SRKPropertyPackage Then
                    If bw IsNot Nothing Then bw.ReportProgress(0, "Stability Line")
                    Dim res As ArrayList = New Utilities.TCP.Methods_SRK().STABILITY_CURVE(Vm2, VTc2, VPc2, VVc2, Vw2, VKij2)
                    i = 0
                    Do
                        TE.Add(res(i)(0))
                        PE.Add(res(i)(1))
                        i += 1
                    Loop Until i = res.Count
                Else
                    TE.Add(0)
                    PE.Add(0)
                End If
            Else
                TE.Add(0)
                PE.Add(0)
            End If

            Dim Pest, Tmax As Double, eos As String

            If TypeOf Me Is PengRobinsonPropertyPackage Then eos = "PR" Else eos = "SRK"

            Pest = PCR * 10
            Dim Tmin As Double = MathEx.Common.Max(Me.RET_VTF)
            If Tmin = 0.0# Then Tmin = MathEx.Common.Min(Me.RET_VTB) * 0.4
            Tmax = TCR * 1.4

            If options.PhaseIdentificationCurve Then
                If bw IsNot Nothing Then bw.ReportProgress(0, "Phase Identification Parameter")
                If TypeOf Me Is PengRobinsonPropertyPackage Or TypeOf Me Is SRKPropertyPackage Then
                    For T = Tmin To Tmax Step 5
                        TI.Add(T)
                        PI.Add(Auxiliary.FlashAlgorithms.FlashAlgorithm.CalcPIPressure(Vz, Pest, T, Me, eos))
                        'Pest = PI(PI.Count - 1)
                    Next
                Else
                    TI.Add(0)
                    PI.Add(0)
                End If
            Else
                TI.Add(0)
                PI.Add(0)
            End If

            If TVB.Count > 1 Then TVB.RemoveAt(TVB.Count - 1)
            If PB.Count > 1 Then PB.RemoveAt(PB.Count - 1)
            If HB.Count > 1 Then HB.RemoveAt(HB.Count - 1)
            If SB.Count > 1 Then SB.RemoveAt(SB.Count - 1)
            If VB.Count > 1 Then VB.RemoveAt(VB.Count - 1)
            If TVB.Count > 1 Then TVB.RemoveAt(TVB.Count - 1)
            If PB.Count > 1 Then PB.RemoveAt(PB.Count - 1)
            If HB.Count > 1 Then HB.RemoveAt(HB.Count - 1)
            If SB.Count > 1 Then SB.RemoveAt(SB.Count - 1)
            If VB.Count > 1 Then VB.RemoveAt(VB.Count - 1)

            If TOWF.Count > 1 Then TOWF.RemoveAt(TOWF.Count - 1)
            If POWF.Count > 1 Then POWF.RemoveAt(POWF.Count - 1)
            If HOWF.Count > 1 Then HOWF.RemoveAt(HOWF.Count - 1)
            If SOWF.Count > 1 Then SOWF.RemoveAt(SOWF.Count - 1)
            If VOWF.Count > 1 Then VOWF.RemoveAt(VOWF.Count - 1)

            If TVD.Count > 1 Then TVD.RemoveAt(TVD.Count - 1)
            If PO.Count > 1 Then PO.RemoveAt(PO.Count - 1)
            If HO.Count > 1 Then HO.RemoveAt(HO.Count - 1)
            If SO.Count > 1 Then SO.RemoveAt(SO.Count - 1)
            If VO.Count > 1 Then VO.RemoveAt(VO.Count - 1)

            If TVD.Count > 1 Then TVD.RemoveAt(TVD.Count - 1)
            If PO.Count > 1 Then PO.RemoveAt(PO.Count - 1)
            If HO.Count > 1 Then HO.RemoveAt(HO.Count - 1)
            If SO.Count > 1 Then SO.RemoveAt(SO.Count - 1)
            If VO.Count > 1 Then VO.RemoveAt(VO.Count - 1)

            If TQ.Count > 1 Then TQ.RemoveAt(TQ.Count - 1)
            If PQ.Count > 1 Then PQ.RemoveAt(PQ.Count - 1)
            If TI.Count > 1 Then TI.RemoveAt(TI.Count - 1)
            If PI.Count > 1 Then PI.RemoveAt(PI.Count - 1)

            Return New Object() {TVB, PB, HB, SB, VB, TVD, PO, HO, SO, VO, TE, PE, TH, PHsI, PHsII, CP, TQ, PQ, TI, PI, TOWF, POWF, HOWF, SOWF, VOWF, TVB1, PB1, HB1, SB1, VB1, TVB2, PB2, HB2, SB2, VB2}

        End Function

        ''' <summary>
        ''' This function returns points to build the binary phase envelope.
        ''' </summary>
        ''' <param name="parameters"></param>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Overridable Function DW_ReturnBinaryEnvelope(ByVal parameters As Object, Optional ByVal bw As System.ComponentModel.BackgroundWorker = Nothing) As Object

            If Settings.EnableGPUProcessing Then Calculator.InitComputeDevice()

            Dim n, i As Integer

            n = Me.CurrentMaterialStream.Phases(0).Compounds.Count - 1

            Dim StepCount
            Dim MinX As Double
            Dim MaxX As Double
            Dim dx As Double

            Dim tipocalc As String
            Dim result As Object
            Dim P, T As Double
            Dim calcP, calcT As Double
            Dim VLE, SLE, LLE, Critical, SolidSolution As Boolean
            Dim prevkib As Double() = Nothing, prevkid As Double() = Nothing

            tipocalc = parameters(0)
            P = parameters(1)
            T = parameters(2)
            VLE = parameters(3)
            LLE = parameters(4)
            SLE = parameters(5)
            Critical = parameters(6)
            SolidSolution = parameters(7)

            Try
                StepCount = parameters(10)
                MinX = parameters(11)
                MaxX = parameters(12)
                dx = (MaxX - MinX) / StepCount
            Catch ex As Exception
                StepCount = 40
                MinX = 0.0
                MaxX = 1.0
                dx = 1.0 / StepCount
            End Try

            Dim MyFlash As IFlashAlgorithm = FlashBase.Clone

            If MyFlash.AlgoType = Enums.FlashMethod.Nested_Loops_SVLLE Then
                DirectCast(MyFlash, NestedLoopsSVLLE).ClearEstimates()
            End If

            Select Case tipocalc

                Case "T-x-y"

                    Dim px, py1, py2, px1l1, px1l2, py3 As New ArrayList 'vle, lle
                    Dim pxs1, pys1, pxs2, pys2 As New ArrayList 'sle
                    Dim pxc, pyc As New ArrayList 'critical
                    Dim unstable As Boolean = False
                    Dim ui, ut As New ArrayList
                    Dim x, y1, y2, Test1, Test2 As Double
                    Dim tmp1 As Object = Nothing, tmp2 As Object = Nothing

                    If VLE Then

                        i = 0

                        Do

                            If bw IsNot Nothing Then If bw.CancellationPending Then Exit Do Else bw.ReportProgress(0, "VLE (" & i + 1 & "/" & StepCount + 1 & ")")

                            x = MinX + i * dx

                            px.Add(x)

                            If i = 0 Then
                                Try
                                    tmp1 = MyFlash.Flash_PV(New Double() {x, 1 - x}, P, 0.0#, 0.0#, Me)
                                    calcT = tmp1(4)
                                    Test1 = calcT
                                    prevkib = tmp1(6)
                                    py1.Add(Test1)
                                Catch ex As Exception
                                    py1.Add(Double.NaN)
                                    SharedClasses.ExceptionProcessing.ExceptionParser.ProcessAndDisplayException(Flowsheet, ex)
                                End Try
                                Try
                                    tmp2 = MyFlash.Flash_PV(New Double() {x, 1 - x}, P, 1.0#, 0.0#, Me)
                                    y2 = tmp2(4)
                                    Test2 = y2
                                    prevkid = tmp1(6)
                                    py2.Add(Test2)
                                Catch ex As Exception
                                    py2.Add(Double.NaN)
                                    SharedClasses.ExceptionProcessing.ExceptionParser.ProcessAndDisplayException(Flowsheet, ex)
                                End Try
                            Else
                                Try
                                    tmp1 = MyFlash.Flash_PV(New Double() {x, 1 - x}, P, 0.0#, Test1, Me)
                                    calcT = tmp1(4)
                                    Test1 = calcT
                                    prevkib = tmp1(6)
                                    py1.Add(calcT)
                                Catch ex As Exception
                                    py1.Add(Double.NaN)
                                    SharedClasses.ExceptionProcessing.ExceptionParser.ProcessAndDisplayException(Flowsheet, ex)
                                End Try
                                Try
                                    tmp2 = MyFlash.Flash_PV(New Double() {x, 1 - x}, P, 1.0#, Test2, Me)
                                    calcT = tmp2(4)
                                    Test2 = calcT
                                    prevkid = tmp1(6)
                                    py2.Add(calcT)
                                Catch ex As Exception
                                    py2.Add(Double.NaN)
                                    SharedClasses.ExceptionProcessing.ExceptionParser.ProcessAndDisplayException(Flowsheet, ex)
                                End Try
                            End If

                            'check if liquid phase is stable.

                            If tmp1(7) > 0 Then
                                unstable = True
                                ui.Add(px.Count - 1)
                                ut.Add(tmp1(4))
                            End If

                            i = i + 1

                        Loop Until i > StepCount
                    End If

                    If MyFlash.AlgoType = Enums.FlashMethod.Nested_Loops_SVLLE Then
                        DirectCast(MyFlash, NestedLoopsSVLLE).ClearEstimates()
                    End If

                    If LLE Then

                        If MyFlash.AlgoType = Enums.FlashMethod.Nested_Loops_SVLLE Then
                            DirectCast(MyFlash, NestedLoopsSVLLE).ClearEstimates()
                        End If

                        If unstable Then

                            Dim ti, tf, uim, tit As Double
                            ti = (ut(0) + ut(ut.Count - 1)) / 2
                            uim = (ui(0) + ui(ui.Count - 1)) / 2
                            tf = MathEx.Common.Max(Me.RET_VTF())
                            If tf = 0.0# Then tf = ti * 0.7

                            For i = 0 To 50
                                tit = tf + (ti - tf) / 50 * i
                                If bw IsNot Nothing Then If bw.CancellationPending Then Exit For Else bw.ReportProgress(0, "LLE (" & i + 1 & "/50)")
                                Try
                                    result = MyFlash.Flash_PT(New Double() {uim * dx + MinX, 1 - uim * dx - MinX}, P, tit, Me)
                                    If result(5) > 0.0# Then
                                        If Abs(result(2)(0) - result(6)(0)) > 0.01 Then
                                            px1l1.Add(result(2)(0))
                                            px1l2.Add(result(6)(0))
                                            py3.Add(tit)
                                        End If
                                    End If
                                Catch ex As Exception
                                End Try
                            Next

                            If MyFlash.AlgoType = Enums.FlashMethod.Nested_Loops_SVLLE Then
                                DirectCast(MyFlash, NestedLoopsSVLLE).ClearEstimates()
                            End If

                        End If

                    End If

                    'replace missing data with interpolated values

                    'VLE

                    Dim data As Tuple(Of Double(), Double()) = New Tuple(Of Double(), Double())(px.ToDoubleArray, py1.ToDoubleArray)

                    Dim validdata = data.ReturnValidSets()

                    Dim interpolator As New MathNet.Numerics.Interpolation.BulirschStoerRationalInterpolation(validdata.Item1, validdata.Item2)

                    i = 0
                    For Each d As Double In py1.Clone
                        If Double.IsNaN(d) Then
                            py1(i) = interpolator.Interpolate(px(i))
                        End If
                        i += 1
                    Next

                    data = New Tuple(Of Double(), Double())(px.ToDoubleArray, py2.ToDoubleArray)

                    validdata = data.ReturnValidSets()

                    interpolator = New MathNet.Numerics.Interpolation.BulirschStoerRationalInterpolation(validdata.Item1, validdata.Item2)

                    i = 0
                    For Each d As Double In py2.Clone
                        If Double.IsNaN(d) Then
                            py2(i) = interpolator.Interpolate(px(i))
                        End If
                        i += 1
                    Next

                    If SLE Then

                        Dim nlsle As New Auxiliary.FlashAlgorithms.NestedLoopsSLE
                        Dim L1, L2 As Double

                        If SolidSolution Then
                            L1 = 0.0#
                            L2 = 1.0#
                        Else
                            L1 = 0.001
                            L2 = 0.999
                        End If

                        nlsle.SolidSolution = SolidSolution

                        Dim constprops As New List(Of Interfaces.ICompoundConstantProperties)
                        For Each su As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                            constprops.Add(su.ConstantProperties)
                        Next
                        nlsle.CompoundProperties = constprops

                        i = 0
                        Do
                            x = MinX + i * dx
                            If bw IsNot Nothing Then If bw.CancellationPending Then Exit Do Else bw.ReportProgress(0, "SLE 1 (" & i + 1 & "/" & StepCount + 1 & ")")
                            Try
                                tmp1 = nlsle.Flash_PSF(New Double() {x, 1 - x}, P, L1, 0, Me)
                                y1 = tmp1(4)
                                pxs1.Add(x)
                                pys1.Add(y1)
                            Catch ex As Exception
                                SharedClasses.ExceptionProcessing.ExceptionParser.ProcessAndDisplayException(Flowsheet, ex)
                            End Try
                            i = i + 1
                        Loop Until i > StepCount

                        i = 0
                        Do
                            x = MinX + i * dx
                            If bw IsNot Nothing Then If bw.CancellationPending Then Exit Do Else bw.ReportProgress(0, "SLE 2 (" & i + 1 & "/" & StepCount + 1 & ")")
                            Try
                                If x = 0 Then
                                    x = 0.001
                                ElseIf x = 1 Then
                                    x = 0.999
                                End If
                                tmp1 = nlsle.Flash_PSF(New Double() {x, 1 - x}, P, L2, 0, Me)
                                y2 = tmp1(4)
                                Test2 = y2
                                pxs2.Add(x)
                                pys2.Add(y2)
                            Catch ex As Exception
                                SharedClasses.ExceptionProcessing.ExceptionParser.ProcessAndDisplayException(Flowsheet, ex)
                            End Try
                            i = i + 1
                        Loop Until i > StepCount
                    End If

                    If Critical Then

                        Dim cpc As New Utilities.TCP.Methods
                        Dim cpcs As New Utilities.TCP.Methods_SRK
                        Dim CP As New ArrayList
                        Dim TCR, PCR, VCR As Double

                        i = 0
                        Do
                            x = MinX + i * dx
                            If bw IsNot Nothing Then If bw.CancellationPending Then Exit Do Else bw.ReportProgress(0, "Critical (" & i + 1 & "/" & StepCount + 1 & ")")
                            Try
                                If TypeOf Me Is PengRobinsonPropertyPackage Then
                                    If x = 0 Or x >= 1 Then
                                        Dim Tc As Double() = Me.RET_VTC
                                        TCR = x * Tc(0) + (1 - x) * Tc(1)
                                        PCR = 0.0#
                                        VCR = 0.0#
                                        CP.Clear()
                                        CP.Add(New Double() {TCR, PCR, VCR})
                                    Else
                                        CP = cpc.CRITPT_PR(New Double() {x, 1 - x}, Me.RET_VTC, Me.RET_VPC, Me.RET_VVC, Me.RET_VW, Me.RET_VKij)
                                    End If
                                ElseIf TypeOf Me Is SRKPropertyPackage Then
                                    If x = 0 Or x >= 1 Then
                                        Dim Tc As Double() = Me.RET_VTC
                                        TCR = x * Tc(0) + (1 - x) * Tc(1)
                                        PCR = 0.0#
                                        VCR = 0.0#
                                        CP.Clear()
                                        CP.Add(New Double() {TCR, PCR, VCR})
                                    Else
                                        CP = cpcs.CRITPT_PR(New Double() {x, 1 - x}, Me.RET_VTC, Me.RET_VPC, Me.RET_VVC, Me.RET_VW, Me.RET_VKij)
                                    End If
                                Else
                                    Dim Tc As Double() = Me.RET_VTC
                                    TCR = x * Tc(0) + (1 - x) * Tc(1)
                                    PCR = 0.0#
                                    VCR = 0.0#
                                    CP.Clear()
                                    CP.Add(New Double() {TCR, PCR, VCR})
                                End If
                                If CP.Count > 0 Then
                                    Dim cp0 = CP(0)
                                    TCR = cp0(0)
                                    PCR = cp0(1)
                                    VCR = cp0(2)
                                    pxc.Add(x)
                                    pyc.Add(TCR)
                                End If
                            Catch ex As Exception
                                SharedClasses.ExceptionProcessing.ExceptionParser.ProcessAndDisplayException(Flowsheet, ex)
                            End Try
                            i = i + 1
                        Loop Until i > StepCount
                    End If

                    Return New Object() {px, py1, py2, px1l1, px1l2, py3, pxs1, pys1, pxs2, pys2, pxc, pyc}

                Case "P-x-y"

                    Dim px, py1, py2, px1l1, px1l2, py3 As New ArrayList
                    Dim unstable As Boolean = False
                    Dim ui, up As New ArrayList
                    Dim x, y1, y2, Pest1, Pest2 As Double
                    Dim tmp As Object = Nothing

                    If Not TypeOf Me.FlashBase Is Auxiliary.FlashAlgorithms.NestedLoopsSLE Then

                        i = 0
                        Do
                            x = MinX + i * dx
                            If bw IsNot Nothing Then If bw.CancellationPending Then Exit Do Else bw.ReportProgress(0, "VLE (" & i + 1 & "/" & StepCount + 1 & ")")
                            Try
                                If i = 0 Then
                                    tmp = Me.FlashBase.Flash_TV(New Double() {x, 1 - x}, T, 0, 0, Me)
                                    calcP = tmp(4)
                                    Pest1 = calcP
                                    tmp = Me.FlashBase.Flash_TV(New Double() {x, 1 - x}, T, 1, 0, Me)
                                    y2 = tmp(4)
                                    Pest2 = y2
                                Else
                                    tmp = Me.FlashBase.Flash_TV(New Double() {x, 1 - x}, T, 0, Pest1, Me)
                                    calcP = tmp(4)
                                    Pest1 = calcP
                                    tmp = Me.FlashBase.Flash_TV(New Double() {x, 1 - x}, T, 1, Pest2, Me)
                                    y2 = tmp(4)
                                    Pest2 = y2
                                End If
                                y1 = calcP
                                px.Add(x)
                                py1.Add(y1)
                                py2.Add(y2)
                                'check if liquid phase is stable.
                                result = Me.FlashBase.Flash_PT(New Double() {x, 1 - x}, calcP * 1.3, T, Me)
                                If result(5) > 0.0# Then
                                    Dim fcl1(1), fcl2(1) As Double
                                    fcl1 = Me.DW_CalcFugCoeff(result(2), T, calcP, State.Liquid)
                                    calcP = result(2)(0) * fcl1(0) * calcP + result(2)(1) * fcl1(1) * calcP
                                    unstable = True
                                    ui.Add(px.Count - 1)
                                    'up.Add(calcP)
                                    If up.Count = 0 Then up.Add(calcP)
                                    py1(py1.Count - 1) = up(0)
                                End If
                            Catch ex As Exception
                                SharedClasses.ExceptionProcessing.ExceptionParser.ProcessAndDisplayException(Flowsheet, ex)
                            End Try
                            i = i + 1
                        Loop Until i > StepCount

                        If unstable Then
                            Dim pi, pf, uim As Double, pit As Integer
                            pi = up(0)
                            uim = (ui(0) + ui(ui.Count - 1)) / 2
                            pf = 2 * pi
                            i = 0
                            For pit = pi To pf Step (pf - pi) / 10
                                If bw IsNot Nothing Then If bw.CancellationPending Then Exit For Else bw.ReportProgress(0, "LLE (" & i + 1 & "/28)")
                                result = Me.FlashBase.Flash_PT(New Double() {uim * dx + MinX, 1 - uim * dx - MinX}, pit, T, Me)
                                If result(5) > 0.0# Then
                                    If Abs(result(2)(0) - result(6)(0)) > 0.01 Then
                                        px1l1.Add(result(2)(0))
                                        px1l2.Add(result(6)(0))
                                        py3.Add(pit)
                                    End If
                                End If
                                i += 1
                            Next
                        End If

                    End If

                    Return New Object() {px, py1, py2, px1l1, px1l2, py3}

                Case "(T)x-y"

                    Dim px, py As New ArrayList
                    Dim Test1 As Double
                    Dim x, y As Double
                    Dim tmp As Object = Nothing

                    If Not TypeOf Me.FlashBase Is Auxiliary.FlashAlgorithms.NestedLoopsSLE Then
                        i = 0
                        Do
                            x = MinX + i * dx
                            If bw IsNot Nothing Then If bw.CancellationPending Then Exit Do Else bw.ReportProgress(0, "VLE (" & i + 1 & StepCount + 1 & ")")
                            Try
                                If i = 0 Then
                                    tmp = Me.FlashBase.Flash_PV(New Double() {x, 1 - x}, P, 0, 0, Me)
                                    calcT = tmp(4)
                                    Test1 = calcT
                                Else
                                    tmp = Me.FlashBase.Flash_PV(New Double() {x, 1 - x}, P, 0, Test1, Me)
                                    calcT = tmp(4)
                                    Test1 = calcT
                                End If
                                y = tmp(3)(0)
                                px.Add(x)
                                py.Add(y)
                            Catch ex As Exception
                            End Try
                            i = i + 1
                        Loop Until i > StepCount
                    End If

                    Return New Object() {px, py}

                Case Else

                    Dim px, py As New ArrayList

                    Dim Pest1 As Double
                    Dim x, y As Double
                    Dim tmp As Object = Nothing

                    If Not TypeOf Me.FlashBase Is Auxiliary.FlashAlgorithms.NestedLoopsSLE Then
                        i = 0
                        Do
                            x = MinX + i * dx
                            If bw IsNot Nothing Then If bw.CancellationPending Then Exit Do Else bw.ReportProgress(0, "VLE (" & i + 1 & StepCount + 1 & ")")
                            Try
                                If i = 0 Then
                                    tmp = Me.FlashBase.Flash_TV(New Double() {x, 1 - x}, T, 0, 0, Me)
                                    calcP = tmp(4)
                                    Pest1 = calcP
                                Else
                                    tmp = Me.FlashBase.Flash_TV(New Double() {x, 1 - x}, T, 0, Pest1, Me)
                                    calcP = tmp(4)
                                    Pest1 = calcP
                                End If
                                y = tmp(3)(0)
                                px.Add(x)
                                py.Add(y)
                            Catch ex As Exception
                            End Try
                            i = i + 1
                        Loop Until i > StepCount
                    End If

                    Return New Object() {px, py}

            End Select

        End Function

        Public MustOverride Sub DW_CalcCompPartialVolume(ByVal phase As PropertyPackages.Phase, ByVal T As Double, ByVal P As Double)

#End Region

#Region "   Commmon Functions"

        Public Sub ProcessOverridenProperties(ptype As PhaseType)

            If PropertyOverrides.Count > 0 Then

                Dim engine As ScriptEngine
                Dim scope As ScriptScope

                engine = IronPython.Hosting.Python.CreateEngine()
                engine.Runtime.LoadAssembly(GetType(System.String).Assembly)
                engine.Runtime.LoadAssembly(GetType(BaseClasses.ConstantProperties).Assembly)
                scope = engine.CreateScope()
                scope.SetVariable("flowsheet", CurrentMaterialStream.Flowsheet)
                scope.SetVariable("this", Me)
                scope.SetVariable("matstr", CurrentMaterialStream)
                scope.SetVariable("T", CurrentMaterialStream.Phases(0).Properties.temperature.GetValueOrDefault)
                scope.SetVariable("P", CurrentMaterialStream.Phases(0).Properties.pressure.GetValueOrDefault)

                For Each item In PropertyOverrides

                    Dim source As Microsoft.Scripting.Hosting.ScriptSource = engine.CreateScriptSourceFromString(item.Value, Microsoft.Scripting.SourceCodeKind.Statements)
                    Try
                        Dim phasename As String = item.Key.Split("/")(0)
                        Dim propname As String = item.Key.Split("/")(1)
                        Dim phase = CurrentMaterialStream.GetPhase(phasename)
                        scope.SetVariable("currval", phase.Properties.GetType.GetProperty(propname).GetValue(phase.Properties))
                        scope.SetVariable("phase", phase)
                        source.Execute(scope)
                        Dim value = scope.GetVariable("propval")
                        phase.Properties.GetType.GetProperty(propname).SetValue(phase.Properties, New Nullable(Of Double)(Convert.ToDouble(value)))
                    Catch ex As Exception
                        Dim ops As ExceptionOperations = engine.GetService(Of ExceptionOperations)()
                        Dim gobj = DirectCast(CurrentMaterialStream, MaterialStream).GraphicObject
                        If gobj IsNot Nothing Then
                            Throw New Exception("[" & Tag & " / " & gobj.Tag & "] Error calculating overriden Property '" & item.Key & "': " & ops.FormatException(ex).ToString)
                        Else
                            Throw New Exception("[" & Tag & "] Error calculating overriden property '" & item.Key & "': " & ops.FormatException(ex).ToString)
                        End If
                    Finally
                        source = Nothing
                    End Try

                Next

                engine = Nothing
                scope = Nothing

            End If

        End Sub

        Public Function DW_GetConstantProperties() As List(Of Interfaces.ICompoundConstantProperties)

            Dim constprops As New List(Of Interfaces.ICompoundConstantProperties)

            For Each su As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                constprops.Add(su.ConstantProperties)
            Next

            Return constprops

        End Function

        Public Overloads Sub DW_CalcKvalue()

            Dim subst As Interfaces.ICompound

            For Each subst In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                subst.Kvalue = Me.CurrentMaterialStream.Phases(2).Compounds(subst.Name).MoleFraction.GetValueOrDefault / Me.CurrentMaterialStream.Phases(1).Compounds(subst.Name).MoleFraction.GetValueOrDefault
                subst.lnKvalue = Log(subst.Kvalue)
            Next

        End Sub

        Public Sub DW_CalcCompMolarFlow(ByVal phaseID As Integer)

            If Not phaseID = -1 Then
                With Me.CurrentMaterialStream.Phases(phaseID)
                    For Each subs As Interfaces.ICompound In .Compounds.Values
                        subs.MolarFlow = .Properties.molarflow.GetValueOrDefault * subs.MoleFraction.GetValueOrDefault
                    Next
                End With
            Else
                For Each phase In Me.CurrentMaterialStream.Phases.Values
                    With phase
                        For Each subs As Interfaces.ICompound In .Compounds.Values
                            subs.MolarFlow = .Properties.molarflow.GetValueOrDefault * subs.MoleFraction.GetValueOrDefault
                        Next
                    End With
                Next
            End If

        End Sub

        Public Overridable Sub DW_CalcCompFugCoeff(ByVal f As Phase)

            Dim fc As Object
            Dim vmol As Object = Me.RET_VMOL(f)
            Dim P, T As Double
            P = Me.CurrentMaterialStream.Phases(0).Properties.pressure.GetValueOrDefault
            T = Me.CurrentMaterialStream.Phases(0).Properties.temperature.GetValueOrDefault
            Select Case f
                Case Phase.Vapor
                    fc = Me.DW_CalcFugCoeff(vmol, T, P, State.Vapor)
                Case Else
                    fc = Me.DW_CalcFugCoeff(vmol, T, P, State.Liquid)
            End Select
            Dim i As Integer = 0
            For Each subs As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(Me.RET_PHASEID(f)).Compounds.Values
                subs.FugacityCoeff = fc(i)
                subs.ActivityCoeff = fc(i) * P / Me.AUX_PVAPi(i, T)
                subs.PartialPressure = subs.MoleFraction.GetValueOrDefault() * P
                i += 1
            Next

        End Sub

        Public Sub DW_CalcCompMassFlow(ByVal phaseID As Integer)

            If Not phaseID = -1 Then
                With Me.CurrentMaterialStream.Phases(phaseID)
                    For Each subs As Interfaces.ICompound In .Compounds.Values
                        subs.MassFlow = .Properties.massflow.GetValueOrDefault * subs.MassFraction.GetValueOrDefault
                    Next
                End With
            Else
                For Each phase In Me.CurrentMaterialStream.Phases.Values
                    With phase
                        For Each subs As Interfaces.ICompound In .Compounds.Values
                            subs.MassFlow = .Properties.massflow.GetValueOrDefault * subs.MassFraction.GetValueOrDefault
                        Next
                    End With
                Next
            End If

        End Sub

        Public Sub DW_CalcCompVolFlow(ByVal phaseID As Integer)

            Dim TotalMolarFlow, TotalVolFlow, MolarFrac, PartialVol, VolFrac, VolFlow, Sum As Double

            Dim T, P As Double
            T = Me.CurrentMaterialStream.Phases(0).Properties.temperature.GetValueOrDefault
            P = Me.CurrentMaterialStream.Phases(0).Properties.pressure.GetValueOrDefault
            Me.DW_CalcCompPartialVolume(Me.RET_PHASECODE(phaseID), T, P)

            If Not phaseID = -1 Then
                With Me.CurrentMaterialStream.Phases(phaseID)

                    Sum = 0

                    For Each subs As Interfaces.ICompound In .Compounds.Values
                        TotalMolarFlow = .Properties.molarflow.GetValueOrDefault
                        TotalVolFlow = .Properties.volumetric_flow.GetValueOrDefault
                        MolarFrac = subs.MoleFraction.GetValueOrDefault
                        PartialVol = subs.PartialVolume.GetValueOrDefault
                        VolFlow = TotalMolarFlow * MolarFrac * PartialVol
                        If TotalVolFlow > 0 Then
                            VolFrac = VolFlow / TotalVolFlow
                        Else
                            VolFrac = 0
                        End If
                        subs.VolumetricFraction = VolFrac
                        Sum += VolFrac
                    Next

                    'Normalization is still needed due to minor deviations in the partial molar volume estimation. Summation of partial flow rates
                    'should match phase flow rate.
                    For Each subs As Interfaces.ICompound In .Compounds.Values
                        If Sum > 0 Then
                            subs.VolumetricFraction = subs.VolumetricFraction.GetValueOrDefault / Sum
                        Else
                            subs.VolumetricFraction = 0
                        End If
                        'Corrects volumetric flow rate after normalization of fractions.
                        subs.VolumetricFlow = subs.VolumetricFraction.GetValueOrDefault * TotalVolFlow
                    Next

                End With
            Else
                For Each phase In Me.CurrentMaterialStream.Phases.Values
                    With phase

                        Sum = 0

                        For Each subs As Interfaces.ICompound In .Compounds.Values
                            TotalMolarFlow = .Properties.molarflow.GetValueOrDefault
                            TotalVolFlow = .Properties.volumetric_flow.GetValueOrDefault
                            MolarFrac = subs.MoleFraction.GetValueOrDefault
                            PartialVol = subs.PartialVolume.GetValueOrDefault
                            VolFlow = TotalMolarFlow * MolarFrac * PartialVol
                            If TotalVolFlow > 0 Then
                                VolFrac = VolFlow / TotalVolFlow
                            Else
                                VolFrac = 0
                            End If
                            subs.VolumetricFraction = VolFrac
                            Sum += VolFrac
                        Next

                        'Normalization is still needed due to minor deviations in the partial molar volume estimation. Summation of partial flow rates
                        'should match phase flow rate.
                        For Each subs As Interfaces.ICompound In .Compounds.Values
                            If Sum > 0 Then
                                subs.VolumetricFraction = subs.VolumetricFraction.GetValueOrDefault / Sum
                            Else
                                subs.VolumetricFraction = 0
                            End If
                            'Corrects volumetric flow rate after normalization of fractions.
                            subs.VolumetricFlow = subs.VolumetricFraction.GetValueOrDefault * TotalVolFlow
                        Next

                    End With
                Next
            End If

            'Totalization for the mixture "phase" should be made separatly, since the concept o partial molar volume is non sense for the whole mixture.
            For Each Subs As Interfaces.ICompound In CurrentMaterialStream.Phases(0).Compounds.Values
                Subs.VolumetricFraction = 0
                Subs.VolumetricFlow = 0
            Next

            'Summation of volumetric flow rates over all phases.
            For APhaseID As Integer = 1 To Me.CurrentMaterialStream.Phases.Count - 1
                For Each Subs As String In CurrentMaterialStream.Phases(APhaseID).Compounds.Keys
                    CurrentMaterialStream.Phases(0).Compounds(Subs).VolumetricFlow = CurrentMaterialStream.Phases(0).Compounds(Subs).VolumetricFlow.GetValueOrDefault + CurrentMaterialStream.Phases(APhaseID).Compounds(Subs).VolumetricFlow.GetValueOrDefault
                Next
            Next

            'Calculate volumetric fractions for the mixture.

            For Each Subs As Interfaces.ICompound In CurrentMaterialStream.Phases(0).Compounds.Values
                If CurrentMaterialStream.Phases(0).Properties.volumetric_flow.GetValueOrDefault > 0 Then
                    Subs.VolumetricFraction = Subs.VolumetricFlow.GetValueOrDefault / CurrentMaterialStream.Phases(0).Properties.volumetric_flow.GetValueOrDefault
                Else
                    Subs.VolumetricFraction = 0
                End If
            Next

        End Sub

        Public Sub DW_ZerarPhaseProps(ByVal Phase As Phase, Optional ByVal CalculatedOnly As Boolean = False)

            Dim phaseID As Integer

            phaseID = Me.RET_PHASEID(Phase)

            If Me.CurrentMaterialStream.Phases.ContainsKey(phaseID) Then

                If phaseID <> 0 Then

                    Me.CurrentMaterialStream.Phases(phaseID).Properties.density = Nothing
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy = Nothing
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy = Nothing
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_enthalpy = Nothing
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_entropy = Nothing
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpyF = Nothing
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.entropyF = Nothing
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_enthalpyF = Nothing
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_entropyF = Nothing
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.compressibilityFactor = Nothing
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCp = Nothing
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCv = Nothing
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight = Nothing
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.thermalConductivity = Nothing
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.speedOfSound = Nothing
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.volumetric_flow = Nothing
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.jouleThomsonCoefficient = Nothing
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.excessEnthalpy = Nothing
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.excessEntropy = Nothing
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.compressibility = Nothing
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.bubbleTemperature = Nothing
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.bubblePressure = Nothing
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.dewTemperature = Nothing
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.dewPressure = Nothing
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.viscosity = Nothing
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.kinematic_viscosity = Nothing
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.molarflow = Nothing
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.massflow = Nothing
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.massfraction = Nothing
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.molarfraction = Nothing
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.bulk_modulus = Nothing
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.isothermal_compressibility = Nothing
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.CO2loading = Nothing
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.CO2partialpressure = Nothing

                Else

                    Me.CurrentMaterialStream.Phases(phaseID).Properties.density = Nothing
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy = Nothing
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy = Nothing
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_enthalpy = Nothing
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_entropy = Nothing
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpyF = Nothing
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.entropyF = Nothing
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_enthalpyF = Nothing
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_entropyF = Nothing
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.compressibilityFactor = Nothing
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCp = Nothing
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCv = Nothing
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight = Nothing
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.thermalConductivity = Nothing
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.speedOfSound = Nothing
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.volumetric_flow = Nothing
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.jouleThomsonCoefficient = Nothing
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.excessEnthalpy = Nothing
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.excessEntropy = Nothing
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.compressibility = Nothing
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.bubbleTemperature = Nothing
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.bubblePressure = Nothing
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.dewTemperature = Nothing
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.dewPressure = Nothing
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.viscosity = Nothing
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.kinematic_viscosity = Nothing
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.bulk_modulus = Nothing
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.isothermal_compressibility = Nothing
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.CO2loading = Nothing
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.CO2partialpressure = Nothing

                    If Not CalculatedOnly Then
                        Me.CurrentMaterialStream.Phases(phaseID).Properties.molarflow = Nothing
                        Me.CurrentMaterialStream.Phases(phaseID).Properties.massflow = Nothing
                    End If

                End If

            End If

        End Sub

        Public Sub DW_ZerarTwoPhaseProps(ByVal Phase1 As Phase, ByVal Phase2 As Phase)

            Me.CurrentMaterialStream.Phases(0).Properties.kvalue = Nothing
            Me.CurrentMaterialStream.Phases(0).Properties.logKvalue = Nothing
            Me.CurrentMaterialStream.Phases(0).Properties.surfaceTension = Nothing

        End Sub

        Public Sub DW_ZerarOverallProps()

        End Sub

        Public Sub DW_ZerarVazaoMolar()
            With Me.CurrentMaterialStream
                .Phases(0).Properties.molarflow = 0
            End With
        End Sub

        Public Sub DW_ZerarVazaoVolumetrica()
            With Me.CurrentMaterialStream
                .Phases(0).Properties.volumetric_flow = 0
            End With
        End Sub

        Public Sub DW_ZerarVazaoMassica()
            With Me.CurrentMaterialStream
                .Phases(0).Properties.massflow = 0
            End With
        End Sub

        Public Sub DW_ZerarComposicoes(ByVal Phase As Phase)

            Dim phaseID As Integer
            phaseID = Me.RET_PHASEID(Phase)

            Dim subst As Interfaces.ICompound

            For Each subst In Me.CurrentMaterialStream.Phases(phaseID).Compounds.Values
                subst.MoleFraction = Nothing
                subst.MassFraction = Nothing
                subst.MolarFlow = Nothing
                subst.MassFlow = Nothing
            Next

        End Sub

        Public Function AUX_TCM(ByVal Phase As Phase) As Double

            Dim Tc As Double
            Dim subst As Interfaces.ICompound

            For Each subst In Me.CurrentMaterialStream.Phases(Me.RET_PHASEID(Phase)).Compounds.Values
                Tc += subst.MoleFraction.GetValueOrDefault * subst.ConstantProperties.Critical_Temperature
            Next

            Return Tc

        End Function

        Public Function AUX_TBM(ByVal Phase As Phase) As Double

            Dim Tb As Double
            Dim subst As Interfaces.ICompound

            For Each subst In Me.CurrentMaterialStream.Phases(Me.RET_PHASEID(Phase)).Compounds.Values
                Tb += subst.MoleFraction.GetValueOrDefault * subst.ConstantProperties.Normal_Boiling_Point
            Next

            Return Tb

        End Function

        Public Function AUX_TFM(ByVal Phase As Phase) As Double

            Dim Tf As Double
            Dim subst As Interfaces.ICompound

            For Each subst In Me.CurrentMaterialStream.Phases(Me.RET_PHASEID(Phase)).Compounds.Values
                Tf += subst.MoleFraction.GetValueOrDefault * subst.ConstantProperties.TemperatureOfFusion
            Next

            Return Tf

        End Function

        Public Function AUX_WM(ByVal Phase As Phase) As Double

            Dim val As Double
            Dim subst As Interfaces.ICompound

            For Each subst In Me.CurrentMaterialStream.Phases(Me.RET_PHASEID(Phase)).Compounds.Values
                val += subst.MoleFraction.GetValueOrDefault * subst.ConstantProperties.Acentric_Factor
            Next

            Return val

        End Function

        Public Function AUX_ZCM(ByVal Phase As Phase) As Double

            Dim val As Double
            Dim subst As Interfaces.ICompound

            For Each subst In Me.CurrentMaterialStream.Phases(Me.RET_PHASEID(Phase)).Compounds.Values
                val += subst.MoleFraction.GetValueOrDefault * subst.ConstantProperties.Critical_Compressibility
            Next

            Return val

        End Function

        Public Function AUX_ZRAM(ByVal Phase As Phase) As Double

            Dim val As Double
            Dim subst As Interfaces.ICompound

            For Each subst In Me.CurrentMaterialStream.Phases(Me.RET_PHASEID(Phase)).Compounds.Values
                If subst.ConstantProperties.Z_Rackett <> 0 Then
                    val += subst.MoleFraction.GetValueOrDefault * subst.ConstantProperties.Z_Rackett
                Else
                    val += subst.MoleFraction.GetValueOrDefault * (0.29056 - 0.08775 * subst.ConstantProperties.Acentric_Factor)
                End If
            Next

            Return val

        End Function

        Public Function AUX_VCM(ByVal Phase As Phase) As Double

            Dim val, vc As Double
            Dim subst As Interfaces.ICompound

            For Each subst In Me.CurrentMaterialStream.Phases(Me.RET_PHASEID(Phase)).Compounds.Values
                vc = subst.ConstantProperties.Critical_Volume
                If vc = 0.0# Then
                    vc = Auxiliary.PROPS.Vc(subst.ConstantProperties.Critical_Temperature, subst.ConstantProperties.Critical_Pressure, subst.ConstantProperties.Acentric_Factor)
                End If
                val += subst.MoleFraction.GetValueOrDefault * vc
            Next

            Return val / 1000

        End Function

        Public Function AUX_PCM(ByVal Phase As Phase) As Double

            Dim val As Double
            Dim subst As Interfaces.ICompound

            For Each subst In Me.CurrentMaterialStream.Phases(Me.RET_PHASEID(Phase)).Compounds.Values
                val += subst.MoleFraction.GetValueOrDefault * subst.ConstantProperties.Critical_Pressure
            Next

            Return val

        End Function

        Public Function HasHenryConstants(compname As String) As Boolean

            Dim CAS = Me.CurrentMaterialStream.Phases(0).Compounds(compname).ConstantProperties.CAS_Number
            If CAS <> "" Then
                Return m_Henry.ContainsKey(CAS)
            Else
                Return False
            End If

        End Function

        Public Function AUX_KHenry(ByVal CompName As String, ByVal T As Double) As Double

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "AUX_KHenry", "Henry's Constant Calculation", "Liquid Phase Fugacity Calculation Routine")

            IObj?.SetCurrent()

            IObj?.Paragraphs.Add(String.Format("Compound: {0}", CompName))
            IObj?.Paragraphs.Add(String.Format("Temperature: {0} K", T))

            Dim KHx As Double
            Dim MW As Double = 18 'mol weight of water [g/mol]
            Dim DW As Double = 996 'density of water at 298.15 K [Kg/m3]
            Dim KHCP As Double = 0.0000064 'nitrogen [mol/m3/Pa]
            Dim C As Double = 1600 'nitrogen
            Dim CAS As String

            CAS = Me.CurrentMaterialStream.Phases(0).Compounds(CompName).ConstantProperties.CAS_Number

            IObj?.Paragraphs.Add(String.Format("CAS Number: {0}", CAS))




            If m_Henry.ContainsKey(CAS) Then
                KHCP = m_Henry(CAS).KHcp
                C = m_Henry(CAS).C
            End If

            IObj?.Paragraphs.Add(String.Format("KHCP: {0}", KHCP))
            IObj?.Paragraphs.Add(String.Format("C: {0}", C))

            KHx = 1 / (KHCP * MW / DW / 1000 * Exp(C * (1 / T - 1 / 298.15)))

            IObj?.Paragraphs.Add(String.Format("Henry's Constant: {0} Pa", KHx))

            IObj?.Close()

            Return KHx '[Pa]

        End Function

        Public Function AUX_PVAPM(ByVal T) As Double

            Dim val As Double = 0
            Dim subst As Interfaces.ICompound
            Dim Tc As Double

            For Each subst In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                Tc = subst.ConstantProperties.Critical_Temperature
                val += subst.MoleFraction.GetValueOrDefault * Me.AUX_PVAPi(subst.Name, T)
            Next

            Return val

        End Function

        Public Function AUX_PVAPM(ByVal T As Double, Vx As Double()) As Double

            Dim val As Double = 0

            For i As Integer = 0 To Vx.Length - 1
                val += Vx(i) * Me.AUX_PVAPi(i, T)
            Next

            Return val

        End Function

        Public Function AUX_PVAPM(ByVal Phase As Phase, ByVal T As Double) As Double

            Dim val As Double = 0
            Dim subst As Interfaces.ICompound

            For Each subst In Me.CurrentMaterialStream.Phases(Me.RET_PHASEID(Phase)).Compounds.Values
                val += subst.MoleFraction.GetValueOrDefault * Me.AUX_PVAPi(subst.Name, T)
            Next

            Return val

        End Function


        Public Function AUX_KIJ(ByVal sub1 As String, ByVal sub2 As String) As Double

            Dim Vc1 As Double = Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.Critical_Volume
            Dim Vc2 As Double = Me.CurrentMaterialStream.Phases(0).Compounds(sub2).ConstantProperties.Critical_Volume

            Dim tmp As Double = 1 - 8 * (Vc1 * Vc2) ^ 0.5 / ((Vc1 ^ (1 / 3) + Vc2 ^ (1 / 3)) ^ 3)

            Return tmp

            Return 0

        End Function

        Public Overridable Function AUX_MMM(ByVal Phase As Phase) As Double

            Dim val As Double
            Dim subst As Interfaces.ICompound

            For Each subst In Me.CurrentMaterialStream.Phases(Me.RET_PHASEID(Phase)).Compounds.Values
                val += subst.MoleFraction.GetValueOrDefault * subst.ConstantProperties.Molar_Weight
            Next

            Return val

        End Function

        Private Function AUX_Rackett_PHIi(ByVal sub1 As String, ByVal Phase As Phase) As Double

            Dim val, vc As Double
            vc = Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.Critical_Volume
            If vc = 0.0# Then vc = Auxiliary.PROPS.Vc(Me.CurrentMaterialStream.Phases(Me.RET_PHASEID(Phase)).Compounds(sub1).ConstantProperties.Critical_Temperature, Me.CurrentMaterialStream.Phases(Me.RET_PHASEID(Phase)).Compounds(sub1).ConstantProperties.Critical_Pressure, Me.CurrentMaterialStream.Phases(Me.RET_PHASEID(Phase)).Compounds(sub1).ConstantProperties.Acentric_Factor)

            val = Me.CurrentMaterialStream.Phases(Me.RET_PHASEID(Phase)).Compounds(sub1).MoleFraction.GetValueOrDefault * vc

            val = val / Me.AUX_VCM(Phase) / 1000

            Return val

        End Function

        Private Function AUX_Rackett_Kij(ByVal sub1 As String, ByVal sub2 As String) As Double

            Dim Vc1 As Double = Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.Critical_Volume
            Dim Vc2 As Double = Me.CurrentMaterialStream.Phases(0).Compounds(sub2).ConstantProperties.Critical_Volume

            If Vc1 = 0.0# Then Vc1 = Auxiliary.PROPS.Vc(Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.Critical_Temperature, Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.Critical_Pressure, Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.Acentric_Factor)
            If Vc2 = 0.0# Then Vc2 = Auxiliary.PROPS.Vc(Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.Critical_Temperature, Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.Critical_Pressure, Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.Acentric_Factor)

            Dim tmp As Double = 8 * (Vc1 * Vc2) ^ 0.5 / ((Vc1 ^ (1 / 3) + Vc2 ^ (1 / 3)) ^ 3)

            Return tmp

        End Function

        Private Function AUX_Rackett_Tcij(ByVal sub1 As String, ByVal sub2 As String) As Double

            Dim Tc1 As Double = Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.Critical_Temperature
            Dim Tc2 As Double = Me.CurrentMaterialStream.Phases(0).Compounds(sub2).ConstantProperties.Critical_Temperature

            Dim tmp As Double = Me.AUX_Rackett_Kij(sub1, sub2) * (Tc1 * Tc2) ^ 0.5

            Return tmp

        End Function

        Private Function AUX_Rackett_Tcm(ByVal Phase As Phase) As Double

            Dim Tc As Double
            Dim subst1, subst2 As Interfaces.ICompound

            For Each subst1 In Me.CurrentMaterialStream.Phases(Me.RET_PHASEID(Phase)).Compounds.Values
                For Each subst2 In Me.CurrentMaterialStream.Phases(Me.RET_PHASEID(Phase)).Compounds.Values
                    Tc += Me.AUX_Rackett_PHIi(subst1.Name, Phase) * Me.AUX_Rackett_PHIi(subst2.Name, Phase) * Me.AUX_Rackett_Tcij(subst1.Name, subst2.Name)
                Next
            Next

            Return Tc

        End Function

        Public Overridable Function AUX_CPi(ByVal sub1 As String, ByVal T As Double) As Double

            Dim ID = Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.ID

            Dim db As String = CompoundPropCache(ID).OriginalDB

            If CompoundPropCache(ID).IsPF = 1 Then

                With CompoundPropCache(ID)
                    Return Auxiliary.PROPS.Cpig_lk(.PF_Watson_K, .Acentric_Factor, T) '* .Molar_Weight
                End With

            Else

                If db = "DWSIM" Or db = "" Then
                    Dim A, B, C, D, E, result As Double
                    A = CompoundPropCache(ID).Ideal_Gas_Heat_Capacity_Const_A
                    B = CompoundPropCache(ID).Ideal_Gas_Heat_Capacity_Const_B
                    C = CompoundPropCache(ID).Ideal_Gas_Heat_Capacity_Const_C
                    D = CompoundPropCache(ID).Ideal_Gas_Heat_Capacity_Const_D
                    E = CompoundPropCache(ID).Ideal_Gas_Heat_Capacity_Const_E
                    'Cp = A + B*T + C*T^2 + D*T^3 + E*T^4 where Cp in kJ/kg-mol , T in K 
                    result = A + B * T + C * T ^ 2 + D * T ^ 3 + E * T ^ 4
                    Return result / CompoundPropCache(ID).Molar_Weight 'kJ/kg.K
                ElseIf db = "CheResources" Then
                    Dim A, B, C, D, E, result As Double
                    A = CompoundPropCache(ID).Ideal_Gas_Heat_Capacity_Const_A
                    B = CompoundPropCache(ID).Ideal_Gas_Heat_Capacity_Const_B
                    C = CompoundPropCache(ID).Ideal_Gas_Heat_Capacity_Const_C
                    D = CompoundPropCache(ID).Ideal_Gas_Heat_Capacity_Const_D
                    E = CompoundPropCache(ID).Ideal_Gas_Heat_Capacity_Const_E
                    'CAL/MOL.K [CP=A+(B*T)+(C*T^2)+(D*T^3)], T in K
                    result = A + B * T + C * T ^ 2 + D * T ^ 3
                    Return result / CompoundPropCache(ID).Molar_Weight * 4.1868 'kJ/kg.K
                ElseIf db = "ChemSep" Or db = "User" Then
                    Dim A, B, C, D, E, result As Double
                    Dim eqno As String = CompoundPropCache(ID).IdealgasCpEquation
                    Dim mw As Double = CompoundPropCache(ID).Molar_Weight
                    A = CompoundPropCache(ID).Ideal_Gas_Heat_Capacity_Const_A
                    B = CompoundPropCache(ID).Ideal_Gas_Heat_Capacity_Const_B
                    C = CompoundPropCache(ID).Ideal_Gas_Heat_Capacity_Const_C
                    D = CompoundPropCache(ID).Ideal_Gas_Heat_Capacity_Const_D
                    E = CompoundPropCache(ID).Ideal_Gas_Heat_Capacity_Const_E
                    If Integer.TryParse(eqno, New Integer) Then
                        result = CalcCSTDepProp(eqno, A, B, C, D, E, T, 0) / 1000 / mw 'kJ/kg.K
                    Else
                        result = ParseEquation(eqno, A, B, C, D, E, T) / mw
                    End If
                    If result = 0.0 Then Return 3.5 * 8.314 / mw Else Return result
                ElseIf db = "ChEDL Thermo" Then
                    Dim A, B, C, D, E, result As Double
                    Dim eqno As String = CompoundPropCache(ID).IdealgasCpEquation
                    A = CompoundPropCache(ID).Ideal_Gas_Heat_Capacity_Const_A
                    B = CompoundPropCache(ID).Ideal_Gas_Heat_Capacity_Const_B
                    C = CompoundPropCache(ID).Ideal_Gas_Heat_Capacity_Const_C
                    D = CompoundPropCache(ID).Ideal_Gas_Heat_Capacity_Const_D
                    E = CompoundPropCache(ID).Ideal_Gas_Heat_Capacity_Const_E
                    result = CalcCSTDepProp(eqno, A, B, C, D, E, T, 0) 'kJ/kg.K
                    Return result
                ElseIf db = "CoolProp" Then
                    Dim A, B, C, D, E, result As Double
                    Dim eqno As String = CompoundPropCache(ID).IdealgasCpEquation
                    Dim mw As Double = CompoundPropCache(ID).Molar_Weight
                    A = CompoundPropCache(ID).Ideal_Gas_Heat_Capacity_Const_A
                    B = CompoundPropCache(ID).Ideal_Gas_Heat_Capacity_Const_B
                    C = CompoundPropCache(ID).Ideal_Gas_Heat_Capacity_Const_C
                    D = CompoundPropCache(ID).Ideal_Gas_Heat_Capacity_Const_D
                    E = CompoundPropCache(ID).Ideal_Gas_Heat_Capacity_Const_E
                    result = CalcCSTDepProp(eqno, A, B, C, D, E, T, 0) 'kJ/kg.K
                    Return result
                ElseIf db = "Biodiesel" Then
                    Dim A, B, C, D, E, result As Double
                    Dim eqno As String = CompoundPropCache(ID).IdealgasCpEquation
                    A = CompoundPropCache(ID).Ideal_Gas_Heat_Capacity_Const_A
                    B = CompoundPropCache(ID).Ideal_Gas_Heat_Capacity_Const_B
                    C = CompoundPropCache(ID).Ideal_Gas_Heat_Capacity_Const_C
                    D = CompoundPropCache(ID).Ideal_Gas_Heat_Capacity_Const_D
                    E = CompoundPropCache(ID).Ideal_Gas_Heat_Capacity_Const_E
                    result = CalcCSTDepProp(eqno, A, B, C, D, E, T, 0) 'kJ/kg.K
                    Return result
                ElseIf db = "KDB" Then
                    Dim A, B, C, D, E As Double
                    Dim eqno As String = CompoundPropCache(ID).IdealgasCpEquation
                    A = CompoundPropCache(ID).Ideal_Gas_Heat_Capacity_Const_A
                    B = CompoundPropCache(ID).Ideal_Gas_Heat_Capacity_Const_B
                    C = CompoundPropCache(ID).Ideal_Gas_Heat_Capacity_Const_C
                    D = CompoundPropCache(ID).Ideal_Gas_Heat_Capacity_Const_D
                    E = CompoundPropCache(ID).Ideal_Gas_Heat_Capacity_Const_E
                    Dim mw As Double = CompoundPropCache(ID).Molar_Weight
                    Return ParseEquation(eqno, A, B, C, D, E, T) / mw
                Else
                    Return 0
                End If

            End If

        End Function

        Public Overridable Function AUX_CPi(cprops As ICompoundConstantProperties, ByVal T As Double) As Double

            Dim db As String = cprops.OriginalDB

            If cprops.IsPF = 1 Then

                With cprops
                    Return Auxiliary.PROPS.Cpig_lk(.PF_Watson_K, .Acentric_Factor, T) '* .Molar_Weight
                End With

            Else

                If db = "DWSIM" Or db = "" Then
                    Dim A, B, C, D, E, result As Double
                    A = cprops.Ideal_Gas_Heat_Capacity_Const_A
                    B = cprops.Ideal_Gas_Heat_Capacity_Const_B
                    C = cprops.Ideal_Gas_Heat_Capacity_Const_C
                    D = cprops.Ideal_Gas_Heat_Capacity_Const_D
                    E = cprops.Ideal_Gas_Heat_Capacity_Const_E
                    'Cp = A + B*T + C*T^2 + D*T^3 + E*T^4 where Cp in kJ/kg-mol , T in K 
                    result = A + B * T + C * T ^ 2 + D * T ^ 3 + E * T ^ 4
                    Return result / cprops.Molar_Weight 'kJ/kg.K
                ElseIf db = "CheResources" Then
                    Dim A, B, C, D, E, result As Double
                    A = cprops.Ideal_Gas_Heat_Capacity_Const_A
                    B = cprops.Ideal_Gas_Heat_Capacity_Const_B
                    C = cprops.Ideal_Gas_Heat_Capacity_Const_C
                    D = cprops.Ideal_Gas_Heat_Capacity_Const_D
                    E = cprops.Ideal_Gas_Heat_Capacity_Const_E
                    'CAL/MOL.K [CP=A+(B*T)+(C*T^2)+(D*T^3)], T in K
                    result = A + B * T + C * T ^ 2 + D * T ^ 3
                    Return result / cprops.Molar_Weight * 4.1868 'kJ/kg.K
                ElseIf db = "ChemSep" Or db = "ChEDL Thermo" Or db = "User" Then
                    Dim A, B, C, D, E, result As Double
                    Dim eqno As String = cprops.IdealgasCpEquation
                    Dim mw As Double = cprops.Molar_Weight
                    A = cprops.Ideal_Gas_Heat_Capacity_Const_A
                    B = cprops.Ideal_Gas_Heat_Capacity_Const_B
                    C = cprops.Ideal_Gas_Heat_Capacity_Const_C
                    D = cprops.Ideal_Gas_Heat_Capacity_Const_D
                    E = cprops.Ideal_Gas_Heat_Capacity_Const_E
                    If Integer.TryParse(eqno, New Integer) Then
                        result = CalcCSTDepProp(eqno, A, B, C, D, E, T, 0) / 1000 / mw 'kJ/kg.K
                    Else
                        result = ParseEquation(eqno, A, B, C, D, E, T) / mw
                    End If
                    If result = 0.0 Then
                        'try estimating from LK method
                        With cprops
                            Dim sg60 = AUX_LIQDENSi(cprops, T) / 1000.0
                            result = Auxiliary.PROPS.Cpig_lk(.Normal_Boiling_Point ^ 0.33 / sg60, .Acentric_Factor, T)
                        End With
                        If Double.IsNaN(result) Or Double.IsInfinity(result) Then
                            Return 3.5 * 8.314 / mw
                        Else
                            Return result
                        End If
                    Else
                        Return result
                    End If
                ElseIf db = "ChEDL Thermo" Then
                    Dim A, B, C, D, E, result As Double
                    Dim eqno As String = cprops.IdealgasCpEquation
                    A = cprops.Ideal_Gas_Heat_Capacity_Const_A
                    B = cprops.Ideal_Gas_Heat_Capacity_Const_B
                    C = cprops.Ideal_Gas_Heat_Capacity_Const_C
                    D = cprops.Ideal_Gas_Heat_Capacity_Const_D
                    E = cprops.Ideal_Gas_Heat_Capacity_Const_E
                    result = CalcCSTDepProp(eqno, A, B, C, D, E, T, 0) 'kJ/kg.K
                    Return result
                ElseIf db = "CoolProp" Then
                    Dim A, B, C, D, E, result As Double
                    Dim eqno As String = cprops.IdealgasCpEquation
                    Dim mw As Double = cprops.Molar_Weight
                    A = cprops.Ideal_Gas_Heat_Capacity_Const_A
                    B = cprops.Ideal_Gas_Heat_Capacity_Const_B
                    C = cprops.Ideal_Gas_Heat_Capacity_Const_C
                    D = cprops.Ideal_Gas_Heat_Capacity_Const_D
                    E = cprops.Ideal_Gas_Heat_Capacity_Const_E
                    result = CalcCSTDepProp(eqno, A, B, C, D, E, T, 0) 'kJ/kg.K
                    Return result
                ElseIf db = "Biodiesel" Then
                    Dim A, B, C, D, E, result As Double
                    Dim eqno As String = cprops.IdealgasCpEquation
                    A = cprops.Ideal_Gas_Heat_Capacity_Const_A
                    B = cprops.Ideal_Gas_Heat_Capacity_Const_B
                    C = cprops.Ideal_Gas_Heat_Capacity_Const_C
                    D = cprops.Ideal_Gas_Heat_Capacity_Const_D
                    E = cprops.Ideal_Gas_Heat_Capacity_Const_E
                    result = CalcCSTDepProp(eqno, A, B, C, D, E, T, 0) 'kJ/kg.K
                    Return result
                ElseIf db = "KDB" Then
                    Dim A, B, C, D, E As Double
                    Dim eqno As String = cprops.IdealgasCpEquation
                    A = cprops.Ideal_Gas_Heat_Capacity_Const_A
                    B = cprops.Ideal_Gas_Heat_Capacity_Const_B
                    C = cprops.Ideal_Gas_Heat_Capacity_Const_C
                    D = cprops.Ideal_Gas_Heat_Capacity_Const_D
                    E = cprops.Ideal_Gas_Heat_Capacity_Const_E
                    Dim mw As Double = cprops.Molar_Weight
                    Return ParseEquation(eqno, A, B, C, D, E, T) / mw
                Else
                    Return 0
                End If

            End If

        End Function

        Public Function AUX_CPm(ByVal Phase As Phase, ByVal T As Double) As Double

            Dim val As Double
            Dim subst As Interfaces.ICompound

            For Each subst In Me.CurrentMaterialStream.Phases(Me.RET_PHASEID(Phase)).Compounds.Values
                val += subst.MassFraction.GetValueOrDefault * Me.AUX_CPi(subst.Name, T)
            Next

            Return val 'KJ/Kg/K

        End Function

        Public Function AUX_CPm(ByVal Phase As IPhase, ByVal T As Double) As Double

            Dim val As Double
            Dim subst As Interfaces.ICompound

            For Each subst In Phase.Compounds.Values
                val += subst.MassFraction.GetValueOrDefault * Me.AUX_CPi(subst.Name, T)
            Next

            Return val 'KJ/Kg/K

        End Function

        Public Overridable Function AUX_GFm25(ByVal Phase As Phase) As Double

            Dim val As Double
            Dim subst As Interfaces.ICompound

            For Each subst In Me.CurrentMaterialStream.Phases(Me.RET_PHASEID(Phase)).Compounds.Values
                val += subst.MoleFraction.GetValueOrDefault * subst.ConstantProperties.IG_Gibbs_Energy_of_Formation_25C * subst.ConstantProperties.Molar_Weight
            Next

            Dim mw As Double = Me.CurrentMaterialStream.Phases(Me.RET_PHASEID(Phase)).Properties.molecularWeight.GetValueOrDefault

            If mw <> 0.0# Then Return val / mw Else Return 0.0#

        End Function

        Public Overridable Function AUX_HFm25(ByVal Phase As Phase) As Double

            Dim val As Double
            Dim subst As Interfaces.ICompound

            For Each subst In Me.CurrentMaterialStream.Phases(Me.RET_PHASEID(Phase)).Compounds.Values
                val += subst.MoleFraction.GetValueOrDefault * subst.ConstantProperties.IG_Enthalpy_of_Formation_25C * subst.ConstantProperties.Molar_Weight
            Next

            Dim mw As Double = Me.CurrentMaterialStream.Phases(Me.RET_PHASEID(Phase)).Properties.molecularWeight.GetValueOrDefault

            If mw <> 0.0# Then Return val / mw Else Return 0.0#

        End Function

        Public Overridable Function AUX_SFm25(ByVal Phase As Phase) As Double

            Dim val As Double
            Dim subst As Interfaces.ICompound

            For Each subst In Me.CurrentMaterialStream.Phases(Me.RET_PHASEID(Phase)).Compounds.Values
                val += subst.MoleFraction.GetValueOrDefault * subst.ConstantProperties.IG_Entropy_of_Formation_25C * subst.ConstantProperties.Molar_Weight
            Next

            Dim mw As Double = Me.CurrentMaterialStream.Phases(Me.RET_PHASEID(Phase)).Properties.molecularWeight.GetValueOrDefault

            If mw <> 0.0# Then Return val / mw Else Return 0.0#

        End Function

        Public Overridable Function AUX_GFm25(ByVal Vx As Double()) As Double

            Dim val As Double
            Dim subst As Interfaces.ICompound

            Dim i As Integer = 0
            For Each subst In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                val += Vx(i) * subst.ConstantProperties.IG_Gibbs_Energy_of_Formation_25C * subst.ConstantProperties.Molar_Weight
                i += 1
            Next

            Dim mw As Double = AUX_MMM(Vx)

            If mw <> 0.0# Then Return val / mw Else Return 0.0#

        End Function

        Public Overridable Function AUX_HFm25(ByVal Vx As Double()) As Double

            Dim val As Double
            Dim subst As Interfaces.ICompound

            Dim i As Integer = 0
            For Each subst In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                val += Vx(i) * subst.ConstantProperties.IG_Enthalpy_of_Formation_25C * subst.ConstantProperties.Molar_Weight
                i += 1
            Next

            Dim mw As Double = AUX_MMM(Vx)

            If mw <> 0.0# Then Return val / mw Else Return 0.0#

        End Function

        Public Overridable Function AUX_SFm25(ByVal Vx As Double()) As Double

            Dim val As Double
            Dim subst As Interfaces.ICompound

            Dim i As Integer = 0
            For Each subst In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                val += Vx(i) * (subst.ConstantProperties.IG_Enthalpy_of_Formation_25C - subst.ConstantProperties.IG_Gibbs_Energy_of_Formation_25C) / 298.15 * subst.ConstantProperties.Molar_Weight
            Next

            Dim mw As Double = AUX_MMM(Vx)

            If mw <> 0.0# Then Return val / mw Else Return 0.0#

        End Function

        Public Overridable Function AUX_PSUBLi(sub1 As String, ByVal T As Double) As Double
            'Calculate sublimation vapor pressure 
            'Calculation is done by Clausius-Clapeyron
            'ln(P2/P1) = -dH/R * (1/T2- 1/T1)
            'with T1/P1 -> triple point -> Tfus/Pt

            Dim ID = Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.ID

            Dim Tf As Double = CompoundPropCache(ID).TemperatureOfFusion 'K
            Dim Hf As Double = CompoundPropCache(ID).EnthalpyOfFusionAtTf * 1000 'J/mol
            Dim Hv As Double = AUX_HVAPi(sub1, T) * CompoundPropCache(ID).Molar_Weight 'enthalpy of evaporation at T -> j/mol
            Dim dH As Double = Hf + Hv 'enthalpy of sublimation
            Dim Pt As Double 'triple point

            Dim a, b, P As Double

            Pt = AUX_PVAPi(sub1, Tf) 'vapor pressure at melting point -> triple point

            a = -dH / 8.314
            b = dH / 8.314 / Tf + Math.Log(Pt)

            P = Math.Exp(a / T + b)
            Return P


        End Function

        Public Overridable Function RET_VPVAP(ByVal T As Double) As Double()

            Dim val(Me.CurrentMaterialStream.Phases(0).Compounds.Count - 1) As Double
            Dim subst As Interfaces.ICompound
            Dim i As Integer = 0

            For Each subst In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                val(i) = AUX_PVAPi(i, T)
                i += 1
            Next

            Return val

        End Function

        Public Overridable Function AUX_PVAPi(ByVal sub1 As ICompoundConstantProperties, ByVal T As Double) As Double

            Dim ID = sub1.ID

            If sub1.IsPF = 1 Then

                With sub1

                    Return Auxiliary.PROPS.Pvp_leekesler(T, .Critical_Temperature, .Critical_Pressure, .Acentric_Factor)

                End With

            Else


                If sub1.OriginalDB = "DWSIM" Or
                sub1.OriginalDB = "" Then
                    Dim A, B, C, D, E, result As Double
                    A = sub1.Vapor_Pressure_Constant_A
                    B = sub1.Vapor_Pressure_Constant_B
                    C = sub1.Vapor_Pressure_Constant_C
                    D = sub1.Vapor_Pressure_Constant_D
                    E = sub1.Vapor_Pressure_Constant_E
                    result = Math.Exp(A + B / T + C * Math.Log(T) + D * T ^ E)
                    Return result
                ElseIf sub1.OriginalDB = "CheResources" Then
                    Dim A, B, C, result As Double
                    A = sub1.Vapor_Pressure_Constant_A
                    B = sub1.Vapor_Pressure_Constant_B
                    C = sub1.Vapor_Pressure_Constant_C
                    '[LN(P)=A-B/(T+C), P(mmHG) T(K)]
                    result = Math.Exp(A - B / (T + C)) * 133.322368 'mmHg to Pascal
                    Return result
                ElseIf sub1.OriginalDB = "ChemSep" Or
                sub1.OriginalDB = "CoolProp" Or
                sub1.OriginalDB = "User" Or
                sub1.OriginalDB = "ChEDL Thermo" Or
                sub1.OriginalDB = "KDB" Then
                    Dim A, B, C, D, E, result As Double
                    Dim eqno As String = sub1.VaporPressureEquation
                    Dim mw As Double = sub1.Molar_Weight
                    A = sub1.Vapor_Pressure_Constant_A
                    B = sub1.Vapor_Pressure_Constant_B
                    C = sub1.Vapor_Pressure_Constant_C
                    D = sub1.Vapor_Pressure_Constant_D
                    E = sub1.Vapor_Pressure_Constant_E
                    '<vp_c name="Vapour pressure"  units="Pa" >
                    If eqno = "0" Then
                        With sub1
                            result = Auxiliary.PROPS.Pvp_leekesler(T, .Critical_Temperature, .Critical_Pressure, .Acentric_Factor)
                        End With
                    Else
                        If Integer.TryParse(eqno, New Integer) Then
                            result = CalcCSTDepProp(eqno, A, B, C, D, E, T, 0) 'Pa
                        Else
                            If eqno = "" Then
                                With sub1
                                    result = Auxiliary.PROPS.Pvp_leekesler(T, .Critical_Temperature, .Critical_Pressure, .Acentric_Factor)
                                End With
                            Else
                                result = ParseEquation(eqno, A, B, C, D, E, T) 'Pa
                            End If
                        End If
                    End If
                    Return result
                ElseIf sub1.OriginalDB = "Biodiesel" Then
                    Dim A, B, C, D, E, result As Double
                    Dim eqno As String = sub1.VaporPressureEquation
                    A = sub1.Vapor_Pressure_Constant_A
                    B = sub1.Vapor_Pressure_Constant_B
                    C = sub1.Vapor_Pressure_Constant_C
                    D = sub1.Vapor_Pressure_Constant_D
                    E = sub1.Vapor_Pressure_Constant_E
                    result = CalcCSTDepProp(eqno, A, B, C, D, E, T, 0) 'kPa
                    Return result * 1000
                Else
                    With sub1
                        Return Auxiliary.PROPS.Pvp_leekesler(T, .Critical_Temperature, .Critical_Pressure, .Acentric_Factor)
                    End With
                End If

            End If

        End Function


        Public Overridable Function AUX_PVAPi(ByVal sub1 As String, ByVal T As Double) As Double

            Dim ID = Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.ID

            If CompoundPropCache(ID).IsPF = 1 Then

                With CompoundPropCache(ID)

                    Return Auxiliary.PROPS.Pvp_leekesler(T, .Critical_Temperature, .Critical_Pressure, .Acentric_Factor)

                End With

            Else


                If CompoundPropCache(ID).OriginalDB = "DWSIM" Or
                CompoundPropCache(ID).OriginalDB = "" Then
                    Dim A, B, C, D, E, result As Double
                    A = CompoundPropCache(ID).Vapor_Pressure_Constant_A
                    B = CompoundPropCache(ID).Vapor_Pressure_Constant_B
                    C = CompoundPropCache(ID).Vapor_Pressure_Constant_C
                    D = CompoundPropCache(ID).Vapor_Pressure_Constant_D
                    E = CompoundPropCache(ID).Vapor_Pressure_Constant_E
                    result = Math.Exp(A + B / T + C * Math.Log(T) + D * T ^ E)
                    Return result
                ElseIf CompoundPropCache(ID).OriginalDB = "CheResources" Then
                    Dim A, B, C, result As Double
                    A = CompoundPropCache(ID).Vapor_Pressure_Constant_A
                    B = CompoundPropCache(ID).Vapor_Pressure_Constant_B
                    C = CompoundPropCache(ID).Vapor_Pressure_Constant_C
                    '[LN(P)=A-B/(T+C), P(mmHG) T(K)]
                    result = Math.Exp(A - B / (T + C)) * 133.322368 'mmHg to Pascal
                    Return result
                ElseIf CompoundPropCache(ID).OriginalDB = "ChemSep" Or
                CompoundPropCache(ID).OriginalDB = "CoolProp" Or
                CompoundPropCache(ID).OriginalDB = "User" Or
                CompoundPropCache(ID).OriginalDB = "ChEDL Thermo" Or
                CompoundPropCache(ID).OriginalDB = "KDB" Then
                    Dim A, B, C, D, E, result As Double
                    Dim eqno As String = CompoundPropCache(ID).VaporPressureEquation
                    Dim mw As Double = CompoundPropCache(ID).Molar_Weight
                    A = CompoundPropCache(ID).Vapor_Pressure_Constant_A
                    B = CompoundPropCache(ID).Vapor_Pressure_Constant_B
                    C = CompoundPropCache(ID).Vapor_Pressure_Constant_C
                    D = CompoundPropCache(ID).Vapor_Pressure_Constant_D
                    E = CompoundPropCache(ID).Vapor_Pressure_Constant_E
                    '<vp_c name="Vapour pressure"  units="Pa" >
                    If eqno = "0" Then
                        With CompoundPropCache(ID)
                            result = Auxiliary.PROPS.Pvp_leekesler(T, .Critical_Temperature, .Critical_Pressure, .Acentric_Factor)
                        End With
                    Else
                        If Integer.TryParse(eqno, New Integer) Then
                            result = CalcCSTDepProp(eqno, A, B, C, D, E, T, 0) 'Pa
                        Else
                            If eqno = "" Then
                                With CompoundPropCache(ID)
                                    result = Auxiliary.PROPS.Pvp_leekesler(T, .Critical_Temperature, .Critical_Pressure, .Acentric_Factor)
                                End With
                            Else
                                result = ParseEquation(eqno, A, B, C, D, E, T) 'Pa
                            End If
                        End If
                    End If
                    Return result
                ElseIf CompoundPropCache(ID).OriginalDB = "Biodiesel" Then
                    Dim A, B, C, D, E, result As Double
                    Dim eqno As String = CompoundPropCache(ID).VaporPressureEquation
                    A = CompoundPropCache(ID).Vapor_Pressure_Constant_A
                    B = CompoundPropCache(ID).Vapor_Pressure_Constant_B
                    C = CompoundPropCache(ID).Vapor_Pressure_Constant_C
                    D = CompoundPropCache(ID).Vapor_Pressure_Constant_D
                    E = CompoundPropCache(ID).Vapor_Pressure_Constant_E
                    result = CalcCSTDepProp(eqno, A, B, C, D, E, T, 0) 'kPa
                    Return result * 1000
                Else
                    With CompoundPropCache(ID)
                        Return Auxiliary.PROPS.Pvp_leekesler(T, .Critical_Temperature, .Critical_Pressure, .Acentric_Factor)
                    End With
                End If

            End If

        End Function

        Public Overridable Function AUX_PVAPi(ByVal index As Integer, ByVal T As Double) As Double

            Dim subst As Interfaces.ICompound
            Dim nome As String = ""
            Dim i As Integer = 0
            Dim Tf As Double = -1

            For Each subst In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                'If i = index Then nome = subst.Name
                If i = index Then
                    nome = subst.Name
                    Tf = subst.ConstantProperties.TemperatureOfFusion
                    Exit For
                End If
                i += 1
            Next

            If T < Tf Then
                Return AUX_PSUBLi(nome, T)
            Else
                Return AUX_PVAPi(nome, T)
            End If

        End Function

        Public Overridable Function AUX_TSATi(ByVal PVAP As Double, ByVal subst As ICompoundConstantProperties) As Double

            Dim i As Integer

            Dim Tinf, Tsup As Double

            Dim fT, fT_inf, nsub, delta_T As Double

            Tinf = 10
            Tsup = 2000

            nsub = 25

            delta_T = (Tsup - Tinf) / nsub

            i = 0
            Do
                i = i + 1
                fT = PVAP - Me.AUX_PVAPi(subst, Tinf)
                Tinf = Tinf + delta_T
                fT_inf = PVAP - Me.AUX_PVAPi(subst, Tinf)
            Loop Until fT * fT_inf < 0 Or fT_inf > fT Or i >= 100
            If i = 100 Then Return 0.0#
            Tsup = Tinf
            Tinf = Tinf - delta_T

            'metodo de Brent para encontrar Vc

            Dim aaa, bbb, ccc, ddd, eee, min11, min22, faa, fbb, fcc, ppp, qqq, rrr, sss, tol11, xmm As Double
            Dim ITMAX2 As Integer = 100
            Dim iter2 As Integer

            aaa = Tinf
            bbb = Tsup
            ccc = Tsup

            faa = PVAP - Me.AUX_PVAPi(subst, aaa)
            fbb = PVAP - Me.AUX_PVAPi(subst, bbb)
            fcc = fbb

            iter2 = 0
            Do
                If (fbb > 0 And fcc > 0) Or (fbb < 0 And fcc < 0) Then
                    ccc = aaa
                    fcc = faa
                    ddd = bbb - aaa
                    eee = ddd
                End If
                If Math.Abs(fcc) < Math.Abs(fbb) Then
                    aaa = bbb
                    bbb = ccc
                    ccc = aaa
                    faa = fbb
                    fbb = fcc
                    fcc = faa
                End If
                tol11 = 0.0001
                xmm = 0.5 * (ccc - bbb)
                If (Math.Abs(xmm) <= tol11) Or (fbb = 0) Then GoTo Final3
                If (Math.Abs(eee) >= tol11) And (Math.Abs(faa) > Math.Abs(fbb)) Then
                    sss = fbb / faa
                    If aaa = ccc Then
                        ppp = 2 * xmm * sss
                        qqq = 1 - sss
                    Else
                        qqq = faa / fcc
                        rrr = fbb / fcc
                        ppp = sss * (2 * xmm * qqq * (qqq - rrr) - (bbb - aaa) * (rrr - 1))
                        qqq = (qqq - 1) * (rrr - 1) * (sss - 1)
                    End If
                    If ppp > 0 Then qqq = -qqq
                    ppp = Math.Abs(ppp)
                    min11 = 3 * xmm * qqq - Math.Abs(tol11 * qqq)
                    min22 = Math.Abs(eee * qqq)
                    Dim tvar2 As Double
                    If min11 < min22 Then tvar2 = min11
                    If min11 > min22 Then tvar2 = min22
                    If 2 * ppp < tvar2 Then
                        eee = ddd
                        ddd = ppp / qqq
                    Else
                        ddd = xmm
                        eee = ddd
                    End If
                Else
                    ddd = xmm
                    eee = ddd
                End If
                aaa = bbb
                faa = fbb
                If (Math.Abs(ddd) > tol11) Then
                    bbb += ddd
                Else
                    bbb += Math.Sign(xmm) * tol11
                End If
                fbb = PVAP - Me.AUX_PVAPi(subst, bbb)
                iter2 += 1
            Loop Until iter2 = ITMAX2

Final3:

            Return bbb

        End Function

        Public Overridable Function AUX_TSATi(ByVal PVAP As Double, ByVal subst As String) As Double

            Dim i As Integer

            Dim Tinf, Tsup As Double

            Dim fT, fT_inf, nsub, delta_T As Double

            Tinf = 10
            Tsup = 2000

            nsub = 25

            delta_T = (Tsup - Tinf) / nsub

            i = 0
            Do
                i = i + 1
                fT = PVAP - Me.AUX_PVAPi(subst, Tinf)
                Tinf = Tinf + delta_T
                fT_inf = PVAP - Me.AUX_PVAPi(subst, Tinf)
            Loop Until fT * fT_inf < 0 Or fT_inf > fT Or i >= 100
            If i = 100 Then Return 0.0#
            Tsup = Tinf
            Tinf = Tinf - delta_T

            'metodo de Brent para encontrar Vc

            Dim aaa, bbb, ccc, ddd, eee, min11, min22, faa, fbb, fcc, ppp, qqq, rrr, sss, tol11, xmm As Double
            Dim ITMAX2 As Integer = 100
            Dim iter2 As Integer

            aaa = Tinf
            bbb = Tsup
            ccc = Tsup

            faa = PVAP - Me.AUX_PVAPi(subst, aaa)
            fbb = PVAP - Me.AUX_PVAPi(subst, bbb)
            fcc = fbb

            iter2 = 0
            Do
                If (fbb > 0 And fcc > 0) Or (fbb < 0 And fcc < 0) Then
                    ccc = aaa
                    fcc = faa
                    ddd = bbb - aaa
                    eee = ddd
                End If
                If Math.Abs(fcc) < Math.Abs(fbb) Then
                    aaa = bbb
                    bbb = ccc
                    ccc = aaa
                    faa = fbb
                    fbb = fcc
                    fcc = faa
                End If
                tol11 = 0.0001
                xmm = 0.5 * (ccc - bbb)
                If (Math.Abs(xmm) <= tol11) Or (fbb = 0) Then GoTo Final3
                If (Math.Abs(eee) >= tol11) And (Math.Abs(faa) > Math.Abs(fbb)) Then
                    sss = fbb / faa
                    If aaa = ccc Then
                        ppp = 2 * xmm * sss
                        qqq = 1 - sss
                    Else
                        qqq = faa / fcc
                        rrr = fbb / fcc
                        ppp = sss * (2 * xmm * qqq * (qqq - rrr) - (bbb - aaa) * (rrr - 1))
                        qqq = (qqq - 1) * (rrr - 1) * (sss - 1)
                    End If
                    If ppp > 0 Then qqq = -qqq
                    ppp = Math.Abs(ppp)
                    min11 = 3 * xmm * qqq - Math.Abs(tol11 * qqq)
                    min22 = Math.Abs(eee * qqq)
                    Dim tvar2 As Double
                    If min11 < min22 Then tvar2 = min11
                    If min11 > min22 Then tvar2 = min22
                    If 2 * ppp < tvar2 Then
                        eee = ddd
                        ddd = ppp / qqq
                    Else
                        ddd = xmm
                        eee = ddd
                    End If
                Else
                    ddd = xmm
                    eee = ddd
                End If
                aaa = bbb
                faa = fbb
                If (Math.Abs(ddd) > tol11) Then
                    bbb += ddd
                Else
                    bbb += Math.Sign(xmm) * tol11
                End If
                fbb = PVAP - Me.AUX_PVAPi(subst, bbb)
                iter2 += 1
            Loop Until iter2 = ITMAX2

Final3:

            Return bbb

        End Function

        Public Overridable Function AUX_TSATi(ByVal PVAP As Double, ByVal index As Integer) As Double

            Dim i As Integer

            Dim Tinf, Tsup As Double

            Dim fT, fT_inf, nsub, delta_T As Double

            Tinf = 0.1
            Tsup = 10000

            nsub = 50

            delta_T = (Tsup - Tinf) / nsub

            i = 0
            Do
                i = i + 1
                fT = PVAP - Me.AUX_PVAPi(index, Tinf)
                Tinf = Tinf + delta_T
                fT_inf = PVAP - Me.AUX_PVAPi(index, Tinf)
            Loop Until fT * fT_inf < 0 Or fT_inf > fT Or i >= 100
            Tsup = Tinf
            Tinf = Tinf - delta_T

            'metodo de Brent para encontrar Vc

            Dim aaa, bbb, ccc, ddd, eee, min11, min22, faa, fbb, fcc, ppp, qqq, rrr, sss, tol11, xmm As Double
            Dim ITMAX2 As Integer = 100
            Dim iter2 As Integer

            aaa = Tinf
            bbb = Tsup
            ccc = Tsup

            faa = PVAP - Me.AUX_PVAPi(index, aaa)
            fbb = PVAP - Me.AUX_PVAPi(index, bbb)
            fcc = fbb

            iter2 = 0
            Do
                If (fbb > 0 And fcc > 0) Or (fbb < 0 And fcc < 0) Then
                    ccc = aaa
                    fcc = faa
                    ddd = bbb - aaa
                    eee = ddd
                End If
                If Math.Abs(fcc) < Math.Abs(fbb) Then
                    aaa = bbb
                    bbb = ccc
                    ccc = aaa
                    faa = fbb
                    fbb = fcc
                    fcc = faa
                End If
                tol11 = 0.0001
                xmm = 0.5 * (ccc - bbb)
                If (Math.Abs(xmm) <= tol11) Or (fbb = 0) Then GoTo Final3
                If (Math.Abs(eee) >= tol11) And (Math.Abs(faa) > Math.Abs(fbb)) Then
                    sss = fbb / faa
                    If aaa = ccc Then
                        ppp = 2 * xmm * sss
                        qqq = 1 - sss
                    Else
                        qqq = faa / fcc
                        rrr = fbb / fcc
                        ppp = sss * (2 * xmm * qqq * (qqq - rrr) - (bbb - aaa) * (rrr - 1))
                        qqq = (qqq - 1) * (rrr - 1) * (sss - 1)
                    End If
                    If ppp > 0 Then qqq = -qqq
                    ppp = Math.Abs(ppp)
                    min11 = 3 * xmm * qqq - Math.Abs(tol11 * qqq)
                    min22 = Math.Abs(eee * qqq)
                    Dim tvar2 As Double
                    If min11 < min22 Then tvar2 = min11
                    If min11 > min22 Then tvar2 = min22
                    If 2 * ppp < tvar2 Then
                        eee = ddd
                        ddd = ppp / qqq
                    Else
                        ddd = xmm
                        eee = ddd
                    End If
                Else
                    ddd = xmm
                    eee = ddd
                End If
                aaa = bbb
                faa = fbb
                If (Math.Abs(ddd) > tol11) Then
                    bbb += ddd
                Else
                    bbb += Math.Sign(xmm) * tol11
                End If
                fbb = PVAP - Me.AUX_PVAPi(index, bbb)
                iter2 += 1
            Loop Until iter2 = ITMAX2

Final3:

            Return bbb

        End Function

        Public Function AUX_TSATi(ByVal PVAP As Double, ByVal comp As ICompoundConstantProperties, ByVal Tguess As Double) As Double

            Dim pvapt, result As Double, err0 As Integer
            Try
                result = MathNet.Numerics.RootFinding.Brent.FindRoot(Function(T)
                                                                         pvapt = AUX_PVAPi(comp, T)
                                                                         If Double.IsNaN(pvapt) Or Double.IsNaN(PVAP) Then
                                                                             Throw New Exception(String.Format("Error calculation vapor pressure for {0} at {1} K.", comp.Name, T))
                                                                         End If
                                                                         Return pvapt - PVAP
                                                                     End Function, 1.0, 2000.0, 1, 1000)
                Return result
            Catch ex As Exception
                err0 = 1
            End Try
            If err0 = 1 Then
                Dim brent As New MathEx.BrentOpt.Brent
                result = brent.BrentOpt2(1.0, 2000.0, 100, 1, 1000, Function(T)
                                                                        pvapt = AUX_PVAPi(comp, T)
                                                                        If Double.IsNaN(pvapt) Or Double.IsNaN(PVAP) Then
                                                                            Throw New Exception(String.Format("Error calculation vapor pressure for {0} at {1} K.", comp.Name, T))
                                                                        End If
                                                                        Return pvapt - PVAP
                                                                    End Function)
                Return result
            End If

        End Function

        Public Overridable Function AUX_PSUBLi(index As Integer, T As Double) As Double

            Dim Tf = RET_VTF()(index)
            Dim Hf = RET_VHF()(index)

            Dim Pvapl = AUX_PVAPi(index, T)
            Dim Pvaps = Math.Exp(Math.Log(Pvapl) - Hf * 1000 / 8.314 * (1 / T - 1 / Tf))

            Return Pvaps

        End Function

        Public Function AUX_TSUBLi(ByVal index As Integer, ByVal PVAP As Double) As Double

            Dim Tmin = RET_VTF()(index) * 0.3
            Dim Tmax = RET_VTB()(index)
            Dim pvapt As Double
            Return MathNet.Numerics.RootFinding.Brent.FindRoot(Function(T)
                                                                   pvapt = AUX_PSUBLi(index, T)
                                                                   If Double.IsNaN(pvapt) Or Double.IsNaN(PVAP) Then
                                                                       Throw New Exception(String.Format("Error calculation sublimation pressure for {0} at {1} K.", "", T))
                                                                   End If
                                                                   Return pvapt - PVAP
                                                               End Function, Tmin, Tmax)

        End Function


        Public Function RET_HVAPM(ByVal Vxw As Array, ByVal T As Double) As Double

            Dim val As Double = 0.0#
            Dim i As Integer
            Dim n As Integer = Me.CurrentMaterialStream.Phases(0).Compounds.Count - 1

            i = 0
            For Each subst As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                val += Vxw(i) * Me.AUX_HVAPi(subst.Name, T)
                i += 1
            Next

            Return val

        End Function

        Public Function RET_HFUSM(ByVal Vxw As Array, ByVal T As Double) As Double

            Dim val As Double = 0.0#
            Dim i As Integer
            Dim n As Integer = Me.CurrentMaterialStream.Phases(0).Compounds.Count - 1

            i = 0
            For Each subst As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                val += Vxw(i) * Me.AUX_HFUSi(subst.Name, T)
                i += 1
            Next

            Return val

        End Function

        Public Function AUX_HVAPi(ByVal sub1 As String, ByVal T As Double)

            Dim A, B, C, D, E, Tr, result As Double
            A = Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.HVap_A
            B = Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.HVap_B
            C = Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.HVap_C
            D = Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.HVap_D
            E = Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.HVap_E

            Tr = T / Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.Critical_Temperature

            If Tr >= 1 Then Return 0.0#

            If Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.OriginalDB = "DWSIM" Or
                                Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.OriginalDB = "" Then
                If Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.IsHYPO = 1 Or
                Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.IsPF = 1 Then
                    Dim tr1 As Double
                    tr1 = Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.Normal_Boiling_Point / Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.Critical_Temperature
                    result = Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.HVap_A * ((1 - Tr) / (1 - tr1)) ^ 0.375
                    Return result 'kJ/kg
                Else
                    result = A * (1 - Tr) ^ (B + C * Tr + D * Tr ^ 2)
                    Return result / Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.Molar_Weight / 1000 'kJ/kg
                End If
            ElseIf Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.OriginalDB = "CheResources" Or
            Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.OriginalDB = "CoolProp" Or
           Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.OriginalDB = "User" Or
           Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.OriginalDB = "KDB" Then
                Dim tr1 As Double
                If Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.OriginalDB = "KDB" Then
                    tr1 = Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.HVap_B / Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.Critical_Temperature
                Else
                    tr1 = Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.Normal_Boiling_Point / Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.Critical_Temperature
                End If
                If Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.HVap_A = 0.0# Then
                    Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.HVap_A = New Utilities.Hypos.Methods.HYP().DHvb_Vetere(Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.Critical_Temperature, Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.Critical_Pressure, Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.Normal_Boiling_Point)
                    Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.HVap_A /= Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.Molar_Weight
                End If
                result = Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.HVap_A * ((1 - Tr) / (1 - tr1)) ^ 0.375
                Return result 'kJ/kg
            ElseIf Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.OriginalDB = "ChemSep" Then
                Dim eqno As String = Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.VaporizationEnthalpyEquation
                result = CalcCSTDepProp(eqno, A, B, C, D, E, T, T / Tr) / Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.Molar_Weight / 1000 'kJ/kg
                Return result
            ElseIf Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.OriginalDB = "ChEDL Thermo" Then
                Dim eqno As String = Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.VaporizationEnthalpyEquation
                result = CalcCSTDepProp(eqno, A, B, C, D, E, T, T / Tr) 'kJ/kg
                Return result
            Else
                Return 0.0#
            End If

        End Function

        Public Overridable Function AUX_HFUSi(ByVal sub1 As String, ByVal T As Double)

            'return DHfus in kJ/kg
            Dim cpc = Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties
            Dim Tfus = cpc.TemperatureOfFusion

            Dim cpterm = cpc.GetSolidHeatCapacity(T) * (T - Tfus)

            Return cpc.EnthalpyOfFusionAtTf * 1000 / cpc.Molar_Weight + cpterm

        End Function

        Public Overridable Function AUX_LIQVISCi(ByVal sub1 As String, ByVal T As Double, ByVal P As Double) As Double

            If Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.IsPF = 1 Then
                With Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties
                    Dim dens = AUX_LIQDENSi(Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties, T)
                    Dim visc = Auxiliary.PROPS.oilvisc_twu(T, .PF_Tv1, .PF_Tv2, .PF_v1, .PF_v2)
                    If Double.IsNaN(visc) Then
                        Dim Tc, Pc, w, Mw As Double
                        Tc = Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.Critical_Temperature
                        Pc = Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.Critical_Pressure
                        w = Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.Acentric_Factor
                        Mw = Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.Molar_Weight
                        visc = Auxiliary.PROPS.viscl_letsti(T, Tc, Pc, w, Mw)
                    End If
                    Return visc * dens
                End With
            Else
                If LiquidViscosityCalculationMode_Subcritical = LiquidViscosityCalcMode.ExpData Then
                    If Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.OriginalDB = "DWSIM" Or
                        Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.OriginalDB = "" Then
                        Dim A, B, C, D, E, result As Double
                        A = Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.Liquid_Viscosity_Const_A
                        B = Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.Liquid_Viscosity_Const_B
                        C = Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.Liquid_Viscosity_Const_C
                        D = Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.Liquid_Viscosity_Const_D
                        E = Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.Liquid_Viscosity_Const_E
                        result = Math.Exp(A + B / T + C * Math.Log(T) + D * T ^ E)
                        Return result
                    ElseIf Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.OriginalDB = "CheResources" Then
                        Dim B, C, result As Double
                        B = Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.Liquid_Viscosity_Const_B
                        C = Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.Liquid_Viscosity_Const_C
                        '[LOG(V)=B*(1/T-1/C), T(K) V(CP)]
                        result = Exp(B * (1 / T - 1 / C)) * 0.001
                        Return result
                    ElseIf Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.OriginalDB = "ChemSep" Or
                        Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.OriginalDB = "CoolProp" Or
                        Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.OriginalDB = "User" Or
                        Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.OriginalDB = "ChEDL Thermo" Or
                        Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.OriginalDB = "KDB" Then
                        Dim A, B, C, D, E, result As Double
                        Dim eqno As String = Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.LiquidViscosityEquation
                        Dim mw As Double = Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.Molar_Weight
                        A = Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.Liquid_Viscosity_Const_A
                        B = Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.Liquid_Viscosity_Const_B
                        C = Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.Liquid_Viscosity_Const_C
                        D = Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.Liquid_Viscosity_Const_D
                        E = Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.Liquid_Viscosity_Const_E
                        '<lvsc name="Liquid viscosity"  units="Pa.s" >
                        If eqno = "0" Or eqno = "" Then
                            Dim Tc, Pc, w As Double
                            Tc = Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.Critical_Temperature
                            Pc = Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.Critical_Pressure
                            w = Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.Acentric_Factor
                            mw = Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.Molar_Weight
                            result = Auxiliary.PROPS.viscl_letsti(T, Tc, Pc, w, mw)
                        Else
                            If Integer.TryParse(eqno, New Integer) Then
                                result = CalcCSTDepProp(eqno, A, B, C, D, E, T, 0) 'Pa.s
                            Else
                                result = ParseEquation(eqno, A, B, C, D, E, T) 'Pa.s
                            End If
                        End If
                        Return result
                    ElseIf Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.OriginalDB = "Biodiesel" Then
                        Dim result As Double
                        Dim Tc, Pc, w, Mw As Double
                        Tc = Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.Critical_Temperature
                        Pc = Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.Critical_Pressure
                        w = Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.Acentric_Factor
                        Mw = Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.Molar_Weight
                        result = Auxiliary.PROPS.viscl_letsti(T, Tc, Pc, w, Mw)
                        Return result
                    End If
                Else
                    Dim result As Double
                    Dim Tc, Pc, w, Mw As Double
                    Tc = Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.Critical_Temperature
                    Pc = Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.Critical_Pressure
                    w = Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.Acentric_Factor
                    Mw = Me.CurrentMaterialStream.Phases(0).Compounds(sub1).ConstantProperties.Molar_Weight
                    result = Auxiliary.PROPS.viscl_letsti(T, Tc, Pc, w, Mw)
                    Return result
                End If
            End If

        End Function

        Public Overridable Function AUX_LIQVISCm(ByVal T As Double, P As Double, Optional ByVal phaseid As Integer = 3) As Double

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "AUX_LIQVISCm", "Liquid Phase Viscosity", "Liquid Phase Viscosity Calculation Routine")

            IObj?.SetCurrent()

            Dim val, lval, sval, result As Double
            Dim subst As Interfaces.ICompound

            IObj?.Paragraphs.Add("Liquid phase viscosity is calculated according to the currently selected mixing rule.")

            IObj?.Paragraphs.Add("Mixing rule: " & LiquidViscosity_MixingRule.ToString)

            IObj?.Paragraphs.Add("Dependence of individual compound viscosity with the temperature  is described in the equation")

            IObj?.Paragraphs.Add("<m>\eta=\exp(A+B/T+C\ln T+DT^{E}),<m>")

            IObj?.Paragraphs.Add("where A, B, C, D and E are experimental coefficients (or 
                                generated by DWSIM for pseudocomponents or hypotheticals).")

            val = 0
            For Each subst In Me.CurrentMaterialStream.Phases(phaseid).Compounds.Values
                IObj?.SetCurrent()
                sval = Me.AUX_LIQVISCi(subst.Name, T, P)
                IObj?.Paragraphs.Add(String.Format("Calculating Liquid Viscosity for {0}... {1} Pa.s (xi = {2})", subst.Name, sval, subst.MoleFraction.GetValueOrDefault))
                lval = sval
                If sval = 0 Then lval = 0.0
                IObj?.Paragraphs.Add(String.Format("Liquid Viscosity : {0} Pa.s", Exp(lval)))
                If LiquidViscosity_CorrectExpDataForPressure And LiquidViscosityCalculationMode_Subcritical = LiquidViscosityCalcMode.ExpData Then
                    'pressure correction
                    Dim pcorr As Double = 1.0
                    If (T / subst.ConstantProperties.Critical_Temperature) > 1.0 Then
                        pcorr = Auxiliary.PROPS.viscl_pcorrection_lucas(T / subst.ConstantProperties.Critical_Temperature, P, subst.ConstantProperties.Critical_Pressure, AUX_KHenry(subst.Name, T), subst.ConstantProperties.Acentric_Factor)
                    Else
                        pcorr = Auxiliary.PROPS.viscl_pcorrection_lucas(T / subst.ConstantProperties.Critical_Temperature, P, subst.ConstantProperties.Critical_Pressure, AUX_PVAPi(subst.Name, T), subst.ConstantProperties.Acentric_Factor)
                    End If
                    IObj?.Paragraphs.Add(String.Format("Compressed Liquid Viscosity Correction Factor: {0}", pcorr))
                    lval = lval * pcorr
                    IObj?.Paragraphs.Add(String.Format("Corrected Liquid Viscosity : {0} Pa.s", Exp(lval)))
                    If Double.IsNaN(lval) Or Double.IsInfinity(lval) And subst.MoleFraction.GetValueOrDefault > 0 Then
                        Throw New Exception(String.Format("Error calculating viscosity for '{0}'. Temperature: {1} K, Pressure: {2} Pa. Calculated value: {3}", subst.Name, T, P, lval))
                    End If
                End If
                Select Case LiquidViscosity_MixingRule
                    Case LiquidViscosityMixRule.MoleAverage
                        val += subst.MoleFraction.GetValueOrDefault * lval
                    Case LiquidViscosityMixRule.LogMoleAverage
                        val += subst.MoleFraction.GetValueOrDefault * Log(lval)
                    Case LiquidViscosityMixRule.InvertedMassAverage
                        val += subst.MassFraction.GetValueOrDefault / lval
                    Case LiquidViscosityMixRule.InvertedLogMassAverage
                        val += subst.MassFraction.GetValueOrDefault / Log(lval)
                End Select
            Next

            Select Case LiquidViscosity_MixingRule
                Case LiquidViscosityMixRule.MoleAverage
                    result = val
                Case LiquidViscosityMixRule.LogMoleAverage
                    result = Exp(val)
                Case LiquidViscosityMixRule.InvertedMassAverage
                    result = 1.0 / val
                Case LiquidViscosityMixRule.InvertedLogMassAverage
                    result = Exp(1.0 / val)
            End Select

            IObj?.Paragraphs.Add("<h2>Results</h2>")

            IObj?.Paragraphs.Add(String.Format("Liquid Phase Viscosity: {0} Pa.s", result))

            IObj?.Close()

            Return result

        End Function

        Public Overridable Function AUX_SURFTM(ByVal T As Double) As Double

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "AUX_SURFTM", "Liquid Phase Surface Tension", "Liquid Phase Surface Tension Calculation Routine")

            IObj?.SetCurrent()

            IObj?.Paragraphs.Add("The liquid phase surface 
                                tension is calculated by doing a molar average of the individual 
                                component tensions. When experimental data is not available, the value 
                                is calculated with the Brock-Bird equation,")

            IObj?.Paragraphs.Add("<m>\frac{\sigma}{P_{c}^{2/3}T_{c}^{1/3}}=(0.132\alpha_{c}-0.279)(1-T_{r})^{11/9}</m>")

            IObj?.Paragraphs.Add("<m>\alpha_{c}=0.9076[1+\frac{T_{br}\ln(P_{c}/1.01325)}{1-T_{br}}],</m>")

            IObj?.Paragraphs.Add("where")

            IObj?.Paragraphs.Add("<mi>\sigma</mi> Surface tension (N/m)")

            IObj?.Paragraphs.Add("<mi>T_{c}</mi> Critical temperature (K)")

            IObj?.Paragraphs.Add("<mi>P_{c}</mi> Critical pressure (Pa)")

            IObj?.Paragraphs.Add("<mi>T_{br}</mi> Reduced normal boiling point, <mi>T_{b}/T_{c}</mi>")

            Dim val As Double = 0
            Dim tmpval As Double = 0.0#
            Dim nbp As Double
            Dim subst As Interfaces.ICompound
            Dim ftotal As Double = 1

            For Each subst In Me.CurrentMaterialStream.Phases(1).Compounds.Values
                IObj?.SetCurrent()
                IObj?.Paragraphs.Add(String.Format("Calculating Surface Tension for {0}... (xi = {1})", subst.Name, subst.MoleFraction.GetValueOrDefault))
                If T / subst.ConstantProperties.Critical_Temperature < 1.0 Then
                    With subst.ConstantProperties
                        If .SurfaceTensionEquation <> "" And .SurfaceTensionEquation <> "0" And Not .IsIon And Not .IsSalt Then
                            tmpval = CalcCSTDepProp(.SurfaceTensionEquation, .Surface_Tension_Const_A, .Surface_Tension_Const_B, .Surface_Tension_Const_C, .Surface_Tension_Const_D, .Surface_Tension_Const_E, T, .Critical_Temperature)
                            IObj?.Paragraphs.Add(String.Format("Value calculated from experimental curve: {0} N/m", tmpval))
                        ElseIf .IsIon Or .IsSalt Then
                            tmpval = 0.0#
                        Else
                            nbp = subst.ConstantProperties.Normal_Boiling_Point
                            If nbp = 0 Then nbp = 0.7 * subst.ConstantProperties.Critical_Temperature
                            tmpval = Auxiliary.PROPS.sigma_bb(T, nbp, subst.ConstantProperties.Critical_Temperature, subst.ConstantProperties.Critical_Pressure)
                            IObj?.Paragraphs.Add(String.Format("Value estimated with Brock-Bird correlation: {0} N/m", tmpval))
                        End If
                    End With
                Else
                    tmpval = 0
                    ftotal -= subst.MoleFraction.GetValueOrDefault
                End If
                val += subst.MoleFraction.GetValueOrDefault * tmpval / ftotal
            Next

            IObj?.Paragraphs.Add("<h2>Results</h2>")

            IObj?.Paragraphs.Add(String.Format("Liquid Phase Surface Tension: {0} N/m", val))

            IObj?.Close()

            Return val

        End Function

        Public Overridable Function AUX_SURFTM(pidx As Integer, T As Double) As Double

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "AUX_SURFTM", "Liquid Phase Surface Tension", "Liquid Phase Surface Tension Calculation Routine")

            IObj?.SetCurrent()

            IObj?.Paragraphs.Add("The liquid phase surface 
                                tension is calculated by doing a molar average of the individual 
                                component tensions. When experimental data is not available, the value 
                                is calculated with the Brock-Bird equation,")

            IObj?.Paragraphs.Add("<m>\frac{\sigma}{P_{c}^{2/3}T_{c}^{1/3}}=(0.132\alpha_{c}-0.279)(1-T_{r})^{11/9}</m>")

            IObj?.Paragraphs.Add("<m>\alpha_{c}=0.9076[1+\frac{T_{br}\ln(P_{c}/1.01325)}{1-T_{br}}],</m>")

            IObj?.Paragraphs.Add("where")

            IObj?.Paragraphs.Add("<mi>\sigma</mi> Surface tension (N/m)")

            IObj?.Paragraphs.Add("<mi>T_{c}</mi> Critical temperature (K)")

            IObj?.Paragraphs.Add("<mi>P_{c}</mi> Critical pressure (Pa)")

            IObj?.Paragraphs.Add("<mi>T_{br}</mi> Reduced normal boiling point, <mi>T_{b}/T_{c}</mi>")

            Dim val As Double = 0
            Dim tmpval As Double = 0.0#
            Dim nbp As Double
            Dim subst As Interfaces.ICompound
            Dim ftotal As Double = 1

            For Each subst In Me.CurrentMaterialStream.Phases(pidx).Compounds.Values
                IObj?.SetCurrent()
                IObj?.Paragraphs.Add(String.Format("Calculating Surface Tension for {0}... (xi = {1})", subst.Name, subst.MoleFraction.GetValueOrDefault))
                If T / subst.ConstantProperties.Critical_Temperature < 1.0 Then
                    With subst.ConstantProperties
                        If .SurfaceTensionEquation <> "" And .SurfaceTensionEquation <> "0" And Not .IsIon And Not .IsSalt Then
                            tmpval = CalcCSTDepProp(.SurfaceTensionEquation, .Surface_Tension_Const_A, .Surface_Tension_Const_B, .Surface_Tension_Const_C, .Surface_Tension_Const_D, .Surface_Tension_Const_E, T, .Critical_Temperature)
                            IObj?.Paragraphs.Add(String.Format("Value calculated from experimental curve: {0} N/m", tmpval))
                        ElseIf .IsIon Or .IsSalt Then
                            tmpval = 0.0#
                        Else
                            nbp = subst.ConstantProperties.Normal_Boiling_Point
                            If nbp = 0 Then nbp = 0.7 * subst.ConstantProperties.Critical_Temperature
                            tmpval = Auxiliary.PROPS.sigma_bb(T, nbp, subst.ConstantProperties.Critical_Temperature, subst.ConstantProperties.Critical_Pressure)
                            IObj?.Paragraphs.Add(String.Format("Value estimated with Brock-Bird correlation: {0} N/m", tmpval))
                        End If
                    End With
                Else
                    tmpval = 0
                    ftotal -= subst.MoleFraction.GetValueOrDefault
                End If
                val += subst.MoleFraction.GetValueOrDefault * tmpval / ftotal
            Next

            IObj?.Paragraphs.Add("<h2>Results</h2>")

            IObj?.Paragraphs.Add(String.Format("Liquid Phase Surface Tension: {0} N/m", val))

            IObj?.Close()

            Return val

        End Function

        Public Overridable Function AUX_SURFTi(ByVal constprop As Interfaces.ICompoundConstantProperties, ByVal T As Double) As Double

            Dim val As Double = 0
            Dim nbp As Double
            Dim ftotal As Double = 1

            If T / constprop.Critical_Temperature < 1 Then
                With constprop
                    If .SurfaceTensionEquation <> "" And .SurfaceTensionEquation <> "0" And Not .IsIon And Not .IsSalt Then
                        If Integer.TryParse(.SurfaceTensionEquation, New Integer) Then
                            val = CalcCSTDepProp(.SurfaceTensionEquation, .Surface_Tension_Const_A, .Surface_Tension_Const_B, .Surface_Tension_Const_C, .Surface_Tension_Const_D, .Surface_Tension_Const_E, T, .Critical_Temperature)
                        Else
                            val = ParseEquation(.SurfaceTensionEquation, .Surface_Tension_Const_A, .Surface_Tension_Const_B, .Surface_Tension_Const_C, .Surface_Tension_Const_D, .Surface_Tension_Const_E, T)
                        End If
                    ElseIf .IsIon Or .IsSalt Then
                        val = 0.0#
                    Else
                        nbp = constprop.Normal_Boiling_Point
                        If nbp = 0 Then nbp = 0.7 * constprop.Critical_Temperature
                        val = Auxiliary.PROPS.sigma_bb(T, nbp, constprop.Critical_Temperature, constprop.Critical_Pressure)
                    End If
                End With
            Else
            End If

            Return val

        End Function

        Public Overridable Function AUX_CONDTL(ByVal T As Double, Optional ByVal phaseid As Integer = 3) As Double

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "AUX_CONDTL", "Liquid Phase Thermal Conductivity", "Liquid Phase Thermal Conductivity Calculation Routine")

            IObj?.SetCurrent()

            IObj?.Paragraphs.Add("When experimental data is not available, the contribution of each 
                                component for the thermal conductivity of the liquid phase is 
                                calculated by the Latini method,")

            IObj?.Paragraphs.Add("<m>\lambda_{i}	=	\frac{A(1-T_{r})^{0.38}}{T_{r}^{1/6}}<m>")

            IObj?.Paragraphs.Add("<m>A=\frac{A^{*}T_{b}^{0.38}}{MM^{\beta}T_{c}^{\gamma}},<m>")

            IObj?.Paragraphs.Add("where <mi>A^{*}</mi>,<mi>\alpha</mi>,<mi>\beta</mi> and <mi>\gamma</mi> depend on the nature of 
                                the liquid (Saturated Hydrocarbon, Aromatic, Water, etc). The 
                                liquid phase thermal conductivity is calculated from the 
                                individual values by the Li method,")

            IObj?.Paragraphs.Add("<m>\lambda_{L}	=\sum\sum\phi_{i}\phi_{j}\lambda_{ij}	<m>")

            IObj?.Paragraphs.Add("<m>\lambda_{ij}	=	2(\lambda_{i}^{-1}+\lambda_{j}^{-1})^{-1}<m>")

            IObj?.Paragraphs.Add("<m>\phi_{i}=\frac{x_{i}V_{c_{i}}}{\sum x_{i}V_{c_{i}}},<m>")

            IObj?.Paragraphs.Add("where")

            IObj?.Paragraphs.Add("<mi>\lambda_{L}</mi> liquid phase thermal conductivity (W/[m.K])")

            Dim val As Double
            Dim subst As Interfaces.ICompound
            Dim vcl(Me.CurrentMaterialStream.Phases(0).Compounds.Count - 1)
            Dim i As Integer = 0
            For Each subst In Me.CurrentMaterialStream.Phases(phaseid).Compounds.Values
                IObj?.SetCurrent()
                IObj?.Paragraphs.Add(String.Format("Calculating Thermal Conductivity for {0}... (xi = {1})", subst.Name, subst.MoleFraction.GetValueOrDefault))
                If subst.ConstantProperties.LiquidThermalConductivityEquation <> "" Then
                    If Integer.TryParse(subst.ConstantProperties.LiquidThermalConductivityEquation, New Integer) Then
                        vcl(i) = CalcCSTDepProp(subst.ConstantProperties.LiquidThermalConductivityEquation, subst.ConstantProperties.Liquid_Thermal_Conductivity_Const_A, subst.ConstantProperties.Liquid_Thermal_Conductivity_Const_B, subst.ConstantProperties.Liquid_Thermal_Conductivity_Const_C, subst.ConstantProperties.Liquid_Thermal_Conductivity_Const_D, subst.ConstantProperties.Liquid_Thermal_Conductivity_Const_E, T, subst.ConstantProperties.Critical_Temperature)
                    Else
                        vcl(i) = ParseEquation(subst.ConstantProperties.LiquidThermalConductivityEquation, subst.ConstantProperties.Liquid_Thermal_Conductivity_Const_A, subst.ConstantProperties.Liquid_Thermal_Conductivity_Const_B, subst.ConstantProperties.Liquid_Thermal_Conductivity_Const_C, subst.ConstantProperties.Liquid_Thermal_Conductivity_Const_D, subst.ConstantProperties.Liquid_Thermal_Conductivity_Const_E, T)
                    End If
                    IObj?.Paragraphs.Add(String.Format("Value calculated from experimental curve: {0} W/[m.K]", vcl(i)))
                    If vcl(i) <= 0.0 Then
                        vcl(i) = Auxiliary.PROPS.condl_latini(T, subst.ConstantProperties.Normal_Boiling_Point, subst.ConstantProperties.Critical_Temperature, subst.ConstantProperties.Molar_Weight, "")
                        IObj?.Paragraphs.Add(String.Format("Value estimated with Latini correlation: {0} W/[m.K]", vcl(i)))
                    End If
                ElseIf subst.ConstantProperties.IsIon Or subst.ConstantProperties.IsSalt Then
                    vcl(i) = 0.0#
                Else
                    vcl(i) = Auxiliary.PROPS.condl_latini(T, subst.ConstantProperties.Normal_Boiling_Point, subst.ConstantProperties.Critical_Temperature, subst.ConstantProperties.Molar_Weight, "")
                    IObj?.Paragraphs.Add(String.Format("Value estimated with Latini correlation: {0} W/[m.K]", vcl(i)))
                End If

                i = i + 1
            Next

            val = Auxiliary.PROPS.condlm_li(Me.RET_VVC, vcl, Me.RET_VMOL(Me.RET_PHASECODE(phaseid)))

            IObj?.Paragraphs.Add("<h2>Results</h2>")

            IObj?.Paragraphs.Add(String.Format("Average Mixture Value from Li's mixing rule: {0} W/[m.K]", val))

            IObj?.Close()

            Return val

        End Function

        Public Overridable Function AUX_CONDTG(ByVal T As Double, ByVal P As Double) As Double

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "AUX_CONDTG", "Vapor Phase Thermal Conductivity", "Vapor Phase Thermal Conductivity Calculation Routine")

            IObj?.SetCurrent()

            Dim val As Double
            Dim subst As Interfaces.ICompound
            Dim i As Integer = 0
            Dim sval As Double = 0
            For Each subst In Me.CurrentMaterialStream.Phases(2).Compounds.Values
                IObj?.SetCurrent()
                IObj?.Paragraphs.Add(String.Format("Calculating Thermal Conductivity for {0}... (xi = {1})", subst.Name, subst.MoleFraction.GetValueOrDefault))
                If subst.ConstantProperties.VaporThermalConductivityEquation <> "" Then
                    If Integer.TryParse(subst.ConstantProperties.VaporThermalConductivityEquation, New Integer) Then
                        sval = CalcCSTDepProp(subst.ConstantProperties.VaporThermalConductivityEquation, subst.ConstantProperties.Vapor_Thermal_Conductivity_Const_A, subst.ConstantProperties.Vapor_Thermal_Conductivity_Const_B, subst.ConstantProperties.Vapor_Thermal_Conductivity_Const_C, subst.ConstantProperties.Vapor_Thermal_Conductivity_Const_D, subst.ConstantProperties.Vapor_Thermal_Conductivity_Const_E, T, subst.ConstantProperties.Critical_Temperature)
                        IObj?.Paragraphs.Add(String.Format("Value calculated from experimental curve: {0} W/[m.K]", sval))
                        val += subst.MoleFraction.GetValueOrDefault * sval
                    Else
                        sval = ParseEquation(subst.ConstantProperties.VaporThermalConductivityEquation, subst.ConstantProperties.Vapor_Thermal_Conductivity_Const_A, subst.ConstantProperties.Vapor_Thermal_Conductivity_Const_B, subst.ConstantProperties.Vapor_Thermal_Conductivity_Const_C, subst.ConstantProperties.Vapor_Thermal_Conductivity_Const_D, subst.ConstantProperties.Vapor_Thermal_Conductivity_Const_E, T)
                        IObj?.Paragraphs.Add(String.Format("Value calculated from experimental curve: {0} W/[m.K]", sval))
                        val += subst.MoleFraction.GetValueOrDefault * sval
                    End If
                Else
                    sval = Auxiliary.PROPS.condtg_elyhanley(T, Me.AUX_TCM(Phase.Vapor), Me.AUX_VCM(Phase.Vapor), Me.AUX_ZCM(Phase.Vapor), Me.AUX_WM(Phase.Vapor), Me.AUX_MMM(Phase.Vapor), Me.DW_CalcCv_ISOL(Phase.Vapor, T, P) * Me.AUX_MMM(Phase.Vapor))
                    IObj?.Paragraphs.Add(String.Format("Value estimated with Ely-Hanley correlation: {0} W/[m.K]", sval))
                    val += subst.MoleFraction.GetValueOrDefault * sval
                End If
                i = i + 1
            Next

            IObj?.Paragraphs.Add("<h2>Results</h2>")

            IObj?.Paragraphs.Add(String.Format("Molar Average Value: {0} W/[m.K]", val))

            IObj?.Close()

            Return val

        End Function

        Public Overridable Function AUX_LIQTHERMCONDi(ByVal cprop As Interfaces.ICompoundConstantProperties, ByVal T As Double) As Double

            Dim val As Double

            If cprop.LiquidThermalConductivityEquation <> "" And cprop.LiquidThermalConductivityEquation <> "0" And Not cprop.IsIon And Not cprop.IsSalt Then
                If Integer.TryParse(cprop.LiquidThermalConductivityEquation, New Integer) Then
                    val = CalcCSTDepProp(cprop.LiquidThermalConductivityEquation, cprop.Liquid_Thermal_Conductivity_Const_A, cprop.Liquid_Thermal_Conductivity_Const_B, cprop.Liquid_Thermal_Conductivity_Const_C, cprop.Liquid_Thermal_Conductivity_Const_D, cprop.Liquid_Thermal_Conductivity_Const_E, T, cprop.Critical_Temperature)
                Else
                    val = ParseEquation(cprop.LiquidThermalConductivityEquation, cprop.Liquid_Thermal_Conductivity_Const_A, cprop.Liquid_Thermal_Conductivity_Const_B, cprop.Liquid_Thermal_Conductivity_Const_C, cprop.Liquid_Thermal_Conductivity_Const_D, cprop.Liquid_Thermal_Conductivity_Const_E, T)
                End If

            ElseIf cprop.IsIon Or cprop.IsSalt Then
                val = 0.0#
            Else
                val = Auxiliary.PROPS.condl_latini(T, cprop.Normal_Boiling_Point, cprop.Critical_Temperature, cprop.Molar_Weight, "")
            End If

            Return val

        End Function

        Public Overridable Function AUX_VAPTHERMCONDi(ByVal cprop As Interfaces.ICompoundConstantProperties, ByVal T As Double, ByVal P As Double) As Double

            Dim val As Double

            If cprop.VaporThermalConductivityEquation <> "" And cprop.VaporThermalConductivityEquation <> "0" And Not cprop.IsIon And Not cprop.IsSalt Then
                If Integer.TryParse(cprop.VaporThermalConductivityEquation, New Integer) Then
                    val = CalcCSTDepProp(cprop.VaporThermalConductivityEquation, cprop.Vapor_Thermal_Conductivity_Const_A, cprop.Vapor_Thermal_Conductivity_Const_B, cprop.Vapor_Thermal_Conductivity_Const_C, cprop.Vapor_Thermal_Conductivity_Const_D, cprop.Vapor_Thermal_Conductivity_Const_E, T, cprop.Critical_Temperature)
                Else
                    val = ParseEquation(cprop.VaporThermalConductivityEquation, cprop.Vapor_Thermal_Conductivity_Const_A, cprop.Vapor_Thermal_Conductivity_Const_B, cprop.Vapor_Thermal_Conductivity_Const_C, cprop.Vapor_Thermal_Conductivity_Const_D, cprop.Vapor_Thermal_Conductivity_Const_E, T)
                End If
            ElseIf cprop.IsIon Or cprop.IsSalt Then
                val = 0.0#
            Else
                val = Auxiliary.PROPS.condtg_elyhanley(T, cprop.Critical_Temperature, cprop.Critical_Volume / 1000, cprop.Critical_Compressibility, cprop.Acentric_Factor, cprop.Molar_Weight, Me.AUX_CPi(cprop.Name, T) * cprop.Molar_Weight - 8.314)
            End If

            Return val

        End Function

        Public Overridable Function AUX_VAPVISCm(ByVal T As Double, ByVal RHO As Double, ByVal MM As Double) As Double

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "AUX_VAPVISCm", "Vapor Phase Viscosity", "Vapor Phase Viscosity Calculation Routine")

            IObj?.SetCurrent()

            IObj?.Paragraphs.Add("Vapor phase viscosity is calculated in two steps. First, when 
                                experimental data is not available, the temperature dependence is 
                                given by the Lucas equation,")

            IObj?.Paragraphs.Add("<m>\eta\xi=[0,.807T_{r}^{0,618}-0.357\exp(-0.449T_{r})+0.34\exp(-4.058T_{r})+0.018]</m>")

            IObj?.Paragraphs.Add("<m>\xi=0,176(\frac{T_{c}}{MM^{3}P_{c}^{4}})^{1/6},</m>")

            IObj?.Paragraphs.Add("where")

            IObj?.Paragraphs.Add("<mi>\eta</mi> Viscosity <mi>(\mu P)</mi>")

            IObj?.Paragraphs.Add("<mi>T_{c},P_{c}</mi> Component (or mixture) critical properties ")

            IObj?.Paragraphs.Add("<mi>T_{r}</mi> Reduced temperature, `T/T_{c}`")

            IObj?.Paragraphs.Add("<mi>MM</mi> Molecular weight (kg/kmol)")

            IObj?.Paragraphs.Add("In the second step, the experimental or calculated viscosity with 
                                the Lucas method is corrected to take into account the effect of 
                                pressure, by the Jossi-Stiel-Thodos method,")

            IObj?.Paragraphs.Add("<m>[(\eta-\eta_{0})(\frac{T_{c}}{MM^{3}P_{c}^{4}})^{1/6}+1]^{1/4}	=	1.023+0.23364\rho_{r}+
	                            +	0.58533\rho_{r}^{2}-0.40758\rho_{r}^{3}+0.093324\rho_{r}^{4},<m>")

            IObj?.Paragraphs.Add("where")

            IObj?.Paragraphs.Add("<mi>\eta,\eta_{0}</mi> Corrected viscosity / Lucas method calculated 
                                viscosity <mi>(\mu P)</mi>")

            IObj?.Paragraphs.Add("<mi>T_{c},P_{c}</mi> Component critical properties")

            IObj?.Paragraphs.Add("<mi>\rho_{r}</mi> Reduced density, <mi>\rho/\rho_{c}=V_{c}/V</mi>")

            IObj?.Paragraphs.Add("`MM` Molecular weight (kg/kmol)")

            IObj?.Paragraphs.Add("If the vapor phase contains more than a component, the viscosity 
                                is calculated by the same procedure, but with the required 
                                properties calculated by a molar average.")

            Dim val As Double = 0.0#
            Dim sval As Double = 0

            For Each subst As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(2).Compounds.Values
                IObj?.Paragraphs.Add(String.Format("Calculating viscosity for {0}... (xi = {1})", subst.Name, subst.MoleFraction.GetValueOrDefault))
                sval = Me.AUX_VAPVISCi(subst.ConstantProperties, T)
                IObj?.Paragraphs.Add(String.Format("Calculated value: {0} Pa.s", sval))
                val += subst.MoleFraction.GetValueOrDefault * sval
            Next

            val = Auxiliary.PROPS.viscg_jossi_stiel_thodos(val, T, MM / RHO / 1000, AUX_TCM(Phase.Vapor), AUX_PCM(Phase.Vapor), AUX_VCM(Phase.Vapor), AUX_MMM(Phase.Vapor))

            IObj?.Paragraphs.Add("<h2>Results</h2>")

            IObj?.Paragraphs.Add(String.Format("Molar Average Value: {0} Pa.s", val))

            IObj?.Close()

            Return val

        End Function

        Public Overridable Function AUX_VAPVISCi(ByVal cprop As Interfaces.ICompoundConstantProperties, ByVal T As Double) As Double

            Dim val As Double

            If cprop.VaporViscosityEquation <> "" And cprop.VaporViscosityEquation <> "0" And Not cprop.IsIon And Not cprop.IsSalt Then
                If Integer.TryParse(cprop.VaporViscosityEquation, New Integer) Then
                    val = CalcCSTDepProp(cprop.VaporViscosityEquation, cprop.Vapor_Viscosity_Const_A, cprop.Vapor_Viscosity_Const_B, cprop.Vapor_Viscosity_Const_C, cprop.Vapor_Viscosity_Const_D, cprop.Vapor_Viscosity_Const_E, T, cprop.Critical_Temperature)
                Else
                    val = ParseEquation(cprop.VaporViscosityEquation, cprop.Vapor_Viscosity_Const_A, cprop.Vapor_Viscosity_Const_B, cprop.Vapor_Viscosity_Const_C, cprop.Vapor_Viscosity_Const_D, cprop.Vapor_Viscosity_Const_E, T)
                End If
            ElseIf cprop.IsIon Or cprop.IsSalt Then
                val = 0.0#
            Else
                If cprop.Critical_Temperature > 0.0# Then val = Auxiliary.PROPS.viscg_lucas(T, cprop.Critical_Temperature, cprop.Critical_Pressure, cprop.Acentric_Factor, cprop.Molar_Weight) Else val = 0.0#
            End If

            Return val

        End Function

        Public Overridable Function AUX_SOLIDDENS() As Double

            Dim val As Double
            Dim subst As Interfaces.ICompound
            Dim zerodens As Double = 0
            Dim db As String
            Dim T As Double = Me.CurrentMaterialStream.Phases(0).Properties.temperature.GetValueOrDefault

            For Each subst In Me.CurrentMaterialStream.Phases(7).Compounds.Values
                db = subst.ConstantProperties.OriginalDB
                If db = "ChemSep" Or (db = "User" And subst.ConstantProperties.SolidDensityEquation <> "") Then
                    Dim A, B, C, D, E, result As Double
                    Dim eqno As String = subst.ConstantProperties.SolidDensityEquation
                    Dim mw As Double = subst.ConstantProperties.Molar_Weight
                    A = subst.ConstantProperties.Solid_Density_Const_A
                    B = subst.ConstantProperties.Solid_Density_Const_B
                    C = subst.ConstantProperties.Solid_Density_Const_C
                    D = subst.ConstantProperties.Solid_Density_Const_D
                    E = subst.ConstantProperties.Solid_Density_Const_E
                    If eqno <> "" Then
                        result = CalcCSTDepProp(eqno, A, B, C, D, E, T, 0) 'kmol/m3
                    End If
                    If eqno = "" OrElse result = 0.0 Then
                        zerodens += subst.MassFraction.GetValueOrDefault
                    Else
                        val += subst.MassFraction.GetValueOrDefault * 1 / (result * mw)
                    End If
                ElseIf db = "ChEDL Thermo" And subst.ConstantProperties.SolidDensityEquation <> "" Then
                    Dim A, B, C, D, E, result As Double
                    Dim eqno As String = subst.ConstantProperties.SolidDensityEquation
                    A = subst.ConstantProperties.Solid_Density_Const_A
                    B = subst.ConstantProperties.Solid_Density_Const_B
                    C = subst.ConstantProperties.Solid_Density_Const_C
                    D = subst.ConstantProperties.Solid_Density_Const_D
                    E = subst.ConstantProperties.Solid_Density_Const_E
                    If eqno <> "" Then
                        result = CalcCSTDepProp(eqno, A, B, C, D, E, T, 0) 'kg/m3
                    End If
                    val += subst.MassFraction.GetValueOrDefault * 1 / (result)
                Else
                    If subst.ConstantProperties.SolidDensityAtTs <> 0.0# Then
                        val += subst.MassFraction.GetValueOrDefault * 1 / subst.ConstantProperties.SolidDensityAtTs
                    Else
                        zerodens += subst.MassFraction.GetValueOrDefault
                    End If
                End If
            Next

            Return 1 / val / (1 - zerodens)

        End Function

        Public Overridable Function AUX_SOLIDDENSi(cprop As Interfaces.ICompoundConstantProperties, T As Double) As Double

            Dim val As Double
            Dim zerodens As Double = 0

            If cprop.OriginalDB = "ChemSep" Or (cprop.OriginalDB = "User" And cprop.SolidDensityEquation <> "") Then
                Dim A, B, C, D, E, result As Double
                Dim eqno As String = cprop.SolidDensityEquation
                Dim mw As Double = cprop.Molar_Weight
                A = cprop.Solid_Density_Const_A
                B = cprop.Solid_Density_Const_B
                C = cprop.Solid_Density_Const_C
                D = cprop.Solid_Density_Const_D
                E = cprop.Solid_Density_Const_E
                If eqno <> "" Then result = CalcCSTDepProp(eqno, A, B, C, D, E, T, 0) 'kmol/m3
                val = 1 / (result * mw)
            ElseIf cprop.OriginalDB = "ChEDL Thermo" Then
                Dim A, B, C, D, E, result As Double
                Dim eqno As String = cprop.SolidDensityEquation
                A = cprop.Solid_Density_Const_A
                B = cprop.Solid_Density_Const_B
                C = cprop.Solid_Density_Const_C
                D = cprop.Solid_Density_Const_D
                E = cprop.Solid_Density_Const_E
                If eqno <> "" Then result = result = CalcCSTDepProp(eqno, A, B, C, D, E, T, 0) 'kg/m3
                val = 1 / (result)
            Else
                If cprop.SolidDensityAtTs <> 0.0# Then
                    val = 1 / cprop.SolidDensityAtTs
                Else
                    val = 1.0E+20
                End If
            End If

            Return 1 / val

        End Function

        Public Overridable Function AUX_SolidHeatCapacity(ByVal cprop As Interfaces.ICompoundConstantProperties, ByVal T As Double) As Double

            Dim val As Double

            If cprop.OriginalDB = "ChemSep" Or (cprop.OriginalDB = "User" And cprop.SolidHeatCapacityEquation <> "") Then
                Dim A, B, C, D, E, result As Double
                Dim eqno As String = cprop.SolidHeatCapacityEquation
                Dim mw As Double = cprop.Molar_Weight
                A = cprop.Solid_Heat_Capacity_Const_A
                B = cprop.Solid_Heat_Capacity_Const_B
                C = cprop.Solid_Heat_Capacity_Const_C
                D = cprop.Solid_Heat_Capacity_Const_D
                E = cprop.Solid_Heat_Capacity_Const_E
                result = CalcCSTDepProp(eqno, A, B, C, D, E, T, 0) 'J/kmol/K
                val = result / 1000 / mw 'kJ/kg.K
            ElseIf cprop.OriginalDB = "ChEDL Thermo" Then
                Dim A, B, C, D, E As Double
                Dim eqno As String = cprop.SolidHeatCapacityEquation
                A = cprop.Solid_Heat_Capacity_Const_A
                B = cprop.Solid_Heat_Capacity_Const_B
                C = cprop.Solid_Heat_Capacity_Const_C
                D = cprop.Solid_Heat_Capacity_Const_D
                E = cprop.Solid_Heat_Capacity_Const_E
                val = CalcCSTDepProp(eqno, A, B, C, D, E, T, 0) 'kJ/kg/K
            Else
                val = 3 ' replacement if no params available
            End If

            Return val

        End Function

        Public Function AUX_SOLIDCP(ByVal Vxm As Array, ByVal cprops As List(Of Interfaces.ICompoundConstantProperties), ByVal T As Double) As Double

            Dim n As Integer = UBound(Vxm)
            Dim val As Double = 0
            For i As Integer = 0 To n
                val += Vxm(i) * AUX_SolidHeatCapacity(cprops(i), T)
            Next
            Return val

        End Function

        Public MustOverride Function AUX_Z(Vx As Double(), T As Double, P As Double, state As PhaseName) As Double Implements IPropertyPackage.AUX_Z

        Public Overridable Function AUX_LIQDENS(ByVal T As Double, Optional ByVal P As Double = 0, Optional ByVal Pvp As Double = 0, Optional ByVal phaseid As Integer = 3, Optional ByVal FORCE_EOS As Boolean = False) As Double

            Return AUX_LIQDENS(T, RET_VMOL(RET_PHASECODE(phaseid)), P, Pvp, False)

        End Function

        Public Overridable Function AUX_LIQDENS(ByVal T As Double, ByVal Vx As Array, Optional ByVal P As Double = 0, Optional ByVal Pvp As Double = 0, Optional ByVal FORCE_EOS As Boolean = False) As Double

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "AUX_LIQDENS", "Liquid Phase Density", "Liquid Phase Density Calculation Routine")

            IObj?.SetCurrent()

            IObj?.Paragraphs.Add("Liquid phase density is calculated with the Rackett equation for 
                                non-EOS models when experimental data is not available,")

            IObj?.Paragraphs.Add("<m>V_{s}=\frac{RT_{C}}{P_{C}}Z_{RA}^{[1+(1-T_{r})^{2/7}]},</m>")

            IObj?.Paragraphs.Add("where:")

            IObj?.Paragraphs.Add("<mi>V_{s}</mi> Saturated molar volume (m³/mol)")

            IObj?.Paragraphs.Add("<mi>T_{c}</mi> Critical temperature (K)")

            IObj?.Paragraphs.Add("<mi>P_{c}</mi> Critical pressure (Pa)")

            IObj?.Paragraphs.Add("<mi>T_{r}</mi> Reduced temperature")

            IObj?.Paragraphs.Add("<mi>Z_{RA}</mi> Rackett constant of the component/mixture")

            IObj?.Paragraphs.Add("<mi>R</mi> Ideal Gas constant (8.314 J/[mol.K])")

            IObj?.Paragraphs.Add("If <mi>T\geq T_{cm}</mi>, the Rackett method does not provide a value 
                                for <mi>V_{s}</mi> and, in this case, DWSIM uses the EOS-generated 
                                compressibility factor to calculate the density of the liquid phase.")

            IObj?.Paragraphs.Add("For mixtures, the equation becomes")

            IObj?.Paragraphs.Add("<m>V_{s}=R(\sum\frac{x_{i}T_{c_{i}}}{P_{c_{i}}})Z_{RA}^{[1+(1-T_{r})^{2/7}]},</m>")

            IObj?.Paragraphs.Add("with <mi>T_{r}=T/T_{cm}</mi>, and")

            IObj?.Paragraphs.Add("<m>T_{c_{m}}=\sum\sum\phi_{i}\phi_{j}T_{c_{ij}},</m>")

            IObj?.Paragraphs.Add("<m>\phi_{i}=\frac{x_{i}V_{c_{i}}}{\sum x_{i}V_{c_{i}}},</m>")

            IObj?.Paragraphs.Add("<m>T_{c_{ij}}=[\frac{8(V_{c_{i}}V_{c_{j}})^{1/2}}{(V_{c_{i}}^{1/3}+V_{c_{j}}^{1/3})^{3}}](T_{c_{i}}T_{c_{j}})^{1/2},</m>")

            IObj?.Paragraphs.Add("where:")

            IObj?.Paragraphs.Add("<mi>x_{i}</mi> Molar fraction")

            IObj?.Paragraphs.Add("<mi>V_{c_{i}}</mi> Critical volume (m³/mol)")

            IObj?.Paragraphs.Add("If <mi>Z_{RA}</mi> isn't available, it is calculated from the component 
                                acentric factor,")

            IObj?.Paragraphs.Add("<mi>Z_{RA}=0.2956-0.08775\omega,</mi>")

            IObj?.Paragraphs.Add("If the component (or mixture) isn't saturated, a correction is 
                                applied in order to account for the effect of pressure in the volume,")

            IObj?.Paragraphs.Add("<mi>V=V_{s}[1-(0.0861488+0.0344483\omega)\ln\frac{\beta+P}{\beta+P_{vp}}],</mi>")

            IObj?.Paragraphs.Add("with")

            IObj?.Paragraphs.Add("<mi>\frac{\beta}{P}	=	-1-9.070217(1-T_{r})^{1/3}+62.45326(1-T_{r})^{2/3}-135.1102(1-T_{r})+
		                        +\exp(4.79594+0.250047\omega+1.14188\omega^{2})(1-T_{r})^{4/3},</mi>")

            IObj?.Paragraphs.Add("where:")

            IObj?.Paragraphs.Add("<mi>V</mi> Compressed liquid volume (m³/mol)")

            IObj?.Paragraphs.Add("<mi>P</mi> Pressure (Pa)")

            IObj?.Paragraphs.Add("<mi>P_{vp}</mi> Vapor pressure / Bubble point pressure (Pa)")

            IObj?.Paragraphs.Add("Finally, density is calculated from the molar volume by the 
                                following relation:")

            IObj?.Paragraphs.Add("<m>\rho=\frac{MM}{1000V},<m>")

            IObj?.Paragraphs.Add("where:")

            IObj?.Paragraphs.Add("<mi>\rho</mi> Density (kg/m³)")

            IObj?.Paragraphs.Add("<mi>V</mi> Specific volume of the fluid (m³/mol)")

            IObj?.Paragraphs.Add("<mi>MM</mi> Liquid phase molecular volume (kg/kmol)")

            IObj?.Paragraphs.Add("<h2>Calculations</h2>")

            Dim val As Double


            If LiquidDensityCalculationMode_Subcritical = LiquidDensityCalcMode.EOS Then
                If T / RET_VTC.MultiplyY(Vx).SumY > 1 Then
                    IObj?.Paragraphs.Add("Temperature is supercritical.")
                End If
                IObj?.Paragraphs.Add("Using EOS to calculate compressibility factor -> density.")
                IObj?.SetCurrent()
                val = AUX_Z(Vx, T, P, PhaseName.Liquid)
                val = (8.314 * val * T / P)
                val = 1 / val * Me.AUX_MMM(Vx) / 1000
            ElseIf LiquidDensityCalculationMode_Subcritical = LiquidDensityCalcMode.Rackett Then
                val = props.liq_dens_rackett(T, props.Tcm(Vx, RET_VTC()), props.Pcm(Vx, RET_VPC()), props.wm(Vx, RET_VW()), AUX_MMM(Vx), props.Zcm(Vx, RET_VZRa()), P, AUX_PVAPM(T))
                IObj?.Paragraphs.Add(String.Format("Value estimated with Rackett correlation: {0} kg/m3", val))
            ElseIf LiquidDensityCalculationMode_Subcritical = LiquidDensityCalcMode.COSTALD Then
                If P <= 101325 Then
                    val = props.liq_dens_COSTALD(Vx, T, RET_VCSTLDCV(), RET_VTC(), RET_VCSTLDAF()) 'm3/kmol
                Else
                    val = props.liq_dens_COSTALD_COMPR(Vx, T, RET_VCSTLDCV(), RET_VTC(), RET_VCSTLDAF(), P) 'm3/kmol
                End If
                val = AUX_MMM(Vx) / val 'kg/m3
                IObj?.Paragraphs.Add(String.Format("Value estimated with COSTALD correlation: {0} kg/m3", val))
            Else
                Dim vk(Me.CurrentMaterialStream.Phases(0).Compounds.Count - 1) As Double
                Dim i As Integer
                i = 0
                For Each subst As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(1).Compounds.Values
                    IObj?.SetCurrent()
                    IObj?.Paragraphs.Add(String.Format("Calculating value for {0}... (xi = {1}, wi = {2})", subst.Name, subst.MoleFraction.GetValueOrDefault, subst.MassFraction.GetValueOrDefault))
                    vk(i) = AUX_LIQDENSi(subst, T)
                    IObj?.Paragraphs.Add(String.Format("Value calculated from experimental curve: {0} kg/m3", vk(i)))
                    If LiquidDensity_CorrectExpDataForPressure Then
                        'pressure correction
                        Dim pcorr = Auxiliary.PROPS.liq_dens_pcorrection(T / subst.ConstantProperties.Critical_Temperature, P, subst.ConstantProperties.Critical_Pressure, AUX_PVAPi(subst.Name, T), subst.ConstantProperties.Acentric_Factor)
                        IObj?.Paragraphs.Add(String.Format("Compressed Liquid Density Correction Factor: {0}", pcorr))
                        vk(i) *= pcorr
                        IObj?.Paragraphs.Add(String.Format("Corrected Liquid Density: {0} kg/m3", vk(i)))
                    End If
                    If T > subst.ConstantProperties.Critical_Temperature Then
                        vk(i) = 1.0E+20
                    End If
                    If Not Double.IsNaN(vk(i)) Then vk(i) = Vx(i) / vk(i) Else vk(i) = 0.0#
                    i = i + 1
                Next
                val = 1 / MathEx.Common.Sum(vk)
            End If

            IObj?.Paragraphs.Add("<h2>Results</h2>")

            IObj?.Paragraphs.Add(String.Format("Liquid Phase Density: {0} kg/m3", val))

            IObj?.Close()

            Return val 'kg/m3

        End Function

        Public Overridable Function AUX_LIQDENSi(ByVal subst As Interfaces.ICompound, ByVal T As Double) As Double

            Return AUX_LIQDENSi(subst.ConstantProperties, T)

        End Function

        Public Function AUX_LIQDENSi(idx As Integer, T As Double) As Double

            Return AUX_LIQDENSi(Me.CurrentMaterialStream.Phases(0).Compounds.Values.ToArray().ElementAt(idx), T)

        End Function

        Public Overridable Function AUX_LIQDENSi(ByVal cprop As Interfaces.ICompoundConstantProperties, ByVal T As Double) As Double

            Dim val As Double

            If cprop.LiquidDensityEquation <> "" And cprop.LiquidDensityEquation <> "0" And Not cprop.IsIon And Not cprop.IsSalt Then
                If Integer.TryParse(cprop.LiquidDensityEquation, New Integer) Then
                    val = CalcCSTDepProp(cprop.LiquidDensityEquation, cprop.Liquid_Density_Const_A, cprop.Liquid_Density_Const_B, cprop.Liquid_Density_Const_C, cprop.Liquid_Density_Const_D, cprop.Liquid_Density_Const_E, T, cprop.Critical_Temperature)
                Else
                    val = ParseEquation(cprop.LiquidDensityEquation, cprop.Liquid_Density_Const_A, cprop.Liquid_Density_Const_B, cprop.Liquid_Density_Const_C, cprop.Liquid_Density_Const_D, cprop.Liquid_Density_Const_E, T)
                End If
                If cprop.OriginalDB <> "CoolProp" And cprop.OriginalDB <> "User" And cprop.OriginalDB <> "ChEDL Thermo" Then val = cprop.Molar_Weight * val
            Else
                val = Auxiliary.PROPS.liq_dens_rackett(T, cprop.Critical_Temperature, cprop.Critical_Pressure, cprop.Acentric_Factor, cprop.Molar_Weight, cprop.Z_Rackett)
            End If

            Return val 'kg/m3

        End Function

        Public Function AUX_LIQCPm(ByVal T As Double, ByVal phaseid As Integer) As Double

            Dim val As Double
            Dim subst As Interfaces.ICompound

            val = 0
            For Each subst In Me.CurrentMaterialStream.Phases(phaseid).Compounds.Values
                val += subst.MassFraction.GetValueOrDefault * Me.AUX_LIQ_Cpi(subst.ConstantProperties, T)
            Next

            Return val

        End Function

        Public Overridable Function AUX_LIQ_Cpi(ByVal cprop As Interfaces.ICompoundConstantProperties, ByVal T As Double) As Double

            Dim val As Double
            If T >= cprop.Critical_Temperature Then
                'surrogate for supercritical gases solved in liquid
                val = CalcCSTDepProp(cprop.IdealgasCpEquation, cprop.Ideal_Gas_Heat_Capacity_Const_A, cprop.Ideal_Gas_Heat_Capacity_Const_B, cprop.Ideal_Gas_Heat_Capacity_Const_C, cprop.Ideal_Gas_Heat_Capacity_Const_D, cprop.Ideal_Gas_Heat_Capacity_Const_E, T, cprop.Critical_Temperature)
                If cprop.OriginalDB <> "CoolProp" Then val = val / 1000 / cprop.Molar_Weight 'kJ/kg.K
            Else
                If cprop.LiquidHeatCapacityEquation <> "" And cprop.LiquidHeatCapacityEquation <> "0" And Not cprop.IsIon And Not cprop.IsSalt Then
                    If Integer.TryParse(cprop.LiquidHeatCapacityEquation, New Integer) Then
                        val = CalcCSTDepProp(cprop.LiquidHeatCapacityEquation, cprop.Liquid_Heat_Capacity_Const_A, cprop.Liquid_Heat_Capacity_Const_B, cprop.Liquid_Heat_Capacity_Const_C, cprop.Liquid_Heat_Capacity_Const_D, cprop.Liquid_Heat_Capacity_Const_E, T, cprop.Critical_Temperature)
                        If cprop.OriginalDB <> "CoolProp" And cprop.OriginalDB <> "ChEDL Thermo" Then
                            val = val / 1000 / cprop.Molar_Weight 'kJ/kg.K
                        End If
                    Else
                        val = ParseEquation(cprop.LiquidHeatCapacityEquation, cprop.Liquid_Heat_Capacity_Const_A, cprop.Liquid_Heat_Capacity_Const_B, cprop.Liquid_Heat_Capacity_Const_C, cprop.Liquid_Heat_Capacity_Const_D, cprop.Liquid_Heat_Capacity_Const_E, T) / cprop.Molar_Weight
                    End If
                Else
                    'estimate using Rownlinson/Bondi correlation
                    val = Auxiliary.PROPS.Cpl_rb(AUX_CPi(cprop.Name, T), T, cprop.Critical_Temperature, cprop.Acentric_Factor, cprop.Molar_Weight) 'kJ/kg.K
                End If
            End If

            Return val

        End Function

        Public MustOverride Function AUX_VAPDENS(ByVal T As Double, ByVal P As Double) As Double

        Public Overridable Function AUX_INT_CPDTi(ByVal T1 As Double, ByVal T2 As Double, ByVal subst As String) As Double

            Dim nsteps As Integer = Math.Abs(T2 - T1) / 10

            If nsteps > 100 Then nsteps = 100

            If nsteps < 10 Then
                If Math.Abs(T2 - T1) < 1 Then
                    nsteps = 2
                ElseIf Math.Abs(T2 - T1) < 3 Then
                    nsteps = 4
                ElseIf Math.Abs(T2 - T1) < 5 Then
                    nsteps = 6
                Else
                    nsteps = 10
                End If
            End If

            Dim deltaT As Double = (T2 - T1) / nsteps

            Dim Ti As Double

            Ti = T1 + deltaT / 2

            Dim integrals(nsteps - 1) As Double

            For i = 0 To nsteps - 1
                integrals(i) = AUX_CPi(subst, Ti + i * deltaT)
            Next

            Dim outval = integrals.Sum * deltaT

            Return outval

        End Function

        Public Overridable Function AUX_INT_CPDTi(ByVal T1 As Double, ByVal T2 As Double, ByVal ID As Integer) As Double

            Return AUX_INT_CPDTi(T1, T2, CompoundPropCache(ID).Name)

        End Function

        Public Function AUX_INT_CPDTi(ByVal T1 As Double, ByVal T2 As Double, ByVal subst As ICompoundConstantProperties) As Double

            Dim nsteps As Integer = Math.Abs(T2 - T1) / 10

            If nsteps < 10 Then
                If Math.Abs(T2 - T1) < 1 Then
                    nsteps = 2
                ElseIf Math.Abs(T2 - T1) < 3 Then
                    nsteps = 4
                ElseIf Math.Abs(T2 - T1) < 5 Then
                    nsteps = 6
                Else
                    nsteps = 10
                End If
            End If

            Dim deltaT As Double = (T2 - T1) / nsteps

            Dim Ti As Double

            Ti = T1 + deltaT / 2

            Dim integrals(nsteps - 1) As Double

            For i = 0 To nsteps - 1
                integrals(i) = AUX_CPi(subst, Ti + i * deltaT)
            Next

            Dim outval = integrals.Sum * deltaT

            Return outval

        End Function


        Public Function AUX_INT_CPDTi_L(ByVal T1 As Double, ByVal T2 As Double, ByVal subst As String) As Double

            Dim nsteps As Integer = Math.Abs(T2 - T1) / 10

            If nsteps < 10 Then
                If Math.Abs(T2 - T1) < 1 Then
                    nsteps = 2
                ElseIf Math.Abs(T2 - T1) < 3 Then
                    nsteps = 4
                ElseIf Math.Abs(T2 - T1) < 5 Then
                    nsteps = 6
                Else
                    nsteps = 10
                End If
            End If

            Dim deltaT As Double = (T2 - T1) / nsteps

            Dim Ti As Double

            Ti = T1 + deltaT / 2

            Dim integrals(nsteps - 1) As Double

            Dim cprops = DW_GetConstantProperties().Where(Function(c) c.Name = subst).SingleOrDefault()

            For i = 0 To nsteps - 1
                integrals(i) = AUX_LIQ_Cpi(cprops, Ti + i * deltaT)
            Next

            Dim outval = integrals.Sum * deltaT

            Return outval

        End Function

        Public Function AUX_INT_CPDTi_L(ByVal T1 As Double, ByVal T2 As Double, ByVal index As Integer) As Double

            Dim nsteps As Integer = Math.Abs(T2 - T1) / 10

            If nsteps < 10 Then
                If Math.Abs(T2 - T1) < 1 Then
                    nsteps = 2
                ElseIf Math.Abs(T2 - T1) < 3 Then
                    nsteps = 4
                ElseIf Math.Abs(T2 - T1) < 5 Then
                    nsteps = 6
                Else
                    nsteps = 10
                End If
            End If

            Dim deltaT As Double = (T2 - T1) / nsteps

            Dim Ti As Double

            Ti = T1 + deltaT / 2

            Dim integrals(nsteps - 1) As Double

            Dim cprops = DW_GetConstantProperties()

            For i = 0 To nsteps - 1
                integrals(i) = AUX_LIQ_Cpi(cprops(index), Ti + i * deltaT)
            Next

            Dim outval = integrals.Sum * deltaT

            Return outval

        End Function


        Public Function AUX_INT_CPDT_Ti(ByVal T1 As Double, ByVal T2 As Double, ByVal subst As String) As Double

            Dim nsteps As Integer = Math.Abs(T2 - T1) / 10

            If nsteps < 10 Then
                If Math.Abs(T2 - T1) < 1 Then
                    nsteps = 2
                ElseIf Math.Abs(T2 - T1) < 3 Then
                    nsteps = 4
                ElseIf Math.Abs(T2 - T1) < 5 Then
                    nsteps = 6
                Else
                    nsteps = 10
                End If
            End If

            Dim deltaT As Double = (T2 - T1) / nsteps

            Dim Ti As Double

            Ti = T1 + deltaT / 2

            Dim integrals(nsteps - 1) As Double

            For i = 0 To nsteps - 1
                integrals(i) = AUX_CPi(subst, Ti + i * deltaT) / (Ti + i * deltaT)
            Next

            Dim outval = integrals.SumY * deltaT

            Return outval

        End Function

        Public Overridable Function AUX_INT_CPDTm(ByVal T1 As Double, ByVal T2 As Double, ByVal Phase As Phase) As Double

            Dim val As Double = 0
            Dim subst As Interfaces.ICompound

            Select Case Phase

                Case PropertyPackages.Phase.Mixture

                    For Each subst In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                        If subst.MoleFraction.GetValueOrDefault <> 0.0# And subst.MassFraction.GetValueOrDefault = 0.0# Then
                            subst.MassFraction = Me.AUX_CONVERT_MOL_TO_MASS(subst.Name, Me.RET_PHASEID(Phase))
                        End If
                        val += subst.MassFraction.GetValueOrDefault * Me.AUX_INT_CPDTi(T1, T2, subst.Name)
                    Next

                Case PropertyPackages.Phase.Liquid

                    For Each subst In Me.CurrentMaterialStream.Phases(1).Compounds.Values
                        If subst.MoleFraction.GetValueOrDefault <> 0.0# And subst.MassFraction.GetValueOrDefault = 0.0# Then
                            subst.MassFraction = Me.AUX_CONVERT_MOL_TO_MASS(subst.Name, Me.RET_PHASEID(Phase))
                        End If
                        val += subst.MassFraction.GetValueOrDefault * Me.AUX_INT_CPDTi(T1, T2, subst.Name)
                    Next

                Case PropertyPackages.Phase.Liquid1

                    For Each subst In Me.CurrentMaterialStream.Phases(3).Compounds.Values
                        If subst.MoleFraction.GetValueOrDefault <> 0.0# And subst.MassFraction.GetValueOrDefault = 0.0# Then
                            subst.MassFraction = Me.AUX_CONVERT_MOL_TO_MASS(subst.Name, Me.RET_PHASEID(Phase))
                        End If
                        val += subst.MassFraction.GetValueOrDefault * Me.AUX_INT_CPDTi(T1, T2, subst.Name)
                    Next

                Case PropertyPackages.Phase.Liquid2

                    For Each subst In Me.CurrentMaterialStream.Phases(4).Compounds.Values
                        If subst.MoleFraction.GetValueOrDefault <> 0.0# And subst.MassFraction.GetValueOrDefault = 0.0# Then
                            subst.MassFraction = Me.AUX_CONVERT_MOL_TO_MASS(subst.Name, Me.RET_PHASEID(Phase))
                        End If
                        val += subst.MassFraction.GetValueOrDefault * Me.AUX_INT_CPDTi(T1, T2, subst.Name)
                    Next

                Case PropertyPackages.Phase.Liquid3

                    For Each subst In Me.CurrentMaterialStream.Phases(5).Compounds.Values
                        If subst.MoleFraction.GetValueOrDefault <> 0.0# And subst.MassFraction.GetValueOrDefault = 0.0# Then
                            subst.MassFraction = Me.AUX_CONVERT_MOL_TO_MASS(subst.Name, Me.RET_PHASEID(Phase))
                        End If
                        val += subst.MassFraction.GetValueOrDefault * Me.AUX_INT_CPDTi(T1, T2, subst.Name)
                    Next

                Case PropertyPackages.Phase.Vapor

                    For Each subst In Me.CurrentMaterialStream.Phases(2).Compounds.Values
                        If subst.MoleFraction.GetValueOrDefault <> 0.0# And subst.MassFraction.GetValueOrDefault = 0.0# Then
                            subst.MassFraction = Me.AUX_CONVERT_MOL_TO_MASS(subst.Name, Me.RET_PHASEID(Phase))
                        End If
                        val += subst.MassFraction.GetValueOrDefault * Me.AUX_INT_CPDTi(T1, T2, subst.Name)
                    Next

            End Select

            Return val

        End Function

        Public Overridable Function AUX_INT_CPDT_Tm(ByVal T1 As Double, ByVal T2 As Double, ByVal Phase As Phase) As Double

            Dim val As Double
            Dim subst As Interfaces.ICompound

            For Each subst In Me.CurrentMaterialStream.Phases(Me.RET_PHASEID(Phase)).Compounds.Values
                If subst.MoleFraction.GetValueOrDefault <> 0.0# And subst.MassFraction.GetValueOrDefault = 0.0# Then
                    subst.MassFraction = Me.AUX_CONVERT_MOL_TO_MASS(subst.Name, Me.RET_PHASEID(Phase))
                End If
                val += subst.MassFraction.GetValueOrDefault * Me.AUX_INT_CPDT_Ti(T1, T2, subst.Name)
            Next

            Return val

        End Function

        Public Function AUX_DELGFM_T(Vx() As Double, T As Double) As Double

            Dim val As Double
            Dim subst As Interfaces.ICompound
            Dim i As Integer = 0
            For Each subst In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                val += Vx(i) * AUX_DELGF_T(298.15, T, subst.Name, False)
                i += 1
            Next

            Return val

        End Function

        Public Function AUX_DELGF_T(ByVal T1 As Double, ByVal T2 As Double, ByVal id As String, Optional ByVal mode2 As Boolean = False) As Double

            Dim dA As Double = 0
            Dim dB As Double = 0
            Dim dC As Double = 0
            Dim dD As Double = 0
            Dim dE As Double = 0
            Dim dHr As Double = 0
            Dim dGr As Double = 0
            Dim term3 As Double = 0

            Dim int1, int2 As Double
            Dim R = 8.314

            With Me.CurrentMaterialStream.Phases(0).Compounds(id).ConstantProperties

                dHr = .IG_Enthalpy_of_Formation_25C
                dGr = .IG_Gibbs_Energy_of_Formation_25C

                If mode2 Then
                    If .IsIon Or .IsSalt Then
                        term3 = .Electrolyte_Cp0 * 1000 / .Molar_Weight * (Log(T2 / T1) + (T1 / T2) - 1) / R
                    Else
                        int1 = Me.AUX_INT_CPDTi(T1, T2, id)
                        int2 = Me.AUX_INT_CPDT_Ti(T1, T2, id)
                        term3 = int1 / (R * T2) - int2 / R
                    End If
                Else
                    int1 = Me.AUX_INT_CPDTi(T1, T2, id)
                    int2 = Me.AUX_INT_CPDT_Ti(T1, T2, id)
                    term3 = int1 / (R * T2) - int2 / R
                End If


            End With

            Dim result As Double

            If mode2 Then
                result = dGr / (R * T1) + dHr / R * (1 / T2 - 1 / T1) + term3
            Else
                result = (dGr - dHr) / (R * T1) + dHr / (R * T2) + term3
            End If

            Return result

        End Function

        Public Function AUX_DELGig_RT(ByVal T1 As Double, ByVal T2 As Double, ByVal ID() As String, ByVal stcoeff() As Double, ByVal baseID As Integer, Optional ByVal mode2 As Boolean = False) As Double Implements Interfaces.IPropertyPackage.AUX_DELGig_RT

            Dim n As Integer = ID.Length

            Dim int1(n - 1), int2(n - 1), sint1, sint2, dgfT(n - 1), sumdgft As Double

            Dim dHr As Double = 0
            Dim dGr As Double = 0

            sint1 = 0
            sint2 = 0

            With Me.CurrentMaterialStream

                Dim i As Integer = 0
                sumdgft = 0
                Do
                    dHr += stcoeff(i) * .Phases(0).Compounds(ID(i)).ConstantProperties.IG_Enthalpy_of_Formation_25C * .Phases(0).Compounds(ID(i)).ConstantProperties.Molar_Weight
                    dGr += stcoeff(i) * .Phases(0).Compounds(ID(i)).ConstantProperties.IG_Gibbs_Energy_of_Formation_25C * .Phases(0).Compounds(ID(i)).ConstantProperties.Molar_Weight
                    int1(i) = stcoeff(i) * Me.AUX_INT_CPDTi(T1, T2, ID(i)) / stcoeff(baseID) * .Phases(0).Compounds(ID(i)).ConstantProperties.Molar_Weight
                    int2(i) = stcoeff(i) * Me.AUX_INT_CPDT_Ti(T1, T2, ID(i)) / stcoeff(baseID) * .Phases(0).Compounds(ID(i)).ConstantProperties.Molar_Weight
                    sint1 += int1(i)
                    sint2 += int2(i)
                    dgfT(i) = stcoeff(i) * Me.AUX_DELGF_T(T1, T2, ID(i), mode2) * .Phases(0).Compounds(ID(i)).ConstantProperties.Molar_Weight
                    sumdgft += dgfT(i) '/ stcoeff(baseID)
                    i = i + 1
                Loop Until i = n

            End With
            dHr /= Abs(stcoeff(baseID))
            dGr /= Abs(stcoeff(baseID))

            Dim R = 8.314

            Dim result = (dGr - dHr) / (R * T1) + dHr / (R * T2) + sint1 / (R * T2) - sint2 / R

            Return sumdgft

        End Function

        Public Function AUX_DELHig_RT(ByVal T1 As Double, ByVal T2 As Double, ByVal ID() As String, ByVal stcoeff() As Double, ByVal baseID As Integer) As Double

            Dim n As Integer = ID.Length

            Dim int1(n - 1), sint1 As Double

            Dim dHr As Double = 0

            sint1 = 0

            With Me.CurrentMaterialStream

                Dim i As Integer = 0
                Do
                    dHr += stcoeff(i) * .Phases(0).Compounds(ID(i)).ConstantProperties.IG_Enthalpy_of_Formation_25C * .Phases(0).Compounds(ID(i)).ConstantProperties.Molar_Weight
                    int1(i) = stcoeff(i) * Me.AUX_INT_CPDTi(T1, T2, ID(i)) / Abs(stcoeff(baseID)) * .Phases(0).Compounds(ID(i)).ConstantProperties.Molar_Weight
                    sint1 += int1(i)
                    i = i + 1
                Loop Until i = n

            End With
            dHr /= Abs(stcoeff(baseID))

            Dim result = dHr + sint1

            Return result / (8.314 * T2)

        End Function

        Public Overridable Function AUX_CONVERT_MOL_TO_MASS(ByVal subst As String, ByVal phasenumber As Integer) As Double

            Dim mol_x_mm As Double
            Dim sub1 As Interfaces.ICompound
            For Each sub1 In Me.CurrentMaterialStream.Phases(phasenumber).Compounds.Values
                mol_x_mm += sub1.MoleFraction.GetValueOrDefault * sub1.ConstantProperties.Molar_Weight
            Next

            sub1 = Me.CurrentMaterialStream.Phases(phasenumber).Compounds(subst)
            If mol_x_mm <> 0.0# Then
                Return sub1.MoleFraction.GetValueOrDefault * sub1.ConstantProperties.Molar_Weight / mol_x_mm
            Else
                Return 0.0#
            End If

        End Function

        Public Overridable Function AUX_CONVERT_MASS_TO_MOL(ByVal subst As String, ByVal phasenumber As Integer) As Double

            Dim mass_div_mm As Double
            Dim sub1 As Interfaces.ICompound
            For Each sub1 In Me.CurrentMaterialStream.Phases(phasenumber).Compounds.Values
                mass_div_mm += sub1.MassFraction.GetValueOrDefault / sub1.ConstantProperties.Molar_Weight
            Next

            sub1 = Me.CurrentMaterialStream.Phases(phasenumber).Compounds(subst)
            Return sub1.MassFraction.GetValueOrDefault / sub1.ConstantProperties.Molar_Weight / mass_div_mm

        End Function

        Public Overridable Function DW_CalcSolidEnthalpy(ByVal T As Double, ByVal Vx As Double(), cprops As List(Of Interfaces.ICompoundConstantProperties)) As Double

            Dim sh As Double = DW_CalcEnthalpy(Vx, T, 101325, State.Solid)

            If sh = 0.0# Then

                Return CalcSolidEnthalpyFromCp(T, Vx, cprops)

            Else

                Return sh

            End If

        End Function

        Public Function CalcSolidEnthalpyFromCp(ByVal T As Double, ByVal Vx As Double(), cprops As List(Of Interfaces.ICompoundConstantProperties)) As Double

            Dim n As Integer = Vx.Length - 1
            Dim i As Integer
            Dim HS As Double = 0.0#
            Dim Cpi As Double
            Dim VMF() As Double

            VMF = RET_VMM().MultiplyY(Vx).NormalizeY 'calculate mass fractions

            For i = 0 To n
                If Vx(i) > 0 Then
                    If cprops(i).OriginalDB = "ChemSep" Or cprops(i).OriginalDB = "User" Then
                        Dim A, B, C, D, E As Double
                        Dim eqno As String = cprops(i).SolidHeatCapacityEquation
                        Dim mw As Double = cprops(i).Molar_Weight
                        A = cprops(i).Solid_Heat_Capacity_Const_A
                        B = cprops(i).Solid_Heat_Capacity_Const_B
                        C = cprops(i).Solid_Heat_Capacity_Const_C
                        D = cprops(i).Solid_Heat_Capacity_Const_D
                        E = cprops(i).Solid_Heat_Capacity_Const_E
                        '<SolidHeatCapacityCp name="Solid heat capacity"  units="J/kmol/K" >
                        Cpi = CalcCSTDepProp(eqno, A, B, C, D, E, T, 0) / 1000 / mw 'kJ/kg.K
                        If cprops(i).TemperatureOfFusion < 298.15 Then
                            HS += VMF(i) * Me.AUX_INT_CPDTi_L(298.15, cprops(i).TemperatureOfFusion, cprops(i).Name)
                            HS -= VMF(i) * cprops(i).EnthalpyOfFusionAtTf * 1000 / mw
                            HS -= VMF(i) * Cpi * (cprops(i).TemperatureOfFusion - T)
                        Else
                            HS -= VMF(i) * cprops(i).EnthalpyOfFusionAtTf * 1000 / mw
                            HS -= VMF(i) * Cpi * (298.15 - T)
                        End If
                    ElseIf cprops(i).OriginalDB = "ChEDL Thermo" Then
                        Dim A, B, C, D, E As Double
                        Dim mw As Double = cprops(i).Molar_Weight
                        Dim eqno As String = cprops(i).SolidHeatCapacityEquation
                        A = cprops(i).Solid_Heat_Capacity_Const_A
                        B = cprops(i).Solid_Heat_Capacity_Const_B
                        C = cprops(i).Solid_Heat_Capacity_Const_C
                        D = cprops(i).Solid_Heat_Capacity_Const_D
                        E = cprops(i).Solid_Heat_Capacity_Const_E
                        Cpi = CalcCSTDepProp(eqno, A, B, C, D, E, T, 0) 'kJ/kg.K
                        If cprops(i).TemperatureOfFusion < 298.15 Then
                            HS += VMF(i) * Me.AUX_INT_CPDTi_L(298.15, cprops(i).TemperatureOfFusion, cprops(i).Name)
                            HS -= VMF(i) * cprops(i).EnthalpyOfFusionAtTf * 1000 / mw
                            HS -= VMF(i) * Cpi * (cprops(i).TemperatureOfFusion - T)
                        Else
                            HS -= VMF(i) * cprops(i).EnthalpyOfFusionAtTf * 1000 / mw
                            HS -= VMF(i) * Cpi * (298.15 - T)
                        End If

                    ElseIf cprops(i).TemperatureOfFusion <> 0.0# Then
                        HS += -VMF(i) * cprops(i).EnthalpyOfFusionAtTf * 1000 / cprops(i).Molar_Weight
                    End If
                End If
            Next

            Return HS 'kJ/kg

        End Function

        Public Function DW_CalcSolidFugCoeff(T As Double, P As Double) As Double()

            Dim Tf = RET_VTF()
            Dim Hf = RET_VHF()

            Dim names = RET_VNAMES()

            If ForcedSolids Is Nothing Then ForcedSolids = New List(Of String)

            Dim i, n As Integer

            n = Tf.Length - 1

            Dim phis(n), cterm(n), Pvaps(n), Pvapl(n) As Double

            For i = 0 To n
                cterm(i) = Math.Exp(Hf(i) * 1000 / (8.314 * Tf(i)) * (Tf(i) / T - 1))
                Pvapl(i) = AUX_PVAPi(i, T)
                Pvaps(i) = Math.Exp(Math.Log(Pvapl(i)) - Hf(i) * 1000 / 8.314 * (1 / T - 1 / Tf(i)))
            Next

            For i = 0 To n
                If ForcedSolids.Contains(names(i)) Then
                    If Pvaps(i) > 0 Then
                        phis(i) = Pvaps(i) / P
                    Else
                        phis(i) = 0.000000000000001
                    End If
                Else
                    If T > Tf(i) Then
                        phis(i) = 10000000000.0 * Pvaps(i) / P
                    Else
                        phis(i) = Pvaps(i) / P
                    End If
                End If
                If Double.IsNaN(phis(i)) Or Double.IsInfinity(phis(i)) Then phis(i) = Double.MaxValue
            Next

            Return phis

        End Function

        Public Overridable Function DW_CalcSolidHeatCapacityCp(ByVal T As Double, ByVal Vx As Double(), cprops As List(Of Interfaces.ICompoundConstantProperties)) As Double

            Dim n As Integer = Vx.Length - 1
            Dim i As Integer
            Dim Cp As Double = 0.0#
            Dim Cpi As Double
            Dim VMF() As Double

            VMF = RET_VMM().MultiplyY(Vx).NormalizeY 'calculate mass fractions

            For i = 0 To n
                If Vx(i) > 0 Then
                    If cprops(i).OriginalDB = "ChemSep" Or cprops(i).OriginalDB = "User" Then
                        Dim A, B, C, D, E As Double
                        Dim eqno As String = cprops(i).SolidHeatCapacityEquation
                        Dim mw As Double = cprops(i).Molar_Weight
                        A = cprops(i).Solid_Heat_Capacity_Const_A
                        B = cprops(i).Solid_Heat_Capacity_Const_B
                        C = cprops(i).Solid_Heat_Capacity_Const_C
                        D = cprops(i).Solid_Heat_Capacity_Const_D
                        E = cprops(i).Solid_Heat_Capacity_Const_E
                        '<SolidHeatCapacityCp name="Solid heat capacity"  units="J/kmol/K" >
                        Cpi = CalcCSTDepProp(eqno, A, B, C, D, E, T, 0) / 1000 / mw 'kJ/kg.K
                        Cp += VMF(i) * Cpi
                    ElseIf cprops(i).OriginalDB = "ChEDL Thermo" Then
                        Dim A, B, C, D, E As Double
                        Dim eqno As String = cprops(i).SolidHeatCapacityEquation
                        A = cprops(i).Solid_Heat_Capacity_Const_A
                        B = cprops(i).Solid_Heat_Capacity_Const_B
                        C = cprops(i).Solid_Heat_Capacity_Const_C
                        D = cprops(i).Solid_Heat_Capacity_Const_D
                        E = cprops(i).Solid_Heat_Capacity_Const_E
                        Cpi = CalcCSTDepProp(eqno, A, B, C, D, E, T, 0) 'kJ/kg.K
                        Cp += VMF(i) * Cpi
                    End If
                End If
            Next

            Return Cp 'kJ/kg.K

        End Function

        Public Overridable Function RET_VMOL(ByVal Phase As Phase) As Double()

            Return CurrentMaterialStream.Phases(Me.RET_PHASEID(Phase)).Compounds.Values.Select(Function(x) x.MoleFraction.GetValueOrDefault).ToArray

        End Function

        Public Overridable Function RET_VMM() As Double()

            Return CurrentMaterialStream.Phases(0).Compounds.Values.Select(Function(x) x.ConstantProperties.Molar_Weight).ToArray

        End Function

        Public Overridable Function RET_VMAS(ByVal Phase As Phase) As Double()

            Dim val(Me.CurrentMaterialStream.Phases(0).Compounds.Count - 1) As Double
            Dim subst As Interfaces.ICompound
            Dim i As Integer = 0
            Dim sum As Double = 0.0#

            For Each subst In Me.CurrentMaterialStream.Phases(Me.RET_PHASEID(Phase)).Compounds.Values
                val(i) = subst.MassFraction.GetValueOrDefault
                sum += val(i)
                i += 1
            Next

            If sum <> 0.0# Then
                Return val
            Else
                Return AUX_CONVERT_MOL_TO_MASS(RET_VMOL(Phase))
            End If

        End Function

        Public Overridable Function RET_VTC() As Double()

            Return CurrentMaterialStream.Phases(0).Compounds.Values.Select(Function(x) x.ConstantProperties.Critical_Temperature).ToArray

        End Function

        Public Overridable Function RET_VTF() As Double()

            Return CurrentMaterialStream.Phases(0).Compounds.Values.Select(Function(x) x.ConstantProperties.TemperatureOfFusion).ToArray

        End Function

        Public Overridable Function RET_VHF() As Double()

            Return CurrentMaterialStream.Phases(0).Compounds.Values.Select(Function(x) x.ConstantProperties.EnthalpyOfFusionAtTf).ToArray

        End Function

        Public Overridable Function RET_VDHF() As Double()

            Return CurrentMaterialStream.Phases(0).Compounds.Values.Select(Function(x) x.ConstantProperties.IG_Enthalpy_of_Formation_25C).ToArray

        End Function

        Public Overridable Function RET_VTB() As Double()

            Return CurrentMaterialStream.Phases(0).Compounds.Values.Select(Function(x) x.ConstantProperties.Normal_Boiling_Point).ToArray

        End Function

        Public Overridable Function RET_VPC() As Double()

            Return CurrentMaterialStream.Phases(0).Compounds.Values.Select(Function(x) x.ConstantProperties.Critical_Pressure).ToArray

        End Function

        Public Overridable Function RET_VZC() As Double()

            Return CurrentMaterialStream.Phases(0).Compounds.Values.Select(Function(x) x.ConstantProperties.Critical_Compressibility).ToArray

        End Function

        Public Overridable Function RET_VZRa() As Double()

            Return CurrentMaterialStream.Phases(0).Compounds.Values.Select(Function(x) x.ConstantProperties.Z_Rackett).ToArray

        End Function

        Public Overridable Function RET_VVC() As Double()

            Dim vc, val(Me.CurrentMaterialStream.Phases(0).Compounds.Count - 1) As Double
            Dim subst As Interfaces.ICompound
            Dim i As Integer = 0

            For Each subst In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                vc = subst.ConstantProperties.Critical_Volume
                If subst.ConstantProperties.Critical_Volume = 0.0# Then
                    vc = Auxiliary.PROPS.Vc(subst.ConstantProperties.Critical_Temperature, subst.ConstantProperties.Critical_Pressure, subst.ConstantProperties.Acentric_Factor)
                End If
                val(i) = vc
                i += 1
            Next

            Return val

        End Function

        Public Overridable Function RET_VW() As Double()

            Return CurrentMaterialStream.Phases(0).Compounds.Values.Select(Function(x) x.ConstantProperties.Acentric_Factor).ToArray

        End Function

        Public Overridable Function RET_VCP(ByVal T As Double) As Double()

            Dim val(Me.CurrentMaterialStream.Phases(0).Compounds.Count - 1) As Double
            Dim subst As Interfaces.ICompound
            Dim i As Integer = 0

            For Each subst In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                val(i) = Me.AUX_CPi(subst.Name, T)
                i += 1
            Next

            Return val

        End Function

        Public Overridable Function RET_VCSTLDCV() As Double()

            Dim val(Me.CurrentMaterialStream.Phases(0).Compounds.Count - 1) As Double
            Dim subst As Interfaces.ICompound
            Dim i As Integer = 0

            For Each subst In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                val(i) = subst.ConstantProperties.COSTALD_Characteristic_Volume
                If val(i) = 0.0 Then val(i) = subst.ConstantProperties.Critical_Volume
                i += 1
            Next

            Return val

        End Function

        Public Overridable Function RET_VCSTLDAF() As Double()

            Dim val(Me.CurrentMaterialStream.Phases(0).Compounds.Count - 1) As Double
            Dim subst As Interfaces.ICompound
            Dim i As Integer = 0

            For Each subst In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                val(i) = subst.ConstantProperties.COSTALD_SRK_Acentric_Factor
                If val(i) = 0.0 Then val(i) = subst.ConstantProperties.Acentric_Factor
                i += 1
            Next

            Return val

        End Function

        Public Overridable Function RET_VHVAP(ByVal T As Double) As Double()

            Dim val(Me.CurrentMaterialStream.Phases(0).Compounds.Count - 1) As Double
            Dim subst As Interfaces.ICompound
            Dim i As Integer = 0

            For Each subst In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                val(i) = Me.AUX_HVAPi(subst.Name, T)
                i += 1
            Next

            Return val

        End Function

        Public Function RET_Hid(ByVal T1 As Double, ByVal T2 As Double, ByVal Phase As Phase) As Double

            Dim val As Double
            'Dim subst As Interfaces.ICompound

            Dim phaseID As Integer

            If Phase = PropertyPackages.Phase.Liquid Then phaseID = 1
            If Phase = PropertyPackages.Phase.Liquid1 Then phaseID = 3
            If Phase = PropertyPackages.Phase.Liquid2 Then phaseID = 4
            If Phase = PropertyPackages.Phase.Liquid3 Then phaseID = 5
            If Phase = PropertyPackages.Phase.Vapor Then phaseID = 2
            If Phase = PropertyPackages.Phase.Mixture Then phaseID = 0

            'For Each subst In Me.CurrentMaterialStream.Phases(phaseID).Compounds.Values
            '    val += subst.MassFraction.GetValueOrDefault * subst.ConstantProperties.Enthalpy_of_Formation_25C
            'Next

            Return Me.AUX_INT_CPDTm(T1, T2, Phase) + val

        End Function

        Public Function RET_Hid_FromLiqCp(Vz As Double(), T As Double, P As Double) As Double

            Dim H, Tb, t1, t2, t3 As Double

            Dim props = DW_GetConstantProperties()

            Dim Vw = AUX_CONVERT_MOL_TO_MASS(Vz)

            t1 = 0
            t2 = 0
            t3 = 0

            For i As Integer = 0 To Vz.Length - 1
                Tb = props(i).Normal_Boiling_Point
                If Tb = 0.0 Then Tb = 0.7 * props(i).Critical_Temperature
                If Tb = 0.0 Then Throw New Exception("Unable to calculate Enthalpy from Liquid Cp data - Normal Boiling Point / Critical Temperature not defined")
                If Vz(i) > 0.0 Then
                    If T > Tb Then
                        t1 += Vw(i) * AUX_INT_CPDTi_L(298.15, Tb, i) + P / 1000 / Me.AUX_LIQDENS(Tb, Vz, P)
                        t2 += Vw(i) * AUX_HVAPi(props(i).Name, Tb)
                        t3 += Vw(i) * RET_Hid_i(Tb, T, props(i).Name)
                    Else
                        t1 += Vw(i) * AUX_INT_CPDTi_L(298.15, T, i) + P / 1000 / Me.AUX_LIQDENS(T, Vz, P)
                        t2 += Vw(i) * AUX_HVAPi(props(i).Name, T)
                        t3 += Vw(i) * RET_Hid_i(298.15, T, props(i).Name)
                    End If
                End If
            Next

            H = t1 + t2 + t3

            Return H

        End Function

        Public Function RET_Sid_FromLiqCp(Vz As Double(), T As Double, P As Double) As Double

            Dim S, Tb, t1, t2, t3 As Double

            Dim props = DW_GetConstantProperties()

            Dim Vw = AUX_CONVERT_MOL_TO_MASS(Vz)

            t1 = 0
            t2 = 0
            t3 = 0

            For i As Integer = 0 To Vz.Length - 1
                Tb = props(i).Normal_Boiling_Point
                If Tb = 0.0 Then Tb = 0.7 * props(i).Critical_Temperature
                If Tb = 0.0 Then Throw New Exception("Unable to calculate Entropy from Liquid Cp data - Normal Boiling Point / Critical Temperature not defined")
                If Vz(i) > 0.0 Then
                    If T > Tb Then
                        t1 += Vw(i) * AUX_INT_CPDTi_L(298.15, Tb, i) / T + P / 1000 / Me.AUX_LIQDENS(Tb, Vz, P) / T
                        t2 += Vw(i) * AUX_HVAPi(props(i).Name, Tb) / T
                        t3 += Vw(i) * RET_Sid_i(Tb, T, P, props(i).Name)
                    Else
                        t1 += Vw(i) * AUX_INT_CPDTi_L(298.15, T, i) / T + P / 1000 / Me.AUX_LIQDENS(T, Vz, P) / T
                        t2 += Vw(i) * AUX_HVAPi(props(i).Name, T) / T
                        t3 += Vw(i) * RET_Sid_i(298.15, T, P, props(i).Name)
                    End If
                End If
            Next

            S = t1 + t2 + t3

            Return S

        End Function

        Public Function RET_Hid(ByVal T1 As Double, ByVal T2 As Double, ByVal Vz As Double()) As Double

            Return Me.AUX_INT_CPDTm(T1, T2, Me.AUX_CONVERT_MOL_TO_MASS(Vz))

        End Function

        Public Function RET_Hid_L(ByVal T1 As Double, ByVal T2 As Double, ByVal Vz As Double()) As Double

            Return Me.AUX_INT_CPDTm_L(T1, T2, Me.AUX_CONVERT_MOL_TO_MASS(Vz))

        End Function

        Public Function RET_Sid_L(ByVal T1 As Double, ByVal T2 As Double, ByVal Vz As Double()) As Double

            Return Me.RET_Hid_L(T1, T2, Vz) / T2

        End Function

        Public Function RET_Hid_i(ByVal T1 As Double, ByVal T2 As Double, ByVal id As String) As Double

            Return Me.AUX_INT_CPDTi(T1, T2, id)

        End Function

        Public Function RET_Hid_i_L(ByVal T1 As Double, ByVal T2 As Double, ByVal id As String) As Double

            Return Me.AUX_INT_CPDTi_L(T1, T2, id)

        End Function

        Public Function RET_Sid(ByVal T1 As Double, ByVal T2 As Double, ByVal P2 As Double, ByVal Phase As Phase) As Double

            Dim val As Double
            Dim subst As Interfaces.ICompound

            Dim phaseID As Integer

            If Phase = PropertyPackages.Phase.Liquid Then phaseID = 1
            If Phase = PropertyPackages.Phase.Liquid1 Then phaseID = 3
            If Phase = PropertyPackages.Phase.Liquid2 Then phaseID = 4
            If Phase = PropertyPackages.Phase.Liquid3 Then phaseID = 5
            If Phase = PropertyPackages.Phase.Vapor Then phaseID = 2
            If Phase = PropertyPackages.Phase.Mixture Then phaseID = 0

            For Each subst In Me.CurrentMaterialStream.Phases(phaseID).Compounds.Values
                If subst.MoleFraction.GetValueOrDefault <> 0 Then val += -8.314 * subst.MoleFraction.GetValueOrDefault * Math.Log(subst.MoleFraction.GetValueOrDefault) / subst.ConstantProperties.Molar_Weight
            Next

            Dim tmp = Me.AUX_INT_CPDT_Tm(T1, T2, Phase) + val - 8.314 * Math.Log(P2 / 101325) / Me.AUX_MMM(Phase)

            Return tmp

        End Function

        Public Function RET_Sid(ByVal T1 As Double, ByVal T2 As Double, ByVal P2 As Double, ByVal Vz As Double()) As Double

            Dim val As Double
            Dim i As Integer = 0

            val = 0.0
            For Each subst In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                If Vz(i) <> 0.0 Then val += -8.314 * Vz(i) * Math.Log(Vz(i)) / subst.ConstantProperties.Molar_Weight
                i = i + 1
            Next
            Dim tmp1 = 8.314 * Math.Log(P2 / 101325) / Me.AUX_MMM(Vz)
            Dim tmp2 = Me.AUX_INT_CPDT_Tm(T1, T2, Me.AUX_CONVERT_MOL_TO_MASS(Vz))
            Return tmp2 + val - tmp1

        End Function

        Public Function RET_Sid_i(ByVal T1 As Double, ByVal T2 As Double, ByVal P2 As Double, ByVal id As String) As Double

            Dim val As Double

            Dim tmp1 = 8.314 * Math.Log(P2 / 101325) / Me.CurrentMaterialStream.Phases(0).Compounds(id).ConstantProperties.Molar_Weight
            Dim tmp2 = Me.AUX_INT_CPDT_Ti(T1, T2, id)
            Return tmp2 + val - tmp1

        End Function

        Public Function RET_Gid(ByVal T1 As Double, ByVal T2 As Double, ByVal P2 As Double, ByVal Vz As Double()) As Double

            Dim hid = Me.RET_Hid(T1, T2, Vz)
            Dim sid = Me.RET_Sid(T1, T2, P2, Vz)

            Return hid - T2 * sid

        End Function

        Public Function RET_Gid_i(ByVal T1 As Double, ByVal T2 As Double, ByVal P2 As Double, ByVal id As String) As Double

            Dim hid = Me.RET_Hid_i(T1, T2, id)
            Dim sid = Me.RET_Sid_i(T1, T2, P2, id)

            Return hid - T2 * sid

        End Function

        Public Function RET_Gid(ByVal T1 As Double, ByVal T2 As Double, ByVal P2 As Double, ByVal Phase As Phase) As Double

            Dim hid = Me.RET_Hid(T1, T2, Phase)
            Dim sid = Me.RET_Sid(T1, T2, P2, Phase)

            Return hid - T2 * sid

        End Function

        Public Overridable Function RET_VKij() As Double(,)

            If dummyipvec Is Nothing Then
                Dim dv(Me.CurrentMaterialStream.Phases(0).Compounds.Count - 1, Me.CurrentMaterialStream.Phases(0).Compounds.Count - 1) As Double
                dummyipvec = dv
                Return dv
            Else
                If dummyipvec.GetLength(0) <> Me.CurrentMaterialStream.Phases(0).Compounds.Count Then
                    Dim dv(Me.CurrentMaterialStream.Phases(0).Compounds.Count - 1, Me.CurrentMaterialStream.Phases(0).Compounds.Count - 1) As Double
                    dummyipvec = dv
                    Return dv
                Else
                    Return dummyipvec
                End If
            End If

        End Function

        Public Function RET_VCSACIDS()

            Dim val(Me.CurrentMaterialStream.Phases(0).Compounds.Count - 1) As String
            Dim subst As Interfaces.ICompound
            Dim i As Integer = 0

            For Each subst In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                val(i) = subst.ConstantProperties.COSMODBName
                If val(i) = "" Then val(i) = subst.ConstantProperties.CAS_Number
                i += 1
            Next

            Return val

        End Function

        Public Overridable Function RET_VIDS()

            Dim val(Me.CurrentMaterialStream.Phases(0).Compounds.Count - 1) As String
            Dim subst As Interfaces.ICompound
            Dim i As Integer = 0

            For Each subst In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                val(i) = subst.ConstantProperties.ID
                i += 1
            Next

            Return val

        End Function

        Public Overridable Function RET_VCAS() As String()

            Dim val(Me.CurrentMaterialStream.Phases(0).Compounds.Count - 1) As String
            Dim subst As Interfaces.ICompound
            Dim i As Integer = 0

            For Each subst In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                val(i) = subst.ConstantProperties.CAS_Number
                i += 1
            Next

            Return val

        End Function

        Public Function RET_VNAMES2(order As Interfaces.Enums.CompoundOrdering) As String()

            Dim val(Me.CurrentMaterialStream.Phases(0).Compounds.Count - 1) As String
            Dim subst As Interfaces.ICompound
            Dim i As Integer = 0

            Select Case order
                Case CompoundOrdering.CAS_ASC
                    For Each subst In Me.CurrentMaterialStream.Phases(0).Compounds.Values.OrderBy(Function(c) c.ConstantProperties.CAS_Number)
                        val(i) = subst.ConstantProperties.Name
                        i += 1
                    Next
                Case CompoundOrdering.CAS_DESC
                    For Each subst In Me.CurrentMaterialStream.Phases(0).Compounds.Values.OrderByDescending(Function(c) c.ConstantProperties.CAS_Number)
                        val(i) = subst.ConstantProperties.Name
                        i += 1
                    Next
                Case CompoundOrdering.MW_ASC
                    For Each subst In Me.CurrentMaterialStream.Phases(0).Compounds.Values.OrderBy(Function(c) c.ConstantProperties.Molar_Weight)
                        val(i) = subst.ConstantProperties.Name
                        i += 1
                    Next
                Case CompoundOrdering.MW_DESC
                    For Each subst In Me.CurrentMaterialStream.Phases(0).Compounds.Values.OrderByDescending(Function(c) c.ConstantProperties.Molar_Weight)
                        val(i) = subst.ConstantProperties.Name
                        i += 1
                    Next
                Case CompoundOrdering.Name_ASC
                    For Each subst In Me.CurrentMaterialStream.Phases(0).Compounds.Values.OrderBy(Function(c) c.ConstantProperties.Name)
                        val(i) = subst.ConstantProperties.Name
                        i += 1
                    Next
                Case CompoundOrdering.Name_DESC
                    For Each subst In Me.CurrentMaterialStream.Phases(0).Compounds.Values.OrderByDescending(Function(c) c.ConstantProperties.Name)
                        val(i) = subst.ConstantProperties.Name
                        i += 1
                    Next
                Case CompoundOrdering.NBP_ASC
                    For Each subst In Me.CurrentMaterialStream.Phases(0).Compounds.Values.OrderBy(Function(c) c.ConstantProperties.Normal_Boiling_Point)
                        val(i) = subst.ConstantProperties.Name
                        i += 1
                    Next
                Case CompoundOrdering.NBP_DESC
                    For Each subst In Me.CurrentMaterialStream.Phases(0).Compounds.Values.OrderByDescending(Function(c) c.ConstantProperties.Normal_Boiling_Point)
                        val(i) = subst.ConstantProperties.Name
                        i += 1
                    Next
                Case CompoundOrdering.TAG_ASC
                    For Each subst In Me.CurrentMaterialStream.Phases(0).Compounds.Values.OrderBy(Function(c) c.ConstantProperties.Tag)
                        val(i) = subst.ConstantProperties.Name
                        i += 1
                    Next
                Case CompoundOrdering.TAG_DESC
                    For Each subst In Me.CurrentMaterialStream.Phases(0).Compounds.Values.OrderByDescending(Function(c) c.ConstantProperties.Tag)
                        val(i) = subst.ConstantProperties.Name
                        i += 1
                    Next
                Case Else
                    For Each subst In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                        val(i) = subst.ConstantProperties.Name
                        i += 1
                    Next
            End Select



            Return val

        End Function

        Public Overridable Function RET_VNAMES() As String()

            Dim val(Me.CurrentMaterialStream.Phases(0).Compounds.Count - 1) As String
            Dim subst As Interfaces.ICompound
            Dim i As Integer = 0

            For Each subst In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                val(i) = subst.ConstantProperties.Name
                i += 1
            Next

            Return val

        End Function

        Public Function RET_NullVector() As Double()

            Dim val(Me.CurrentMaterialStream.Phases(0).Compounds.Count - 1) As Double
            Dim subst As Interfaces.ICompound
            Dim i As Integer = 0
            For Each subst In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                val(i) = 0.0#
                i += 1
            Next
            Return val

        End Function

        Public Function RET_UnitaryVector() As Double()

            Dim val(Me.CurrentMaterialStream.Phases(0).Compounds.Count - 1) As Double
            Dim subst As Interfaces.ICompound
            Dim i As Integer = 0
            For Each subst In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                val(i) = 1.0#
                i += 1
            Next
            Return val

        End Function

        Public Function RET_PHASECODE(ByVal phaseID As Integer) As PropertyPackages.Phase
            Select Case phaseID
                Case 0
                    Return Phase.Mixture
                Case 1
                    Return Phase.Liquid
                Case 2
                    Return Phase.Vapor
                Case 3
                    Return Phase.Liquid1
                Case 4
                    Return Phase.Liquid2
                Case 5
                    Return Phase.Liquid3
                Case 6
                    Return Phase.Aqueous
                Case 7
                    Return Phase.Solid
            End Select
        End Function

        Public Function RET_PHASEID(ByVal phasecode As PropertyPackages.Phase) As Integer
            Select Case phasecode
                Case Phase.Mixture
                    Return 0
                Case Phase.Liquid
                    Return 1
                Case Phase.Vapor
                    Return 2
                Case Phase.Liquid1
                    Return 3
                Case Phase.Liquid2
                    Return 4
                Case Phase.Liquid3
                    Return 5
                Case Phase.Aqueous
                    Return 6
                Case Phase.Solid
                    Return 7
                Case Else
                    Return 0
            End Select
        End Function

        Public Overridable Function AUX_MMM(ByVal Vz() As Double, Optional ByVal state As String = "") As Double

            Dim val As Double
            Dim subst As Interfaces.ICompound

            Dim i As Integer = 0

            For Each subst In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                val += Vz(i) * subst.ConstantProperties.Molar_Weight
                i += 1
            Next

            Return val

        End Function

        Public Function AUX_NORMALIZE(ByVal Vx() As Double) As Double()

            Dim sum As Double = 0
            Dim i, n As Integer

            n = Vx.Length - 1

            Dim Vxnew(n) As Double

            For i = 0 To n
                sum += Vx(i)
            Next

            For i = 0 To n
                Vxnew(i) = Vx(i) / sum
                If Double.IsNaN(Vxnew(i)) Then Vxnew(i) = 0.0#
            Next

            Return Vxnew

        End Function

        Public Function AUX_ERASE(ByVal Vx As Double()) As Double()

            Dim i, n As Integer

            n = Vx.Length - 1

            Dim Vx2(n) As Double

            For i = 0 To n
                Vx2(i) = 0
            Next

            Return Vx2

        End Function

        Public Function AUX_SINGLECOMPIDX(ByVal Vx As Object) As Integer

            Dim i, n As Integer

            n = Vx.Length - 1

            For i = 0 To n
                If Vx(i) <> 0 Then Return i
            Next

            Return -1

        End Function

        Public Function AUX_SINGLECOMPIDX(ByVal Phase As Phase) As Integer

            Dim i As Integer

            Dim subst As Interfaces.ICompound

            i = 0
            For Each subst In Me.CurrentMaterialStream.Phases(Me.RET_PHASEID(Phase)).Compounds.Values
                If subst.MoleFraction.GetValueOrDefault <> 0 Then Return i
            Next

            Return -1

        End Function

        Public Function AUX_IS_SINGLECOMP(ByVal Vx As Double()) As Boolean

            Dim i, n As Integer
            Dim sc, bo As Boolean

            n = Vx.Length - 1

            bo = False
            For i = 0 To n
                If Me.DW_GetConstantProperties(i).IsBlackOil Then
                    bo = True
                    Exit For
                End If
            Next
            sc = False
            For i = 0 To n
                If Vx(i) > SingleCompoundCheckThreshold Then
                    sc = True
                    Exit For
                End If
            Next

            If sc And Not bo Then Return True Else Return False

        End Function

        Public Overridable Function AUX_IS_SINGLECOMP(ByVal Phase As Phase) As Boolean

            Dim sc, bo As Boolean

            Dim subst As Interfaces.ICompound

            bo = False
            sc = False
            For Each subst In Me.CurrentMaterialStream.Phases(Me.RET_PHASEID(Phase)).Compounds.Values
                If subst.ConstantProperties.IsBlackOil Then
                    bo = True
                    Exit For
                End If
            Next
            For Each subst In Me.CurrentMaterialStream.Phases(Me.RET_PHASEID(Phase)).Compounds.Values
                If subst.MoleFraction.GetValueOrDefault > SingleCompoundCheckThreshold Then
                    sc = True
                    Exit For
                End If
            Next

            If sc And Not bo Then Return True Else Return False

        End Function

        Public Overridable Function AUX_INT_CPDTm(ByVal T1 As Double, ByVal T2 As Double, ByVal Vw As Double()) As Double

            Dim val As Double
            Dim i As Integer = 0
            Dim subst As Interfaces.ICompound

            val = 0.0#
            For Each subst In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                val += Vw(i) * Me.AUX_INT_CPDTi(T1, T2, subst.ConstantProperties.ID)
                i += 1
            Next

            Return val

        End Function

        Public Function AUX_INT_CPDTm_L(ByVal T1 As Double, ByVal T2 As Double, ByVal Vw As Double()) As Double

            Dim val As Double
            Dim i As Integer = 0
            Dim subst As Interfaces.ICompound
            For Each subst In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                val += Vw(i) * Me.AUX_INT_CPDTi_L(T1, T2, i)
                i += 1
            Next

            Return val

        End Function

        Public Overridable Function AUX_INT_CPDT_Tm(ByVal T1 As Double, ByVal T2 As Double, ByVal Vw As Double()) As Double

            Dim val As Double
            Dim i As Integer = 0
            Dim subst As Interfaces.ICompound
            For Each subst In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                val += Vw(i) * Me.AUX_INT_CPDT_Ti(T1, T2, subst.Name)
                i += 1
            Next

            Return val

        End Function

        Public Overridable Function AUX_CONVERT_MOL_TO_MASS(ByVal Vz As Double()) As Double()

            Dim Vwe(Vz.Length - 1) As Double
            Dim mol_x_mm As Double = 0
            Dim i As Integer = 0
            Dim sub1 As Interfaces.ICompound
            For Each sub1 In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                mol_x_mm += Vz(i) * sub1.ConstantProperties.Molar_Weight
                i += 1
            Next

            i = 0
            For Each sub1 In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                If mol_x_mm <> 0 Then
                    Vwe(i) = Vz(i) * sub1.ConstantProperties.Molar_Weight / mol_x_mm
                Else
                    Vwe(i) = 0.0#
                End If
                i += 1
            Next

            Return Vwe

        End Function

        Public Overridable Function AUX_CONVERT_MASS_TO_MOL(ByVal Vz As Double()) As Double()

            Dim Vw(Vz.Length - 1) As Double
            Dim mass_div_mm As Double
            Dim i As Integer = 0
            Dim sub1 As Interfaces.ICompound
            For Each sub1 In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                mass_div_mm += Vz(i) / sub1.ConstantProperties.Molar_Weight
                i += 1
            Next

            i = 0
            For Each sub1 In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                Vw(i) = Vz(i) / sub1.ConstantProperties.Molar_Weight / mass_div_mm
                i += 1
            Next

            Return Vw

        End Function

        Public Function AUX_CheckTrivial(ByVal Ki As Double(), Optional ByVal tolerance As Double = 0.01) As Boolean

            Dim isTrivial As Boolean = True
            Dim n, i As Integer
            n = Ki.Length - 1

            For i = 0 To n
                If Abs(Ki(i) - 1.0) > tolerance Then
                    isTrivial = False
                    Exit For
                End If
            Next

            Return isTrivial

        End Function

        Public Shared Function CalcCSTDepProp(ByVal eqno As String, ByVal A As Double, ByVal B As Double, ByVal C As Double, ByVal D As Double, ByVal E As Double, ByVal T As Double, ByVal Tc As Double) As Double

            Dim Tr As Double = T / Tc

            If eqno = "" Then Return 0.0

            Dim eq = Integer.Parse(eqno)

            Select Case eq
                Case 1
                    Return A
                Case 2
                    Return A + B * T
                Case 3
                    Return A + B * T + C * T ^ 2
                Case 4
                    Return A + B * T + C * T ^ 2 + D * T ^ 3
                Case 5
                    Return A + B * T + C * T ^ 2 + D * T ^ 3 + E * T ^ 4
                Case 6
                    Return A + B * T + C * T ^ 2 + D * T ^ 3 + E / T ^ 2
                Case 10
                    Return Exp(A - B / (T + C))
                Case 11
                    Return Exp(A)
                Case 12
                    Return Exp(A + B * T)
                Case 13
                    Return Exp(A + B * T + C * T ^ 2)
                Case 14
                    Return Exp(A + B * T + C * T ^ 2 + D * T ^ 3)
                Case 15
                    Return Exp(A + B * T + C * T ^ 2 + D * T ^ 3 + E * T ^ 4)
                Case 16
                    Return A + Exp(B / T + C + D * T + E * T ^ 2)
                Case 17
                    Return A + Exp(B + C * T + D * T ^ 2 + E * T ^ 3)
                Case 45
                    Return A * T + B * T ^ 2 / 2 + C * T ^ 3 / 3 + D * T ^ 4 / 4 + E * T ^ 5 / 5
                Case 75
                    Return B + 2 * C * T + 3 * D * T ^ 2 + 4 * E * T ^ 3
                Case 100
                    Return A + B * T + C * T ^ 2 + D * T ^ 3 + E * T ^ 4
                Case 101
                    Return Exp(A + B / T + C * Log(T) + D * T ^ E)
                Case 102
                    Return A * T ^ B / (1 + C / T + D / T ^ 2)
                Case 103
                    Return A + B * Exp(-C / (T ^ D))
                Case 104
                    Return A + B / T + C / T ^ 2 + D / T ^ 8 + E / T ^ 9
                Case 105
                    Return A / (B ^ (1 + (1 - T / C) ^ D))
                Case 106
                    Return A * (1 - Tr) ^ (B + C * Tr + D * Tr ^ 2 + E * Tr ^ 3)
                Case 107
                    Return A + B * (C / T / Sinh(C / T)) ^ 2 + D * (E / T / Cosh(E / T)) ^ 2
                Case 114
                    Return A * T + B * T ^ 2 / 2 + C * T ^ 3 / 3 + D * T ^ 4 / 4
                Case 115
                    Return Exp(A + B / T + C * Log(T) + D * T ^ 2 + E / T ^ 2)
                Case 116
                    Return A + B * (1 - Tr) ^ 0.35 + C * (1 - Tr) ^ (2 / 3) + D * (1 - Tr) + E * (1 - Tr) ^ (4 / 3)
                Case 117
                    Return A * T + B * (C / T) / Tanh(C / T) - D * (E / T) / Tanh(E / T)
                Case 119
                    Return Exp(A / T + B + C * T + D * T ^ 2 + E * Log(T))
                Case 207
                    Return Exp(A - B / (T + C))
                Case 208
                    Return 10 ^ (A - B / (T + C))
                Case 209
                    Return 10 ^ (A * (1 / T - 1 / B))
                Case 210
                    Return 10 ^ (A + B / T + C * T + D * T ^ 2)
                Case 211
                    Return A * ((B - T) / (B - C)) ^ D
                Case 212
                    Return Exp((E / T) * (A * (1 - T / E) + B * (1 - T / E) ^ 1.5 + C * (1 - T / E) ^ 3 + D * (1 - T / E) ^ 6))
                Case 213
                    Return (E / T) * (A * (1 - T / E) + B * (1 - T / E) ^ 1.5 + C * (1 - T / E) ^ 3 + D * (1 - T / E) ^ 6)
                Case 221
                    Return -B / T ^ 2 + C / T + D * E * T ^ (E - 1)
                Case 230
                    Return -B / T ^ 2 + C / T + D - 2 * E / T ^ 3
                Case 231
                    Return B - C / (T - D) ^ 2
                Case Else
                    Return 0.0#
            End Select

        End Function

        Public Shared Function GetEquationString(ByVal eqno As String) As String

            If eqno = "" Then Return "Not Defined"

            Dim eq = Integer.Parse(eqno)

            Select Case eq
                Case 1
                    Return "A"
                Case 2
                    Return "A + B * T"
                Case 3
                    Return "A + B * T + C * T ^ 2"
                Case 4
                    Return "A + B * T + C * T ^ 2 + D * T ^ 3"
                Case 5
                    Return "A + B * T + C * T ^ 2 + D * T ^ 3 + E * T ^ 4"
                Case 6
                    Return "A + B * T + C * T ^ 2 + D * T ^ 3 + E / T ^ 2"
                Case 10
                    Return "Exp(A - B / (T + C))"
                Case 11
                    Return "Exp(A)"
                Case 12
                    Return "Exp(A + B * T)"
                Case 13
                    Return "Exp(A + B * T + C * T ^ 2)"
                Case 14
                    Return "Exp(A + B * T + C * T ^ 2 + D * T ^ 3)"
                Case 15
                    Return "Exp(A + B * T + C * T ^ 2 + D * T ^ 3 + E * T ^ 4)"
                Case 16
                    Return "A + Exp(B / T + C + D * T + E * T ^ 2)"
                Case 17
                    Return "A + Exp(B + C * T + D * T ^ 2 + E * T ^ 3)"
                Case 45
                    Return "A * T + B * T ^ 2 / 2 + C * T ^ 3 / 3 + D * T ^ 4 / 4 + E * T ^ 5 / 5"
                Case 75
                    Return "B + 2 * C * T + 3 * D * T ^ 2 + 4 * E * T ^ 3"
                Case 100
                    Return "A + B * T + C * T ^ 2 + D * T ^ 3 + E * T ^ 4"
                Case 101
                    Return "Exp(A + B / T + C * Log(T) + D * T ^ E)"
                Case 102
                    Return "A * T ^ B / (1 + C / T + D / T ^ 2)"
                Case 103
                    Return "A + B * Exp(-C / (T ^ D))"
                Case 104
                    Return "A + B / T + C / T ^ 2 + D / T ^ 8 + E / T ^ 9"
                Case 105
                    Return "A / (B ^ (1 + (1 - T / C) ^ D))"
                Case 106
                    Return "A * (1 - Tr) ^ (B + C * Tr + D * Tr ^ 2 + E * Tr ^ 3)"
                Case 107
                    Return "A + B * (C / T / Sinh(C / T)) ^ 2 + D * (E / T / Cosh(E / T)) ^ 2"
                Case 114
                    Return "A * T + B * T ^ 2 / 2 + C * T ^ 3 / 3 + D * T ^ 4 / 4"
                Case 115
                    Return "Exp(A + B / T + C * Log(T) + D * T ^ 2 + E / T ^ 2)"
                Case 116
                    Return "A + B * (1 - Tr) ^ 0.35 + C * (1 - Tr) ^ (2 / 3) + D * (1 - Tr) + E * (1 - Tr) ^ (4 / 3)"
                Case 117
                    Return "A * T + B * (C / T) / Tanh(C / T) - D * (E / T) / Tanh(E / T)"
                Case 119
                    Return "Exp(A / T + B + C * T + D * T ^ 2 + E * Log(T))"
                Case 207
                    Return "Exp(A - B / (T + C))"
                Case 208
                    Return "10 ^ (A - B / (T + C))"
                Case 209
                    Return "10 ^ (A * (1 / T - 1 / B))"
                Case 210
                    Return "10 ^ (A + B / T + C * T + D * T ^ 2)"
                Case 211
                    Return "A * ((B - T) / (B - C)) ^ D"
                Case 212
                    Return "Exp((E / T) * (A * (1 - T / E) + B * (1 - T / E) ^ 1.5 + C * (1 - T / E) ^ 3 + D * (1 - T / E) ^ 6))"
                Case 213
                    Return "(E / T) * (A * (1 - T / E) + B * (1 - T / E) ^ 1.5 + C * (1 - T / E) ^ 3 + D * (1 - T / E) ^ 6)"
                Case 221
                    Return "-B / T ^ 2 + C / T + D * E * T ^ (E - 1)"
                Case 230
                    Return "-B / T ^ 2 + C / T + D - 2 * E / T ^ 3"
                Case 231
                    Return "B - C / (T - D) ^ 2"
                Case Else
                    Return "Not Defined"
            End Select

        End Function


        Public Shared Function ParseEquation(ByVal expression As String, ByVal A As Double, ByVal B As Double, ByVal C As Double, ByVal D As Double, ByVal E As Double, ByVal T As Double) As Double

            If expression = "" Then Return 0.0

            Try

                Dim lterm As String = ""
                Dim rterm As String = ""

                If expression.Contains("=") Then
                    lterm = expression.Split("=")(0).Replace(" ", "").ToLower()
                    rterm = expression.Split("=")(1).TrimEnd(".").ToLower()
                Else
                    rterm = expression.TrimEnd(".").ToLower()
                End If

                Dim numexp As String = ""
                Dim yunit As String = ""
                Dim xunit As String = ""

                If expression.Contains("where") Then
                    numexp = rterm.Split("where")(0).Replace(" ", "").Replace("ln", "log")
                    Dim unit1, unit2 As String
                    unit1 = rterm.Split(New String() {"where"}, StringSplitOptions.RemoveEmptyEntries)(1).Split(New String() {"and", ","}, StringSplitOptions.RemoveEmptyEntries)(0).Trim
                    unit2 = rterm.Split(New String() {"where"}, StringSplitOptions.RemoveEmptyEntries)(1).Split(New String() {"and", ","}, StringSplitOptions.RemoveEmptyEntries)(1).Trim
                    If unit1.Contains("T In") Then
                        xunit = unit1.Split(New String() {"in"}, StringSplitOptions.RemoveEmptyEntries)(1).Trim
                        yunit = unit2.Split(New String() {"in"}, StringSplitOptions.RemoveEmptyEntries)(1).Trim
                    Else
                        xunit = unit2.Split(New String() {"in"}, StringSplitOptions.RemoveEmptyEntries)(1).Trim
                        yunit = unit1.Split(New String() {"in"}, StringSplitOptions.RemoveEmptyEntries)(1).Trim
                    End If
                Else
                    numexp = rterm.Replace(" ", "").Replace("ln", "log")
                End If

                numexp = numexp.Trim(New Char() {vbCrLf, vbCr, vbLf, vbTab})

                If lterm.Contains("ln") Then
                    numexp = "exp(" + numexp + ")"
                End If

                Dim ec = New Ciloci.Flee.ExpressionContext
                ec.Imports.AddType(GetType(System.Math))
                ec.Options.ParseCulture = Globalization.CultureInfo.InvariantCulture

                ec.Variables.Clear()
                ec.Variables.Add("t", cv.ConvertToSI(xunit, T))
                ec.Variables.Add("a", A)
                ec.Variables.Add("b", B)
                ec.Variables.Add("c", C)
                ec.Variables.Add("d", D)
                ec.Variables.Add("k", E)
                ec.Variables.Add("f", 0.0#)
                ec.Variables.Add("g", 0.0#)
                ec.Variables.Add("h", 0.0#)

                Dim result As Double = ec.CompileGeneric(Of Double)(numexp.Replace("e", "k").Replace("kxp", "exp").Trim).Evaluate()

                Return cv.ConvertToSI(yunit, result)

            Catch ex As Exception

                Throw New Exception("Error parsing String For numerical expression: '" + expression + "'. Check temperature-dependent property expressions for the selected compounds and try again.")

            End Try

        End Function

#End Region

#Region "   CAPE-OPEN 1.0 Methods and Properties"

        Private _compdesc, _compname As String

        ''' <summary>
        ''' Gets the name of the component.
        ''' </summary>
        ''' <value></value>
        ''' <returns>CapeString</returns>
        ''' <remarks>Implements CapeOpen.ICapeIdentification.ComponentDescription</remarks>
        Public Overridable Property ComponentDescription() As String Implements CapeOpen.ICapeIdentification.ComponentDescription
            Get
                Return _compdesc
            End Get
            Set(ByVal value As String)
                _compdesc = value
            End Set
        End Property

        ''' <summary>
        ''' Gets the description of the component.
        ''' </summary>
        ''' <value></value>
        ''' <returns>CapeString</returns>
        ''' <remarks>Implements CapeOpen.ICapeIdentification.ComponentName</remarks>
        Public Overridable Property ComponentName() As String Implements CapeOpen.ICapeIdentification.ComponentName
            Get
                Return _compname
            End Get
            Set(ByVal value As String)
                _compname = value
            End Set
        End Property

        ''' <summary>
        ''' Method responsible for calculating/delegating phase equilibria.
        ''' </summary>
        ''' <param name="materialObject">The Material Object</param>
        ''' <param name="flashType">Flash calculation type.</param>
        ''' <param name="props">Properties to be calculated at equilibrium. UNDEFINED for no
        ''' properties. If a list, then the property values should be set for each
        ''' phase present at equilibrium. (not including the overall phase).</param>
        ''' <remarks><para>On the Material Object the CalcEquilibrium method must set the amounts (phaseFraction) and compositions
        ''' (fraction) for all phases present at equilibrium, as well as the temperature and pressure for the overall
        ''' mixture, if these are not set as part of the calculation specifications. The CalcEquilibrium method must not
        ''' set on the Material Object any other value - in particular it must not set any values for phases that do not
        ''' exist. See 5.2.1 for more information.</para>
        ''' <para>The available list of flashes is given in section 5.6.1.</para>
        ''' <para>When calling this method, it is advised not to combine a flash calculation with a property calculation.
        ''' Through the returned error one cannot see which has failed, plus the additional arguments available in a
        ''' CalcProp call (such as calculation type) cannot be specified in a CalcEquilibrium call. Advice is to perform a
        ''' CalcEquilibrium, get the phaseIDs and perform a CalcProp for the existing phases.</para></remarks>
        Public Overridable Sub CalcEquilibrium(ByVal materialObject As Object, ByVal flashType As String, ByVal props As Object) Implements CapeOpen.ICapeThermoPropertyPackage.CalcEquilibrium

            Dim mymat As ICapeThermoMaterial = materialObject
            Me.CurrentMaterialStream = mymat
            Select Case flashType.ToLower
                Case "tp"
                    Me.DW_CalcEquilibrium(FlashSpec.T, FlashSpec.P)
                Case "ph"
                    Me.DW_CalcEquilibrium(FlashSpec.P, FlashSpec.H)
                Case "ps"
                    Me.DW_CalcEquilibrium(FlashSpec.P, FlashSpec.S)
                Case "tvf"
                    Me.DW_CalcEquilibrium(FlashSpec.T, FlashSpec.VAP)
                Case "pvf"
                    Me.DW_CalcEquilibrium(FlashSpec.P, FlashSpec.VAP)
                Case "pt"
                    Me.DW_CalcEquilibrium(FlashSpec.T, FlashSpec.P)
                Case "hp"
                    Me.DW_CalcEquilibrium(FlashSpec.P, FlashSpec.H)
                Case "sp"
                    Me.DW_CalcEquilibrium(FlashSpec.P, FlashSpec.S)
                Case "vft"
                    Me.DW_CalcEquilibrium(FlashSpec.T, FlashSpec.VAP)
                Case "vfp"
                    Me.DW_CalcEquilibrium(FlashSpec.P, FlashSpec.VAP)
                Case Else
                    Throw New CapeOpen.CapeNoImplException()
            End Select

        End Sub

        ''' <summary>
        ''' This method is responsible for doing all calculations on behalf of the Calculation Routine component.
        ''' </summary>
        ''' <param name="materialObject">The Material Object of the calculation.</param>
        ''' <param name="props">The list of properties to be calculated.</param>
        ''' <param name="phases">List of phases for which the properties are to be calculated.</param>
        ''' <param name="calcType">Type of calculation: Mixture Property or Pure Compound Property. For
        ''' partial property, such as fugacity coefficients of compounds in a
        ''' mixture, use “Mixture” CalcType. For pure compound fugacity
        ''' coefficients, use “Pure” CalcType.</param>
        ''' <remarks></remarks>
        Public Overridable Sub CalcProp(ByVal materialObject As Object, ByVal props As Object, ByVal phases As Object, ByVal calcType As String) Implements CapeOpen.ICapeThermoPropertyPackage.CalcProp
            Dim mymat As Interfaces.IMaterialStream = materialObject
            Me.CurrentMaterialStream = mymat
            Dim ph As String() = phases
            For Each f As String In ph
                For Each pi As PhaseInfo In Me.PhaseMappings.Values
                    If f = pi.PhaseLabel Then
                        If Not pi.DWPhaseID = Phase.Solid Then
                            For Each p As String In props
                                Me.DW_CalcProp(p, pi.DWPhaseID)
                            Next
                            Exit For
                        Else
                            Me.DW_CalcSolidPhaseProps()
                        End If
                    End If
                Next
            Next
        End Sub

        ''' <summary>
        ''' Returns the values of the Constant properties of the compounds contained in the passed Material Object.
        ''' </summary>
        ''' <param name="materialObject">The Material Object.</param>
        ''' <param name="props">The list of properties.</param>
        ''' <returns>Compound Constant values.</returns>
        ''' <remarks></remarks>
        Public Overridable Function GetComponentConstant(ByVal materialObject As Object, ByVal props As Object) As Object Implements CapeOpen.ICapeThermoPropertyPackage.GetComponentConstant
            Dim vals As New ArrayList
            Dim mymat As Interfaces.IMaterialStream = materialObject
            For Each c As Interfaces.ICompound In mymat.Phases(0).Compounds.Values
                For Each p As String In props
                    Select Case p.ToLower
                        Case "molecularweight"
                            vals.Add(c.ConstantProperties.Molar_Weight)
                        Case "criticaltemperature"
                            vals.Add(c.ConstantProperties.Critical_Temperature)
                        Case "criticalpressure"
                            vals.Add(c.ConstantProperties.Critical_Pressure)
                        Case "criticalvolume"
                            vals.Add(c.ConstantProperties.Critical_Volume / 1000)
                        Case "criticalcompressibilityfactor"
                            vals.Add(c.ConstantProperties.Critical_Compressibility)
                        Case "acentricfactor"
                            vals.Add(c.ConstantProperties.Acentric_Factor)
                        Case "normalboilingpoint"
                            vals.Add(c.ConstantProperties.Normal_Boiling_Point)
                        Case "idealgasgibbsfreeenergyofformationat25c"
                            vals.Add(c.ConstantProperties.IG_Gibbs_Energy_of_Formation_25C * c.ConstantProperties.Molar_Weight)
                        Case "idealgasenthalpyofformationat25c"
                            vals.Add(c.ConstantProperties.IG_Enthalpy_of_Formation_25C * c.ConstantProperties.Molar_Weight)
                        Case "casregistrynumber"
                            vals.Add(c.ConstantProperties.CAS_Number)
                        Case "chemicalformula"
                            vals.Add(c.ConstantProperties.Formula)
                        Case Else
                            Throw New CapeOpen.CapeNoImplException
                    End Select
                Next
            Next
            Dim arr2(vals.Count) As Double
            Array.Copy(vals.ToArray, arr2, vals.Count)
            Return arr2
        End Function

        ''' <summary>
        ''' Returns the list of compounds of a given Property Package.
        ''' </summary>
        ''' <param name="compIds">List of compound IDs</param>
        ''' <param name="formulae">List of compound formulae</param>
        ''' <param name="names">List of compound names.</param>
        ''' <param name="boilTemps">List of boiling point temperatures.</param>
        ''' <param name="molWt">List of molecular weights.</param>
        ''' <param name="casNo">List of CAS numbers .</param>
        ''' <remarks>Compound identification could be necessary if the PME has internal knowledge of chemical compounds, or in case of
        ''' use of multiple Property Packages. In order to identify the compounds of a Property Package, the PME will use the
        ''' 'casno’ argument instead of the compIds. The reason is that different PMEs may give different names to the same
        ''' chemical compounds, whereas CAS Numbers are universal. Therefore, it is recommended to provide a value for the
        ''' casno argument wherever available.</remarks>
        Public Overridable Sub GetComponentList(ByRef compIds As Object, ByRef formulae As Object, ByRef names As Object, ByRef boilTemps As Object, ByRef molWt As Object, ByRef casNo As Object) Implements CapeOpen.ICapeThermoPropertyPackage.GetComponentList

            Dim ids, formulas, nms, casnos As New List(Of String), bts, molws As New List(Of Double)

            If Settings.CAPEOPENMode Then
                For Each c As Interfaces.ICompoundConstantProperties In _selectedcomps.Values
                    ids.Add(c.Name)
                    formulas.Add(c.Formula)
                    nms.Add(c.Name)
                    bts.Add(c.Normal_Boiling_Point)
                    If c.CAS_Number <> "" Then casnos.Add(c.CAS_Number) Else casnos.Add(c.Name)
                    molws.Add(c.Molar_Weight)
                Next
            Else
                For Each c As BaseClasses.Compound In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                    ids.Add(c.ConstantProperties.Name)
                    formulas.Add(c.ConstantProperties.Formula)
                    nms.Add(c.ConstantProperties.Name)
                    bts.Add(c.ConstantProperties.Normal_Boiling_Point)
                    If c.ConstantProperties.CAS_Number <> "" Then casnos.Add(c.ConstantProperties.CAS_Number) Else casnos.Add(c.ConstantProperties.Name)
                    molws.Add(c.ConstantProperties.Molar_Weight)
                Next
            End If

            compIds = ids.ToArray
            formulae = formulas.ToArray
            names = nms.ToArray
            boilTemps = bts.ToArray
            casNo = casnos.ToArray
            molWt = molws.ToArray

        End Sub

        ''' <summary>
        ''' Provides the list of the supported phases. When supported for one or more property calculations, the Overall
        ''' phase and multiphase identifiers must be returned by this method.
        ''' </summary>
        ''' <returns>The list of phases supported by the Property Package.</returns>
        ''' <remarks></remarks>
        Public Overridable Function GetPhaseList() As Object Implements CapeOpen.ICapeThermoPropertyPackage.GetPhaseList
            Dim pl As New ArrayList
            For Each pi As PhaseInfo In Me.PhaseMappings.Values
                If pi.PhaseLabel <> "Disabled" Then
                    pl.Add(pi.PhaseLabel)
                End If
            Next
            pl.Add("Overall")
            Dim arr(pl.Count - 1) As String
            Array.Copy(pl.ToArray, arr, pl.Count)
            Return arr
        End Function

        ''' <summary>
        ''' Returns list of properties supported by the Property Package.
        ''' </summary>
        ''' <returns>List of all supported Properties.</returns>
        ''' <remarks>GetPropList should return identifiers for the non-constant properties calculated by CalcProp. Standard
        ''' identifiers are listed in 3.10.1. Other non-standard properties that are supported by the Property Package can
        ''' also be returned. GetPropList must not return identifiers for compound constant properties returned by
        ''' GetComponentConstant.
        ''' The properties temperature, pressure, fraction, flow, phaseFraction, totalFlow cannot be returned by
        ''' GetPropList, since all thermodynamic software components must support them. Although the property
        ''' identifier of derivative properties is formed from the identifier of another property, the GetPropList method
        ''' must return the identifiers of all supported derivative and non-derivative properties. For instance, a Property
        ''' Package could return the following list:
        ''' enthalpy, enthalpy.Dtemperature, entropy, entropy.Dpressure.</remarks>
        Public Overridable Function GetPropList() As Object Implements CapeOpen.ICapeThermoPropertyPackage.GetPropList
            Dim arr As New ArrayList
            With arr
                .Add("vaporPressure")
                .Add("surfaceTension")
                .Add("compressibilityFactor")
                .Add("heatOfVaporization")
                .Add("heatCapacity")
                .Add("heatCapacityCv")
                .Add("idealGasHeatCapacity")
                .Add("idealGasEnthalpy")
                .Add("excessEnthalpy")
                .Add("excessGibbsFreeEnergy")
                .Add("excessEntropy")
                .Add("viscosity")
                .Add("thermalConductivity")
                .Add("fugacity")
                .Add("fugacityCoefficient")
                .Add("activity")
                .Add("activityCoefficient")
                .Add("dewPointPressure")
                .Add("dewPointTemperature")
                .Add("kvalue")
                .Add("logFugacityCoefficient")
                .Add("logkvalue")
                .Add("volume")
                .Add("density")
                .Add("enthalpy")
                .Add("entropy")
                .Add("enthalpyF")
                .Add("entropyF")
                .Add("enthalpyNF")
                .Add("entropyNF")
                .Add("gibbsFreeEnergy")
                .Add("moles")
                .Add("mass")
                .Add("molecularWeight")
                .Add("boilingPointTemperature")
            End With

            Dim arr2(arr.Count) As String
            Array.Copy(arr.ToArray, arr2, arr.Count)
            Return arr2

        End Function

        ''' <summary>
        ''' Returns the values of the Universal Constants.
        ''' </summary>
        ''' <param name="materialObject">The Material Object.</param>
        ''' <param name="props">List of requested Universal Constants;</param>
        ''' <returns>Values of Universal Constants.</returns>
        ''' <remarks></remarks>
        Public Overridable Function GetUniversalConstant(ByVal materialObject As Object, ByVal props As Object) As Object Implements CapeOpen.ICapeThermoPropertyPackage.GetUniversalConstant
            Dim res As New ArrayList
            For Each p As String In props
                Select Case p.ToLower
                    Case "standardaccelerationofgravity"
                        res.Add(9.80665)
                    Case "avogadroconstant"
                        res.Add(6.0221419947E+23)
                    Case "boltzmannconstant"
                        res.Add(1.38065324E-23)
                    Case "molargasconstant"
                        res.Add(8.31447215)
                End Select
            Next

            Dim arr2(res.Count) As Double
            Array.Copy(res.ToArray, arr2, res.Count)
            Return arr2

        End Function

        ''' <summary>
        ''' Check to see if properties can be calculated.
        ''' </summary>
        ''' <param name="materialObject">The Material Object for the calculations.</param>
        ''' <param name="props">List of Properties to check.</param>
        ''' <returns>The array of booleans for each property.</returns>
        ''' <remarks>As it was unclear from the original specification what PropCheck should exactly be checking, and as the
        ''' argument list does not include a phase specification, implementations vary. It is generally expected that
        ''' PropCheck at least verifies that the Property is available for calculation in the property Package. However,
        ''' this can also be verified with PropList. It is advised not to use PropCheck.</remarks>
        Public Overridable Function PropCheck(ByVal materialObject As Object, ByVal props As Object) As Object Implements CapeOpen.ICapeThermoPropertyPackage.PropCheck
            Return True
        End Function

        ''' <summary>
        ''' Checks the validity of the calculation. This method is deprecated.
        ''' </summary>
        ''' <param name="materialObject">The Material Object for the calculations.</param>
        ''' <param name="props">The list of properties to check.</param>
        ''' <returns>The properties for which reliability is checked.</returns>
        ''' <remarks>The ValidityCheck method must not be used, since the ICapeThermoReliability interface is not yet defined.</remarks>
        Public Overridable Function ValidityCheck(ByVal materialObject As Object, ByVal props As Object) As Object Implements CapeOpen.ICapeThermoPropertyPackage.ValidityCheck
            Return True
        End Function

        ''' <summary>
        ''' Method responsible for calculating phase equilibria.
        ''' </summary>
        ''' <param name="materialObject">Material Object of the calculation</param>
        ''' <param name="flashType">Flash calculation type.</param>
        ''' <param name="props">Properties to be calculated at equilibrium. UNDEFINED for no
        ''' properties. If a list, then the property values should be set for each
        ''' phase present at equilibrium. (not including the overall phase).</param>
        ''' <remarks>The CalcEquilibrium method must set on the Material Object the amounts (phaseFraction) and compositions
        ''' (fraction) for all phases present at equilibrium, as well as the temperature and pressure for the overall
        ''' mixture, if not set as part of the calculation specifications. The CalcEquilibrium method must not set on the
        ''' Material Object any other value - in particular it must not set any values for phases that do not exist. See
        ''' 5.2.1 for more information.
        ''' The available list of flashes is given in section 5.6.1.
        ''' It is advised not to combine a flash calculation with a property calculation. By the returned error one cannot
        ''' see which has failed, plus the additional arguments to CalcProp (such as calculation type) cannot be
        ''' specified. Advice is to perform a CalcEquilibrium, get the phaseIDs and perform a CalcProp for those
        ''' phases.</remarks>
        Public Overridable Sub CalcEquilibrium2(ByVal materialObject As Object, ByVal flashType As String, ByVal props As Object) Implements CapeOpen.ICapeThermoEquilibriumServer.CalcEquilibrium
            CalcEquilibrium(materialObject, flashType, props)
        End Sub

        ''' <summary>
        ''' Checks to see if a given type of flash calculations can be performed and whether the properties can be
        ''' calculated after the flash calculation.
        ''' </summary>
        ''' <param name="materialObject">The Material Object for the calculations.</param>
        ''' <param name="flashType">Type of flash calculation to check</param>
        ''' <param name="props">List of Properties to check. UNDEFINED for none.</param>
        ''' <param name="valid">The array of booleans for flash and property. First element is reserved for flashType.</param>
        ''' <remarks>As it was unclear from the original specification what PropCheck should exactly be checking, and as the
        ''' argument list does not include a phase specification, implementations vary. It is generally expected that
        ''' PropCheck at least verifies that the Property is available for calculation in the Material Object. However, this
        ''' can also be verified with PropList. It is advised not to use PropCheck.
        ''' DWSIM doesn't implement this interface.</remarks>
        Public Overridable Sub PropCheck1(ByVal materialObject As Object, ByVal flashType As String, ByVal props As Object, ByRef valid As Object) Implements CapeOpen.ICapeThermoEquilibriumServer.PropCheck
            Throw New CapeOpen.CapeNoImplException
        End Sub

        ''' <summary>
        ''' Returns the flash types, properties, phases, and calculation types that are supported by a given Equilibrium Server Routine.
        ''' </summary>
        ''' <param name="flashType">Type of flash calculations supported.</param>
        ''' <param name="props">List of supported properties.</param>
        ''' <param name="phases">List of supported phases.</param>
        ''' <param name="calcType">List of supported calculation types.</param>
        ''' <remarks></remarks>
        Public Overridable Sub PropList(ByRef flashType As Object, ByRef props As Object, ByRef phases As Object, ByRef calcType As Object) Implements CapeOpen.ICapeThermoEquilibriumServer.PropList
            props = GetPropList()
            flashType = New String() {"TP", "PH", "PS", "TVF", "PVF", "PT", "HP", "SP", "VFT", "VFP"}
            calcType = New String() {"Mixture"}
            If GlobalSettings.Settings.HideSolidPhaseFromCAPEOPENComponents Then
                phases = New String() {"Vapor", "Liquid1", "Liquid2", "Overall"}
            Else
                phases = New String() {"Vapor", "Liquid1", "Liquid2", "Solid", "Overall"}
            End If
        End Sub

        Public Overridable Sub ValidityCheck1(ByVal materialObject As Object, ByVal props As Object, ByRef relList As Object) Implements CapeOpen.ICapeThermoEquilibriumServer.ValidityCheck
            Throw New CapeOpen.CapeNoImplException
        End Sub

        Public Overridable Sub CalcProp1(ByVal materialObject As Object, ByVal props As Object, ByVal phases As Object, ByVal calcType As String) Implements CapeOpen.ICapeThermoCalculationRoutine.CalcProp
            CalcProp(materialObject, props, phases, calcType)
        End Sub

        Public Overridable Function GetPropList1() As Object Implements CapeOpen.ICapeThermoCalculationRoutine.GetPropList
            Return GetPropList()
        End Function

        Public Overridable Function PropCheck2(ByVal materialObject As Object, ByVal props As Object) As Object Implements CapeOpen.ICapeThermoCalculationRoutine.PropCheck
            Return True
        End Function

        Public Overridable Function ValidityCheck2(ByVal materialObject As Object, ByVal props As Object) As Object Implements CapeOpen.ICapeThermoCalculationRoutine.ValidityCheck
            Return True
        End Function

#End Region

#Region "   CAPE-OPEN 1.1 Thermo & Physical Properties"

        ''' <summary>
        ''' Returns the values of constant Physical Properties for the specified Compounds.
        ''' </summary>
        ''' <param name="props">The list of Physical Property identifiers. Valid
        ''' identifiers for constant Physical Properties are listed in section 7.5.2.</param>
        ''' <param name="compIds">List of Compound identifiers for which constants are to
        ''' be retrieved. Set compIds to UNDEFINED to denote all Compounds in the component that implements the ICapeThermoCompounds interface.</param>
        ''' <returns>Values of constants for the specified Compounds.</returns>
        ''' <remarks>The GetConstPropList method can be used in order to check which constant Physical
        ''' Properties are available.
        ''' If the number of requested Physical Properties is P and the number of Compounds is C, the
        ''' propvals array will contain C*P variants. The first C variants will be the values for the first
        ''' requested Physical Property (one variant for each Compound) followed by C values of constants
        ''' for the second Physical Property, and so on. The actual type of values returned
        ''' (Double, String, etc.) depends on the Physical Property as specified in section 7.5.2.
        ''' Physical Properties are returned in a fixed set of units as specified in section 7.5.2.
        ''' If the compIds argument is set to UNDEFINED this is a request to return property values for
        ''' all compounds in the component that implements the ICapeThermoCompounds interface
        ''' with the compound order the same as that returned by the GetCompoundList method. For
        ''' example, if the interface is implemented by a Property Package component the property
        ''' request with compIds set to UNDEFINED means all compounds in the Property Package
        ''' rather than all compounds in the Material Object passed to the Property package.
        ''' If any Physical Property is not available for one or more Compounds, then undefined values
        ''' must be returned for those combinations and an ECapeThrmPropertyNotAvailable exception
        ''' must be raised. If the exception is raised, the client should check all the values returned to
        ''' determine which is undefined.</remarks>
        Public Overridable Function GetCompoundConstant(ByVal props As Object, ByVal compIds As Object) As Object Implements ICapeThermoCompounds.GetCompoundConstant
            Dim vals As New ArrayList
            If props(0).ToString().ToLower() = "charge" And compIds Is Nothing Then
                vals.Add(0.0)
            Else
                For Each s As String In compIds
                    Dim c As Interfaces.ICompound = Me.CurrentMaterialStream.Phases(0).Compounds(s)
                    For Each p As String In props
                        Select Case p.ToLower
                            Case "molecularweight"
                                vals.Add(c.ConstantProperties.Molar_Weight)
                            Case "criticaltemperature"
                                vals.Add(c.ConstantProperties.Critical_Temperature)
                            Case "criticalpressure"
                                vals.Add(c.ConstantProperties.Critical_Pressure)
                            Case "criticalvolume"
                                vals.Add(c.ConstantProperties.Critical_Volume / 1000)
                            Case "criticalcompressibilityfactor"
                                vals.Add(c.ConstantProperties.Critical_Compressibility)
                            Case "acentricfactor"
                                vals.Add(c.ConstantProperties.Acentric_Factor)
                            Case "normalboilingpoint"
                                vals.Add(c.ConstantProperties.Normal_Boiling_Point)
                            Case "idealgasgibbsfreeenergyofformationat25c"
                                vals.Add(c.ConstantProperties.IG_Gibbs_Energy_of_Formation_25C * c.ConstantProperties.Molar_Weight)
                            Case "idealgasenthalpyofformationat25c"
                                vals.Add(c.ConstantProperties.IG_Enthalpy_of_Formation_25C * c.ConstantProperties.Molar_Weight)
                            Case "casregistrynumber"
                                vals.Add(c.ConstantProperties.CAS_Number)
                            Case "chemicalformula", "structureformula"
                                vals.Add(c.ConstantProperties.Formula)
                            Case "triplepointtemperature"
                                vals.Add(c.ConstantProperties.TemperatureOfFusion)
                            Case Else
                                vals.Add(Double.MinValue)
                        End Select
                    Next
                Next
            End If
            Dim arr2(vals.Count - 1) As Object
            Array.Copy(vals.ToArray, arr2, vals.Count)
            Return arr2
        End Function

        ''' <summary>
        ''' Returns the list of all Compounds. This includes the Compound identifiers recognised and extra
        '''information that can be used to further identify the Compounds.
        ''' </summary>
        ''' <param name="compIds">List of Compound identifiers</param>
        ''' <param name="formulae">List of Compound formulae</param>
        ''' <param name="names">List of Compound names.</param>
        ''' <param name="boilTemps">List of boiling point temperatures.</param>
        ''' <param name="molwts">List of molecular weights.</param>
        ''' <param name="casnos">List of Chemical Abstract Service (CAS) Registry numbers.</param>
        ''' <remarks>If any item cannot be returned then the value should be set to UNDEFINED. The same information
        ''' can also be extracted using the GetCompoundConstant method. The equivalences
        ''' between GetCompoundList arguments and Compound constant Physical Properties, as
        ''' specified in section 7.5.2, is given in the table below.
        ''' When the ICapeThermoCompounds interface is implemented by a Material Object, the list
        ''' of Compounds returned is fixed when the Material Object is configured.
        ''' For a Property Package component, the Property Package will normally contain a limited set
        ''' of Compounds selected for a particular application, rather than all possible Compounds that
        ''' could be available to a proprietary Properties System.
        ''' The compIds returned by the GetCompoundList method must be unique within the
        ''' component that implements the ICapeThermoCompounds interface. There is no restriction
        ''' on the length of the strings returned in compIds. However, it should be recognised that a
        ''' PME may restrict the length of Compound identifiers internally. In such a case the PME’s
        ''' CAPE-OPEN socket must maintain a method of mapping the, potentially long, identifiers
        ''' used by a CAPE-OPEN Property package component to the identifiers used within the PME.
        ''' In order to identify the Compounds of a Property Package, the PME, or other client, will use
        ''' the casnos argument rather than the compIds. This is because different PMEs and different
        ''' Property Packages may give different names to the same Compounds and the casnos is
        ''' (almost always) unique. If the casnos is not available (e.g. for petroleum fractions), or not
        ''' unique, the other pieces of information returned by GetCompoundList can be used to
        ''' distinguish the Compounds. It should be noted, however, that for communication with a
        ''' Property Package a client must use the Compound identifiers returned in the compIds
        ''' argument. It is the responsibility of the client to maintain appropriate data structures that
        ''' allow it to reconcile the different Compound identifiers used by different Property Packages
        ''' and any native property system.</remarks>
        Public Overridable Sub GetCompoundList(ByRef compIds As Object, ByRef formulae As Object, ByRef names As Object, ByRef boilTemps As Object, ByRef molwts As Object, ByRef casnos As Object) Implements ICapeThermoCompounds.GetCompoundList
            GetComponentList(compIds, formulae, names, boilTemps, molwts, casnos)
        End Sub

        ''' <summary>
        ''' Returns the list of supported constant Physical Properties.
        ''' </summary>
        ''' <returns>List of identifiers for all supported constant Physical Properties. The standard constant property identifiers are listed in section 7.5.2.</returns>
        ''' <remarks>GetConstPropList returns identifiers for all the constant Physical Properties that can be
        ''' retrieved by the GetCompoundConstant method. If no properties are supported,
        ''' UNDEFINED should be returned. The CAPE-OPEN standards do not define a minimum list
        ''' of Physical Properties to be made available by a software component that implements the
        ''' ICapeThermoCompounds interface.
        ''' A component that implements the ICapeThermoCompounds interface may return constant
        ''' Physical Property identifiers which do not belong to the list defined in section 7.5.2.
        ''' However, these proprietary identifiers may not be understood by most of the clients of this
        ''' component.</remarks>
        Public Overridable Function GetConstPropList() As Object Implements ICapeThermoCompounds.GetConstPropList
            Dim vals As New ArrayList
            With vals
                .Add("molecularweight")
                .Add("criticaltemperature")
                .Add("criticalpressure")
                .Add("criticalvolume")
                .Add("criticalcompressibilityfactor")
                .Add("acentricfactor")
                .Add("normalboilingpoint")
                .Add("idealgasgibbsfreeenergyofformationat25c")
                .Add("idealgasenthalpyofformationat25c")
                .Add("casregistrynumber")
                .Add("chemicalformula")
            End With
            Dim arr2(vals.Count - 1) As String
            Array.Copy(vals.ToArray, arr2, vals.Count)
            Return arr2
        End Function

        ''' <summary>
        ''' Returns the number of Compounds supported.
        ''' </summary>
        ''' <returns>Number of Compounds supported.</returns>
        ''' <remarks>The number of Compounds returned by this method must be equal to the number of
        ''' Compound identifiers that are returned by the GetCompoundList method of this interface. It
        ''' must be zero or a positive number.</remarks>
        Public Overridable Function GetNumCompounds() As Integer Implements ICapeThermoCompounds.GetNumCompounds
            If Settings.CAPEOPENMode Then
                Return Me._selectedcomps.Count
            Else
                Return Me.CurrentMaterialStream.Phases(0).Compounds.Count
            End If
        End Function

        ''' <summary>
        ''' Returns the values of pressure-dependent Physical Properties for the specified pure Compounds.
        ''' </summary>
        ''' <param name="props">The list of Physical Property identifiers. Valid identifiers for pressure-dependent 
        ''' Physical Properties are listed in section 7.5.4</param>
        ''' <param name="pressure">Pressure (in Pa) at which Physical Properties are evaluated</param>
        ''' <param name="compIds">List of Compound identifiers for which Physical Properties are to be retrieved. 
        ''' Set compIds to UNDEFINED to denote all Compounds in the component that implements the ICapeThermoCompounds interface.</param>
        ''' <param name="propVals">Property values for the Compounds specified.</param>
        ''' <remarks></remarks>
        Public Overridable Sub GetPDependentProperty(ByVal props As Object, ByVal pressure As Double, ByVal compIds As Object, ByRef propVals As Object) Implements ICapeThermoCompounds.GetPDependentProperty
            Dim vals As New ArrayList
            For Each c As String In compIds
                For Each p As String In props
                    Select Case p.ToLower
                        Case "boilingpointtemperature"
                            vals.Add(Me.AUX_TSATi(pressure, c))
                    End Select
                Next
            Next
            Dim arr2(vals.Count - 1) As Double
            Array.Copy(vals.ToArray, arr2, vals.Count)
            propVals = arr2
        End Sub

        ''' <summary>
        ''' Returns the list of supported pressure-dependent properties.
        ''' </summary>
        ''' <returns>The list of Physical Property identifiers for all supported pressure-dependent properties. The standard identifiers are listed in section 7.5.4</returns>
        ''' <remarks>GetPDependentPropList returns identifiers for all the pressure-dependent properties that can
        ''' be retrieved by the GetPDependentProperty method. If no properties are supported
        ''' UNDEFINED should be returned. The CAPE-OPEN standards do not define a minimum list
        ''' of Physical Properties to be made available by a software component that implements the
        ''' ICapeThermoCompounds interface.
        ''' A component that implements the ICapeThermoCompounds interface may return identifiers
        ''' which do not belong to the list defined in section 7.5.4. However, these proprietary
        ''' identifiers may not be understood by most of the clients of this component.</remarks>
        Public Overridable Function GetPDependentPropList() As Object Implements ICapeThermoCompounds.GetPDependentPropList
            Return New String() {"boilingPointTemperature"}
        End Function

        ''' <summary>
        ''' Returns the values of temperature-dependent Physical Properties for the specified pure Compounds.
        ''' </summary>
        ''' <param name="props">The list of Physical Property identifiers. Valid identifiers for 
        ''' temperature-dependent Physical Properties are listed in section 7.5.3</param>
        ''' <param name="temperature">Temperature (in K) at which properties are evaluated</param>
        ''' <param name="compIds">List of Compound identifiers for which Physical Properties are to be retrieved. 
        ''' Set compIds to UNDEFINED to denote all Compounds in the component that implements the ICapeThermoCompounds interface.</param>
        ''' <param name="propVals">Physical Property values for the Compounds specified.</param>
        ''' <remarks>The GetTDependentPropList method can be used in order to check which Physical
        ''' Properties are available.
        ''' If the number of requested Physical Properties is P and the number of Compounds is C, the
        ''' propvals array will contain C*P values. The first C will be the values for the first requested
        ''' Physical Property followed by C values for the second Physical Property, and so on.
        ''' Properties are returned in a fixed set of units as specified in section 7.5.3.
        ''' If the compIds argument is set to UNDEFINED this is a request to return property values for
        ''' all compounds in the component that implements the ICapeThermoCompounds interface
        ''' with the compound order the same as that returned by the GetCompoundList method. For
        ''' example, if the interface is implemented by a Property Package component the property
        ''' request with compIds set to UNDEFINED means all compounds in the Property Package
        ''' rather than all compounds in the Material Object passed to the Property package.
        ''' If any Physical Property is not available for one or more Compounds, then undefined values
        ''' must be returned for those combinations and an ECapeThrmPropertyNotAvailable exception
        ''' must be raised. If the exception is raised, the client should check all the values returned to
        ''' determine which is undefined.</remarks>
        Public Overridable Sub GetTDependentProperty(ByVal props As Object, ByVal temperature As Double, ByVal compIds As Object, ByRef propVals As Object) Implements ICapeThermoCompounds.GetTDependentProperty
            Dim vals As New ArrayList
            For Each c As String In compIds
                For Each p As String In props
                    Select Case p.ToLower
                        Case "heatofvaporization"
                            vals.Add(Me.AUX_HVAPi(c, temperature) * Me.CurrentMaterialStream.Phases(0).Compounds(c).ConstantProperties.Molar_Weight)
                        Case "idealgasenthalpy"
                            vals.Add(Me.RET_Hid_i(298.15, temperature, c) * Me.CurrentMaterialStream.Phases(0).Compounds(c).ConstantProperties.Molar_Weight)
                        Case "idealgasentropy"
                            vals.Add(Me.RET_Sid_i(298.15, temperature, 101325, c) * Me.CurrentMaterialStream.Phases(0).Compounds(c).ConstantProperties.Molar_Weight)
                        Case "idealgasheatcapacity"
                            vals.Add(Me.AUX_CPi(c, temperature) * Me.CurrentMaterialStream.Phases(0).Compounds(c).ConstantProperties.Molar_Weight)
                        Case "vaporpressure"
                            vals.Add(Me.AUX_PVAPi(c, temperature))
                        Case "viscosityofliquid"
                            vals.Add(Me.AUX_LIQVISCi(c, temperature, 101325))
                        Case "heatcapacityofliquid"
                            vals.Add(Me.AUX_LIQ_Cpi(Me.CurrentMaterialStream.Phases(0).Compounds(c).ConstantProperties, temperature) * Me.CurrentMaterialStream.Phases(0).Compounds(c).ConstantProperties.Molar_Weight)
                        Case "heatcapacityofsolid"
                            vals.Add(Me.AUX_SolidHeatCapacity(Me.CurrentMaterialStream.Phases(0).Compounds(c).ConstantProperties, temperature) * Me.CurrentMaterialStream.Phases(0).Compounds(c).ConstantProperties.Molar_Weight)
                        Case "thermalconductivityofliquid"
                            vals.Add(Me.AUX_LIQTHERMCONDi(Me.CurrentMaterialStream.Phases(0).Compounds(c).ConstantProperties, temperature))
                        Case "thermalconductivityofvapor"
                            vals.Add(Me.AUX_VAPTHERMCONDi(Me.CurrentMaterialStream.Phases(0).Compounds(c).ConstantProperties, temperature, Me.AUX_PVAPi(Me.CurrentMaterialStream.Phases(0).Compounds(c).ConstantProperties.Name, temperature)))
                        Case "viscosityofvapor"
                            vals.Add(Me.AUX_VAPVISCi(Me.CurrentMaterialStream.Phases(0).Compounds(c).ConstantProperties, temperature))
                        Case "densityofliquid"
                            vals.Add(Me.AUX_LIQDENSi(Me.CurrentMaterialStream.Phases(0).Compounds(c).ConstantProperties, temperature))
                        Case "densityofsolid"
                            vals.Add(Me.AUX_SOLIDDENSi(Me.CurrentMaterialStream.Phases(0).Compounds(c).ConstantProperties, temperature))
                    End Select
                Next
            Next
            Dim arr2(vals.Count - 1) As Double
            Array.Copy(vals.ToArray, arr2, vals.Count)
            propVals = arr2
        End Sub

        ''' <summary>
        ''' Returns the list of supported temperature-dependent Physical Properties.
        ''' </summary>
        ''' <returns>The list of Physical Property identifiers for all supported temperature-dependent 
        ''' properties. The standard identifiers are listed in section 7.5.3</returns>
        ''' <remarks>GetTDependentPropList returns identifiers for all the temperature-dependent Physical
        ''' Properties that can be retrieved by the GetTDependentProperty method. If no properties are
        ''' supported UNDEFINED should be returned. The CAPE-OPEN standards do not define a
        ''' minimum list of properties to be made available by a software component that implements
        ''' the ICapeThermoCompounds interface.
        ''' A component that implements the ICapeThermoCompounds interface may return identifiers
        ''' which do not belong to the list defined in section 7.5.3. However, these proprietary identifiers
        ''' may not be understood by most of the clients of this component.</remarks>
        Public Overridable Function GetTDependentPropList() As Object Implements ICapeThermoCompounds.GetTDependentPropList
            Dim vals As New ArrayList
            With vals

                .Add("heatOfVaporization")
                .Add("idealGasEnthalpy")
                .Add("idealGasEntropy")
                .Add("idealGasHeatCapacity")
                .Add("vaporPressure")
                .Add("viscosityOfLiquid")

                .Add("heatCapacityOfLiquid")
                .Add("heatCapacityOfSolid")
                .Add("thermalConductivityOfLiquid")
                .Add("thermalConductivityOfVapor")
                .Add("viscosityOfVapor")

                '.Add("densityOfLiquid")
                '.Add("densityOfSolid")

            End With
            Dim arr2(vals.Count - 1) As String
            Array.Copy(vals.ToArray, arr2, vals.Count)
            Return arr2
        End Function

        Public Overridable Function GetNumPhases() As Integer Implements ICapeThermoPhases.GetNumPhases
            Dim i As Integer = 0
            For Each pi As PhaseInfo In Me.PhaseMappings.Values
                If pi.PhaseLabel <> "Disabled" Then
                    i += 1
                End If
            Next
            Return i
        End Function

        ''' <summary>
        ''' Returns information on an attribute associated with a Phase for the purpose of understanding 
        ''' what lies behind a Phase label.
        ''' </summary>
        ''' <param name="phaseLabel">A (single) Phase label. This must be one of the values returned by GetPhaseList method.</param>
        ''' <param name="phaseAttribute">One of the Phase attribute identifiers from the table below.</param>
        ''' <returns>The value corresponding to the Phase attribute identifier – see table below.</returns>
        ''' <remarks>GetPhaseInfo is intended to allow a PME, or other client, to identify a Phase with an arbitrary
        ''' label. A PME, or other client, will need to do this to map stream data into a Material
        ''' Object, or when importing a Property Package. If the client cannot identify the Phase, it can
        ''' ask the user to provide a mapping based on the values of these properties.
        ''' The list of supported Phase attributes is defined in the following table:
        ''' 
        ''' Phase attribute identifier            Supported values
        ''' 
        ''' StateOfAggregation                    One of the following strings:
        '''                                       Vapor
        '''                                       Liquid
        '''                                       Solid
        '''                                       Unknown
        ''' 
        ''' KeyCompoundId                         The identifier of the Compound (compId as returned by GetCompoundList) 
        '''                                       that is expected to be present in highest concentration in the Phase. 
        '''                                       May be undefined in which case UNDEFINED should be returned.
        ''' 
        ''' ExcludedCompoundId                    The identifier of the Compound (compId as returned by
        '''                                       GetCompoundList) that is expected to be present in low or zero
        '''                                       concentration in the Phase. May not be defined in which case
        '''                                       UNDEFINED should be returned.
        ''' 
        ''' DensityDescription                    A description that indicates the density range expected for the Phase.
        '''                                       One of the following strings or UNDEFINED:
        '''                                       Heavy
        '''                                       Light
        ''' 
        ''' UserDescription                       A description that helps the user or PME to identify the Phase.
        '''                                       It can be any string or UNDEFINED.
        ''' 
        ''' TypeOfSolid                           A description that provides more information about a solid Phase. For
        '''                                       Phases with a “Solid” state of aggregation it may be one of the
        '''                                       following standard strings or UNDEFINED:
        '''                                       PureSolid
        '''                                       SolidSolution
        '''                                       HydrateI
        '''                                       HydrateII
        '''                                       HydrateH
        '''                                       Other values may be returned for solid Phases but these may not be
        '''                                       understood by most clients.
        '''                                       For Phases with any other state of aggregation it must be
        '''                                       UNDEFINED.</remarks>
        Public Overridable Function GetPhaseInfo(ByVal phaseLabel As String, ByVal phaseAttribute As String) As Object Implements ICapeThermoPhases.GetPhaseInfo
            Dim retval As Object = Nothing
            Select Case phaseLabel
                Case "Vapor"
                    Select Case phaseAttribute
                        Case "StateOfAggregation"
                            retval = "Vapor"
                        Case "keyCompoundId"
                            retval = Nothing
                        Case "ExcludedCompoundId"
                            retval = Nothing
                        Case "DensityDescription"
                            retval = Nothing
                        Case "UserDescription"
                            retval = "Vapor Phase"
                        Case Else
                            retval = Nothing
                    End Select
                Case "Liquid1", "Liquid2", "Liquid3", "Liquid", "Aqueous"
                    Select Case phaseAttribute
                        Case "StateOfAggregation"
                            retval = "Liquid"
                        Case "keyCompoundId"
                            retval = Nothing
                        Case "ExcludedCompoundId"
                            retval = Nothing
                        Case "DensityDescription"
                            retval = Nothing
                        Case "UserDescription"
                            retval = "Liquid Phase"
                        Case Else
                            retval = Nothing
                    End Select
                Case "Solid"
                    Select Case phaseAttribute
                        Case "StateOfAggregation"
                            retval = "Solid"
                        Case "TypeOfSolid"
                            retval = "SolidSolution"
                        Case "keyCompoundId"
                            retval = Nothing
                        Case "ExcludedCompoundId"
                            retval = Nothing
                        Case "DensityDescription"
                            retval = Nothing
                        Case "UserDescription"
                            retval = "Solid Phase"
                        Case Else
                            retval = Nothing
                    End Select
            End Select
            Return retval
        End Function

        ''' <summary>
        ''' Returns Phase labels and other important descriptive information for all the Phases supported.
        ''' </summary>
        ''' <param name="phaseLabels">he list of Phase labels for the Phases supported. A Phase label can 
        ''' be any string but each Phase must have a unique label. If, for some reason, no Phases are 
        ''' supported an UNDEFINED value should be returned for the phaseLabels. The number of Phase labels 
        ''' must also be equal to the number of Phases returned by the GetNumPhases method.</param>
        ''' <param name="stateOfAggregation">The physical State of Aggregation associated with each of the 
        ''' Phases. This must be one of the following strings: ”Vapor”, “Liquid”, “Solid” or “Unknown”. Each 
        ''' Phase must have a single State of Aggregation. The value must not be left undefined, but may be 
        ''' set to “Unknown”.</param>
        ''' <param name="keyCompoundId">The key Compound for the Phase. This must be the Compound identifier 
        ''' (as returned by GetCompoundList), or it may be undefined in which case a UNDEFINED value is returned. 
        ''' The key Compound is an indication of the Compound that is expected to be present in high concentration 
        ''' in the Phase, e.g. water for an aqueous liquid phase. Each Phase can have a single key Compound.</param>
        ''' <remarks>The Phase label allows the phase to be uniquely identified in methods of the ICapeThermo-
        ''' Phases interface and other CAPE-OPEN interfaces. The State of Aggregation and key
        ''' Compound provide a way for the PME, or other client, to interpret the meaning of a Phase
        ''' label in terms of the physical characteristics of the Phase.
        ''' All arrays returned by this method must be of the same length, i.e. equal to the number of
        ''' Phase labels.
        ''' To get further information about a Phase, use the GetPhaseInfo method.</remarks>
        Public Overridable Sub GetPhaseList1(ByRef phaseLabels As Object, ByRef stateOfAggregation As Object, ByRef keyCompoundId As Object) Implements ICapeThermoPhases.GetPhaseList
            Dim pl, sa, kci As New ArrayList, tmpstr As Object
            For Each pin As PhaseInfo In Me.PhaseMappings.Values
                If pin.PhaseLabel <> "Disabled" Then
                    pl.Add(pin.PhaseLabel)
                    tmpstr = Me.GetPhaseInfo(pin.PhaseLabel, "StateOfAggregation")
                    sa.Add(tmpstr)
                    tmpstr = Me.GetPhaseInfo(pin.PhaseLabel, "keyCompoundId")
                    kci.Add(tmpstr)
                End If
            Next
            Dim myarr1(pl.Count - 1), myarr2(pl.Count - 1), myarr3(pl.Count - 1) As String
            Array.Copy(pl.ToArray, myarr1, pl.Count)
            Array.Copy(sa.ToArray, myarr2, pl.Count)
            Array.Copy(kci.ToArray, myarr3, pl.Count)
            phaseLabels = myarr1
            stateOfAggregation = myarr2
            keyCompoundId = myarr3
        End Sub

        ''' <summary>
        ''' This method is used to calculate the natural logarithm of the fugacity coefficients (and
        ''' optionally their derivatives) in a single Phase mixture. The values of temperature, pressure
        ''' and composition are specified in the argument list and the results are also returned through
        ''' the argument list.
        ''' </summary>
        ''' <param name="phaseLabel">Phase label of the Phase for which the properties are to be calculated. 
        ''' The Phase label must be one of the strings returned by the GetPhaseList method on the ICapeThermoPhases interface.</param>
        ''' <param name="temperature">The temperature (K) for the calculation.</param>
        ''' <param name="pressure">The pressure (Pa) for the calculation.</param>
        ''' <param name="moleNumbers">Number of moles of each Compound in the mixture.</param>
        ''' <param name="fFlags">Code indicating whether natural logarithm of the fugacity coefficients and/or derivatives 
        ''' should be calculated (see notes).</param>
        ''' <param name="lnPhi">Natural logarithm of the fugacity coefficients (if requested).</param>
        ''' <param name="lnPhiDT">Derivatives of natural logarithm of the fugacity coefficients w.r.t. temperature (if requested).</param>
        ''' <param name="lnPhiDP">Derivatives of natural logarithm of the fugacity coefficients w.r.t. pressure (if requested).</param>
        ''' <param name="lnPhiDn">Derivatives of natural logarithm of the fugacity coefficients w.r.t. mole numbers (if requested).</param>
        ''' <remarks>This method is provided to allow the natural logarithm of the fugacity coefficient, which is
        ''' the most commonly used thermodynamic property, to be calculated and returned in a highly
        ''' efficient manner.
        ''' The temperature, pressure and composition (mole numbers) for the calculation are specified
        ''' by the arguments and are not obtained from the Material Object by a separate request. Likewise,
        ''' any quantities calculated are returned through the arguments and are not stored in the
        ''' Material Object. The state of the Material Object is not affected by calling this method. It
        ''' should be noted however, that prior to calling CalcAndGetLnPhi a valid Material Object
        ''' must have been defined by calling the SetMaterial method on the
        ''' ICapeThermoMaterialContext interface of the component that implements the
        ''' ICapeThermoPropertyRoutine interface. The compounds in the Material Object must have
        ''' been identified and the number of values supplied in the moleNumbers argument must be
        ''' equal to the number of Compounds in the Material Object.
        ''' The fugacity coefficient information is returned as the natural logarithm of the fugacity
        ''' coefficient. This is because thermodynamic models naturally provide the natural logarithm
        ''' of this quantity and also a wider range of values may be safely returned.
        ''' The quantities actually calculated and returned by this method are controlled by an integer
        ''' code fFlags. The code is formed by summing contributions for the property and each
        ''' derivative required using the enumerated constants eCapeCalculationCode (defined in the
        ''' Thermo version 1.1 IDL) shown in the following table. For example, to calculate log
        ''' fugacity coefficients and their T-derivatives the fFlags argument would be set to
        ''' CAPE_LOG_FUGACITY_COEFFICIENTS + CAPE_T_DERIVATIVE.
        ''' 
        '''                                       code                            numerical value
        ''' no calculation                        CAPE_NO_CALCULATION             0
        ''' log fugacity coefficients             CAPE_LOG_FUGACITY_COEFFICIENTS  1
        ''' T-derivative                          CAPE_T_DERIVATIVE               2
        ''' P-derivative                          CAPE_P_DERIVATIVE               4
        ''' mole number derivatives               CAPE_MOLE_NUMBERS_DERIVATIVES   8
        ''' 
        ''' If CalcAndGetLnPhi is called with fFlags set to CAPE_NO_CALCULATION no property
        ''' values are returned.
        ''' A typical sequence of operations for this method when implemented by a Property Package
        ''' component would be:
        ''' - Check that the phaseLabel specified is valid.
        ''' - Check that the moleNumbers array contains the number of values expected
        ''' (should be consistent with the last call to the SetMaterial method).
        ''' - Calculate the requested properties/derivatives at the T/P/composition specified in
        ''' the argument list.
        ''' - Store values for the properties/derivatives in the corresponding arguments.
        ''' Note that this calculation can be carried out irrespective of whether the Phase actually exists
        ''' in the Material Object.</remarks>
        Public Overridable Sub CalcAndGetLnPhi(ByVal phaseLabel As String, ByVal temperature As Double, ByVal pressure As Double, ByVal moleNumbers As Object, ByVal fFlags As Integer, ByRef lnPhi As Object, ByRef lnPhiDT As Object, ByRef lnPhiDP As Object, ByRef lnPhiDn As Object) Implements ICapeThermoPropertyRoutine.CalcAndGetLnPhi
            Select Case fFlags
                Case CapeCalculationCode.CAPE_LOG_FUGACITY_COEFFICIENTS
                    'normalize mole fractions
                    Dim tmols As Double
                    For i As Integer = 0 To moleNumbers.length - 1
                        tmols += moleNumbers(i)
                    Next
                    For i As Integer = 0 To moleNumbers.length - 1
                        moleNumbers(i) /= tmols
                    Next
                    Select Case phaseLabel
                        Case "Vapor"
                            lnPhi = Me.DW_CalcFugCoeff(moleNumbers, temperature, pressure, State.Vapor)
                        Case "Liquid"
                            lnPhi = Me.DW_CalcFugCoeff(moleNumbers, temperature, pressure, State.Liquid)
                        Case "Solid"
                            lnPhi = Me.DW_CalcFugCoeff(moleNumbers, temperature, pressure, State.Solid)
                    End Select
                    For i As Integer = 0 To moleNumbers.length - 1
                        lnPhi(i) = Log(lnPhi(i))
                    Next
                Case Else
                    Throw New CapeOpen.CapeThrmPropertyNotAvailableException
            End Select
        End Sub

        ''' <summary>
        ''' CalcSinglePhaseProp is used to calculate properties and property derivatives of a mixture in
        ''' a single Phase at the current values of temperature, pressure and composition set in the
        ''' Material Object. CalcSinglePhaseProp does not perform phase Equilibrium Calculations.
        ''' </summary>
        ''' <param name="props">The list of identifiers for the single-phase properties or derivatives to 
        ''' be calculated. See sections 7.5.5 and 7.6 for the standard identifiers.</param>
        ''' <param name="phaseLabel">Phase label of the Phase for which the properties are to be calculated. 
        ''' The Phase label must be one of the strings returned by the GetPhaseList method on the 
        ''' ICapeThermoPhases interface and the phase must be present in the Material Object.</param>
        ''' <remarks>CalcSinglePhaseProp calculates properties, such as enthalpy or viscosity that are defined for
        ''' a single Phase. Physical Properties that depend on more than one Phase, for example surface
        ''' tension or K-values, are handled by CalcTwoPhaseProp method.
        ''' Components that implement this method must get the input specification for the calculation
        ''' (temperature, pressure and composition) from the associated Material Object and set the
        ''' results in the Material Object.
        ''' Thermodynamic and Physical Properties Components, such as a Property Package or Property
        ''' Calculator, must implement the ICapeThermoMaterialContext interface so that an
        ''' ICapeThermoMaterial interface can be passed via the SetMaterial method.
        ''' The component that implements the ICapeThermoPropertyRoutine interface (e.g. a Property
        ''' Package or Property Calculator) must also implement the ICapeThermoPhases interface so
        ''' that it is possible to get a list of supported phases. The phaseLabel passed to this method
        ''' must be one of the phase labels returned by the GetPhaseList method of the
        ''' ICapeThermoPhases interface and it must also be present in the Material Object, ie. one of
        ''' the phase labels returned by the GetPresentPhases method of the ICapeThermoMaterial
        ''' interface. This latter condition will be satisfied if the phase is made present explicitly by
        ''' calling the SetPresentPhases method or if any phase properties have been set by calling the
        ''' SetSinglePhaseProp or SetTwoPhaseProp methods.
        ''' A typical sequence of operations for CalcSinglePhaseProp when implemented by a Property
        ''' Package component would be:
        ''' - Check that the phaseLabel specified is valid.
        ''' - Use the GetTPFraction method (of the Material Object specified in the last call to the
        ''' SetMaterial method) to get the temperature, pressure and composition of the
        ''' specified Phase.
        ''' - Calculate the properties.
        ''' - Store values for the properties of the Phase in the Material Object using the
        ''' SetSinglePhaseProp method of the ICapeThermoMaterial interface.
        ''' CalcSinglePhaseProp will request the input Property values it requires from the Material
        ''' Object through GetSinglePhaseProp calls. If a requested property is not available, the
        ''' exception raised will be ECapeThrmPropertyNotAvailable. If this error occurs then the
        ''' Property Package can return it to the client, or request a different property. Material Object
        ''' implementations must be able to supply property values using the client’s choice of basis by
        ''' implementing conversion from one basis to another.
        ''' Clients should not assume that Phase fractions and Compound fractions in a Material Object
        ''' are normalised. Fraction values may also lie outside the range 0 to 1. If fractions are not
        ''' normalised, or are outside the expected range, it is the responsibility of the Property Package
        ''' to decide how to deal with the situation.
        ''' It is recommended that properties are requested one at a time in order to simplify error
        ''' handling. However, it is recognised that there are cases where the potential efficiency gains
        ''' of requesting several properties simultaneously are more important. One such example
        ''' might be when a property and its derivatives are required.
        ''' If a client uses multiple properties in a call and one of them fails then the whole call should
        ''' be considered to have failed. This implies that no value should be written back to the Material
        ''' Object by the Property Package until it is known that the whole request can be satisfied.
        ''' It is likely that a PME might request values of properties for a Phase at conditions of temperature,
        ''' pressure and composition where the Phase does not exist (according to the
        ''' mathematical/physical models used to represent properties). The exception
        ''' ECapeThrmPropertyNotAvailable may be raised or an extrapolated value may be returned.
        ''' It is responsibility of the implementer to decide how to handle this circumstance.</remarks>
        Public Overridable Sub CalcSinglePhaseProp(ByVal props As Object, ByVal phaseLabel As String) Implements ICapeThermoPropertyRoutine.CalcSinglePhaseProp

            If Not Settings.CAPEOPENMode Then

                For Each pi As PhaseInfo In Me.PhaseMappings.Values
                    If phaseLabel = pi.PhaseLabel Then
                        For Each p As String In props
                            Select Case p
                                Case "partialpressure"
                                Case "enthalpyF.Dmoles"
                                    Dim st As State
                                    Select Case pi.DWPhaseID
                                        Case Phase.Aqueous, Phase.Liquid, Phase.Liquid1, Phase.Liquid2, Phase.Liquid3
                                            st = State.Liquid
                                        Case Phase.Vapor
                                            st = State.Vapor
                                        Case Phase.Solid
                                            st = State.Solid
                                    End Select
                                    Dim ms = DirectCast(Me.CurrentMaterialStream, MaterialStream)
                                    Dim cres = DW_CalcdEnthalpydmoles(ms.GetPhaseComposition(pi.DWPhaseIndex), ms.GetTemperature, ms.GetPressure, st)
                                    Me.CurrentMaterialStream.SetSinglePhaseProp(p, phaseLabel, "", cres)
                                Case "entropyF.Dmoles"
                                    Dim st As State
                                    Select Case pi.DWPhaseID
                                        Case Phase.Aqueous, Phase.Liquid, Phase.Liquid1, Phase.Liquid2, Phase.Liquid3
                                            st = State.Liquid
                                        Case Phase.Vapor
                                            st = State.Vapor
                                        Case Phase.Solid
                                            st = State.Solid
                                    End Select
                                    Dim ms = DirectCast(Me.CurrentMaterialStream, MaterialStream)
                                    Dim cres = DW_CalcdEntropydmoles(ms.GetPhaseComposition(pi.DWPhaseIndex), ms.GetTemperature, ms.GetPressure, st)
                                    Me.CurrentMaterialStream.SetSinglePhaseProp(p, phaseLabel, "", cres)
                                Case Else
                                    Me.DW_CalcProp(p, pi.DWPhaseID)
                            End Select
                        Next
                        Exit For
                    End If
                Next

            Else

                Dim res As New ArrayList
                Dim comps As New ArrayList

                If TryCast(Me, PropertyPackages.CAPEOPENPropertyPackage) IsNot Nothing Then
                    Dim complist As Object = Nothing
                    Me.GetCompoundList(complist, Nothing, Nothing, Nothing, Nothing, Nothing)
                    For Each s As String In complist
                        For Each kvp As KeyValuePair(Of String, String) In CType(Me, PropertyPackages.CAPEOPENPropertyPackage)._mappings
                            If kvp.Value = s Then
                                comps.Add(kvp.Key)
                                Exit For
                            End If
                        Next
                    Next
                Else
                    For Each c As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                        comps.Add(c.Name)
                    Next
                End If

                Dim f As Integer = -1
                Dim phs As PropertyPackages.Phase
                Dim st As State
                Select Case phaseLabel.ToLower
                    Case "overall"
                        f = 0
                        phs = PropertyPackages.Phase.Mixture
                    Case Else
                        For Each pi As PhaseInfo In Me.PhaseMappings.Values
                            If phaseLabel = pi.PhaseLabel Then
                                f = pi.DWPhaseIndex
                                phs = pi.DWPhaseID
                                Exit For
                            End If
                        Next
                End Select

                Select Case phs
                    Case Phase.Aqueous, Phase.Liquid, Phase.Liquid1, Phase.Liquid2, Phase.Liquid3
                        st = State.Liquid
                    Case Phase.Vapor
                        st = State.Vapor
                    Case Phase.Solid
                        st = State.Solid
                End Select

                If f = -1 Then
                    Dim ex As New CapeOpen.CapeInvalidArgumentException("Invalid Phase ID", New ArgumentException, 0)
                    Dim hcode As Integer = 0
                    ThrowCAPEException(ex, "Error", ex.Message, "ICapeThermoMaterial", ex.Source, ex.StackTrace, "CalcSinglePhaseProp", hcode)
                End If

                Dim basis As String = "Mole"

                For Each [property] As String In props

                    Dim mymo As ICapeThermoMaterial = _como
                    Dim T As Double
                    Dim P As Double
                    Dim Vx As Object = Nothing

                    mymo.GetTPFraction(phaseLabel, T, P, Vx)

                    Me.CurrentMaterialStream.Phases(0).Properties.temperature = T
                    Me.CurrentMaterialStream.Phases(0).Properties.pressure = P
                    Me.CurrentMaterialStream.SetPhaseComposition(Vx, phs)

                    Select Case [property].ToLower
                        Case "isothermalcompressibility",
                             "bulkmodulus",
                             "joulethomsoncoefficient",
                             "speedofsound",
                             "internalenergy",
                             "helmholtzenergy",
                             "gibbsenergy"
                            Me.DW_CalcProp("enthalpy", phs)
                            Me.DW_CalcProp("entropy", phs)
                            Me.DW_CalcProp("compressibilityfactor", phs)
                            Me.DW_CalcProp("density", phs)
                            Me.DW_CalcProp("heatcapacitycp", phs)
                            Me.DW_CalcProp("heatcapacitycv", phs)
                            CalcAdditionalPhaseProperties(f)
                    End Select

                    If phs = Phase.Solid Then
                        Me.DW_CalcSolidPhaseProps()
                    Else
                        Try
                            Me.DW_CalcProp([property], phs)
                        Catch ex As Exception
                        End Try
                    End If

                    basis = "Mole"

                    Select Case [property].ToLower
                        Case "compressibilityfactor"
                            res.Add(Me.CurrentMaterialStream.Phases(f).Properties.compressibilityFactor.GetValueOrDefault)
                        Case "isothermalcompressibility"
                            res.Add(Me.CurrentMaterialStream.Phases(f).Properties.isothermal_compressibility.GetValueOrDefault)
                        Case "bulkmodulus"
                            res.Add(Me.CurrentMaterialStream.Phases(f).Properties.bulk_modulus.GetValueOrDefault)
                        Case "joulethomsoncoefficient"
                            res.Add(Me.CurrentMaterialStream.Phases(f).Properties.jouleThomsonCoefficient.GetValueOrDefault)
                        Case "speedofsound"
                            res.Add(Me.CurrentMaterialStream.Phases(f).Properties.speedOfSound.GetValueOrDefault)
                        Case "compressibilityfactor"
                            res.Add(Me.CurrentMaterialStream.Phases(f).Properties.compressibilityFactor.GetValueOrDefault)
                            basis = ""
                        Case "heatofvaporization"
                        Case "heatcapacity", "heatcapacitycp"
                            Select Case basis
                                Case "Molar", "molar", "mole", "Mole"
                                    res.Add(Me.CurrentMaterialStream.Phases(f).Properties.heatCapacityCp.GetValueOrDefault * Me.AUX_MMM(phs))
                                Case "Mass", "mass"
                                    res.Add(Me.CurrentMaterialStream.Phases(f).Properties.heatCapacityCp.GetValueOrDefault * 1000)
                            End Select
                        Case "heatcapacitycv"
                            Select Case basis
                                Case "Molar", "molar", "mole", "Mole"
                                    res.Add(Me.CurrentMaterialStream.Phases(f).Properties.heatCapacityCv.GetValueOrDefault * Me.AUX_MMM(phs))
                                Case "Mass", "mass"
                                    res.Add(Me.CurrentMaterialStream.Phases(f).Properties.heatCapacityCv.GetValueOrDefault * 1000)
                            End Select
                        Case "idealgasheatcapacity", "idealgasheatcapacitycp"
                            If f = 1 Then
                                res.Add(Me.AUX_CPm(PropertyPackages.Phase.Liquid, Me.CurrentMaterialStream.Phases(0).Properties.temperature) * 1000)
                            ElseIf f = 2 Then
                                res.Add(Me.AUX_CPm(PropertyPackages.Phase.Vapor, Me.CurrentMaterialStream.Phases(0).Properties.temperature) * 1000)
                            Else
                                res.Add(Me.AUX_CPm(PropertyPackages.Phase.Solid, Me.CurrentMaterialStream.Phases(0).Properties.temperature) * 1000)
                            End If
                        Case "idealgasheatcapacityratio"
                            If f = 1 Then
                                Dim Cp = AUX_CPm(Phase.Liquid, T)
                                Dim Cv = Cp - 8.314 / Me.CurrentMaterialStream.Phases(1).Properties.molecularWeight.GetValueOrDefault()
                                res.Add(Cp / Cv)
                            ElseIf f = 2 Then
                                Dim Cp = AUX_CPm(Phase.Vapor, T)
                                Dim Cv = Cp - 8.314 / Me.CurrentMaterialStream.Phases(2).Properties.molecularWeight.GetValueOrDefault()
                                res.Add(Cp / Cv)
                            Else
                                Dim Cp = AUX_CPm(Phase.Mixture, T)
                                Dim Cv = Cp - 8.314 / Me.CurrentMaterialStream.Phases(0).Properties.molecularWeight.GetValueOrDefault()
                                res.Add(Cp / Cv)
                            End If
                        Case "idealgasenthalpy"
                            If f = 1 Then
                                res.Add(Me.RET_Hid(298.15, Me.CurrentMaterialStream.Phases(0).Properties.temperature.GetValueOrDefault * 1000, PropertyPackages.Phase.Liquid))
                            ElseIf f = 2 Then
                                res.Add(Me.RET_Hid(298.15, Me.CurrentMaterialStream.Phases(0).Properties.temperature.GetValueOrDefault * 1000, PropertyPackages.Phase.Vapor))
                            Else
                                res.Add(Me.RET_Hid(298.15, Me.CurrentMaterialStream.Phases(0).Properties.temperature.GetValueOrDefault * 1000, PropertyPackages.Phase.Solid))
                            End If
                        Case "excessenthalpy"
                            Select Case basis
                                Case "Molar", "molar", "mole", "Mole"
                                    res.Add(Me.CurrentMaterialStream.Phases(f).Properties.excessEnthalpy.GetValueOrDefault * Me.AUX_MMM(phs))
                                Case "Mass", "mass"
                                    res.Add(Me.CurrentMaterialStream.Phases(f).Properties.excessEnthalpy.GetValueOrDefault * 1000)
                            End Select
                        Case "excessentropy"
                            Select Case basis
                                Case "Molar", "molar", "mole", "Mole"
                                    res.Add(Me.CurrentMaterialStream.Phases(f).Properties.excessEntropy.GetValueOrDefault * Me.AUX_MMM(phs))
                                Case "Mass", "mass"
                                    res.Add(Me.CurrentMaterialStream.Phases(f).Properties.excessEntropy.GetValueOrDefault * 1000)
                            End Select
                        Case "viscosity"
                            res.Add(Me.CurrentMaterialStream.Phases(f).Properties.viscosity.GetValueOrDefault)
                            basis = ""
                        Case "thermalconductivity"
                            res.Add(Me.CurrentMaterialStream.Phases(f).Properties.thermalConductivity.GetValueOrDefault)
                            basis = ""
                        Case "fugacity"
                            For Each c As String In comps
                                res.Add(Me.CurrentMaterialStream.Phases(f).Compounds(c).MoleFraction.GetValueOrDefault * Me.CurrentMaterialStream.Phases(f).Compounds(c).FugacityCoeff.GetValueOrDefault * Me.CurrentMaterialStream.Phases(0).Properties.pressure.GetValueOrDefault)
                            Next
                            basis = ""
                        Case "activity"
                            For Each c As String In comps
                                res.Add(Me.CurrentMaterialStream.Phases(f).Compounds(c).ActivityCoeff.GetValueOrDefault * Me.CurrentMaterialStream.Phases(f).Compounds(c).MoleFraction.GetValueOrDefault)
                            Next
                            basis = ""
                        Case "fugacitycoefficient"
                            For Each c As String In comps
                                res.Add(Me.CurrentMaterialStream.Phases(f).Compounds(c).FugacityCoeff.GetValueOrDefault)
                            Next
                            basis = ""
                        Case "activitycoefficient"
                            For Each c As String In comps
                                res.Add(Me.CurrentMaterialStream.Phases(f).Compounds(c).ActivityCoeff.GetValueOrDefault)
                            Next
                            basis = ""
                        Case "logfugacitycoefficient"
                            For Each c As String In comps
                                res.Add(Math.Log(Me.CurrentMaterialStream.Phases(f).Compounds(c).FugacityCoeff.GetValueOrDefault))
                            Next
                            basis = ""
                        Case "volume"
                            res.Add(Me.CurrentMaterialStream.Phases(f).Properties.molecularWeight.GetValueOrDefault / Me.CurrentMaterialStream.Phases(f).Properties.density.GetValueOrDefault / 1000)
                        Case "density"
                            res.Add(Me.CurrentMaterialStream.Phases(f).Properties.density.GetValueOrDefault / Me.CurrentMaterialStream.Phases(f).Properties.molecularWeight.GetValueOrDefault * 1000)
                        Case "enthalpy", "enthalpynf"
                            Select Case basis
                                Case "Molar", "molar", "mole", "Mole"
                                    Dim val = Me.CurrentMaterialStream.Phases(f).Properties.molecularWeight.GetValueOrDefault
                                    If val = 0.0# Then
                                        res.Add(Me.CurrentMaterialStream.Phases(f).Properties.molar_enthalpy.GetValueOrDefault)
                                    Else
                                        res.Add(Me.CurrentMaterialStream.Phases(f).Properties.enthalpy.GetValueOrDefault * val)
                                    End If
                                Case "Mass", "mass"
                                    res.Add(Me.CurrentMaterialStream.Phases(f).Properties.enthalpy.GetValueOrDefault * 1000)
                            End Select
                        Case "entropy", "entropynf"
                            Select Case basis
                                Case "Molar", "molar", "mole", "Mole"
                                    Dim val = Me.CurrentMaterialStream.Phases(f).Properties.molecularWeight.GetValueOrDefault
                                    If val = 0.0# Then
                                        res.Add(Me.CurrentMaterialStream.Phases(f).Properties.molar_entropy.GetValueOrDefault)
                                    Else
                                        res.Add(Me.CurrentMaterialStream.Phases(f).Properties.entropy.GetValueOrDefault * val)
                                    End If
                                Case "Mass", "mass"
                                    res.Add(Me.CurrentMaterialStream.Phases(f).Properties.entropy.GetValueOrDefault * 1000)
                            End Select
                        Case "enthalpyf"
                            Select Case basis
                                Case "Molar", "molar", "mole", "Mole"
                                    Dim val = Me.CurrentMaterialStream.Phases(f).Properties.molecularWeight.GetValueOrDefault
                                    If val = 0.0# Then
                                        res.Add(Me.CurrentMaterialStream.Phases(f).Properties.molar_enthalpyF.GetValueOrDefault)
                                    Else
                                        res.Add(Me.CurrentMaterialStream.Phases(f).Properties.enthalpyF.GetValueOrDefault * val)
                                    End If
                                Case "Mass", "mass"
                                    res.Add(Me.CurrentMaterialStream.Phases(f).Properties.enthalpyF.GetValueOrDefault * 1000)
                            End Select
                        Case "entropyf"
                            Select Case basis
                                Case "Molar", "molar", "mole", "Mole"
                                    Dim val = Me.CurrentMaterialStream.Phases(f).Properties.molecularWeight.GetValueOrDefault
                                    If val = 0.0# Then
                                        res.Add(Me.CurrentMaterialStream.Phases(f).Properties.molar_entropyF.GetValueOrDefault)
                                    Else
                                        res.Add(Me.CurrentMaterialStream.Phases(f).Properties.entropyF.GetValueOrDefault * val)
                                    End If
                                Case "Mass", "mass"
                                    res.Add(Me.CurrentMaterialStream.Phases(f).Properties.entropy.GetValueOrDefault * 1000)
                            End Select
                        Case "internalenergy"
                            Select Case basis
                                Case "Molar", "molar", "mole", "Mole"
                                    Dim val As Double = Me.CurrentMaterialStream.Phases(f).Properties.molecularWeight.GetValueOrDefault
                                    If val = 0.0# Then
                                        res.Add(Me.CurrentMaterialStream.Phases(f).Properties.molar_internal_energy.GetValueOrDefault)
                                    Else
                                        res.Add(Me.CurrentMaterialStream.Phases(f).Properties.internal_energy.GetValueOrDefault * val)
                                    End If
                                Case "Mass", "mass"
                                    res.Add(Me.CurrentMaterialStream.Phases(f).Properties.internal_energy.GetValueOrDefault * 1000)
                            End Select
                        Case "gibbsenergy"
                            Select Case basis
                                Case "Molar", "molar", "mole", "Mole"
                                    Dim val As Double = Me.CurrentMaterialStream.Phases(f).Properties.molecularWeight.GetValueOrDefault
                                    If val = 0.0# Then
                                        res.Add(Me.CurrentMaterialStream.Phases(f).Properties.molar_gibbs_free_energy.GetValueOrDefault)
                                    Else
                                        res.Add(Me.CurrentMaterialStream.Phases(f).Properties.gibbs_free_energy.GetValueOrDefault * val)
                                    End If
                                Case "Mass", "mass"
                                    res.Add(Me.CurrentMaterialStream.Phases(f).Properties.gibbs_free_energy.GetValueOrDefault * 1000)
                            End Select
                        Case "helmholtzenergy"
                            Select Case basis
                                Case "Molar", "molar", "mole", "Mole"
                                    Dim val As Double = Me.CurrentMaterialStream.Phases(f).Properties.molecularWeight.GetValueOrDefault
                                    If val = 0.0# Then
                                        res.Add(Me.CurrentMaterialStream.Phases(f).Properties.molar_helmholtz_energy.GetValueOrDefault)
                                    Else
                                        res.Add(Me.CurrentMaterialStream.Phases(f).Properties.helmholtz_energy.GetValueOrDefault * val)
                                    End If
                                Case "Mass", "mass"
                                    res.Add(Me.CurrentMaterialStream.Phases(f).Properties.helmholtz_energy.GetValueOrDefault * 1000)
                            End Select
                        Case "moles"
                            res.Add(Me.CurrentMaterialStream.Phases(f).Properties.molarflow.GetValueOrDefault)
                            basis = ""
                        Case "mass"
                            res.Add(Me.CurrentMaterialStream.Phases(f).Properties.massflow.GetValueOrDefault)
                            basis = ""
                        Case "molecularweight"
                            res.Add(Me.CurrentMaterialStream.Phases(f).Properties.molecularWeight.GetValueOrDefault)
                            basis = ""
                        Case "temperature"
                            res.Add(Me.CurrentMaterialStream.Phases(0).Properties.temperature.GetValueOrDefault)
                            basis = ""
                        Case "pressure"
                            res.Add(Me.CurrentMaterialStream.Phases(0).Properties.pressure.GetValueOrDefault)
                            basis = ""
                        Case "flow"
                            Select Case basis
                                Case "Molar", "molar", "mole", "Mole"
                                    For Each c As String In comps
                                        res.Add(Me.CurrentMaterialStream.Phases(f).Compounds(c).MolarFlow.GetValueOrDefault)
                                    Next
                                Case "Mass", "mass"
                                    For Each c As String In comps
                                        res.Add(Me.CurrentMaterialStream.Phases(f).Compounds(c).MassFlow.GetValueOrDefault)
                                    Next
                            End Select
                        Case "fraction", "massfraction", "molarfraction"
                            Select Case basis
                                Case "Molar", "molar", "mole", "Mole"
                                    For Each c As String In comps
                                        res.Add(Me.CurrentMaterialStream.Phases(f).Compounds(c).MoleFraction.GetValueOrDefault)
                                    Next
                                Case "Mass", "mass"
                                    For Each c As String In comps
                                        res.Add(Me.CurrentMaterialStream.Phases(f).Compounds(c).MassFraction.GetValueOrDefault)
                                    Next
                                Case ""
                                    If [property].ToLower.Contains("mole") Then
                                        For Each c As String In comps
                                            res.Add(Me.CurrentMaterialStream.Phases(f).Compounds(c).MoleFraction.GetValueOrDefault)
                                        Next
                                    ElseIf [property].ToLower.Contains("mass") Then
                                        For Each c As String In comps
                                            res.Add(Me.CurrentMaterialStream.Phases(f).Compounds(c).MassFraction.GetValueOrDefault)
                                        Next
                                    End If
                            End Select
                        Case "concentration"
                            For Each c As String In comps
                                res.Add(Me.CurrentMaterialStream.Phases(f).Compounds(c).MassFlow.GetValueOrDefault / Me.CurrentMaterialStream.Phases(f).Properties.volumetric_flow.GetValueOrDefault)
                            Next
                            basis = ""
                        Case "molarity"
                            For Each c As String In comps
                                res.Add(Me.CurrentMaterialStream.Phases(f).Compounds(c).MolarFlow.GetValueOrDefault / Me.CurrentMaterialStream.Phases(f).Properties.volumetric_flow.GetValueOrDefault)
                            Next
                            basis = ""
                        Case "partialpressure"
                            For Each c As String In comps
                                res.Add(Me.CurrentMaterialStream.Phases(f).Compounds(c).MoleFraction.GetValueOrDefault *
                                        Me.CurrentMaterialStream.Phases(f).Compounds(c).FugacityCoeff.GetValueOrDefault * P)
                            Next
                            basis = ""
                        Case "enthalpyf.dmoles"
                            Dim val = Me.CurrentMaterialStream.Phases(f).Properties.molecularWeight.GetValueOrDefault
                            Dim cres = DW_CalcdEnthalpydmoles(Vx, T, P, st)
                            Select Case basis
                                Case "Molar", "molar", "mole", "Mole"
                                    res = New ArrayList(cres.MultiplyConstY(val))
                                Case "Mass", "mass"
                                    res = New ArrayList(cres.MultiplyConstY(1000))
                            End Select
                        Case "entropyf.dmoles"
                            Dim val = Me.CurrentMaterialStream.Phases(f).Properties.molecularWeight.GetValueOrDefault
                            Dim cres = DW_CalcdEntropydmoles(Vx, T, P, st)
                            Select Case basis
                                Case "Molar", "molar", "mole", "Mole"
                                    res = New ArrayList(cres.MultiplyConstY(val))
                                Case "Mass", "mass"
                                    res = New ArrayList(cres.MultiplyConstY(1000))
                            End Select
                        Case "phasefraction"
                            Select Case basis
                                Case "Molar", "molar", "mole", "Mole"
                                    res.Add(Me.CurrentMaterialStream.Phases(f).Properties.molarfraction.GetValueOrDefault)
                                Case "Mass", "mass"
                                    res.Add(Me.CurrentMaterialStream.Phases(f).Properties.massfraction.GetValueOrDefault)
                            End Select
                        Case "totalflow"
                            Select Case basis
                                Case "Molar", "molar", "mole", "Mole"
                                    res.Add(Me.CurrentMaterialStream.Phases(f).Properties.molarflow.GetValueOrDefault)
                                Case "Mass", "mass"
                                    res.Add(Me.CurrentMaterialStream.Phases(f).Properties.massflow.GetValueOrDefault)
                            End Select
                        Case Else
                            Dim ex = New CapeOpen.CapeThrmPropertyNotAvailableException
                            Dim hcode As Integer = 0
                            ThrowCAPEException(ex, "Error", ex.Message, "ICapeThermoMaterial", ex.Source, ex.StackTrace, "CalcSinglePhaseProp", hcode)
                    End Select

                    Dim i As Integer
                    For i = 0 To res.Count - 1
                        If TypeOf res(i) Is Double Then
                            If Double.IsNaN(res(i)) Then res(i) = Double.NegativeInfinity
                        End If
                    Next

                    Dim arr(res.Count - 1) As Double
                    Array.Copy(res.ToArray, arr, res.Count)

                    mymo.SetSinglePhaseProp([property], phaseLabel, basis, arr)

                Next

            End If

        End Sub

        ''' <summary>
        ''' CalcTwoPhaseProp is used to calculate mixture properties and property derivatives that depend on
        ''' two Phases at the current values of temperature, pressure and composition set in the Material Object.
        ''' It does not perform Equilibrium Calculations.
        ''' </summary>
        ''' <param name="props">The list of identifiers for properties to be calculated. This must be one or more 
        ''' of the supported two-phase properties and derivatives (as given by the GetTwoPhasePropList method). 
        ''' The standard identifiers for two-phase properties are given in section 7.5.7 and 7.6.</param>
        ''' <param name="phaseLabels">Phase labels of the phases for which the properties are to be calculated. 
        ''' The phase labels must be two of the strings returned by the GetPhaseList method on the ICapeThermoPhases 
        ''' interface and the phases must also be present in the Material Object.</param>
        ''' <remarks>CalcTwoPhaseProp calculates the values of properties such as surface tension or K-values.
        ''' Properties that pertain to a single Phase are handled by the CalcSinglePhaseProp method of
        ''' the ICapeThermoPropertyRoutine interface.Components that implement this method must
        ''' get the input specification for the calculation (temperature, pressure and composition) from
        ''' the associated Material Object and set the results in the Material Object.
        ''' Components such as a Property Package or Property Calculator must implement the
        ''' ICapeThermoMaterialContext interface so that an ICapeThermoMaterial interface can be
        ''' passed via the SetMaterial method.
        ''' The component that implements the ICapeThermoPropertyRoutine interface (e.g. a Property
        ''' Package or Property Calculator) must also implement the ICapeThermoPhases interface so
        ''' that it is possible to get a list of supported phases. The phaseLabels passed to this method
        ''' must be in the list of phase labels returned by the GetPhaseList method of the
        ''' ICapeThermoPhases interface and they must also be present in the Material Object, ie. in the
        ''' list of phase labels returned by the GetPresentPhases method of the ICapeThermoMaterial
        ''' interface. This latter condition will be satisfied if the phases are made present explicitly by
        ''' calling the SetPresentPhases method or if any phase properties have been set by calling the
        ''' SetSinglePhaseProp or SetTwoPhaseProp methods.</remarks>
        Public Overridable Sub CalcTwoPhaseProp(ByVal props As Object, ByVal phaseLabels As Object) Implements ICapeThermoPropertyRoutine.CalcTwoPhaseProp

            Me.DW_CalcTwoPhaseProps(Phase.Liquid, Phase.Vapor)

            If Settings.CAPEOPENMode Then

                Dim res As New ArrayList
                Dim comps As New ArrayList
                For Each c As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                    comps.Add(c.Name)
                Next

                Dim basis As String = Nothing

                For Each [property] As String In props
                    Select Case [property].ToLower
                        Case "kvalue"
                            For Each c As String In comps
                                res.Add(Me.CurrentMaterialStream.Phases(0).Compounds(c).Kvalue)
                            Next
                        Case "logkvalue"
                            For Each c As String In comps
                                res.Add(Me.CurrentMaterialStream.Phases(0).Compounds(c).lnKvalue)
                            Next
                        Case "surfacetension"
                            res.Add(Me.CurrentMaterialStream.Phases(0).Properties.surfaceTension.GetValueOrDefault)
                        Case Else
                            Dim ex = New CapeOpen.CapeThrmPropertyNotAvailableException
                            Dim hcode As Integer = 0
                            ThrowCAPEException(ex, "Error", ex.Message, "ICapeThermoMaterial", ex.Source, ex.StackTrace, "CalcTwoPhaseProp", hcode)
                    End Select

                    Dim arr(res.Count - 1) As Double
                    Array.Copy(res.ToArray, arr, res.Count)

                    Dim mymo As ICapeThermoMaterial = _como
                    mymo.SetTwoPhaseProp([property], phaseLabels, basis, arr)

                Next

            End If

        End Sub

        ''' <summary>
        ''' Checks whether it is possible to calculate a property with the CalcSinglePhaseProp method for a given Phase.
        ''' </summary>
        ''' <param name="property">The identifier of the property to check. To be valid this must be one of the supported 
        ''' single-phase properties or derivatives (as given by the GetSinglePhasePropList method).</param>
        ''' <param name="phaseLabel">The Phase label for the calculation check. This must be one of the labels 
        ''' returned by the GetPhaseList method on the ICapeThermoPhases interface.</param>
        ''' <returns>Set to True if the combination of property and phaseLabel is supported or False if 
        ''' not supported.</returns>
        ''' <remarks>The result of the check should only depend on the capabilities and configuration
        ''' (Compounds and Phases supported) of the component that implements the
        ''' ICapeThermoPropertyRoutine interface (e.g. a Property Package). It should not depend on
        ''' whether a Material Object has been set nor on the state (temperature, pressure, composition
        ''' etc.), or configuration of a Material Object that might be set.
        ''' It is expected that the PME, or other client, will use this method to check whether the properties
        ''' it requires are supported by the Property Package when the package is imported. If any
        ''' essential properties are not available, the import process should be aborted.
        ''' If either the property or the phaseLabel arguments are not recognised by the component that
        ''' implements the ICapeThermoPropertyRoutine interface this method should return False.</remarks>
        Public Overridable Function CheckSinglePhasePropSpec(ByVal [property] As String, ByVal phaseLabel As String) As Boolean Implements ICapeThermoPropertyRoutine.CheckSinglePhasePropSpec
            Select Case [property].ToLower
                Case "compressibilityfactor", "heatofvaporization", "heatcapacity", "heatcapacitycv",
                    "idealgasheatcapacity", "idealgasenthalpy", "excessenthalpy", "excessentropy",
                    "viscosity", "thermalconductivity", "fugacity", "fugacitycoefficient", "activity", "activitycoefficient",
                    "dewpointpressure", "dewpointtemperature", "logfugacitycoefficient", "volume", "density",
                    "enthalpy", "entropy", "gibbsfreeenergy", "moles", "mass", "molecularweight", "totalflow"
                    Return True
                Case Else
                    Return False
            End Select
        End Function

        ''' <summary>
        ''' Checks whether it is possible to calculate a property with the CalcTwoPhaseProp method for a given set of Phases.
        ''' </summary>
        ''' <param name="property">The identifier of the property to check. To be valid this must be one of the supported 
        ''' two-phase properties (including derivatives), as given by the GetTwoPhasePropList method.</param>
        ''' <param name="phaseLabels">Phase labels of the Phases for which the properties are to be calculated. The Phase 
        ''' labels must be two of the identifiers returned by the GetPhaseList method on the ICapeThermoPhases interface.</param>
        ''' <returns>Set to True if the combination of property and phaseLabels is supported, or False if not supported.</returns>
        ''' <remarks>The result of the check should only depend on the capabilities and configuration
        ''' (Compounds and Phases supported) of the component that implements the
        ''' ICapeThermoPropertyRoutine interface (e.g. a Property Package). It should not depend on
        ''' whether a Material Object has been set nor on the state (temperature, pressure, composition
        ''' etc.), or configuration of a Material Object that might be set.
        ''' It is expected that the PME, or other client, will use this method to check whether the
        ''' properties it requires are supported by the Property Package when the Property Package is
        ''' imported. If any essential properties are not available, the import process should be aborted.
        ''' If either the property argument or the values in the phaseLabels arguments are not
        ''' recognised by the component that implements the ICapeThermoPropertyRoutine interface
        ''' this method should return False.</remarks>
        Public Overridable Function CheckTwoPhasePropSpec(ByVal [property] As String, ByVal phaseLabels As Object) As Boolean Implements ICapeThermoPropertyRoutine.CheckTwoPhasePropSpec
            Return True
        End Function

        ''' <summary>
        ''' Returns the list of supported non-constant single-phase Physical Properties.
        ''' </summary>
        ''' <returns>List of all supported non-constant single-phase property identifiers. 
        ''' The standard single-phase property identifiers are listed in section 7.5.5.</returns>
        ''' <remarks>A non-constant property depends on the state of the Material Object.
        ''' Single-phase properties, e.g. enthalpy, only depend on the state of one phase.
        ''' GetSinglePhasePropList must return all the single-phase properties that can be calculated by
        ''' CalcSinglePhaseProp. If derivatives can be calculated these must also be returned. The list
        ''' of standard property identifiers in section 7.5.5 also contains properties such as temperature,
        ''' pressure, fraction, phaseFraction, flow and totalFlow that are not usually calculated by the
        ''' CalcSinglePhaseProp method and hence these property identifiers would not be returned by
        ''' GetSinglePhasePropList. These properties would normally be used in calls to the
        ''' Set/GetSinglePhaseProp methods of the ICapeThermoMaterial interface.
        ''' If no single-phase properties are supported this method should return UNDEFINED.
        ''' To get the list of supported two-phase properties, use GetTwoPhasePropList.
        ''' A component that implements this method may return non-constant single-phase property
        ''' identifiers which do not belong to the list defined in section 7.5.5. However, these
        ''' proprietary identifiers may not be understood by most of the clients of this component.</remarks>
        Public Overridable Function GetSinglePhasePropList() As Object Implements ICapeThermoPropertyRoutine.GetSinglePhasePropList
            Dim arr As New ArrayList
            With arr
                .Add("molecularWeight")
                .Add("compressibilityFactor")
                .Add("isothermalCompressibility")
                .Add("bulkModulus")
                .Add("jouleThomsonCoefficient")
                .Add("speedOfSound")
                .Add("volume")
                .Add("density")
                .Add("viscosity")
                .Add("thermalConductivity")
                .Add("heatCapacityCp")
                .Add("heatCapacityCv")
                .Add("idealGasHeatCapacityCp")
                .Add("idealGasHeatCapacityRatio")
                .Add("enthalpy")
                .Add("entropy")
                .Add("enthalpyF")
                .Add("enthalpyF.Dmoles")
                .Add("entropyF")
                .Add("entropyF.Dmoles")
                .Add("internalEnergy")
                .Add("helmholtzEnergy")
                .Add("gibbsEnergy")
                .Add("fugacity")
                .Add("fugacityCoefficient")
                .Add("activityCoefficient")
                .Add("logFugacityCoefficient")
            End With
            Dim arr2(arr.Count - 1) As String
            Array.Copy(arr.ToArray, arr2, arr.Count)
            Return arr2
        End Function

        ''' <summary>
        ''' Returns the list of supported non-constant two-phase properties.
        ''' </summary>
        ''' <returns>List of all supported non-constant two-phase property identifiers. The standard two-phase 
        ''' property identifiers are listed in section 7.5.7.</returns>
        ''' <remarks>A non-constant property depends on the state of the Material Object. Two-phase properties
        ''' are those that depend on more than one co-existing phase, e.g. K-values.
        ''' GetTwoPhasePropList must return all the properties that can be calculated by
        ''' CalcTwoPhaseProp. If derivatives can be calculated, these must also be returned.
        ''' If no two-phase properties are supported this method should return UNDEFINED.
        ''' To check whether a property can be evaluated for a particular set of phase labels use the
        ''' CheckTwoPhasePropSpec method.
        ''' A component that implements this method may return non-constant two-phase property
        ''' identifiers which do not belong to the list defined in section 7.5.7. However, these
        ''' proprietary identifiers may not be understood by most of the clients of this component.
        ''' To get the list of supported single-phase properties, use GetSinglePhasePropList.</remarks>
        Public Overridable Function GetTwoPhasePropList() As Object Implements ICapeThermoPropertyRoutine.GetTwoPhasePropList
            Return New String() {"kvalue", "logKvalue", "surfaceTension"}
        End Function

        ''' <summary>
        ''' Retrieves the value of a Universal Constant.
        ''' </summary>
        ''' <param name="constantId">Identifier of Universal Constant. The list of constants supported should be 
        ''' obtained by using the GetUniversalConstantList method.</param>
        ''' <returns>Value of Universal Constant. This could be a numeric or a string value. For numeric values 
        ''' the units of measurement are specified in section 7.5.5.</returns>
        ''' <remarks>Universal Constants (often called fundamental constants) are quantities like the gas constant,
        ''' or the Avogadro constant.</remarks>
        Public Overridable Function GetUniversalConstant1(ByVal constantId As String) As Object Implements ICapeThermoUniversalConstant.GetUniversalConstant
            Dim res As New ArrayList
            Select Case constantId.ToLower
                Case "standardaccelerationofgravity"
                    res.Add(9.80665)
                Case "avogadroconstant"
                    res.Add(6.0221419947E+23)
                Case "boltzmannconstant"
                    res.Add(1.38065324E-23)
                Case "molargasconstant"
                    res.Add(8.31447215)
            End Select
            Dim arr2(res.Count) As Object
            Array.Copy(res.ToArray, arr2, res.Count)
            Return arr2
        End Function

        ''' <summary>
        ''' Returns the identifiers of the supported Universal Constants.
        ''' </summary>
        ''' <returns>List of identifiers of Universal Constants. The list of standard identifiers is given in section 7.5.5.</returns>
        ''' <remarks>A component may return Universal Constant identifiers that do not belong to the list defined
        ''' in section 7.5.5. However, these proprietary identifiers may not be understood by most of the
        ''' clients of this component.</remarks>
        Public Overridable Function GetUniversalConstantList() As Object Implements ICapeThermoUniversalConstant.GetUniversalConstantList
            Return New String() {"standardAccelerationOfGravity", "avogadroConstant", "boltzmannConstant", "molarGasConstant"}
        End Function

        ''' <summary>
        ''' CalcEquilibrium is used to calculate the amounts and compositions of Phases at equilibrium.
        ''' CalcEquilibrium will calculate temperature and/or pressure if these are not among the two
        ''' specifications that are mandatory for each Equilibrium Calculation considered.
        ''' </summary>
        ''' <param name="specification1">First specification for the Equilibrium Calculation. The 
        ''' specification information is used to retrieve the value of the specification from the 
        ''' Material Object. See below for details.</param>
        ''' <param name="specification2">Second specification for the Equilibrium Calculation in 
        ''' the same format as specification1.</param>
        ''' <param name="solutionType">The identifier for the required solution type. The
        ''' standard identifiers are given in the following list:
        ''' Unspecified
        ''' Normal
        ''' Retrograde
        ''' The meaning of these terms is defined below in the notes. Other identifiers may be supported 
        ''' but their interpretation is not part of the CO standard.</param>
        ''' <remarks>The specification1 and specification2 arguments provide the information necessary to
        ''' retrieve the values of two specifications, for example the pressure and temperature, for the
        ''' Equilibrium Calculation. The CheckEquilibriumSpec method can be used to check for
        ''' supported specifications. Each specification variable contains a sequence of strings in the
        ''' order defined in the following table (hence, the specification arguments may have 3 or 4
        ''' items):
        ''' 
        ''' item                        meaning
        ''' 
        ''' property identifier         The property identifier can be any of the identifiers listed in section 7.5.5 but
        '''                             only certain property specifications will normally be supported by any
        '''                             Equilibrium Routine.
        ''' 
        ''' basis                       The basis for the property value. Valid settings for basis are given in section
        '''                             7.4. Use UNDEFINED as a placeholder for a property for which basis does
        '''                             not apply. For most Equilibrium Specifications, the result of the calculation
        '''                             is not dependent on the basis, but, for example, for phase fraction
        '''                             specifications the basis (Mole or Mass) does make a difference.
        ''' 
        ''' phase label                 The phase label denotes the Phase to which the specification applies. It must
        '''                             either be one of the labels returned by GetPresentPhases, or the special value
        '''                             “Overall”.
        ''' 
        ''' compound identifier         The compound identifier allows for specifications that depend on a particular
        '''                             Compound. This item of the specification array is optional and may be
        '''                             omitted. In case of a specification without compound identifier, the array
        '''                             element may be present and empty, or may be absent.
        '''                             The values corresponding to the specifications in the argument list and the overall
        '''                             composition of the mixture must be set in the associated Material Object before a call to
        '''                             CalcEquilibrium.
        ''' 
        ''' Components such as a Property Package or an Equilibrium Calculator must implement the
        ''' ICapeThermoMaterialContext interface, so that an ICapeThermoMaterial interface can be
        ''' passed via the SetMaterial method. It is the responsibility of the implementation of
        ''' CalcEquilibrium to validate the Material Object before attempting a calculation.
        ''' The Phases that will be considered in the Equilibrium Calculation are those that exist in the
        ''' Material Object, i.e. the list of phases specified in a SetPresentPhases call. This provides a
        ''' way for a client to specify whether, for example, a vapour-liquid, liquid-liquid, or vapourliquid-
        ''' liquid calculation is required. CalcEquilibrium must use the GetPresentPhases method
        ''' to retrieve the list of Phases and the associated Phase status flags. The Phase status flags may
        ''' be used by the client to provide information about the Phases, for example whether estimates
        ''' of the equilibrium state are provided. See the description of the GetPresentPhases and
        ''' SetPresentPhases methods of the ICapeThermoMaterial interface for details. When the
        ''' Equilibrium Calculation has been completed successfully, the SetPresentPhases method
        ''' must be used to specify which Phases are present at equilibrium and the Phase status flags
        ''' for the phases should be set to Cape_AtEquilibrium. This must include any Phases that are
        ''' present in zero amount such as the liquid Phase in a dew point calculation.
        ''' Some types of Phase equilibrium specifications may result in more than one solution. A
        ''' common example of this is the case of a dew point calculation. However, CalcEquilibrium
        ''' can provide only one solution through the Material Object. The solutionType argument
        ''' allows the “Normal” or “Retrograde” solution to be explicitly requested. When none of the
        ''' specifications includes a phase fraction, the solutionType argument should be set to
        ''' “Unspecified”.
        ''' 
        ''' CalcEquilibrium must set the amounts (phase fractions), compositions, temperature and
        ''' pressure for all Phases present at equilibrium, as well as the temperature and pressure for the
        ''' overall mixture if not set as part of the calculation specifications. It must not set any other
        ''' values – in particular it must not set any values for phases that are not present.
        ''' 
        ''' As an example, the following sequence of operations might be performed by
        ''' CalcEquilibrium in the case of an Equilibrium Calculation at fixed pressure and temperature:
        ''' 
        ''' - With the ICapeThermoMaterial interface of the supplied Material Object:
        ''' 
        ''' -- Use the GetPresentPhases method to find the list of Phases that the Equilibrium
        ''' Calculation should consider.
        ''' 
        ''' -- With the ICapeThermoCompounds interface of the Material Object use the
        ''' GetCompoundList method to find which Compounds are present.
        ''' 
        ''' -- Use the GetOverallProp method to get the temperature, pressure and composition
        ''' for the overall mixture.
        ''' 
        ''' - Perform the Equilibrium Calculation.
        ''' 
        ''' -- Use SetPresentPhases to specify the Phases present at equilibrium and set the
        ''' Phase status flags to Cape_AtEquilibrium.
        ''' 
        ''' -- Use SetSinglePhaseProp to set pressure, temperature, Phase amount (or Phase
        ''' fraction) and composition for all Phases present.</remarks>
        Public Overridable Sub CalcEquilibrium1(ByVal specification1 As Object, ByVal specification2 As Object, ByVal solutionType As String) Implements ICapeThermoEquilibriumRoutine.CalcEquilibrium

            Dim spec1, spec2 As FlashSpec
            If specification1(0).ToString.ToLower = "temperature" And specification2(0).ToString.ToLower = "pressure" Then
                spec1 = FlashSpec.T
                spec2 = FlashSpec.P
            ElseIf specification1(0).ToString.ToLower = "pressure" And specification2(0).ToString.ToLower = "enthalpy" Then
                spec1 = FlashSpec.P
                spec2 = FlashSpec.H
            ElseIf specification1(0).ToString.ToLower = "pressure" And specification2(0).ToString.ToLower = "entropy" Then
                spec1 = FlashSpec.P
                spec2 = FlashSpec.S
            ElseIf specification1(0).ToString.ToLower = "pressure" And specification2(0).ToString.ToLower = "phasefraction" Then
                spec1 = FlashSpec.P
                spec2 = FlashSpec.VAP
            ElseIf specification1(0).ToString.ToLower = "temperature" And specification2(0).ToString.ToLower = "phasefraction" Then
                spec1 = FlashSpec.T
                spec2 = FlashSpec.VAP
            ElseIf specification2(0).ToString.ToLower = "temperature" And specification1(0).ToString.ToLower = "pressure" Then
                spec1 = FlashSpec.T
                spec2 = FlashSpec.P
            ElseIf specification2(0).ToString.ToLower = "pressure" And specification1(0).ToString.ToLower = "enthalpy" Then
                spec1 = FlashSpec.P
                spec2 = FlashSpec.H
            ElseIf specification2(0).ToString.ToLower = "pressure" And specification1(0).ToString.ToLower = "entropy" Then
                spec1 = FlashSpec.P
                spec2 = FlashSpec.S
            ElseIf specification2(0).ToString.ToLower = "pressure" And specification1(0).ToString.ToLower = "phasefraction" Then
                spec1 = FlashSpec.P
                spec2 = FlashSpec.VAP
            ElseIf specification2(0).ToString.ToLower = "temperature" And specification1(0).ToString.ToLower = "phasefraction" Then
                spec1 = FlashSpec.T
                spec2 = FlashSpec.VAP
            Else
                Throw New CapeOpen.CapeLimitedImplException("Flash spec not supported.")
            End If

            Dim T, P, Hm, Sm As Double

            If Settings.CAPEOPENMode Then

                Dim res As Object = Nothing

                Dim mys As ICapeThermoMaterial = _como

                Dim Vx As Object = Nothing

                mys.GetOverallProp("fraction", "Mole", Vx)

                Me.CurrentMaterialStream.SetPhaseComposition(Vx, Phase.Mixture)

                If spec1 = FlashSpec.T And spec2 = P Then
                    mys.GetOverallProp("temperature", Nothing, res)
                    T = res(0)
                    mys.GetOverallProp("pressure", Nothing, res)
                    P = res(0)
                    Me.CurrentMaterialStream.Phases(0).Properties.temperature = T
                    Me.CurrentMaterialStream.Phases(0).Properties.pressure = P
                ElseIf spec1 = FlashSpec.T And spec2 = FlashSpec.VAP Then
                    mys.GetOverallProp("temperature", Nothing, res)
                    T = res(0)
                    Me.CurrentMaterialStream.Phases(0).Properties.temperature = T
                    mys.GetSinglePhaseProp("phaseFraction", "Vapor", "Mole", res)
                    Me.CurrentMaterialStream.Phases(2).Properties.molarfraction = res(0)
                ElseIf spec1 = FlashSpec.P And spec2 = FlashSpec.H Then
                    mys.GetOverallProp("pressure", Nothing, res)
                    P = res(0)
                    mys.GetOverallProp("enthalpy", "Mole", res)
                    Hm = res(0) / AUX_MMM(Phase.Mixture)
                    Me.CurrentMaterialStream.Phases(0).Properties.enthalpy = Hm
                    Me.CurrentMaterialStream.Phases(0).Properties.pressure = P
                ElseIf spec1 = FlashSpec.P And spec2 = FlashSpec.S Then
                    mys.GetOverallProp("pressure", Nothing, res)
                    P = res(0)
                    mys.GetOverallProp("entropy", "Mole", res)
                    Sm = res(0) / AUX_MMM(Phase.Mixture)
                    Me.CurrentMaterialStream.Phases(0).Properties.entropy = Sm
                    Me.CurrentMaterialStream.Phases(0).Properties.pressure = P
                ElseIf spec1 = FlashSpec.P And spec2 = FlashSpec.VAP Then
                    mys.GetOverallProp("pressure", Nothing, res)
                    P = res(0)
                    Me.CurrentMaterialStream.Phases(0).Properties.pressure = P
                    mys.GetSinglePhaseProp("phaseFraction", "Vapor", "Mole", res)
                    Me.CurrentMaterialStream.Phases(2).Properties.molarfraction = res(0)
                End If

            End If

            Try
                Me.DW_CalcEquilibrium(spec1, spec2)
            Catch ex As Exception
                ThrowCAPEException(ex, ex.GetType.ToString, ex.ToString, "ICapeThermoEquilibriumRoutine", ex.ToString, "CalcEquilibrium", "", 0)
            End Try

            If Settings.CAPEOPENMode Then

                Dim ms As Interfaces.IMaterialStream = Me.CurrentMaterialStream
                Dim mo As ICapeThermoMaterial = _como

                Dim vok As Boolean = False
                Dim l1ok As Boolean = False
                Dim l2ok As Boolean = False
                Dim sok As Boolean = False

                If ms.Phases(2).Properties.molarfraction.GetValueOrDefault >= 1.0E-20 Then vok = True
                If ms.Phases(3).Properties.molarfraction.GetValueOrDefault >= 1.0E-20 Then l1ok = True
                If ms.Phases(4).Properties.molarfraction.GetValueOrDefault >= 1.0E-20 Then l2ok = True
                If ms.Phases(7).Properties.molarfraction.GetValueOrDefault >= 1.0E-20 Then sok = True

                If GlobalSettings.Settings.HideSolidPhaseFromCAPEOPENComponents Then sok = False

                Dim phases As String() = Nothing
                Dim statuses As CapePhaseStatus() = Nothing

                If vok And l1ok And l2ok Then
                    phases = New String() {"Vapor", "Liquid", "Liquid2"}
                    statuses = New CapePhaseStatus() {CapePhaseStatus.CAPE_ATEQUILIBRIUM, CapePhaseStatus.CAPE_ATEQUILIBRIUM, CapePhaseStatus.CAPE_ATEQUILIBRIUM}
                ElseIf vok And l1ok And Not l2ok Then
                    phases = New String() {"Vapor", "Liquid"}
                    statuses = New CapePhaseStatus() {CapePhaseStatus.CAPE_ATEQUILIBRIUM, CapePhaseStatus.CAPE_ATEQUILIBRIUM}
                ElseIf vok And l1ok And Not l2ok And sok Then
                    phases = New String() {"Vapor", "Liquid", "Solid"}
                    statuses = New CapePhaseStatus() {CapePhaseStatus.CAPE_ATEQUILIBRIUM, CapePhaseStatus.CAPE_ATEQUILIBRIUM, CapePhaseStatus.CAPE_ATEQUILIBRIUM}
                ElseIf vok And Not l1ok And l2ok Then
                    phases = New String() {"Vapor", "Liquid2"}
                    statuses = New CapePhaseStatus() {CapePhaseStatus.CAPE_ATEQUILIBRIUM, CapePhaseStatus.CAPE_ATEQUILIBRIUM}
                ElseIf Not vok And l1ok And l2ok Then
                    phases = New String() {"Liquid", "Liquid2"}
                    statuses = New CapePhaseStatus() {CapePhaseStatus.CAPE_ATEQUILIBRIUM, CapePhaseStatus.CAPE_ATEQUILIBRIUM}
                ElseIf vok And Not l1ok And Not l2ok Then
                    phases = New String() {"Vapor"}
                    statuses = New CapePhaseStatus() {CapePhaseStatus.CAPE_ATEQUILIBRIUM}
                ElseIf Not vok And l1ok And Not l2ok Then
                    phases = New String() {"Liquid"}
                    statuses = New CapePhaseStatus() {CapePhaseStatus.CAPE_ATEQUILIBRIUM}
                ElseIf vok And Not l1ok And sok Then
                    phases = New String() {"Vapor", "Solid"}
                    statuses = New CapePhaseStatus() {CapePhaseStatus.CAPE_ATEQUILIBRIUM, CapePhaseStatus.CAPE_ATEQUILIBRIUM}
                ElseIf Not vok And l1ok And sok Then
                    phases = New String() {"Liquid", "Solid"}
                    statuses = New CapePhaseStatus() {CapePhaseStatus.CAPE_ATEQUILIBRIUM, CapePhaseStatus.CAPE_ATEQUILIBRIUM}
                ElseIf Not vok And Not l1ok And l2ok Then
                    phases = New String() {"Liquid2"}
                    statuses = New CapePhaseStatus() {CapePhaseStatus.CAPE_ATEQUILIBRIUM}
                ElseIf Not vok And Not l1ok And sok Then
                    phases = New String() {"Solid"}
                    statuses = New CapePhaseStatus() {CapePhaseStatus.CAPE_ATEQUILIBRIUM}
                End If

                mo.SetPresentPhases(phases, statuses)

                Dim nc As Integer = ms.Phases(0).Compounds.Count
                T = ms.Phases(0).Properties.temperature.GetValueOrDefault
                P = ms.Phases(0).Properties.pressure.GetValueOrDefault

                Dim vz(nc - 1), pf As Double, i As Integer

                mo.SetOverallProp("temperature", Nothing, New Double() {T})
                mo.SetOverallProp("pressure", Nothing, New Double() {P})

                If vok Then

                    i = 0
                    For Each s As Interfaces.ICompound In ms.Phases(2).Compounds.Values
                        vz(i) = s.MoleFraction.GetValueOrDefault
                        i += 1
                    Next
                    pf = ms.Phases(2).Properties.molarfraction.GetValueOrDefault

                    mo.SetSinglePhaseProp("temperature", "Vapor", Nothing, New Double() {T})
                    mo.SetSinglePhaseProp("pressure", "Vapor", Nothing, New Double() {P})
                    mo.SetSinglePhaseProp("phasefraction", "Vapor", "Mole", New Double() {pf})
                    mo.SetSinglePhaseProp("fraction", "Vapor", "Mole", vz)

                End If

                If l1ok Then

                    i = 0
                    For Each s As Interfaces.ICompound In ms.Phases(3).Compounds.Values
                        vz(i) = s.MoleFraction.GetValueOrDefault
                        i += 1
                    Next
                    pf = ms.Phases(3).Properties.molarfraction.GetValueOrDefault

                    mo.SetSinglePhaseProp("temperature", "Liquid", Nothing, New Double() {T})
                    mo.SetSinglePhaseProp("pressure", "Liquid", Nothing, New Double() {P})
                    mo.SetSinglePhaseProp("phasefraction", "Liquid", "Mole", New Double() {pf})
                    mo.SetSinglePhaseProp("fraction", "Liquid", "Mole", vz)

                End If

                If l2ok Then

                    i = 0
                    For Each s As Interfaces.ICompound In ms.Phases(4).Compounds.Values
                        vz(i) = s.MoleFraction.GetValueOrDefault
                        i += 1
                    Next
                    pf = ms.Phases(4).Properties.molarfraction.GetValueOrDefault

                    mo.SetSinglePhaseProp("temperature", "Liquid2", Nothing, New Double() {T})
                    mo.SetSinglePhaseProp("pressure", "Liquid2", Nothing, New Double() {P})
                    mo.SetSinglePhaseProp("phasefraction", "Liquid2", "Mole", New Double() {pf})
                    mo.SetSinglePhaseProp("fraction", "Liquid2", "Mole", vz)

                End If

                If sok Then

                    i = 0
                    For Each s As Interfaces.ICompound In ms.Phases(7).Compounds.Values
                        vz(i) = s.MoleFraction.GetValueOrDefault
                        i += 1
                    Next
                    pf = ms.Phases(7).Properties.molarfraction.GetValueOrDefault

                    mo.SetSinglePhaseProp("temperature", "Solid", Nothing, New Double() {T})
                    mo.SetSinglePhaseProp("pressure", "Solid", Nothing, New Double() {P})
                    mo.SetSinglePhaseProp("phasefraction", "Solid", "Mole", New Double() {pf})
                    mo.SetSinglePhaseProp("fraction", "Solid", "Mole", vz)

                End If

            End If

        End Sub

        ''' <summary>
        ''' Checks whether the Property Package can support a particular type of Equilibrium Calculation.
        ''' </summary>
        ''' <param name="specification1">First specification for the Equilibrium Calculation.</param>
        ''' <param name="specification2">Second specification for the Equilibrium Calculation.</param>
        ''' <param name="solutionType">The required solution type.</param>
        ''' <returns>Set to True if the combination of specifications and solutionType is supported 
        ''' for a particular combination of present phases or False if not supported.</returns>
        ''' <remarks>The meaning of the specification1, specification2 and solutionType arguments is the same as
        ''' for the CalcEquilibrium method. If solutionType, specification1 and specification2
        ''' arguments appear valid but the actual specifications are not supported or not recognised a
        ''' False value should be returned.
        ''' The result of the check should depend primarily on the capabilities and configuration
        ''' (compounds and phases supported) of the component that implements the ICapeThermo-
        ''' EquilibriumRoutine interface (egg. a Property package). A component that supports
        ''' calculation specifications for any combination of supported phases is capable of checking
        ''' the specification without any reference to a Material Object. However, it is possible that
        ''' there may be restrictions on the combinations of phases supported in an equilibrium
        ''' calculation. For example a component may support vapor-liquid and liquid-liquid
        ''' calculations but not vapor-liquid-liquid calculations. In general it is therefore a necessary
        ''' prerequisite that a Material Object has been set (using the SetMaterial method of the
        ''' ICapeThermoMaterialContext interface) and that the SetPresentPhases method of the
        ''' ICapeThermoMaterial interface has been called to specify the combination of phases for the
        ''' equilibrium calculation. The result of the check should not depend on the state (temperature,
        ''' pressure, composition etc.) of the Material Object.</remarks>
        Public Overridable Function CheckEquilibriumSpec(ByVal specification1 As Object, ByVal specification2 As Object, ByVal solutionType As String) As Boolean Implements ICapeThermoEquilibriumRoutine.CheckEquilibriumSpec
            If specification1(0).ToString.ToLower = "temperature" And specification2(0).ToString.ToLower = "pressure" Then
                Return True
            ElseIf specification1(0).ToString.ToLower = "pressure" And specification2(0).ToString.ToLower = "enthalpy" Then
                Return True
            ElseIf specification1(0).ToString.ToLower = "pressure" And specification2(0).ToString.ToLower = "entropy" Then
                Return True
            ElseIf specification1(0).ToString.ToLower = "pressure" And specification2(0).ToString.ToLower = "phasefraction" Then
                Return True
            ElseIf specification1(0).ToString.ToLower = "temperature" And specification2(0).ToString.ToLower = "phasefraction" Then
                Return True
            ElseIf specification2(0).ToString.ToLower = "temperature" And specification1(0).ToString.ToLower = "pressure" Then
                Return True
            ElseIf specification2(0).ToString.ToLower = "pressure" And specification1(0).ToString.ToLower = "enthalpy" Then
                Return True
            ElseIf specification2(0).ToString.ToLower = "pressure" And specification1(0).ToString.ToLower = "entropy" Then
                Return True
            ElseIf specification2(0).ToString.ToLower = "pressure" And specification1(0).ToString.ToLower = "phasefraction" Then
                Return True
            ElseIf specification2(0).ToString.ToLower = "temperature" And specification1(0).ToString.ToLower = "phasefraction" Then
                Return True
            Else
                Return False
            End If
        End Function

        ''' <summary>
        ''' Allows the client of a component that implements this interface to pass an ICapeThermoMaterial 
        ''' interface to the component, so that it can access the properties of a Material.
        ''' </summary>
        ''' <param name="material">The Material interface.</param>
        ''' <remarks>The SetMaterial method allows a Thermodynamic and Physical Properties component, such
        ''' as a Property Package, to be given the ICapeThermoMaterial interface of a Material Object.
        ''' This interface gives the component access to the description of the Material for which
        ''' Property Calculations or Equilibrium Calculations are required. The component can access
        ''' property values directly using this interface. A client can also use the ICapeThermoMaterial
        ''' interface to query a Material Object for its ICapeThermoCompounds and ICapeThermo-
        ''' Phases interfaces, which provide access to Compound and Phase information, respectively.
        ''' It is envisaged that the SetMaterial method will be used to check that the Material Interface
        ''' supplied is valid and useable. For example, a Property Package may check that there are
        ''' some Compounds in a Material Object and that those Compounds can be identified by the
        ''' Property Package. In addition a Property Package may perform any initialisation that
        ''' depends on the configuration of a Material Object. A Property Calculator component might
        ''' typically use this method to query the Material Object for any required information
        ''' concerning the Compounds.
        ''' Calling the UnsetMaterial method of the ICapeThermoMaterialContext interface has the
        ''' effect of removing the interface set by the SetMaterial method.
        ''' After a call to SetMaterial() has been received, the object implementing the ICapeThermo-
        ''' MaterialContext interface can assume that the number, name and order of compounds for
        ''' that Material Object will remain fixed until the next call to SetMaterial() or UnsetMaterial().</remarks>
        Public Overridable Sub SetMaterial(ByVal material As Object) Implements ICapeThermoMaterialContext.SetMaterial
            Me.CurrentMaterialStream = Nothing
            If _como IsNot Nothing Then
                If System.Runtime.InteropServices.Marshal.IsComObject(_como) Then
                    System.Runtime.InteropServices.Marshal.ReleaseComObject(_como)
                End If
            End If
            If Settings.CAPEOPENMode Then
                _como = material
                Me.CurrentMaterialStream = COMaterialtoDWMaterial(material)
            Else
                Me.CurrentMaterialStream = material
            End If
        End Sub

        ''' <summary>
        ''' Removes any previously set Material interface.
        ''' </summary>
        ''' <remarks>The UnsetMaterial method removes any Material interface previously set by a call to the
        ''' SetMaterial method of the ICapeThermoMaterialContext interface. This means that any
        ''' methods of other interfaces that depend on having a valid Material Interface, for example
        ''' methods of the ICapeThermoPropertyRoutine or ICapeThermoEquilibriumRoutine
        ''' interfaces, should behave in the same way as if the SetMaterial method had never been
        ''' called.
        ''' If UnsetMaterial is called before a call to SetMaterial it has no effect and no exception
        ''' should be raised.</remarks>
        Public Overridable Sub UnsetMaterial() Implements ICapeThermoMaterialContext.UnsetMaterial
            Me.CurrentMaterialStream = Nothing
            If Settings.CAPEOPENMode Then
                If _como IsNot Nothing Then
                    If System.Runtime.InteropServices.Marshal.IsComObject(_como) Then
                        System.Runtime.InteropServices.Marshal.ReleaseComObject(_como)
                    End If
                End If
            End If
        End Sub

        ''' <summary>
        ''' Converts a COM Material Object into a DWSIM Material Stream.
        ''' </summary>
        ''' <param name="material">The Material Object to convert from</param>
        ''' <returns>A DWSIM Material Stream</returns>
        ''' <remarks>This function is called by SetMaterial when DWSIM Property Packages are working in outside environments (CAPE-OPEN COSEs) like COCO/COFE.</remarks>
        Public Function COMaterialtoDWMaterial(ByVal material As Object) As Interfaces.IMaterialStream

            If TryCast(material, Interfaces.IMaterialStream) Is Nothing Then

                Dim ms As New Streams.MaterialStream("", "")

                For Each phase In ms.Phases.Values
                    For Each tmpcomp In _selectedcomps.Values
                        phase.Compounds.Add(tmpcomp.Name, New BaseClasses.Compound(tmpcomp.Name, ""))
                        phase.Compounds(tmpcomp.Name).ConstantProperties = tmpcomp
                    Next
                Next

                Me.CurrentMaterialStream = ms

                Return ms

            Else

                Return material

            End If

        End Function

        Public Sub COMaterial_GetCompositions()

            Dim mys As ICapeThermoMaterial = _como

            'transfer values

            Dim Tv, Tl1, Tl2, Ts, Pv, Pl1, Pl2, Ps, xv, xl1, xl2, xs As Double
            Dim Vz As Object = Nothing
            Dim Vy As Object = Nothing
            Dim Vx1 As Object = Nothing
            Dim Vx2 As Object = Nothing
            Dim Vs As Object = Nothing
            Dim Vwy As Object = Nothing
            Dim Vwx1 As Object = Nothing
            Dim Vwx2 As Object = Nothing
            Dim Vws As Object = Nothing
            Dim labels As Object = Nothing
            Dim statuses As Object = Nothing
            Dim res As Object = Nothing

            Try
                mys.GetOverallProp("fraction", "Mole", res)
                Vz = res
            Catch ex As Exception
                Vz = RET_NullVector()
            End Try

            mys.GetPresentPhases(labels, statuses)

            Dim data(0) As Double

            Dim i As Integer = 0
            Dim n As Integer = UBound(labels)

            For i = 0 To n
                If statuses(i) = CapeOpen.CapePhaseStatus.CAPE_ATEQUILIBRIUM Then
                    Select Case labels(i)
                        Case "Vapor"
                            mys.GetTPFraction(labels(i), Tv, Pv, Vy)
                        Case "Liquid"
                            mys.GetTPFraction(labels(i), Tl1, Pl1, Vx1)
                        Case "Liquid2"
                            mys.GetTPFraction(labels(i), Tl2, Pl2, Vx2)
                        Case "Solid"
                            mys.GetTPFraction(labels(i), Ts, Ps, Vs)
                    End Select
                    Select Case labels(i)
                        Case "Vapor"
                            mys.GetSinglePhaseProp("phasefraction", labels(i), "Mole", res)
                            xv = res(0)
                        Case "Liquid"
                            mys.GetSinglePhaseProp("phasefraction", labels(i), "Mole", res)
                            xl1 = res(0)
                        Case "Liquid2"
                            mys.GetSinglePhaseProp("phasefraction", labels(i), "Mole", res)
                            xl2 = res(0)
                        Case "Solid"
                            mys.GetSinglePhaseProp("phasefraction", labels(i), "Mole", res)
                            xs = res(0)
                    End Select
                Else
                    Select Case labels(i)
                        Case "Vapor"
                            xv = 0.0#
                        Case "Liquid"
                            xl1 = 0.0#
                        Case "Liquid2"
                            xl2 = 0.0#
                        Case "Solid"
                            xs = 0.0#
                    End Select
                End If
            Next

            'copy fractions

            With Me.CurrentMaterialStream
                i = 0
                For Each s As Interfaces.ICompound In .Phases(0).Compounds.Values
                    s.MoleFraction = Vz(i)
                    i += 1
                Next
                If Vy IsNot Nothing Then
                    i = 0
                    For Each s As Interfaces.ICompound In .Phases(2).Compounds.Values
                        s.MoleFraction = Vy(i)
                        i += 1
                    Next
                    Vwy = Me.AUX_CONVERT_MOL_TO_MASS(Vy)
                    i = 0
                    For Each s As Interfaces.ICompound In .Phases(2).Compounds.Values
                        s.MassFraction = Vwy(i)
                        i += 1
                    Next
                Else
                    i = 0
                    For Each s As Interfaces.ICompound In .Phases(2).Compounds.Values
                        s.MoleFraction = 0.0#
                        i += 1
                    Next
                    i = 0
                    For Each s As Interfaces.ICompound In .Phases(2).Compounds.Values
                        s.MassFraction = 0.0#
                        i += 1
                    Next
                End If
                .Phases(2).Properties.molarfraction = xv
                If Vx1 IsNot Nothing Then
                    i = 0
                    For Each s As Interfaces.ICompound In .Phases(3).Compounds.Values
                        s.MoleFraction = Vx1(i)
                        i += 1
                    Next
                    Vwx1 = Me.AUX_CONVERT_MOL_TO_MASS(Vx1)
                    i = 0
                    For Each s As Interfaces.ICompound In .Phases(3).Compounds.Values
                        s.MassFraction = Vwx1(i)
                        i += 1
                    Next
                Else
                    i = 0
                    For Each s As Interfaces.ICompound In .Phases(3).Compounds.Values
                        s.MoleFraction = 0.0#
                        i += 1
                    Next
                    i = 0
                    For Each s As Interfaces.ICompound In .Phases(3).Compounds.Values
                        s.MassFraction = 0.0#
                        i += 1
                    Next
                End If
                .Phases(3).Properties.molarfraction = xl1
                If Vx2 IsNot Nothing Then
                    i = 0
                    For Each s As Interfaces.ICompound In .Phases(4).Compounds.Values
                        s.MoleFraction = Vx2(i)
                        i += 1
                    Next
                    Vwx2 = Me.AUX_CONVERT_MOL_TO_MASS(Vx2)
                    i = 0
                    For Each s As Interfaces.ICompound In .Phases(4).Compounds.Values
                        s.MassFraction = Vwx2(i)
                        i += 1
                    Next
                Else
                    i = 0
                    For Each s As Interfaces.ICompound In .Phases(4).Compounds.Values
                        s.MoleFraction = 0.0#
                        i += 1
                    Next
                    i = 0
                    For Each s As Interfaces.ICompound In .Phases(4).Compounds.Values
                        s.MassFraction = 0.0#
                        i += 1
                    Next
                End If
                .Phases(4).Properties.molarfraction = xl2
                If Vs IsNot Nothing Then
                    i = 0
                    For Each s As Interfaces.ICompound In .Phases(7).Compounds.Values
                        s.MoleFraction = Vs(i)
                        i += 1
                    Next
                    Vws = Me.AUX_CONVERT_MOL_TO_MASS(Vs)
                    i = 0
                    For Each s As Interfaces.ICompound In .Phases(7).Compounds.Values
                        s.MassFraction = Vws(i)
                        i += 1
                    Next
                Else
                    i = 0
                    For Each s As Interfaces.ICompound In .Phases(7).Compounds.Values
                        s.MoleFraction = 0.0#
                        i += 1
                    Next
                    i = 0
                    For Each s As Interfaces.ICompound In .Phases(7).Compounds.Values
                        s.MassFraction = 0.0#
                        i += 1
                    Next
                End If
                .Phases(7).Properties.molarfraction = xs
            End With

        End Sub

#End Region

#Region "   CAPE-OPEN ICapeUtilities Implementation"

        Public _availablecomps As New Dictionary(Of String, BaseClasses.ConstantProperties)
        Public _selectedcomps As New Dictionary(Of String, BaseClasses.ConstantProperties)

        <System.NonSerialized()> Protected Friend _pme As Object

        ''' <summary>
        ''' The PMC displays its user interface and allows the Flowsheet User to interact with it. If no user interface is
        ''' available it returns an error.</summary>
        ''' <remarks></remarks>
        Public Overridable Sub Edit() Implements CapeOpen.ICapeUtilities.Edit
            Dim cf As New FormConfigCAPEOPENPPackage With {._pp = Me}
            cf.ShowDialog()
        End Sub

        ''' <summary>
        ''' Initially, this method was only present in the ICapeUnit interface. Since ICapeUtilities.Initialize is now
        ''' available for any kind of PMC, ICapeUnit. Initialize is deprecated.
        ''' The PME will order the PMC to get initialized through this method. Any initialisation that could fail must be
        ''' placed here. Initialize is guaranteed to be the first method called by the client (except low level methods such
        ''' as class constructors or initialization persistence methods). Initialize has to be called once when the PMC is
        ''' instantiated in a particular flowsheet.
        ''' When the initialization fails, before signalling an error, the PMC must free all the resources that were
        ''' allocated before the failure occurred. When the PME receives this error, it may not use the PMC anymore.
        ''' The method terminate of the current interface must not either be called. Hence, the PME may only release
        ''' the PMC through the middleware native mechanisms.
        ''' </summary>
        ''' <remarks></remarks>
        Public Overridable Sub Initialize() Implements CapeOpen.ICapeUtilities.Initialize

            Me.m_ip = New DataTable

            ConfigParameters()

            Me.FlashSettings = Auxiliary.FlashAlgorithms.FlashAlgorithm.GetDefaultSettings()

            'load Henry Coefficients

            Dim pathsep = IO.Path.DirectorySeparatorChar

            Dim HenryLines() As String

            SyncLock m_Henry

                m_Henry.Clear()

                Dim t0 As Type = Type.GetType("DWSIM.Thermodynamics.PropertyPackages.PropertyPackage")

                Using filestr As Stream = Assembly.GetAssembly(t0).GetManifestResourceStream("DWSIM.Thermodynamics.henry_constants.csv")
                    Using t As New StreamReader(filestr)
                        HenryLines = t.ReadToEnd().Split(vbLf)
                    End Using
                End Using

                For i = 3 To HenryLines.Length - 2
                    Dim HP As New HenryParam
                    Dim cols = HenryLines(i).Split(",")
                    HP.Component = cols(4).Replace(Chr(34), "")
                    HP.CAS = cols(10).Replace(Chr(34), "")
                    Dim val1 = cols(1).Replace(Chr(34), "")
                    Dim val2 = cols(3).Replace(Chr(34), "")
                    Dim val3 = cols(2).Replace(Chr(34), "")
                    If val1 <> "" And val3 <> "" And val2.Contains("L") Then
                        HP.KHcp = val1.ToDoubleFromInvariant()
                        HP.C = val3.ToDoubleFromInvariant()
                        If Not m_Henry.ContainsKey(HP.CAS) Then
                            m_Henry.Add(HP.CAS, HP)
                        End If
                    End If
                Next

            End SyncLock

            If Settings.CAPEOPENMode And Not Settings.ExcelMode Then

                'add default compounds

                AddDefaultCompounds(New String() {"Methane", "Ethane", "Propane"})

            End If

        End Sub

        ''' <summary>
        ''' Returns an ICapeCollection interface.
        ''' </summary>
        ''' <value></value>
        ''' <returns>CapeInterface (ICapeCollection)</returns>
        ''' <remarks>This interface will contain a collection of ICapeParameter interfaces.
        ''' This method allows any client to access all the CO Parameters exposed by a PMC. Initially, this method was
        ''' only present in the ICapeUnit interface. Since ICapeUtilities.GetParameters is now available for any kind of
        ''' PMC, ICapeUnit.GetParameters is deprecated. Consult the “Open Interface Specification: Parameter
        ''' Common Interface” document for more information about parameter. Consult the “Open Interface
        ''' Specification: Collection Common Interface” document for more information about collection.
        ''' If the PMC does not support exposing its parameters, it should raise the ECapeNoImpl error, instead of
        ''' returning a NULL reference or an empty Collection. But if the PMC supports parameters but has for this call
        ''' no parameters, it should return a valid ICapeCollection reference exposing zero parameters.</remarks>
        Public Overridable ReadOnly Property parameters1() As Object Implements CapeOpen.ICapeUtilities.parameters
            Get
                Dim parms As New CapeOpen.ParameterCollection
                Return parms
            End Get
        End Property

        ''' <summary>
        ''' Allows the PME to convey the PMC a reference to the former’s simulation context. 
        ''' </summary>
        ''' <value>The reference to the PME’s simulation context class. For the PMC to
        ''' use this class, this reference will have to be converted to each of the
        ''' defined CO Simulation Context interfaces.</value>
        ''' <remarks>The simulation context
        ''' will be PME objects which will expose a given set of CO interfaces. Each of these interfaces will allow the
        ''' PMC to call back the PME in order to benefit from its exposed services (such as creation of material
        ''' templates, diagnostics or measurement unit conversion). If the PMC does not support accessing the
        ''' simulation context, it is recommended to raise the ECapeNoImpl error.
        ''' Initially, this method was only present in the ICapeUnit interface. Since ICapeUtilities.SetSimulationContext
        ''' is now available for any kind of PMC, ICapeUnit. SetSimulationContext is deprecated.</remarks>
        Public Overridable WriteOnly Property simulationContext() As Object Implements CapeOpen.ICapeUtilities.simulationContext
            Set(ByVal value As Object)
                _pme = value
            End Set
        End Property

        ''' <summary>
        ''' Initially, this method was only present in the ICapeUnit interface. Since ICapeUtilities.Terminate is now
        ''' available for any kind of PMC, ICapeUnit.Terminate is deprecated.
        ''' The PME will order the PMC to get destroyed through this method. Any uninitialization that could fail must
        ''' be placed here. ‘Terminate’ is guaranteed to be the last method called by the client (except low level methods
        ''' such as class destructors). ‘Terminate’ may be called at any time, but may be only called once.
        ''' When this method returns an error, the PME should report the user. However, after that the PME is not
        ''' allowed to use the PMC anymore.
        ''' The Unit specification stated that “Terminate may check if the data has been saved and return an error if
        ''' not.” It is suggested not to follow this recommendation, since it’s the PME responsibility to save the state of
        ''' the PMC before terminating it. In the case that a user wants to close a simulation case without saving it, it’s
        ''' better to leave the PME to handle the situation instead of each PMC providing a different implementation.
        ''' </summary>
        ''' <remarks></remarks>
        Public Overridable Sub Terminate() Implements CapeOpen.ICapeUtilities.Terminate
            Me.CurrentMaterialStream = Nothing
            If _como IsNot Nothing Then System.Runtime.InteropServices.Marshal.ReleaseComObject(_como)
            If Not _pme Is Nothing Then
                If System.Runtime.InteropServices.Marshal.IsComObject(_pme) Then
                    System.Runtime.InteropServices.Marshal.ReleaseComObject(_pme)
                End If
            End If
            Me.simulationContext = Nothing
            Me.Dispose()
        End Sub

#End Region

#Region "   CAPE-OPEN Persistence Implementation"

        Private m_dirty As Boolean = True

        Public Sub GetClassID(ByRef pClassID As System.Guid) Implements IPersistStreamInit.GetClassID
            pClassID = New Guid(PropertyPackage.ClassId)
        End Sub

        Public Sub GetSizeMax(ByRef pcbSize As Long) Implements IPersistStreamInit.GetSizeMax
            pcbSize = 1024 * 1024
        End Sub

        Public Sub InitNew() Implements IPersistStreamInit.InitNew
            'do nothing
        End Sub

        Public Function IsDirty() As Integer Implements IPersistStreamInit.IsDirty
            Return m_dirty
        End Function

        Public Sub Load(ByVal pStm As System.Runtime.InteropServices.ComTypes.IStream) Implements IPersistStreamInit.Load

            Try

                Dim mySerializer As Binary.BinaryFormatter = New Binary.BinaryFormatter(Nothing, New System.Runtime.Serialization.StreamingContext())

                Dim domain As AppDomain = AppDomain.CurrentDomain
                AddHandler domain.AssemblyResolve, New ResolveEventHandler(AddressOf MyResolveEventHandler)

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

                Dim myarr As ArrayList

                Using memoryStream As New System.IO.MemoryStream(bytes)

                    myarr = mySerializer.Deserialize(memoryStream)

                End Using

                _availablecomps = myarr(0)
                _selectedcomps = myarr(1)
                '_ioquick = myarr(2)

                Dim xmldoc = XDocument.Parse(myarr(6))
                Dim fadata As List(Of XElement) = xmldoc.Element("Data").Elements.ToList
                Try
                    _FlashAlgorithm = ReturnInstance(fadata.Where(Function(x) x.Name = "Type").FirstOrDefault.Value)
                    DirectCast(_FlashAlgorithm, Interfaces.ICustomXMLSerialization).LoadData(fadata)
                Catch ex As Exception
                End Try

                Select Case Me.ComponentName
                    Case "Peng-Robinson (PR)"
                        CType(Me, PengRobinsonPropertyPackage).m_pr = myarr(7)
                    Case "Peng-Robinson-Stryjek-Vera 2 (PRSV2-M)", "Peng-Robinson-Stryjek-Vera 2 (PRSV2)"
                        CType(Me, PRSV2PropertyPackage).m_pr = myarr(7)
                    Case "Peng-Robinson-Stryjek-Vera 2 (PRSV2-VL)"
                        CType(Me, PRSV2VLPropertyPackage).m_pr = myarr(7)
                    Case "Soave-Redlich-Kwong (SRK)"
                        CType(Me, SRKPropertyPackage).m_pr = myarr(7)
                    Case "Peng-Robinson / Lee-Kesler (PR/LK)"
                        CType(Me, PengRobinsonLKPropertyPackage).m_pr = myarr(7)
                        CType(Me, PengRobinsonLKPropertyPackage).m_lk = myarr(8)
                    Case "UNIFAC"
                        CType(Me, UNIFACPropertyPackage).m_pr = myarr(7)
                    Case "UNIFAC-LL"
                        CType(Me, UNIFACLLPropertyPackage).m_pr = myarr(7)
                    Case "NRTL"
                        CType(Me, NRTLPropertyPackage).m_pr = myarr(7)
                        CType(Me, NRTLPropertyPackage).m_uni = myarr(8)
                    Case "UNIQUAC"
                        CType(Me, UNIQUACPropertyPackage).m_pr = myarr(7)
                        CType(Me, UNIQUACPropertyPackage).m_uni = myarr(8)
                    Case "Modified UNIFAC (Dortmund)"
                        CType(Me, MODFACPropertyPackage).m_pr = myarr(7)
                    Case "Chao-Seader"
                        CType(Me, ChaoSeaderPropertyPackage).m_pr = myarr(7)
                        CType(Me, ChaoSeaderPropertyPackage).m_lk = myarr(8)
                        CType(Me, ChaoSeaderPropertyPackage).m_cs = myarr(9)
                    Case "Grayson-Streed"
                        CType(Me, GraysonStreedPropertyPackage).m_pr = myarr(7)
                        CType(Me, GraysonStreedPropertyPackage).m_lk = myarr(8)
                        CType(Me, GraysonStreedPropertyPackage).m_cs = myarr(9)
                    Case "Lee-Kesler-Plöcker"
                        CType(Me, LKPPropertyPackage).m_pr = myarr(7)
                        CType(Me, LKPPropertyPackage).m_lk = myarr(8)
                    Case "Raoult's Law", "IAPWS-IF97 Steam Tables"
                End Select

                myarr = Nothing
                mySerializer = Nothing

                RemoveHandler domain.AssemblyResolve, New ResolveEventHandler(AddressOf MyResolveEventHandler)

            Catch p_Ex As System.Exception

                Dim hcode As Integer = 0
                Dim comEx As COMException = New COMException(p_Ex.Message.ToString, p_Ex)
                If Not IsNothing(comEx) Then hcode = comEx.ErrorCode
                ThrowCAPEException(p_Ex, "Error", p_Ex.Message, "IPersistStream", p_Ex.Source, p_Ex.StackTrace, "Load", hcode)

            End Try

        End Sub

        Public Sub Save(ByVal pStm As System.Runtime.InteropServices.ComTypes.IStream, ByVal fClearDirty As Boolean) Implements IPersistStreamInit.Save
            Try

                Dim props As New ArrayList

                With props

                    .Add(_availablecomps)
                    .Add(_selectedcomps)
                    .Add("")
                    .Add(Nothing)

                    Dim xdata As New XDocument()
                    xdata.AddFirst(New XElement("Data"))
                    xdata.Element("Data").Add(DirectCast(FlashBase, Interfaces.ICustomXMLSerialization).SaveData())

                    .Add(xdata.ToString)

                    Select Case Me.ComponentName
                        Case "Peng-Robinson (PR)"
                            .Add(CType(Me, PengRobinsonPropertyPackage).m_pr)
                        Case "Peng-Robinson-Stryjek-Vera 2 (PRSV2-M)", "Peng-Robinson-Stryjek-Vera 2 (PRSV2)"
                            .Add(CType(Me, PRSV2PropertyPackage).m_pr)
                        Case "Peng-Robinson-Stryjek-Vera 2 (PRSV2-VL)"
                            .Add(CType(Me, PRSV2VLPropertyPackage).m_pr)
                        Case "Soave-Redlich-Kwong (SRK)"
                            .Add(CType(Me, SRKPropertyPackage).m_pr)
                        Case "Peng-Robinson / Lee-Kesler (PR/LK)"
                            .Add(CType(Me, PengRobinsonLKPropertyPackage).m_pr)
                            .Add(CType(Me, PengRobinsonLKPropertyPackage).m_lk)
                        Case "UNIFAC"
                            .Add(CType(Me, UNIFACPropertyPackage).m_pr)
                        Case "UNIFAC-LL"
                            .Add(CType(Me, UNIFACLLPropertyPackage).m_pr)
                        Case "NRTL"
                            .Add(CType(Me, NRTLPropertyPackage).m_pr)
                            .Add(CType(Me, NRTLPropertyPackage).m_uni)
                        Case "UNIQUAC"
                            .Add(CType(Me, UNIQUACPropertyPackage).m_pr)
                            .Add(CType(Me, UNIQUACPropertyPackage).m_uni)
                        Case "Modified UNIFAC (Dortmund)"
                            .Add(CType(Me, MODFACPropertyPackage).m_pr)
                        Case "Chao-Seader"
                            .Add(CType(Me, ChaoSeaderPropertyPackage).m_pr)
                            .Add(CType(Me, ChaoSeaderPropertyPackage).m_lk)
                            .Add(CType(Me, ChaoSeaderPropertyPackage).m_cs)
                        Case "Grayson-Streed"
                            .Add(CType(Me, GraysonStreedPropertyPackage).m_pr)
                            .Add(CType(Me, GraysonStreedPropertyPackage).m_lk)
                            .Add(CType(Me, GraysonStreedPropertyPackage).m_cs)
                        Case "Lee-Kesler-Plöcker"
                            .Add(CType(Me, LKPPropertyPackage).m_pr)
                            .Add(CType(Me, LKPPropertyPackage).m_lk)
                        Case "Raoult's Law", "IAPWS-IF97 Steam Tables"
                    End Select

                End With

                Dim mySerializer As Binary.BinaryFormatter = New Binary.BinaryFormatter(Nothing, New System.Runtime.Serialization.StreamingContext())
                Dim mstr As New MemoryStream
                mySerializer.Serialize(mstr, props)
                Dim bytes As Byte() = mstr.ToArray()
                mstr.Close()

                ' construct length (separate into two separate bytes)    

                Dim arrLen As Byte() = BitConverter.GetBytes(bytes.Length)

                ' Save the array in the stream    
                pStm.Write(arrLen, arrLen.Length, IntPtr.Zero)
                pStm.Write(bytes, bytes.Length, IntPtr.Zero)
                If System.Runtime.InteropServices.Marshal.IsComObject(pStm) Then System.Runtime.InteropServices.Marshal.ReleaseComObject(pStm)

            Catch p_Ex As System.Exception

                Dim hcode As Integer = 0
                Dim comEx As COMException = New COMException(p_Ex.Message.ToString, p_Ex)
                If Not IsNothing(comEx) Then hcode = comEx.ErrorCode
                ThrowCAPEException(p_Ex, "Error", p_Ex.Message, "IPersistStreamInit", p_Ex.Source, p_Ex.StackTrace, "Save", hcode)

            End Try

            If fClearDirty Then
                m_dirty = False
            End If

        End Sub

        Private Function MyResolveEventHandler(ByVal sender As Object, ByVal args As ResolveEventArgs) As System.Reflection.Assembly
            Return Me.[GetType]().Assembly
        End Function

#End Region

#Region "   CAPE-OPEN Error Interfaces"

        Sub ThrowCAPEException(ByRef ex As Exception, ByVal name As String, ByVal description As String, ByVal interf As String, ByVal moreinfo As String, ByVal operation As String, ByVal scope As String, ByVal code As Integer)

            _name = name
            _code = code
            _description = description
            _interfacename = interf
            _moreinfo = moreinfo
            _operation = operation
            _scope = scope

            ExceptionLog += Date.Now + ": " + vbCrLf + vbCrLf + ex.ToString + vbCrLf + vbCrLf

            Throw New CapeComputationException(ex.Message.ToString, ex)

        End Sub

        Private _name, _description, _interfacename, _moreinfo, _operation, _scope As String, _code As Integer

        Public ReadOnly Property Name() As String Implements CapeOpen.ECapeRoot.Name, IPropertyPackage.Name
            Get
                Return _name
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

#Region "   IDisposable Support "
        Private disposedValue As Boolean = False        ' To detect redundant calls

        ' IDisposable
        Protected Overridable Sub Dispose(ByVal disposing As Boolean)
            If Not Me.disposedValue Then
                If disposing Then
                    ' TODO: free other state (managed objects).
                End If

                ' TODO: free your own state (unmanaged objects).
                If _como IsNot Nothing Then
                    If System.Runtime.InteropServices.Marshal.IsComObject(_como) Then
                        System.Runtime.InteropServices.Marshal.ReleaseComObject(_como)
                    Else
                        _como = Nothing
                    End If
                End If
                If _pme IsNot Nothing Then
                    If System.Runtime.InteropServices.Marshal.IsComObject(_pme) Then
                        System.Runtime.InteropServices.Marshal.ReleaseComObject(_pme)
                    Else
                        _pme = Nothing
                    End If
                End If


                ' TODO: set large fields to null.
            End If
            Me.disposedValue = True
        End Sub

#Region " IDisposable Support "
        ' This code added by Visual Basic to correctly implement the disposable pattern.
        Public Overridable Sub Dispose() Implements IDisposable.Dispose
            ' Do not change this code.  Put cleanup code in Dispose(ByVal disposing As Boolean) above.
            Dispose(True)
            GC.SuppressFinalize(Me)
        End Sub
#End Region

#End Region

#Region "   XML data persistence"

        Public Overridable Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean Implements Interfaces.ICustomXMLSerialization.LoadData

            Dim ci As Globalization.CultureInfo = Globalization.CultureInfo.InvariantCulture

            Dim uid_el = (From el As XElement In data Select el Where el.Name = "ID").FirstOrDefault()
            Dim tag_el = (From el As XElement In data Select el Where el.Name = "Tag").FirstOrDefault()

            If uid_el IsNot Nothing Then Me.UniqueID = uid_el.Value
            If tag_el IsNot Nothing Then Me.Tag = tag_el.Value

            Me.ComponentName = (From el As XElement In data Select el Where el.Name = "ComponentName").FirstOrDefault.Value
            Me.ComponentDescription = (From el As XElement In data Select el Where el.Name = "ComponentDescription").FirstOrDefault.Value

            Dim e1 = (From el As XElement In data Select el Where el.Name = "Parameters").FirstOrDefault
            If e1 IsNot Nothing Then Me.ParametersXMLString = e1.ToString()

            e1 = (From el As XElement In data Select el Where el.Name = "UseHenryConstants").FirstOrDefault
            If e1 IsNot Nothing Then UseHenryConstants = e1.Value

            e1 = (From el As XElement In data Select el Where el.Name = "AutoEstimateMissingNRTLUNIQUACParameters").FirstOrDefault
            If e1 IsNot Nothing Then AutoEstimateMissingNRTLUNIQUACParameters = e1.Value

            e1 = (From el As XElement In data Select el Where el.Name = "SingleCompoundCheckThreshold").FirstOrDefault
            If e1 IsNot Nothing Then SingleCompoundCheckThreshold = e1.Value.ToDoubleFromInvariant()

            e1 = (From el As XElement In data Select el Where el.Name = "OverrideKvalFugCoeff").FirstOrDefault
            If e1 IsNot Nothing Then OverrideKvalFugCoeff = e1.Value

            e1 = (From el As XElement In data Select el Where el.Name = "OverrideEnthalpyCalculation").FirstOrDefault
            If e1 IsNot Nothing Then OverrideEnthalpyCalculation = e1.Value

            e1 = (From el As XElement In data Select el Where el.Name = "OverrideEntropyCalculation").FirstOrDefault
            If e1 IsNot Nothing Then OverrideEntropyCalculation = e1.Value

            e1 = (From el As XElement In data Select el Where el.Name = "LiquidDensity_CorrectExpDataForPressure").FirstOrDefault
            If e1 IsNot Nothing Then LiquidDensity_CorrectExpDataForPressure = e1.Value

            e1 = (From el As XElement In data Select el Where el.Name = "LiquidDensity_UsePenelouxVolumeTranslation").FirstOrDefault
            If e1 IsNot Nothing Then LiquidDensity_UsePenelouxVolumeTranslation = e1.Value

            e1 = (From el As XElement In data Select el Where el.Name = "LiquidDensity_UsePenelouxVolumeTranslation").FirstOrDefault
            If e1 IsNot Nothing Then LiquidViscosity_CorrectExpDataForPressure = e1.Value

            e1 = (From el As XElement In data Select el Where el.Name = "LiquidFugacity_UsePoyntingCorrectionFactor").FirstOrDefault
            If e1 IsNot Nothing Then LiquidFugacity_UsePoyntingCorrectionFactor = e1.Value

            e1 = (From el As XElement In data Select el Where el.Name = "SolidPhaseFugacity_UseIdealLiquidPhaseFugacity").FirstOrDefault
            If e1 IsNot Nothing Then SolidPhaseFugacity_UseIdealLiquidPhaseFugacity = e1.Value

            e1 = (From el As XElement In data Select el Where el.Name = "SolidPhaseEnthalpy_UsesCp").FirstOrDefault
            If e1 IsNot Nothing Then SolidPhaseEnthalpy_UsesCp = e1.Value

            e1 = (From el As XElement In data Select el Where el.Name = "ActivityCoefficientModels_IgnoreMissingInteractionParameters").FirstOrDefault
            If e1 IsNot Nothing Then ActivityCoefficientModels_IgnoreMissingInteractionParameters = e1.Value

            e1 = (From el As XElement In data Select el Where el.Name = "IgnoreVaporFractionLimit").FirstOrDefault
            If e1 IsNot Nothing Then IgnoreVaporFractionLimit = e1.Value

            e1 = (From el As XElement In data Select el Where el.Name = "IgnoreSalinityLimit").FirstOrDefault
            If e1 IsNot Nothing Then IgnoreSalinityLimit = e1.Value

            e1 = (From el As XElement In data Select el Where el.Name = "CalculateAdditionalMaterialStreamProperties").FirstOrDefault
            If e1 IsNot Nothing Then CalculateAdditionalMaterialStreamProperties = e1.Value

            If (From el As XElement In data Select el Where el.Name = "LiquidDensityCalculationMode_Supercritical").FirstOrDefault IsNot Nothing Then
                Try
                    LiquidDensityCalculationMode_Subcritical = [Enum].Parse(LiquidDensityCalculationMode_Subcritical.GetType, (From el As XElement In data Select el Where el.Name = "LiquidDensityCalculationMode_Supercritical").FirstOrDefault.Value)
                    LiquidDensityCalculationMode_Supercritical = [Enum].Parse(LiquidDensityCalculationMode_Supercritical.GetType, (From el As XElement In data Select el Where el.Name = "LiquidDensityCalculationMode_Supercritical").FirstOrDefault.Value)
                    LiquidViscosityCalculationMode_Subcritical = [Enum].Parse(LiquidViscosityCalculationMode_Subcritical.GetType, (From el As XElement In data Select el Where el.Name = "LiquidViscosityCalculationMode_Subcritical").FirstOrDefault.Value)
                    LiquidViscosityCalculationMode_Supercritical = [Enum].Parse(LiquidViscosityCalculationMode_Supercritical.GetType, (From el As XElement In data Select el Where el.Name = "LiquidViscosityCalculationMode_Supercritical").FirstOrDefault.Value)
                    LiquidViscosity_MixingRule = [Enum].Parse(LiquidViscosity_MixingRule.GetType, (From el As XElement In data Select el Where el.Name = "LiquidViscosity_MixingRule").FirstOrDefault.Value)
                    VaporPhaseFugacityCalculationMode = [Enum].Parse(VaporPhaseFugacityCalculationMode.GetType, (From el As XElement In data Select el Where el.Name = "VaporPhaseFugacityCalculationMode").FirstOrDefault.Value)
                    SolidPhaseFugacityCalculationMethod = [Enum].Parse(SolidPhaseFugacityCalculationMethod.GetType, (From el As XElement In data Select el Where el.Name = "SolidPhaseFugacityCalculationMethod").FirstOrDefault.Value)
                    EnthalpyEntropyCpCvCalculationMode = [Enum].Parse(EnthalpyEntropyCpCvCalculationMode.GetType, (From el As XElement In data Select el Where el.Name = "EnthalpyEntropyCpCvCalculationMode").FirstOrDefault.Value)
                Catch ex As Exception
                End Try
            End If

            If (From el As XElement In data Select el Where el.Name = "LiquidEnthalpyEntropyCpCvCalculationMode_EOS").FirstOrDefault IsNot Nothing Then
                Try
                    LiquidEnthalpyEntropyCpCvCalculationMode_EOS = [Enum].Parse(LiquidEnthalpyEntropyCpCvCalculationMode_EOS.GetType, (From el As XElement In data Select el Where el.Name = "LiquidEnthalpyEntropyCpCvCalculationMode_EOS").FirstOrDefault.Value)
                Catch ex As Exception
                End Try
            End If

            If (From el As XElement In data Select el Where el.Name = "FlashCalculationApproach").FirstOrDefault IsNot Nothing Then
                FlashCalculationApproach = [Enum].Parse(FlashCalculationApproach.GetType, (From el As XElement In data Select el Where el.Name = "FlashCalculationApproach").FirstOrDefault.Value)
            End If

            Dim jsonoptions As New JsonSerializerSettings With {.StringEscapeHandling = StringEscapeHandling.EscapeHtml, .Formatting = Formatting.Indented}

            Try
                Dim fselement = (From el As XElement In data Select el Where el.Name = "ForcedSolids").FirstOrDefault
                If fselement IsNot Nothing AndAlso fselement.Value <> "" Then
                    ForcedSolids = JsonConvert.DeserializeObject(Of List(Of String))(fselement.Value)
                Else
                    ForcedSolids = New List(Of String)
                End If
            Catch ex As Exception
            End Try

            Try
                Dim porrelement = (From el As XElement In data Select el Where el.Name = "PropertyOverrides").FirstOrDefault
                If porrelement IsNot Nothing AndAlso porrelement.Value <> "" Then
                    PropertyOverrides = JsonConvert.DeserializeObject(Of Dictionary(Of String, String))(porrelement.Value)
                Else
                    PropertyOverrides = New Dictionary(Of String, String)
                End If
            Catch ex As Exception
            End Try

            Select Case Me.ComponentName

                Case "Peng-Robinson (PR)"

                    Try
                        Dim pp As PengRobinsonPropertyPackage = Me
                        'pp.m_pr.InteractionParameters.Clear()
                        For Each xel As XElement In (From xel2 As XElement In data Select xel2 Where xel2.Name = "InteractionParameters").FirstOrDefault.Elements.ToList
                            Dim ip As New Auxiliary.PR_IPData() With {.Owner = Me.GetModel(), .kij = Double.Parse(xel.@Value, ci)}
                            Dim dic As New Dictionary(Of String, Auxiliary.PR_IPData)
                            dic.Add(xel.@Compound2, ip)
                            If Not pp.m_pr.InteractionParameters.ContainsKey(xel.@Compound1) Then
                                If Not pp.m_pr.InteractionParameters.ContainsKey(xel.@Compound2) Then
                                    pp.m_pr.InteractionParameters.Add(xel.@Compound1, dic)
                                Else
                                    If Not pp.m_pr.InteractionParameters(xel.@Compound2).ContainsKey(xel.@Compound1) Then
                                        pp.m_pr.InteractionParameters(xel.@Compound2).Add(xel.@Compound1, ip)
                                    Else
                                        pp.m_pr.InteractionParameters(xel.@Compound2)(xel.@Compound1) = ip
                                    End If
                                End If
                            Else
                                If Not pp.m_pr.InteractionParameters(xel.@Compound1).ContainsKey(xel.@Compound2) Then
                                    pp.m_pr.InteractionParameters(xel.@Compound1).Add(xel.@Compound2, ip)
                                Else
                                    pp.m_pr.InteractionParameters(xel.@Compound1)(xel.@Compound2) = ip
                                End If
                            End If
                        Next

                    Catch ex As Exception
                    End Try

                Case "Peng-Robinson 1978 (PR78)"

                    Try
                        Dim pp As PengRobinson1978PropertyPackage = Me
                        'pp.m_pr.InteractionParameters.Clear()
                        For Each xel As XElement In (From xel2 As XElement In data Select xel2 Where xel2.Name = "InteractionParameters").FirstOrDefault.Elements.ToList
                            Dim ip As New Auxiliary.PR_IPData() With {.Owner = Me.GetModel(), .kij = Double.Parse(xel.@Value, ci)}
                            Dim dic As New Dictionary(Of String, Auxiliary.PR_IPData)
                            dic.Add(xel.@Compound2, ip)
                            If Not pp.m_pr.InteractionParameters.ContainsKey(xel.@Compound1) Then
                                If Not pp.m_pr.InteractionParameters.ContainsKey(xel.@Compound2) Then
                                    pp.m_pr.InteractionParameters.Add(xel.@Compound1, dic)
                                Else
                                    If Not pp.m_pr.InteractionParameters(xel.@Compound2).ContainsKey(xel.@Compound1) Then
                                        pp.m_pr.InteractionParameters(xel.@Compound2).Add(xel.@Compound1, ip)
                                    Else
                                        pp.m_pr.InteractionParameters(xel.@Compound2)(xel.@Compound1) = ip
                                    End If
                                End If
                            Else
                                If Not pp.m_pr.InteractionParameters(xel.@Compound1).ContainsKey(xel.@Compound2) Then
                                    pp.m_pr.InteractionParameters(xel.@Compound1).Add(xel.@Compound2, ip)
                                Else
                                    pp.m_pr.InteractionParameters(xel.@Compound1)(xel.@Compound2) = ip
                                End If
                            End If
                        Next

                    Catch ex As Exception
                    End Try

                Case "Peng-Robinson-Stryjek-Vera 2 (PRSV2-M)", "Peng-Robinson-Stryjek-Vera 2 (PRSV2)"

                    Dim pp As PRSV2PropertyPackage = Me
                    'pp.m_pr.InteractionParameters.Clear()
                    For Each xel As XElement In (From xel2 As XElement In data Select xel2 Where xel2.Name = "InteractionParameters").FirstOrDefault.Elements.ToList
                        Dim ip As New Auxiliary.PRSV2_IPData() With {.id1 = xel.@Compound1, .id2 = xel.@Compound2, .kij = Double.Parse(xel.@Value, ci)}
                        Dim dic As New Dictionary(Of String, Auxiliary.PRSV2_IPData)
                        dic.Add(xel.@Compound2, ip)
                        If Not pp.m_pr.InteractionParameters.ContainsKey(xel.@Compound1) Then
                            If Not pp.m_pr.InteractionParameters.ContainsKey(xel.@Compound2) Then
                                pp.m_pr.InteractionParameters.Add(xel.@Compound1, dic)
                            Else
                                If Not pp.m_pr.InteractionParameters(xel.@Compound2).ContainsKey(xel.@Compound1) Then
                                    pp.m_pr.InteractionParameters(xel.@Compound2).Add(xel.@Compound1, ip)
                                Else
                                    pp.m_pr.InteractionParameters(xel.@Compound2)(xel.@Compound1) = ip
                                End If
                            End If
                        Else
                            If Not pp.m_pr.InteractionParameters(xel.@Compound1).ContainsKey(xel.@Compound2) Then
                                pp.m_pr.InteractionParameters(xel.@Compound1).Add(xel.@Compound2, ip)
                            Else
                                pp.m_pr.InteractionParameters(xel.@Compound1)(xel.@Compound2) = ip
                            End If
                        End If
                    Next

                Case "Peng-Robinson-Stryjek-Vera 2 (PRSV2-VL)"

                    Dim pp As PRSV2VLPropertyPackage = Me
                    'pp.m_pr.InteractionParameters.Clear()
                    For Each xel As XElement In (From xel2 As XElement In data Select xel2 Where xel2.Name = "InteractionParameters").FirstOrDefault.Elements.ToList
                        Dim ip As New Auxiliary.PRSV2_IPData() With {.id1 = xel.@Compound1, .id2 = xel.@Compound2, .kij = Double.Parse(xel.@kij, ci), .kji = Double.Parse(xel.@kji, ci)}
                        Dim dic As New Dictionary(Of String, Auxiliary.PRSV2_IPData)
                        dic.Add(xel.@Compound2, ip)
                        If Not pp.m_pr.InteractionParameters.ContainsKey(xel.@Compound1) Then
                            If Not pp.m_pr.InteractionParameters.ContainsKey(xel.@Compound2) Then
                                pp.m_pr.InteractionParameters.Add(xel.@Compound1, dic)
                            Else
                                If Not pp.m_pr.InteractionParameters(xel.@Compound2).ContainsKey(xel.@Compound1) Then
                                    pp.m_pr.InteractionParameters(xel.@Compound2).Add(xel.@Compound1, ip)
                                Else
                                    pp.m_pr.InteractionParameters(xel.@Compound2)(xel.@Compound1) = ip
                                End If
                            End If
                        Else
                            If Not pp.m_pr.InteractionParameters(xel.@Compound1).ContainsKey(xel.@Compound2) Then
                                pp.m_pr.InteractionParameters(xel.@Compound1).Add(xel.@Compound2, ip)
                            Else
                                pp.m_pr.InteractionParameters(xel.@Compound1)(xel.@Compound2) = ip
                            End If
                        End If
                    Next

                Case "Soave-Redlich-Kwong (SRK)"

                    Try
                        Dim pp As SRKPropertyPackage = Me
                        'pp.m_pr.InteractionParameters.Clear()
                        For Each xel As XElement In (From xel2 As XElement In data Select xel2 Where xel2.Name = "InteractionParameters").FirstOrDefault.Elements.ToList
                            Dim ip As New Auxiliary.PR_IPData() With {.Owner = Me.GetModel(), .kij = Double.Parse(xel.@Value, ci)}
                            Dim dic As New Dictionary(Of String, Auxiliary.PR_IPData)
                            dic.Add(xel.@Compound2, ip)
                            If Not pp.m_pr.InteractionParameters.ContainsKey(xel.@Compound1) Then
                                If Not pp.m_pr.InteractionParameters.ContainsKey(xel.@Compound2) Then
                                    pp.m_pr.InteractionParameters.Add(xel.@Compound1, dic)
                                Else
                                    If Not pp.m_pr.InteractionParameters(xel.@Compound2).ContainsKey(xel.@Compound1) Then
                                        pp.m_pr.InteractionParameters(xel.@Compound2).Add(xel.@Compound1, ip)
                                    Else
                                        pp.m_pr.InteractionParameters(xel.@Compound2)(xel.@Compound1) = ip
                                    End If
                                End If
                            Else
                                If Not pp.m_pr.InteractionParameters(xel.@Compound1).ContainsKey(xel.@Compound2) Then
                                    pp.m_pr.InteractionParameters(xel.@Compound1).Add(xel.@Compound2, ip)
                                Else
                                    pp.m_pr.InteractionParameters(xel.@Compound1)(xel.@Compound2) = ip
                                End If
                            End If
                        Next

                    Catch ex As Exception
                    End Try

                Case "Peng-Robinson / Lee-Kesler (PR/LK)"

                    Dim pp As PengRobinsonLKPropertyPackage = Me
                    'pp.m_pr.InteractionParameters.Clear()
                    For Each xel As XElement In (From xel2 As XElement In data Select xel2 Where xel2.Name = "InteractionParameters").FirstOrDefault.Elements.ToList
                        Dim ip As New Auxiliary.PR_IPData() With {.kij = Double.Parse(xel.@Value, ci)}
                        Dim dic As New Dictionary(Of String, Auxiliary.PR_IPData)
                        dic.Add(xel.@Compound2, ip)
                        If Not pp.m_pr.InteractionParameters.ContainsKey(xel.@Compound1) Then
                            If Not pp.m_pr.InteractionParameters.ContainsKey(xel.@Compound2) Then
                                pp.m_pr.InteractionParameters.Add(xel.@Compound1, dic)
                            Else
                                If Not pp.m_pr.InteractionParameters(xel.@Compound2).ContainsKey(xel.@Compound1) Then
                                    pp.m_pr.InteractionParameters(xel.@Compound2).Add(xel.@Compound1, ip)
                                Else
                                    pp.m_pr.InteractionParameters(xel.@Compound2)(xel.@Compound1) = ip
                                End If
                            End If
                        Else
                            If Not pp.m_pr.InteractionParameters(xel.@Compound1).ContainsKey(xel.@Compound2) Then
                                pp.m_pr.InteractionParameters(xel.@Compound1).Add(xel.@Compound2, ip)
                            Else
                                pp.m_pr.InteractionParameters(xel.@Compound1)(xel.@Compound2) = ip
                            End If
                        End If
                    Next


                Case "UNIFAC"

                    Dim pp As UNIFACPropertyPackage = Me
                    'pp.m_pr.InteractionParameters.Clear()
                    For Each xel As XElement In (From xel2 As XElement In data Select xel2 Where xel2.Name = "InteractionParameters").FirstOrDefault.Elements.ToList
                        Dim ip As New Auxiliary.PR_IPData() With {.kij = Double.Parse(xel.@Value, ci)}
                        Dim dic As New Dictionary(Of String, Auxiliary.PR_IPData)
                        dic.Add(xel.@Compound2, ip)
                        If Not pp.m_pr.InteractionParameters.ContainsKey(xel.@Compound1) Then
                            If Not pp.m_pr.InteractionParameters.ContainsKey(xel.@Compound2) Then
                                pp.m_pr.InteractionParameters.Add(xel.@Compound1, dic)
                            Else
                                If Not pp.m_pr.InteractionParameters(xel.@Compound2).ContainsKey(xel.@Compound1) Then
                                    pp.m_pr.InteractionParameters(xel.@Compound2).Add(xel.@Compound1, ip)
                                Else
                                    pp.m_pr.InteractionParameters(xel.@Compound2)(xel.@Compound1) = ip
                                End If
                            End If
                        Else
                            If Not pp.m_pr.InteractionParameters(xel.@Compound1).ContainsKey(xel.@Compound2) Then
                                pp.m_pr.InteractionParameters(xel.@Compound1).Add(xel.@Compound2, ip)
                            Else
                                pp.m_pr.InteractionParameters(xel.@Compound1)(xel.@Compound2) = ip
                            End If
                        End If
                    Next

                Case "UNIFAC-LL"

                    Dim pp As UNIFACLLPropertyPackage = Me
                    'pp.m_pr.InteractionParameters.Clear()
                    For Each xel As XElement In (From xel2 As XElement In data Select xel2 Where xel2.Name = "InteractionParameters").FirstOrDefault.Elements.ToList
                        Dim ip As New Auxiliary.PR_IPData() With {.kij = Double.Parse(xel.@Value, ci)}
                        Dim dic As New Dictionary(Of String, Auxiliary.PR_IPData)
                        dic.Add(xel.@Compound2, ip)
                        If Not pp.m_pr.InteractionParameters.ContainsKey(xel.@Compound1) Then
                            If Not pp.m_pr.InteractionParameters.ContainsKey(xel.@Compound2) Then
                                pp.m_pr.InteractionParameters.Add(xel.@Compound1, dic)
                            Else
                                If Not pp.m_pr.InteractionParameters(xel.@Compound2).ContainsKey(xel.@Compound1) Then
                                    pp.m_pr.InteractionParameters(xel.@Compound2).Add(xel.@Compound1, ip)
                                Else
                                    pp.m_pr.InteractionParameters(xel.@Compound2)(xel.@Compound1) = ip
                                End If
                            End If
                        Else
                            If Not pp.m_pr.InteractionParameters(xel.@Compound1).ContainsKey(xel.@Compound2) Then
                                pp.m_pr.InteractionParameters(xel.@Compound1).Add(xel.@Compound2, ip)
                            Else
                                pp.m_pr.InteractionParameters(xel.@Compound1)(xel.@Compound2) = ip
                            End If
                        End If
                    Next

                Case "NRTL"

                    Try
                        Dim pp As NRTLPropertyPackage = Me
                        'pp.m_pr.InteractionParameters.Clear()
                        For Each xel As XElement In (From xel2 As XElement In data Select xel2 Where xel2.Name = "InteractionParameters_PR").FirstOrDefault.Elements.ToList
                            Dim ip As New Auxiliary.PR_IPData() With {.kij = Double.Parse(xel.@Value, ci)}
                            Dim dic As New Dictionary(Of String, Auxiliary.PR_IPData)
                            dic.Add(xel.@Compound2, ip)
                            If Not pp.m_pr.InteractionParameters.ContainsKey(xel.@Compound1) Then
                                If Not pp.m_pr.InteractionParameters.ContainsKey(xel.@Compound2) Then
                                    pp.m_pr.InteractionParameters.Add(xel.@Compound1, dic)
                                Else
                                    If Not pp.m_pr.InteractionParameters(xel.@Compound2).ContainsKey(xel.@Compound1) Then
                                        pp.m_pr.InteractionParameters(xel.@Compound2).Add(xel.@Compound1, ip)
                                    Else
                                        pp.m_pr.InteractionParameters(xel.@Compound2)(xel.@Compound1) = ip
                                    End If
                                End If
                            Else
                                If Not pp.m_pr.InteractionParameters(xel.@Compound1).ContainsKey(xel.@Compound2) Then
                                    pp.m_pr.InteractionParameters(xel.@Compound1).Add(xel.@Compound2, ip)
                                Else
                                    pp.m_pr.InteractionParameters(xel.@Compound1)(xel.@Compound2) = ip
                                End If
                            End If
                        Next

                        'pp.m_uni.InteractionParameters.Clear()
                        For Each xel As XElement In (From xel2 As XElement In data Select xel2 Where xel2.Name = "InteractionParameters_NRTL").FirstOrDefault.Elements.ToList
                            Dim ip As New Auxiliary.NRTL_IPData() With {.ID1 = xel.@ID1, .ID2 = xel.@ID2, .A12 = Double.Parse(xel.@A12, ci), .A21 = Double.Parse(xel.@A21, ci),
                                                                        .B12 = Double.Parse(xel.@B12, ci), .B21 = Double.Parse(xel.@B21, ci),
                                                                        .C12 = Double.Parse(xel.@C12, ci), .C21 = Double.Parse(xel.@C21, ci),
                                                                        .alpha12 = Double.Parse(xel.@alpha12, ci)}
                            Dim dic As New Dictionary(Of String, Auxiliary.NRTL_IPData)
                            dic.Add(xel.@Compound2, ip)
                            If Not pp.m_uni.InteractionParameters.ContainsKey(xel.@Compound1) Then
                                pp.m_uni.InteractionParameters.Add(xel.@Compound1, dic)
                            Else
                                If Not pp.m_uni.InteractionParameters(xel.@Compound1).ContainsKey(xel.@Compound2) Then
                                    pp.m_uni.InteractionParameters(xel.@Compound1).Add(xel.@Compound2, ip)
                                Else
                                    pp.m_uni.InteractionParameters(xel.@Compound1)(xel.@Compound2) = ip
                                End If
                            End If
                        Next

                    Catch ex As Exception

                    End Try

                Case "UNIQUAC"

                    Try
                        Dim pp As UNIQUACPropertyPackage = Me

                        'pp.m_pr.InteractionParameters.Clear()
                        For Each xel As XElement In (From xel2 As XElement In data Select xel2 Where xel2.Name = "InteractionParameters_PR").FirstOrDefault.Elements.ToList
                            Dim ip As New Auxiliary.PR_IPData() With {.kij = Double.Parse(xel.@Value, ci)}
                            Dim dic As New Dictionary(Of String, Auxiliary.PR_IPData)
                            dic.Add(xel.@Compound2, ip)
                            If Not pp.m_pr.InteractionParameters.ContainsKey(xel.@Compound1) Then
                                If Not pp.m_pr.InteractionParameters.ContainsKey(xel.@Compound2) Then
                                    pp.m_pr.InteractionParameters.Add(xel.@Compound1, dic)
                                Else
                                    If Not pp.m_pr.InteractionParameters(xel.@Compound2).ContainsKey(xel.@Compound1) Then
                                        pp.m_pr.InteractionParameters(xel.@Compound2).Add(xel.@Compound1, ip)
                                    Else
                                        pp.m_pr.InteractionParameters(xel.@Compound2)(xel.@Compound1) = ip
                                    End If
                                End If
                            Else
                                If Not pp.m_pr.InteractionParameters(xel.@Compound1).ContainsKey(xel.@Compound2) Then
                                    pp.m_pr.InteractionParameters(xel.@Compound1).Add(xel.@Compound2, ip)
                                Else
                                    pp.m_pr.InteractionParameters(xel.@Compound1)(xel.@Compound2) = ip
                                End If
                            End If
                        Next

                        'pp.m_uni.InteractionParameters.Clear()
                        For Each xel As XElement In (From xel2 As XElement In data Select xel2 Where xel2.Name = "InteractionParameters_UNIQUAC").FirstOrDefault.Elements.ToList
                            Dim ip As New Auxiliary.UNIQUAC_IPData() With {.ID1 = xel.@ID1, .ID2 = xel.@ID2, .A12 = Double.Parse(xel.@A12, ci), .A21 = Double.Parse(xel.@A21, ci),
                                                                           .B12 = Double.Parse(xel.@B12, ci), .B21 = Double.Parse(xel.@B21, ci),
                                                                           .C12 = Double.Parse(xel.@C12, ci), .C21 = Double.Parse(xel.@C21, ci)}
                            Dim dic As New Dictionary(Of String, Auxiliary.UNIQUAC_IPData)
                            dic.Add(xel.@Compound2, ip)
                            If Not pp.m_uni.InteractionParameters.ContainsKey(xel.@Compound1) Then
                                pp.m_uni.InteractionParameters.Add(xel.@Compound1, dic)
                            Else
                                If Not pp.m_uni.InteractionParameters(xel.@Compound1).ContainsKey(xel.@Compound2) Then
                                    pp.m_uni.InteractionParameters(xel.@Compound1).Add(xel.@Compound2, ip)
                                Else
                                    pp.m_uni.InteractionParameters(xel.@Compound1)(xel.@Compound2) = ip
                                End If
                            End If
                        Next

                    Catch ex As Exception

                    End Try

                Case "Wilson"

                    Try

                        Dim pp As WilsonPropertyPackage = Me

                        For Each xel As XElement In (From xel2 As XElement In data Select xel2 Where xel2.Name = "InteractionParameters_PR").FirstOrDefault.Elements.ToList
                            Dim ip As New Auxiliary.PR_IPData() With {.kij = Double.Parse(xel.@Value, ci)}
                            Dim dic As New Dictionary(Of String, Auxiliary.PR_IPData)
                            dic.Add(xel.@Compound2, ip)
                            If Not pp.m_pr.InteractionParameters.ContainsKey(xel.@Compound1) Then
                                If Not pp.m_pr.InteractionParameters.ContainsKey(xel.@Compound2) Then
                                    pp.m_pr.InteractionParameters.Add(xel.@Compound1, dic)
                                Else
                                    If Not pp.m_pr.InteractionParameters(xel.@Compound2).ContainsKey(xel.@Compound1) Then
                                        pp.m_pr.InteractionParameters(xel.@Compound2).Add(xel.@Compound1, ip)
                                    Else
                                        pp.m_pr.InteractionParameters(xel.@Compound2)(xel.@Compound1) = ip
                                    End If
                                End If
                            Else
                                If Not pp.m_pr.InteractionParameters(xel.@Compound1).ContainsKey(xel.@Compound2) Then
                                    pp.m_pr.InteractionParameters(xel.@Compound1).Add(xel.@Compound2, ip)
                                Else
                                    pp.m_pr.InteractionParameters(xel.@Compound1)(xel.@Compound2) = ip
                                End If
                            End If
                        Next

                        For Each xel As XElement In (From xel2 As XElement In data Select xel2 Where xel2.Name = "InteractionParameters_Wilson").FirstOrDefault.Elements.ToList
                            Dim ip = New Double() {Double.Parse(xel.@A12, ci), Double.Parse(xel.@A21, ci)}
                            Dim dic As New Dictionary(Of String, Double())
                            dic.Add(xel.@CAS2, ip)
                            If Not pp.WilsonM.BIPs.ContainsKey(xel.@CAS1) Then
                                pp.WilsonM.BIPs.Add(xel.@CAS1, dic)
                            Else
                                If Not pp.WilsonM.BIPs(xel.@CAS1).ContainsKey(xel.@CAS2) Then
                                    pp.WilsonM.BIPs(xel.@CAS1).Add(xel.@CAS2, ip)
                                Else
                                    pp.WilsonM.BIPs(xel.@CAS1)(xel.@CAS2) = ip
                                End If
                            End If
                        Next

                    Catch ex As Exception

                    End Try

                Case "Modified UNIFAC (Dortmund)"

                    Dim pp As MODFACPropertyPackage = Me
                    'pp.m_pr.InteractionParameters.Clear()
                    For Each xel As XElement In (From xel2 As XElement In data Select xel2 Where xel2.Name = "InteractionParameters").FirstOrDefault.Elements.ToList
                        Dim ip As New Auxiliary.PR_IPData() With {.kij = Double.Parse(xel.@Value, ci)}
                        Dim dic As New Dictionary(Of String, Auxiliary.PR_IPData)
                        dic.Add(xel.@Compound2, ip)
                        If Not pp.m_pr.InteractionParameters.ContainsKey(xel.@Compound1) Then
                            If Not pp.m_pr.InteractionParameters.ContainsKey(xel.@Compound2) Then
                                pp.m_pr.InteractionParameters.Add(xel.@Compound1, dic)
                            Else
                                If Not pp.m_pr.InteractionParameters(xel.@Compound2).ContainsKey(xel.@Compound1) Then
                                    pp.m_pr.InteractionParameters(xel.@Compound2).Add(xel.@Compound1, ip)
                                Else
                                    pp.m_pr.InteractionParameters(xel.@Compound2)(xel.@Compound1) = ip
                                End If
                            End If
                        Else
                            If Not pp.m_pr.InteractionParameters(xel.@Compound1).ContainsKey(xel.@Compound2) Then
                                pp.m_pr.InteractionParameters(xel.@Compound1).Add(xel.@Compound2, ip)
                            Else
                                pp.m_pr.InteractionParameters(xel.@Compound1)(xel.@Compound2) = ip
                            End If
                        End If
                    Next

                Case "Lee-Kesler-Plöcker"

                    Try
                        Dim pp As LKPPropertyPackage = Me
                        'pp.m_pr.InteractionParameters.Clear()

                        Dim el = (From xel2 As XElement In data Select xel2 Where xel2.Name = "InteractionParameters").FirstOrDefault

                        If Not el Is Nothing Then

                            For Each xel As XElement In el.Elements.ToList
                                Dim ip As New Auxiliary.LKP_IPData() With {.kij = Double.Parse(xel.@Value, ci)}
                                Dim dic As New Dictionary(Of String, Auxiliary.LKP_IPData)
                                dic.Add(xel.@Compound2, ip)
                                If Not pp.m_lk.InteractionParameters.ContainsKey(xel.@Compound1) Then
                                    If Not pp.m_lk.InteractionParameters.ContainsKey(xel.@Compound2) Then
                                        pp.m_lk.InteractionParameters.Add(xel.@Compound1, dic)
                                    Else
                                        If Not pp.m_lk.InteractionParameters(xel.@Compound2).ContainsKey(xel.@Compound1) Then
                                            pp.m_lk.InteractionParameters(xel.@Compound2).Add(xel.@Compound1, ip)
                                        Else
                                            pp.m_lk.InteractionParameters(xel.@Compound2)(xel.@Compound1) = ip
                                        End If
                                    End If
                                Else
                                    If Not pp.m_lk.InteractionParameters(xel.@Compound1).ContainsKey(xel.@Compound2) Then
                                        pp.m_lk.InteractionParameters(xel.@Compound1).Add(xel.@Compound2, ip)
                                    Else
                                        pp.m_lk.InteractionParameters(xel.@Compound1)(xel.@Compound2) = ip
                                    End If
                                End If
                            Next

                        End If

                    Catch ex As Exception

                    End Try

            End Select

            Dim elf = (From xel As XElement In data Select xel Where xel.Name = "FlashSettings").SingleOrDefault

            If Not elf Is Nothing Then

                FlashSettings.Clear()

                For Each xel3 In elf.Elements
                    Try
                        Dim esname = [Enum].Parse(Interfaces.Enums.Helpers.GetEnumType("DWSIM.Interfaces.Enums.FlashSetting"), xel3.@Name)
                        If xel3.@Value = "0" And esname.ToString().Contains("tolerance") Then
                            FlashSettings.Add(esname, "0.0001")
                        Else
                            FlashSettings.Add(esname, xel3.@Value)
                        End If
                    Catch ex As Exception
                    End Try
                Next

                If Not FlashSettings.ContainsKey(Interfaces.Enums.FlashSetting.PVFlash_FixedDampingFactor) Then
                    FlashSettings.Add(Interfaces.Enums.FlashSetting.PVFlash_FixedDampingFactor, 1.0.ToString(Globalization.CultureInfo.InvariantCulture))
                End If
                If Not FlashSettings.ContainsKey(Interfaces.Enums.FlashSetting.PVFlash_MaximumTemperatureChange) Then
                    FlashSettings.Add(Interfaces.Enums.FlashSetting.PVFlash_MaximumTemperatureChange, 10.0.ToString(Globalization.CultureInfo.InvariantCulture))
                End If
                If Not FlashSettings.ContainsKey(Interfaces.Enums.FlashSetting.PVFlash_TemperatureDerivativeEpsilon) Then
                    FlashSettings.Add(Interfaces.Enums.FlashSetting.PVFlash_TemperatureDerivativeEpsilon, 0.1.ToString(Globalization.CultureInfo.InvariantCulture))
                End If
                If Not FlashSettings.ContainsKey(Interfaces.Enums.FlashSetting.ST_Number_of_Random_Tries) Then
                    FlashSettings.Add(Interfaces.Enums.FlashSetting.ST_Number_of_Random_Tries, 20)
                End If
                If Not FlashSettings.ContainsKey(Interfaces.Enums.FlashSetting.CheckIncipientLiquidForStability) Then
                    FlashSettings.Add(Interfaces.Enums.FlashSetting.CheckIncipientLiquidForStability, False)
                End If
                If Not FlashSettings.ContainsKey(Interfaces.Enums.FlashSetting.PHFlash_MaximumTemperatureChange) Then
                    FlashSettings.Add(Interfaces.Enums.FlashSetting.PHFlash_MaximumTemperatureChange, 30.0.ToString(Globalization.CultureInfo.InvariantCulture))
                End If
                If Not FlashSettings.ContainsKey(Interfaces.Enums.FlashSetting.PTFlash_DampingFactor) Then
                    FlashSettings.Add(Interfaces.Enums.FlashSetting.PTFlash_DampingFactor, 1.0.ToString(Globalization.CultureInfo.InvariantCulture))
                End If
                If Not FlashSettings.ContainsKey(Interfaces.Enums.FlashSetting.ForceEquilibriumCalculationType) Then
                    FlashSettings.Add(Interfaces.Enums.FlashSetting.ForceEquilibriumCalculationType, "Default")
                End If
                If Not FlashSettings.ContainsKey(Interfaces.Enums.FlashSetting.ImmiscibleWaterOption) Then
                    FlashSettings.Add(Interfaces.Enums.FlashSetting.ImmiscibleWaterOption, False)
                End If
                If Not FlashSettings.ContainsKey(Interfaces.Enums.FlashSetting.HandleSolidsInDefaultEqCalcMode) Then
                    FlashSettings.Add(Interfaces.Enums.FlashSetting.HandleSolidsInDefaultEqCalcMode, False)
                End If
                If Not FlashSettings.ContainsKey(Interfaces.Enums.FlashSetting.UseIOFlash) Then
                    FlashSettings.Add(Interfaces.Enums.FlashSetting.UseIOFlash, False)
                End If
                If Not FlashSettings.ContainsKey(Interfaces.Enums.FlashSetting.GibbsMinimizationExternalSolver) Then
                    FlashSettings.Add(Interfaces.Enums.FlashSetting.GibbsMinimizationExternalSolver, "")
                End If
                If Not FlashSettings.ContainsKey(Interfaces.Enums.FlashSetting.GibbsMinimizationExternalSolverConfigData) Then
                    FlashSettings.Add(Interfaces.Enums.FlashSetting.GibbsMinimizationExternalSolverConfigData, "")
                End If
                If Not FlashSettings.ContainsKey(Interfaces.Enums.FlashSetting.PHFlash_Use_Interpolated_Result_In_Oscillating_Temperature_Cases) Then
                    FlashSettings.Add(Interfaces.Enums.FlashSetting.PHFlash_Use_Interpolated_Result_In_Oscillating_Temperature_Cases, True)
                End If
                If Not FlashSettings.ContainsKey(Interfaces.Enums.FlashSetting.PVFlash_TryIdealCalcOnFailure) Then
                    FlashSettings.Add(Interfaces.Enums.FlashSetting.PVFlash_TryIdealCalcOnFailure, True)
                End If
                If Not FlashSettings.ContainsKey(Interfaces.Enums.FlashSetting.FailSafeCalculationMode) Then
                    FlashSettings.Add(Interfaces.Enums.FlashSetting.FailSafeCalculationMode, 1)
                End If
            End If

        End Function

        Public Overridable Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement) Implements Interfaces.ICustomXMLSerialization.SaveData

            Dim elements As New System.Collections.Generic.List(Of System.Xml.Linq.XElement)
            Dim ci As Globalization.CultureInfo = Globalization.CultureInfo.InvariantCulture

            With elements

                .Add(New XElement("Type", Me.GetType.ToString))
                .Add(New XElement("ComponentName", ComponentName))
                .Add(New XElement("ComponentDescription", ComponentDescription))
                .Add(New XElement("Tag", Tag))

                If ParametersXMLString <> "" Then
                    Try
                        .Add(XElement.Parse(Me.ParametersXMLString))
                    Catch ex As Exception
                    End Try
                End If

                .Add(New XElement("UseHenryConstants", UseHenryConstants))
                .Add(New XElement("AutoEstimateMissingNRTLUNIQUACParameters", AutoEstimateMissingNRTLUNIQUACParameters))

                .Add(New XElement("SingleCompoundCheckThreshold", SingleCompoundCheckThreshold.ToString(ci)))

                .Add(New XElement("OverrideKvalFugCoeff", OverrideKvalFugCoeff))
                .Add(New XElement("OverrideEnthalpyCalculation", OverrideEnthalpyCalculation))
                .Add(New XElement("OverrideEntropyCalculation", OverrideEntropyCalculation))

                .Add(New XElement("LiquidDensityCalculationMode_Subcritical", LiquidDensityCalculationMode_Subcritical))
                .Add(New XElement("LiquidDensityCalculationMode_Supercritical", LiquidDensityCalculationMode_Supercritical))
                .Add(New XElement("LiquidDensity_CorrectExpDataForPressure", LiquidDensity_CorrectExpDataForPressure))
                .Add(New XElement("LiquidDensity_UsePenelouxVolumeTranslation", LiquidDensity_UsePenelouxVolumeTranslation))
                .Add(New XElement("LiquidViscosityCalculationMode_Subcritical", LiquidViscosityCalculationMode_Subcritical))
                .Add(New XElement("LiquidViscosityCalculationMode_Supercritical", LiquidViscosityCalculationMode_Supercritical))
                .Add(New XElement("LiquidViscosity_CorrectExpDataForPressure", LiquidViscosity_CorrectExpDataForPressure))
                .Add(New XElement("LiquidViscosity_MixingRule", LiquidViscosity_MixingRule))
                .Add(New XElement("VaporPhaseFugacityCalculationMode", VaporPhaseFugacityCalculationMode))
                .Add(New XElement("SolidPhaseFugacityCalculationMethod", SolidPhaseFugacityCalculationMethod))
                .Add(New XElement("SolidPhaseFugacity_UseIdealLiquidPhaseFugacity", SolidPhaseFugacity_UseIdealLiquidPhaseFugacity))
                .Add(New XElement("SolidPhaseEnthalpy_UsesCp", SolidPhaseEnthalpy_UsesCp))
                .Add(New XElement("EnthalpyEntropyCpCvCalculationMode", EnthalpyEntropyCpCvCalculationMode))
                .Add(New XElement("LiquidEnthalpyEntropyCpCvCalculationMode_EOS", LiquidEnthalpyEntropyCpCvCalculationMode_EOS))
                .Add(New XElement("LiquidFugacity_UsePoyntingCorrectionFactor", LiquidFugacity_UsePoyntingCorrectionFactor))
                .Add(New XElement("ActivityCoefficientModels_IgnoreMissingInteractionParameters", ActivityCoefficientModels_IgnoreMissingInteractionParameters))
                .Add(New XElement("IgnoreVaporFractionLimit", IgnoreVaporFractionLimit))
                .Add(New XElement("IgnoreSalinityLimit", IgnoreSalinityLimit))
                .Add(New XElement("CalculateAdditionalMaterialStreamProperties", CalculateAdditionalMaterialStreamProperties))
                .Add(New XElement("FlashCalculationApproach", FlashCalculationApproach))

                Dim jsonoptions As New JsonSerializerSettings With {.StringEscapeHandling = StringEscapeHandling.EscapeHtml, .Formatting = Formatting.Indented}

                .Add(New XElement("ForcedSolids", JsonConvert.SerializeObject(ForcedSolids, jsonoptions)))
                .Add(New XElement("PropertyOverrides", JsonConvert.SerializeObject(PropertyOverrides, jsonoptions)))

                '.Add(New XElement("Parameters"))

                'For Each kvp As KeyValuePair(Of String, Double) In m_par
                '    elements(elements.Count - 1).Add(New XElement("Parameter", {New XAttribute("ID", kvp.Key), New XAttribute("Value", kvp.Value.ToString(ci))}))
                'Next

                Select Case Me.ComponentName

                    Case "Peng-Robinson (PR)"

                        Dim pp As PengRobinsonPropertyPackage = Me

                        .Add(New XElement("InteractionParameters"))
                        For Each kvp As KeyValuePair(Of String, Dictionary(Of String, Auxiliary.PR_IPData)) In pp.m_pr.InteractionParameters
                            For Each kvp2 As KeyValuePair(Of String, Auxiliary.PR_IPData) In kvp.Value
                                If Not Me.CurrentMaterialStream Is Nothing Then
                                    If Me.CurrentMaterialStream.Phases(0).Compounds.ContainsKey(kvp.Key) And Me.CurrentMaterialStream.Phases(0).Compounds.ContainsKey(kvp2.Key) Then
                                        .Item(.Count - 1).Add(New XElement("InteractionParameter", New XAttribute("Compound1", kvp.Key),
                                                                        New XAttribute("Compound2", kvp2.Key),
                                                                        New XAttribute("Value", kvp2.Value.kij.ToString(ci))))
                                    End If
                                End If
                            Next
                        Next

                    Case "Peng-Robinson 1978 (PR78)"

                        Dim pp As PengRobinson1978PropertyPackage = Me

                        .Add(New XElement("InteractionParameters"))
                        For Each kvp As KeyValuePair(Of String, Dictionary(Of String, Auxiliary.PR_IPData)) In pp.m_pr.InteractionParameters
                            For Each kvp2 As KeyValuePair(Of String, Auxiliary.PR_IPData) In kvp.Value
                                If Not Me.CurrentMaterialStream Is Nothing Then
                                    If Me.CurrentMaterialStream.Phases(0).Compounds.ContainsKey(kvp.Key) And Me.CurrentMaterialStream.Phases(0).Compounds.ContainsKey(kvp2.Key) Then
                                        .Item(.Count - 1).Add(New XElement("InteractionParameter", New XAttribute("Compound1", kvp.Key),
                                                                        New XAttribute("Compound2", kvp2.Key),
                                                                        New XAttribute("Value", kvp2.Value.kij.ToString(ci))))
                                    End If
                                End If
                            Next
                        Next

                    Case "Peng-Robinson-Stryjek-Vera 2 (PRSV2-M)", "Peng-Robinson-Stryjek-Vera 2 (PRSV2)"

                        Dim pp As PRSV2PropertyPackage = Me

                        .Add(New XElement("InteractionParameters"))
                        For Each kvp As KeyValuePair(Of String, Dictionary(Of String, Auxiliary.PRSV2_IPData)) In pp.m_pr.InteractionParameters
                            For Each kvp2 As KeyValuePair(Of String, Auxiliary.PRSV2_IPData) In kvp.Value
                                If Not Me.CurrentMaterialStream Is Nothing Then
                                    If Me.CurrentMaterialStream.Phases(0).Compounds.ContainsKey(kvp.Key) And Me.CurrentMaterialStream.Phases(0).Compounds.ContainsKey(kvp2.Key) Then
                                        .Item(.Count - 1).Add(New XElement("InteractionParameter", New XAttribute("Compound1", kvp.Key),
                                                                        New XAttribute("Compound2", kvp2.Key),
                                                                        New XAttribute("Value", kvp2.Value.kij.ToString(ci))))
                                    End If
                                End If
                            Next
                        Next

                    Case "Peng-Robinson-Stryjek-Vera 2 (PRSV2-VL)"

                        Dim pp As PRSV2VLPropertyPackage = Me

                        .Add(New XElement("InteractionParameters"))
                        For Each kvp As KeyValuePair(Of String, Dictionary(Of String, Auxiliary.PRSV2_IPData)) In pp.m_pr.InteractionParameters
                            For Each kvp2 As KeyValuePair(Of String, Auxiliary.PRSV2_IPData) In kvp.Value
                                If Not Me.CurrentMaterialStream Is Nothing Then
                                    If Me.CurrentMaterialStream.Phases(0).Compounds.ContainsKey(kvp.Key) And Me.CurrentMaterialStream.Phases(0).Compounds.ContainsKey(kvp2.Key) Then
                                        .Item(.Count - 1).Add(New XElement("InteractionParameter", New XAttribute("Compound1", kvp.Key),
                                                                        New XAttribute("Compound2", kvp2.Key),
                                                                        New XAttribute("kij", kvp2.Value.kij.ToString(ci)),
                                                                        New XAttribute("kji", kvp2.Value.kji.ToString(ci))))
                                    End If
                                End If
                            Next
                        Next

                    Case "Soave-Redlich-Kwong (SRK)"

                        Dim pp As SRKPropertyPackage = Me

                        .Add(New XElement("InteractionParameters"))
                        For Each kvp As KeyValuePair(Of String, Dictionary(Of String, Auxiliary.PR_IPData)) In pp.m_pr.InteractionParameters
                            For Each kvp2 As KeyValuePair(Of String, Auxiliary.PR_IPData) In kvp.Value
                                If Not Me.CurrentMaterialStream Is Nothing Then
                                    If Me.CurrentMaterialStream.Phases(0).Compounds.ContainsKey(kvp.Key) And Me.CurrentMaterialStream.Phases(0).Compounds.ContainsKey(kvp2.Key) Then
                                        .Item(.Count - 1).Add(New XElement("InteractionParameter", New XAttribute("Compound1", kvp.Key),
                                                                      New XAttribute("Compound2", kvp2.Key),
                                                                      New XAttribute("Value", kvp2.Value.kij.ToString(ci))))
                                    End If
                                End If
                            Next
                        Next

                    Case "Peng-Robinson / Lee-Kesler (PR/LK)"

                        Dim pp As PengRobinsonLKPropertyPackage = Me

                        .Add(New XElement("InteractionParameters"))

                        For Each kvp As KeyValuePair(Of String, Dictionary(Of String, Auxiliary.PR_IPData)) In pp.m_pr.InteractionParameters
                            For Each kvp2 As KeyValuePair(Of String, Auxiliary.PR_IPData) In kvp.Value
                                If Not Me.CurrentMaterialStream Is Nothing Then
                                    If Me.CurrentMaterialStream.Phases(0).Compounds.ContainsKey(kvp.Key) And Me.CurrentMaterialStream.Phases(0).Compounds.ContainsKey(kvp2.Key) Then
                                        .Item(.Count - 1).Add(New XElement("InteractionParameter", New XAttribute("Compound1", kvp.Key),
                                                                           New XAttribute("Compound2", kvp2.Key),
                                                                           New XAttribute("Value", kvp2.Value.kij.ToString(ci))))
                                    End If
                                End If
                            Next
                        Next

                    Case "UNIFAC"

                        Dim pp As UNIFACPropertyPackage = Me

                        .Add(New XElement("InteractionParameters"))

                        For Each kvp As KeyValuePair(Of String, Dictionary(Of String, Auxiliary.PR_IPData)) In pp.m_pr.InteractionParameters
                            For Each kvp2 As KeyValuePair(Of String, Auxiliary.PR_IPData) In kvp.Value
                                If Not Me.CurrentMaterialStream Is Nothing Then
                                    If Me.CurrentMaterialStream.Phases(0).Compounds.ContainsKey(kvp.Key) And Me.CurrentMaterialStream.Phases(0).Compounds.ContainsKey(kvp2.Key) Then
                                        .Item(.Count - 1).Add(New XElement("InteractionParameter", New XAttribute("Compound1", kvp.Key),
                                                                        New XAttribute("Compound2", kvp2.Key),
                                                                        New XAttribute("Value", kvp2.Value.kij.ToString(ci))))
                                    End If
                                End If
                            Next
                        Next

                    Case "UNIFAC-LL"

                        Dim pp As UNIFACLLPropertyPackage = Me

                        .Add(New XElement("InteractionParameters"))

                        For Each kvp As KeyValuePair(Of String, Dictionary(Of String, Auxiliary.PR_IPData)) In pp.m_pr.InteractionParameters
                            For Each kvp2 As KeyValuePair(Of String, Auxiliary.PR_IPData) In kvp.Value
                                If Not Me.CurrentMaterialStream Is Nothing Then
                                    If Me.CurrentMaterialStream.Phases(0).Compounds.ContainsKey(kvp.Key) And Me.CurrentMaterialStream.Phases(0).Compounds.ContainsKey(kvp2.Key) Then
                                        .Item(.Count - 1).Add(New XElement("InteractionParameter", New XAttribute("Compound1", kvp.Key),
                                                                          New XAttribute("Compound2", kvp2.Key),
                                                                          New XAttribute("Value", kvp2.Value.kij.ToString(ci))))
                                    End If
                                End If
                            Next
                        Next

                    Case "NRTL"

                        Dim pp As NRTLPropertyPackage = Me

                        .Add(New XElement("InteractionParameters_PR"))

                        For Each kvp As KeyValuePair(Of String, Dictionary(Of String, Auxiliary.PR_IPData)) In pp.m_pr.InteractionParameters
                            For Each kvp2 As KeyValuePair(Of String, Auxiliary.PR_IPData) In kvp.Value
                                If Not Me.CurrentMaterialStream Is Nothing Then
                                    If Me.CurrentMaterialStream.Phases(0).Compounds.ContainsKey(kvp.Key) And Me.CurrentMaterialStream.Phases(0).Compounds.ContainsKey(kvp2.Key) Then
                                        .Item(.Count - 1).Add(New XElement("InteractionParameter", New XAttribute("Compound1", kvp.Key),
                                                                        New XAttribute("Compound2", kvp2.Key),
                                                                        New XAttribute("Value", kvp2.Value.kij.ToString(ci))))
                                    End If
                                End If
                            Next
                        Next

                        .Add(New XElement("InteractionParameters_NRTL"))

                        For Each kvp As KeyValuePair(Of String, Dictionary(Of String, Auxiliary.NRTL_IPData)) In pp.m_uni.InteractionParameters
                            For Each kvp2 As KeyValuePair(Of String, Auxiliary.NRTL_IPData) In kvp.Value
                                If Not Me.CurrentMaterialStream Is Nothing Then
                                    If Me.CurrentMaterialStream.Phases(0).Compounds.ContainsKey(kvp.Key) And Me.CurrentMaterialStream.Phases(0).Compounds.ContainsKey(kvp2.Key) Then
                                        .Item(.Count - 1).Add(New XElement("InteractionParameter", New XAttribute("Compound1", kvp.Key),
                                                                         New XAttribute("Compound2", kvp2.Key),
                                                                         New XAttribute("ID1", kvp2.Value.ID1),
                                                                         New XAttribute("ID2", kvp2.Value.ID2),
                                                                         New XAttribute("A12", kvp2.Value.A12.ToString(ci)),
                                                                         New XAttribute("A21", kvp2.Value.A21.ToString(ci)),
                                                                         New XAttribute("B12", kvp2.Value.B12.ToString(ci)),
                                                                         New XAttribute("B21", kvp2.Value.B21.ToString(ci)),
                                                                         New XAttribute("C12", kvp2.Value.C12.ToString(ci)),
                                                                         New XAttribute("C21", kvp2.Value.C21.ToString(ci)),
                                                                         New XAttribute("alpha12", kvp2.Value.alpha12.ToString(ci))))
                                    End If
                                End If
                            Next
                        Next

                    Case "UNIQUAC"

                        Dim pp As UNIQUACPropertyPackage = Me

                        .Add(New XElement("InteractionParameters_PR"))

                        For Each kvp As KeyValuePair(Of String, Dictionary(Of String, Auxiliary.PR_IPData)) In pp.m_pr.InteractionParameters
                            For Each kvp2 As KeyValuePair(Of String, Auxiliary.PR_IPData) In kvp.Value
                                If Not Me.CurrentMaterialStream Is Nothing Then
                                    If Me.CurrentMaterialStream.Phases(0).Compounds.ContainsKey(kvp.Key) And Me.CurrentMaterialStream.Phases(0).Compounds.ContainsKey(kvp2.Key) Then
                                        .Item(.Count - 1).Add(New XElement("InteractionParameter", New XAttribute("Compound1", kvp.Key),
                                                                      New XAttribute("Compound2", kvp2.Key),
                                                                      New XAttribute("Value", kvp2.Value.kij.ToString(ci))))
                                    End If
                                End If
                            Next
                        Next

                        .Add(New XElement("InteractionParameters_UNIQUAC"))

                        For Each kvp As KeyValuePair(Of String, Dictionary(Of String, Auxiliary.UNIQUAC_IPData)) In pp.m_uni.InteractionParameters
                            For Each kvp2 As KeyValuePair(Of String, Auxiliary.UNIQUAC_IPData) In kvp.Value
                                If Not Me.CurrentMaterialStream Is Nothing Then
                                    If Me.CurrentMaterialStream.Phases(0).Compounds.ContainsKey(kvp.Key) And Me.CurrentMaterialStream.Phases(0).Compounds.ContainsKey(kvp2.Key) Then
                                        .Item(.Count - 1).Add(New XElement("InteractionParameter", New XAttribute("Compound1", kvp.Key),
                                                                      New XAttribute("Compound2", kvp2.Key),
                                                                      New XAttribute("ID1", kvp2.Value.ID1),
                                                                      New XAttribute("ID2", kvp2.Value.ID2),
                                                                      New XAttribute("A12", kvp2.Value.A12.ToString(ci)),
                                                                      New XAttribute("A21", kvp2.Value.A21.ToString(ci)),
                                                                      New XAttribute("B12", kvp2.Value.B12.ToString(ci)),
                                                                      New XAttribute("B21", kvp2.Value.B21.ToString(ci)),
                                                                      New XAttribute("C12", kvp2.Value.C12.ToString(ci)),
                                                                      New XAttribute("C21", kvp2.Value.C21.ToString(ci))))
                                    End If
                                End If
                            Next
                        Next

                    Case "Wilson"

                        Dim pp As WilsonPropertyPackage = Me


                        .Add(New XElement("InteractionParameters_PR"))

                        For Each kvp As KeyValuePair(Of String, Dictionary(Of String, Auxiliary.PR_IPData)) In pp.m_pr.InteractionParameters
                            For Each kvp2 As KeyValuePair(Of String, Auxiliary.PR_IPData) In kvp.Value
                                If Not Me.CurrentMaterialStream Is Nothing Then
                                    If Me.CurrentMaterialStream.Phases(0).Compounds.ContainsKey(kvp.Key) And Me.CurrentMaterialStream.Phases(0).Compounds.ContainsKey(kvp2.Key) Then
                                        .Item(.Count - 1).Add(New XElement("InteractionParameter", New XAttribute("Compound1", kvp.Key),
                                                                        New XAttribute("Compound2", kvp2.Key),
                                                                        New XAttribute("Value", kvp2.Value.kij.ToString(ci))))
                                    End If
                                End If
                            Next
                        Next

                        .Add(New XElement("InteractionParameters_Wilson"))

                        For Each kvp As KeyValuePair(Of String, Dictionary(Of String, Double())) In pp.WilsonM.BIPs
                            For Each kvp2 As KeyValuePair(Of String, Double()) In kvp.Value
                                If Not Me.CurrentMaterialStream Is Nothing Then
                                    Dim c1 = CurrentMaterialStream.Phases(0).Compounds.Values.Where(Function(c) c.ConstantProperties.CAS_Number = kvp.Key).FirstOrDefault()
                                    Dim c2 = CurrentMaterialStream.Phases(0).Compounds.Values.Where(Function(c) c.ConstantProperties.CAS_Number = kvp2.Key).FirstOrDefault()
                                    If c1 IsNot Nothing And c2 IsNot Nothing Then
                                        If Me.CurrentMaterialStream.Phases(0).Compounds.ContainsKey(c1.Name) And Me.CurrentMaterialStream.Phases(0).Compounds.ContainsKey(c2.Name) Then
                                            .Item(.Count - 1).Add(New XElement("InteractionParameter", New XAttribute("CAS1", kvp.Key),
                                                                      New XAttribute("CAS2", kvp2.Key),
                                                                      New XAttribute("A12", kvp2.Value(0).ToString(ci)),
                                                                      New XAttribute("A21", kvp2.Value(1).ToString(ci))))
                                        End If
                                    End If
                                End If
                            Next
                        Next

                    Case "Modified UNIFAC (Dortmund)"

                        Dim pp As MODFACPropertyPackage = Me

                        .Add(New XElement("InteractionParameters"))

                        For Each kvp As KeyValuePair(Of String, Dictionary(Of String, Auxiliary.PR_IPData)) In pp.m_pr.InteractionParameters
                            For Each kvp2 As KeyValuePair(Of String, Auxiliary.PR_IPData) In kvp.Value
                                If Not Me.CurrentMaterialStream Is Nothing Then
                                    If Me.CurrentMaterialStream.Phases(0).Compounds.ContainsKey(kvp.Key) And Me.CurrentMaterialStream.Phases(0).Compounds.ContainsKey(kvp2.Key) Then
                                        .Item(.Count - 1).Add(New XElement("InteractionParameter", New XAttribute("Compound1", kvp.Key),
                                                                     New XAttribute("Compound2", kvp2.Key),
                                                                     New XAttribute("Value", kvp2.Value.kij.ToString(ci))))
                                    End If
                                End If
                            Next
                        Next

                    Case "Lee-Kesler-Plöcker"

                        Dim pp As LKPPropertyPackage = Me

                        .Add(New XElement("InteractionParameters"))

                        For Each kvp As KeyValuePair(Of String, Dictionary(Of String, Auxiliary.LKP_IPData)) In pp.m_lk.InteractionParameters
                            For Each kvp2 As KeyValuePair(Of String, Auxiliary.LKP_IPData) In kvp.Value
                                If Not Me.CurrentMaterialStream Is Nothing Then
                                    If Me.CurrentMaterialStream.Phases(0).Compounds.ContainsKey(kvp.Key) And Me.CurrentMaterialStream.Phases(0).Compounds.ContainsKey(kvp2.Key) Then
                                        .Item(.Count - 1).Add(New XElement("InteractionParameter", New XAttribute("Compound1", kvp.Key),
                                                                           New XAttribute("Compound2", kvp2.Key),
                                                                           New XAttribute("Value", kvp2.Value.kij.ToString(ci))))
                                    End If
                                End If
                            Next
                        Next

                    Case "Raoult's Law", "IAPWS-IF97 Steam Tables"
                    Case "Chao-Seader"
                    Case "Grayson-Streed"

                End Select

            End With

            elements.Add(New XElement("FlashSettings"))

            For Each item In FlashSettings
                elements(elements.Count - 1).Add(New XElement("Setting", New XAttribute("Name", item.Key), New XAttribute("Value", item.Value)))
            Next

            Return elements

        End Function

#End Region

#Region "   DWSIM IPropertyPackage implementation"

        Public Function AUX_CPm1(phase As Enums.PhaseLabel, Ti As Double) As Double Implements IPropertyPackage.AUX_CPm

            Select Case phase
                Case Enums.PhaseLabel.Vapor
                    Return AUX_CPm(PropertyPackages.Phase.Vapor, Ti)
                Case Enums.PhaseLabel.Solid
                    Return AUX_CPm(PropertyPackages.Phase.Solid, Ti)
                Case Enums.PhaseLabel.Mixture
                    Return AUX_CPm(PropertyPackages.Phase.Mixture, Ti)
                Case Enums.PhaseLabel.LiquidMixture
                    Return AUX_CPm(PropertyPackages.Phase.Liquid, Ti)
                Case Else
                    Return AUX_CPm(PropertyPackages.Phase.Mixture, Ti)
            End Select

        End Function

        Public Function AUX_MMM1(phase As Enums.PhaseLabel) As Double Implements IPropertyPackage.AUX_MMM

            Select Case phase
                Case Enums.PhaseLabel.Vapor
                    Return AUX_MMM(PropertyPackages.Phase.Vapor)
                Case Enums.PhaseLabel.Solid
                    Return AUX_MMM(PropertyPackages.Phase.Solid)
                Case Enums.PhaseLabel.Mixture
                    Return AUX_MMM(PropertyPackages.Phase.Mixture)
                Case Enums.PhaseLabel.LiquidMixture
                    Return AUX_MMM(PropertyPackages.Phase.Liquid)
                Case Else
                    Return AUX_MMM(PropertyPackages.Phase.Mixture)
            End Select
        End Function

        Public Function Clone1() As IPropertyPackage Implements IPropertyPackage.Clone
            Return Clone()
        End Function

        <XmlIgnore> Public Property FlashAlgorithm As Interfaces.IFlashAlgorithm Implements IPropertyPackage.FlashAlgorithm

        Public Overridable Sub DisplayEditingForm() Implements IPropertyPackage.DisplayEditingForm

        End Sub

        Public Overridable Function GetEditingForm() As System.Windows.Forms.Form

            Return New FormConfigPropertyPackage() With {._pp = Me, ._comps = Flowsheet.SelectedCompounds}

        End Function

        Public Sub DisplayFlashConfigForm()
            Dim fset As New FlashAlgorithmConfig
            fset.Settings = FlashSettings
            fset.PropPack = Me
            If Settings.IsRunningOnMono() Then
                fset.ShowDialog()
            Else
                fset.Show()
            End If
        End Sub

        Public Overridable Sub DisplayGroupedEditingForm() Implements IPropertyPackage.DisplayGroupedEditingForm

            If TypeOf Me Is CAPEOPENPropertyPackage Then

                DisplayEditingForm()

            Else

                Dim eform = GetEditingForm()
                eform.TopLevel = False
                eform.FormBorderStyle = FormBorderStyle.None
                eform.Dock = DockStyle.Fill
                eform.Visible = True

                Dim fset As New FlashAlgorithmConfig
                fset.Settings = FlashSettings
                fset.PropPack = Me
                fset.TopLevel = False
                fset.FormBorderStyle = FormBorderStyle.None
                fset.Dock = DockStyle.Fill
                fset.Visible = True

                Dim pform = New PropertyPackageSettingsEditingControl(Me) With {.Dock = DockStyle.Fill}

                Dim peditor As New Thermodynamics.FormGroupedPPConfigWindows()
                peditor.Flowsheet = Flowsheet
                peditor.PropertyPackage = Me

                peditor.TabPageBIPs.Controls.Add(eform)
                peditor.TabPageFlash.Controls.Add(fset)
                peditor.TabPageProps.Controls.Add(pform)

                peditor.Text += " (" & Tag & ") [" + ComponentName + "]"

                AddHandler peditor.FormClosing, Sub(s2, e2)
                                                    fset.FlashAlgorithmConfig_FormClosing(s2, e2)
                                                End Sub

                If Settings.IsRunningOnMono() Then
                    peditor.ShowDialog()
                Else
                    If Flowsheet IsNot Nothing Then
                        Flowsheet.DisplayForm(peditor)
                    Else
                        peditor.Show()
                    End If
                End If

            End If

        End Sub


        Public Function DisplayAdvancedEditingForm() As Object Implements IPropertyPackage.DisplayAdvancedEditingForm

            Dim form = GetAdvancedEditingForm()

            Dim sf = GlobalSettings.Settings.DpiScale

            If GlobalSettings.Settings.OldUI Then
                form.Topmost = True
                form.Show()
                form.Center()
                ExtensionMethods.Eto.Extensions2.SetFontAndPadding(form)
                form.Width += 25 * sf
                form.Height += 70 * sf
                Return form
            Else
                form.Topmost = True
                form.Show()
                Return form
            End If

        End Function

        Public Function GetAdvancedEditingForm() As Eto.Forms.Form

            Dim containers = GetAdvancedEditingContainers()

            Dim sf = GlobalSettings.Settings.DpiScale

            If GlobalSettings.Settings.OldUI Then
                Dim form = sui.GetDefaultEditorForm("Advanced Property Package Settings", 700 * sf, 600 * sf, containers(1))
                form.Topmost = True
                Return form
            Else
                Dim form = sui.GetDefaultTabbedForm("Advanced Property Package Settings", 700, 600, {containers(0), containers(1)})
                form.Topmost = True
                Return form
            End If


        End Function

        Public Function GetAdvancedEditingContainers() As Eto.Forms.DynamicLayout()

            Dim sf = GlobalSettings.Settings.DpiScale

            Dim container1 = sui.GetDefaultContainer()

            container1.Tag = "Advanced Settings"
            container1.CreateAndAddLabelRow("Forced Solids")
            container1.CreateAndAddLabelRow2("Select the compounds which will be forcedly put into the solid phase." & vbCrLf &
                                             "This setting will work only with the Nested Loops SVLE (Eutetic) Flash Algorithm.")

            For Each comp In Flowsheet.SelectedCompounds.Values
                container1.CreateAndAddCheckBoxRow(comp.Name, ForcedSolids.Contains(comp.Name),
                                                   Sub(sender, e)
                                                       If sender.Checked.GetValueOrDefault Then
                                                           If Not ForcedSolids.Contains(comp.Name) Then ForcedSolids.Add(comp.Name)
                                                       Else
                                                           If ForcedSolids.Contains(comp.Name) Then ForcedSolids.Remove(comp.Name)
                                                       End If
                                                   End Sub)
            Next

            Dim container2 = sui.GetDefaultContainer()

            container2.Tag = "Property Overrides"
            container2.CreateAndAddLabelRow("Override Phase Properties")
            container2.CreateAndAddLabelRow2("You can write a python script to override calculated phase properties." & vbCrLf &
                                             "Select the Phase/Property pair and write the override script. Your script must contain a variable named 'propval' which will hold the override value." & vbCrLf & vbCrLf &
                                             "Available variables:" & vbCrLf & vbCrLf &
                                             "'flowsheet': the Flowsheet Object" & vbCrLf &
                                             "'this': the Property Package itself" & vbCrLf &
                                             "'matstr': the currently associated Material Stream" & vbCrLf &
                                             "'phase': the current phase object" & vbCrLf &
                                             "'currval': the current property value" & vbCrLf &
                                             "'T': temperature (K) of the currently associated Material Stream" & vbCrLf &
                                             "'P': pressure (Pa) of the currently associated Material Stream")

            Dim phases = New String() {"Mixture", "Vapor", "OverallLiquid", "Liquid1", "Liquid2", "Solid"}

            Dim pprops = Type.GetType("DWSIM.Thermodynamics.BaseClasses.PhaseProperties").GetProperties()

            Dim plist As New List(Of String)

            plist.Add("")
            For Each p In phases
                For Each prop In pprops
                    plist.Add(p & "/" & prop.Name)
                Next
            Next

            Dim codeeditor As New Eto.Forms.Controls.Scintilla.Shared.ScintillaControl() With {.Height = 300, .Width = 670}

            If GlobalSettings.Settings.OldUI Then
                codeeditor.Height *= sf
                codeeditor.Width *= sf
            End If

            Dim dd = container2.CreateAndAddDropDownRow("Phase/Property", plist, 0,
                                               Sub(sender, e)
                                                   If PropertyOverrides.ContainsKey(sender.SelectedKey) Then
                                                       codeeditor.ScriptText = PropertyOverrides(sender.SelectedKey)
                                                   Else
                                                       codeeditor.ScriptText = ""
                                                   End If
                                               End Sub)

            container2.CreateAndAddEmptySpace()
            container2.CreateAndAddControlRow(codeeditor)
            container2.CreateAndAddLabelAndButtonRow("Update/Save Override Script", "Save", Nothing,
                                             Sub(sender, e)
                                                 If Not PropertyOverrides.ContainsKey(dd.SelectedKey) Then
                                                     PropertyOverrides.Add(dd.SelectedKey, "")
                                                 End If
                                                 PropertyOverrides(dd.SelectedKey) = codeeditor.ScriptText
                                             End Sub)
            container2.CreateAndAddLabelAndButtonRow("Clear/Remove Override Script", "Clear", Nothing,
                                             Sub(sender, e)
                                                 codeeditor.ScriptText = ""
                                                 If PropertyOverrides.ContainsKey(dd.SelectedKey) Then
                                                     PropertyOverrides.Remove(dd.SelectedKey)
                                                 End If
                                             End Sub)
            container2.CreateAndAddLabelAndButtonRow("View SI Units for Phase Properties", "View Units", Nothing,
                                                     Sub(sender, e)
                                                         Process.Start("https://github.com/DanWBR/dwsim6/blob/windows/DWSIM.SharedClasses/UnitsOfMeasure/SystemsOfUnits.vb#L278")
                                                     End Sub)

            Return New Eto.Forms.DynamicLayout() {container1, container2}

        End Function

        Public Overridable Function GetDisplayIcon() As Drawing.Bitmap

            Return My.Resources.DWSIM_ico_64

        End Function

        <JsonIgnore> <XmlIgnore> Property Flowsheet As IFlowsheet Implements IPropertyPackage.Flowsheet
            Get
                Return m_Flowsheet
            End Get
            Set(value As IFlowsheet)
                m_Flowsheet = value
            End Set
        End Property

        Public MustOverride ReadOnly Property MobileCompatible As Boolean Implements IPropertyPackage.MobileCompatible

        Public Overridable Function ReturnInstance(typename As String) As Object Implements IPropertyPackage.ReturnInstance
            If typename.StartsWith("PropertyPackage") Then typename = typename.Insert(0, "DWSIM.Thermodynamics.")
            typename = typename.Replace("SoaveRedlichKwong", "SRK")
            Dim t As Type = Type.GetType(typename, False)
            Return Activator.CreateInstance(t)
        End Function

        Public Shared Function GetOverallPropList() As String()
            Dim arr As New List(Of String)
            With arr
                .Add("heatCapacityCp")
                .Add("heatCapacityCv")
                .Add("thermalConductivity")
                .Add("volume")
                .Add("density")
                .Add("enthalpy")
                .Add("entropy")
                .Add("molecularWeight")
            End With
            Dim arr2(arr.Count - 1) As String
            Array.Copy(arr.ToArray(), arr2, arr.Count)
            Return arr2
        End Function

        ''' <summary>
        ''' Sets the Phase Equilibria Calculation mode for this instance. Default behavior is SVLLE. 
        ''' </summary>
        ''' <param name="mode">Recognized values: 'Default', 'VLE', 'VLLE', 'SVLE' and 'SVLLE'.</param>
        Public Sub SetPhaseEquilibriaCalculationMode(mode As String)

            FlashSettings(FlashSetting.ForceEquilibriumCalculationType) = mode

        End Sub

#End Region

#Region "   Derivatives"

        Public Overridable Function DW_CalcdEnthalpydmoles(ByVal Vx As Double(), ByVal T As Double, ByVal P As Double, ByVal st As State) As Double()

            Dim nmols As Double() = Vx.Clone
            Dim n As Integer = Vx.Length - 1
            Dim i, k As Integer

            Dim deltan As Double = 0.0000001

            Dim deriv As Double() = Vx.Clone()

            Dim H1, H2, M1, M2 As Double

            M1 = AUX_MMM(Vx)
            H1 = (DW_CalcEnthalpy(Vx, T, P, st) + AUX_HFm25(Vx)) * M1

            For i = 0 To n
                Dim newVx As Double() = Vx.Clone()
                For k = 0 To n
                    If i = k Then
                        newVx(k) = nmols(k) + deltan
                    Else
                        newVx(k) = nmols(k)
                    End If
                Next
                newVx = newVx.NormalizeY()
                M2 = AUX_MMM(newVx)
                H2 = (DW_CalcEnthalpy(newVx, T, P, st) + AUX_HFm25(newVx)) * M2
                deriv(i) = (H2 * (1 + deltan) - H1) / deltan
            Next

            Return deriv

        End Function

        Public Overridable Function DW_CalcdEntropydmoles(ByVal Vx As Double(), ByVal T As Double, ByVal P As Double, ByVal st As State) As Double()

            Dim nmols As Double() = Vx.Clone
            Dim n As Integer = Vx.Length - 1
            Dim i, k As Integer

            Dim deltan As Double = 0.0000001

            Dim deriv As Double() = Vx.Clone()

            Dim S1, S2, M1, M2 As Double

            M1 = AUX_MMM(Vx)
            S1 = (DW_CalcEntropy(Vx, T, P, st) + AUX_SFm25(Vx)) * M1

            For i = 0 To n
                Dim newVx As Double() = Vx.Clone()
                For k = 0 To n
                    If i = k Then
                        newVx(k) = nmols(k) + deltan
                    Else
                        newVx(k) = nmols(k)
                    End If
                Next
                'newVx = newVx.NormalizeY()
                M2 = AUX_MMM(newVx)
                S2 = (DW_CalcEntropy(newVx, T, P, st) + AUX_SFm25(newVx)) * M2
                deriv(i) = (S2 * (1 + deltan) - S1) / deltan
            Next

            Return deriv

        End Function

        Public Overridable Function DW_CalcdEnthalpydT(ByVal Vx As Double(), ByVal T As Double, ByVal P As Double, ByVal st As State) As Double

            Dim deriv As Double

            Dim H1, H2 As Double

            H1 = DW_CalcEnthalpy(Vx, T, P, st)
            H2 = DW_CalcEnthalpy(Vx, T + 0.01, P, st)

            deriv = (H2 - H1) / 0.01

            Return deriv

        End Function

        Public Overridable Function DW_CalcdEntropydT(ByVal Vx As Double(), ByVal T As Double, ByVal P As Double, ByVal st As State) As Double

            Dim deriv As Double

            Dim S1, S2 As Double

            S1 = DW_CalcEntropy(Vx, T, P, st)
            S2 = DW_CalcEntropy(Vx, T + 0.01, P, st)

            deriv = (S2 - S1) / 0.01

            Return deriv

        End Function

        Public Overridable Function DW_CalcdKdT(ByVal Vx As Double(), ByVal Vy As Double(), ByVal T As Double, ByVal P As Double, Optional ByVal type As String = "LV") As Double()

            Dim deriv As Double()

            Dim K1, K2 As Double()

            K1 = DW_CalcKvalue(Vx, Vy, T, P, type)
            K2 = DW_CalcKvalue(Vx, Vy, T + 0.01, P, type)

            deriv = K2.SubtractY(K1).MultiplyConstY(1.0 / 0.01)

            Return deriv

        End Function

        Public Overridable Function DW_CalcdFugCoeffdT(ByVal Vx As Double(), ByVal T As Double, ByVal P As Double, st As State) As Double()

            Dim deriv As Double()

            Dim F1, F2 As Double()

            F1 = DW_CalcFugCoeff(Vx, T, P, st)
            F2 = DW_CalcFugCoeff(Vx, T + 0.01, P, st)

            deriv = F2.SubtractY(F1).MultiplyConstY(1.0 / 0.01)

            Return deriv

        End Function

        Public Function GetAsObject() As Object Implements IPropertyPackage.GetAsObject
            Return Me
        End Function

#End Region

    End Class

    ''' <summary>
    ''' Class to store Phase Info and mapping for CAPE-OPEN Property Packages
    ''' </summary>
    ''' <remarks>Used only in the context of CAPE-OPEN Objects.</remarks>
    <System.Serializable()> Public Class PhaseInfo

        Public PhaseLabel As String = ""
        Public DWPhaseIndex As Integer
        Public DWPhaseID As Phase = Phase.Mixture

        Sub New(ByVal pl As String, ByVal pi As Integer, ByVal pid As Phase)
            PhaseLabel = pl
            DWPhaseIndex = pi
            DWPhaseID = pid
        End Sub

    End Class

    ''' <summary>
    ''' COM IStream Class Implementation
    ''' </summary>
    ''' <remarks></remarks>
    <System.Serializable()> Public Class ComStreamWrapper
        Inherits System.IO.Stream
        Private mSource As IStream
        Private mInt64 As IntPtr

        Public Sub New(ByVal source As IStream)
            mSource = source
            mInt64 = iop.Marshal.AllocCoTaskMem(8)
        End Sub

        Protected Overrides Sub Finalize()
            Try
                iop.Marshal.FreeCoTaskMem(mInt64)
            Finally
                MyBase.Finalize()
            End Try
        End Sub

        Public Overrides ReadOnly Property CanRead() As Boolean
            Get
                Return True
            End Get
        End Property

        Public Overrides ReadOnly Property CanSeek() As Boolean
            Get
                Return True
            End Get
        End Property

        Public Overrides ReadOnly Property CanWrite() As Boolean
            Get
                Return True
            End Get
        End Property

        Public Overrides Sub Flush()
            mSource.Commit(0)
        End Sub

        Public Overrides ReadOnly Property Length() As Long
            Get
                Dim stat As ComTypes.STATSTG
                stat = Nothing
                mSource.Stat(stat, 1)
                Return stat.cbSize
            End Get
        End Property

        Public Overrides Property Position() As Long
            Get
                Throw New NotImplementedException()
            End Get
            Set(ByVal value As Long)
                Throw New NotImplementedException()
            End Set
        End Property

        Public Overrides Function Read(ByVal buffer As Byte(), ByVal offset As Integer, ByVal count As Integer) As Integer
            If offset <> 0 Then
                Throw New NotImplementedException()
            End If
            mSource.Read(buffer, count, mInt64)
            Return iop.Marshal.ReadInt32(mInt64)
        End Function

        Public Overrides Function Seek(ByVal offset As Long, ByVal origin As System.IO.SeekOrigin) As Long
            mSource.Seek(offset, Convert.ToInt32(origin), mInt64)
            Return iop.Marshal.ReadInt64(mInt64)
        End Function

        Public Overrides Sub SetLength(ByVal value As Long)
            mSource.SetSize(value)
        End Sub

        Public Overrides Sub Write(ByVal buffer As Byte(), ByVal offset As Integer, ByVal count As Integer)
            If offset <> 0 Then
                Throw New NotImplementedException()
            End If
            mSource.Write(buffer, count, IntPtr.Zero)
        End Sub

    End Class

    <System.Serializable()> Public Class HenryParam
        Public Component As String
        Public CAS As String
        Public KHcp As Double
        Public C As Double
    End Class

End Namespace
