'    DWSIM Interface definitions
'    Copyright 2010-2017 Daniel Wagner O. de Medeiros
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

Namespace Enums

    Public Class Helpers
        Public Shared Function GetEnumType(typename As String) As Type

            Return Reflection.Assembly.GetExecutingAssembly().GetType(typename)

        End Function

    End Class

    Public Class Scripts

        Public Enum Interpreter
            IronPython = 0
            Python_NET = 1
        End Enum

        Public Enum ObjectType
            Simulation = 0
            Solver = 1
            FlowsheetObject = 2
        End Enum

        Public Enum EventType
            SimulationOpened = 0
            SimulationSaved = 1
            SimulationClosed = 2
            ObjectCalculationStarted = 3
            ObjectCalculationFinished = 4
            ObjectCalculationError = 5
            SolverStarted = 6
            SolverFinished = 7
            SolverRecycleLoop = 8
            SimulationTimer1 = 9
            SimulationTimer5 = 10
            SimulationTimer15 = 11
            SimulationTimer30 = 12
            SimulationTimer60 = 13
        End Enum

    End Class

    Public Enum SampleType
        Light = 0
        Average = 1
        Heavy = 2
    End Enum

    Public Enum UndoRedoActionType
        SimulationObjectPropertyChanged = 0
        FlowsheetObjectPropertyChanged = 1
        FlowsheetObjectConnected = 2
        FlowsheetObjectDisconnected = 3
        ObjectAdded = 4
        ObjectRemoved = 5
        SystemOfUnitsAdded = 6
        SystemOfUnitsRemoved = 7
        SystemOfUnitsChanged = 8
        CompoundAdded = 9
        CompoundRemoved = 10
        PropertyPackagePropertyChanged = 11
        PropertyPackageAdded = 12
        PropertyPackageRemoved = 13
        CutObjects = 14
        PasteObjects = 15
        SpreadsheetCellChanged = 16
    End Enum

    Public Enum FlowsheetUtility
        PhaseEnvelope = 0
        PhaseEnvelopeBinary = 1
        PhaseEnvelopeTernary = 2
        NaturalGasHydrates = 3
        TrueCriticalPoint = 4
        PSVSizing = 5
        SeparatorSizing = 6
        PetroleumProperties = 7
        PureCompoundProperties = 8
    End Enum

    Public Enum UnitOfMeasure

        accel
        activity
        activityCoefficient
        area
        boilingPointTemperature
        cakeresistance
        cinematic_viscosity
        compressibility
        compressibilityFactor
        deltaP
        deltaT
        density
        diameter
        distance
        enthalpy
        entropy
        excessEnthalpy
        excessEntropy
        force
        foulingfactor
        fugacity
        fugacityCoefficient
        gor
        head
        heat_transf_coeff
        heatCapacityCp
        heatCapacityCv
        heatflow
        idealGasHeatCapacity
        jouleThomsonCoefficient
        kvalue
        logFugacityCoefficient
        logKvalue
        mass
        mass_conc
        massflow
        massfraction
        mediumresistance
        meltingTemperature
        molar_conc
        molar_enthalpy
        molar_entropy
        molar_volume
        molarflow
        molarfraction
        molecularWeight
        pressure
        reac_rate
        reac_rate_heterog
        spec_vol
        speedOfSound
        surfaceTension
        temperature
        thermalConductivity
        thermalConductivityOfLiquid
        thermalConductivityOfVapor
        thickness
        time
        vaporPressure
        velocity
        viscosity
        viscosityOfLiquid
        viscosityOfVapor
        volume
        volumetricFlow
        diffusivity
        none

    End Enum

    Public Enum OptimizationMethod
        Limited_Memory_BGFS = 0
        Truncated_Newton = 1
        Simplex = 2
        IPOPT = 3
        ParticleSwarm = 4
        LocalUnimodalSampling = 5
        GradientDescent = 6
        DifferentialEvolution = 7
        ParticleSwarmOptimization = 8
        ManyOptimizingLiaisons = 9
        Mesh = 10
    End Enum

    Public Enum AccelMethod
        None
        Wegstein
        Dominant_Eigenvalue
        GlobalBroyden
    End Enum

    Public Enum FlashMethod
        Default_Algorithm = 0
        Nested_Loops_VLE = 1
        Nested_Loops_VLLE = 2
        Nested_Loops_Immiscible_VLLE = 3
        Inside_Out_VLE = 4
        Inside_Out_VLLE = 5
        Gibbs_Minimization_VLE = 6
        Gibbs_Minimization_VLLE = 7
        Simple_LLE = 8
        Nested_Loops_SLE_Eutectic = 9
        Nested_Loops_SLE_SolidSolution = 10
        BlackOil = 11
        Electrolyte = 12
        SeaWater = 13
        SourWater = 14
        CAPE_OPEN_Equilibrium_Server = 15
        SteamTables = 16
        Nested_Loops_SVLLE = 17
        CoolProp_Incompressibles = 18
        CoolProp_IncompressibleMixtures = 19
        UserDefined = 20
    End Enum

    Public Enum FlashSetting

        PTFlash_External_Loop_Tolerance = 0
        PTFlash_Internal_Loop_Tolerance = 1
        PTFlash_Maximum_Number_Of_External_Iterations = 2
        PTFlash_Maximum_Number_Of_Internal_Iterations = 3

        PHFlash_Internal_Loop_Tolerance = 4
        PHFlash_External_Loop_Tolerance = 5
        PHFlash_Maximum_Number_Of_Internal_Iterations = 6
        PHFlash_Maximum_Number_Of_External_Iterations = 7

        ThreePhaseFlashStabTestSeverity = 8
        ThreePhaseFlashStabTestCompIds = 9

        CalculateBubbleAndDewPoints = 10

        ValidateEquilibriumCalc = 11
        ValidationGibbsTolerance = 12

        UsePhaseIdentificationAlgorithm = 13

        Replace_PTFlash = 14

        GM_OptimizationMethod = 15

        IO_FastMode = 16

        NL_FastMode = 17

        PVFlash_TemperatureDerivativeEpsilon = 18
        PVFlash_MaximumTemperatureChange = 19
        PVFlash_FixedDampingFactor = 20

        ST_Number_of_Random_Tries = 21

        CheckIncipientLiquidForStability = 22

    End Enum

    Public Enum FlashCalculationType
        PressureTemperature = 0
        PressureEnthalpy = 1
        PressureEntropy = 2
        TemperatureEnthalpy = 3
        TemperatureEntropy = 4
        PressureVaporFraction = 5
        TemperatureVaporFraction = 6
        PressureSolidFraction = 7
        TemperatureSolidFraction = 8
    End Enum

    Public Enum AdjustVarType
        Manipulated = 0
        Controlled = 1
        Reference = 2
        None = 3
    End Enum

    Public Enum SpecVarType
        Source = 0
        Target = 1
        None = 2
    End Enum

    Public Enum PropertyType
        RO = 0
        RW = 1
        WR = 2
        ALL = 3
    End Enum

    Public Enum StreamSpec
        Temperature_and_Pressure = 0
        Pressure_and_Enthalpy = 1
        Pressure_and_Entropy = 2
        Pressure_and_VaporFraction = 3
        Temperature_and_VaporFraction = 4
        Pressure_and_SolidFraction = 5
    End Enum

    Public Enum CompositionBasis
        Molar_Fractions
        Mass_Fractions
        Volumetric_Fractions
        Molar_Flows
        Mass_Flows
        Volumetric_Flows
        DefaultBasis
    End Enum

    Public Enum PhaseLabel
        Mixture
        Vapor
        LiquidMixture
        Liquid1
        Liquid2
        Liquid3
        Aqueous
        Solid
    End Enum

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

    Public Enum ReactionKineticType
        Arrhenius = 0
        UserDefined = 1
    End Enum

    Public Enum KOpt
        Gibbs
        Expression
        Constant
    End Enum

    Public Enum WarningType
        Ignore = 0
        ShowWarning = 1
        RaiseError = 2
    End Enum

    Public Enum ListPosition
        Top = 0
        TopRight = 1
        Right = 2
        RightBottom = 3
        Bottom = 4
        BottomLeft = 5
        Left = 6
        LeftTop = 7
    End Enum

    Public Enum SimulationObjectClass

        Streams = 0
        PressureChangers = 1
        Separators = 2
        MixersSplitters = 3
        Exchangers = 4
        Reactors = 5
        Columns = 6
        Solids = 7
        CAPEOPEN = 8
        UserModels = 9
        Logical = 10
        Other = 11

    End Enum

    Public Enum ReportItemType

        Label = 0
        Description = 1
        TripleColumn = 2
        DoubleColumn = 3
        SingleColumn = 4

    End Enum

    Public Enum ReportType

        PlainText = 0
        RichText = 1
        Structured = 2

    End Enum

    Public Enum CompoundOrdering

        AsAdded = 0
        Name_ASC = 1
        Name_DESC = 2
        NBP_ASC = 3
        NBP_DESC = 4
        MW_ASC = 5
        MW_DESC = 6
        CAS_ASC = 7
        CAS_DESC = 8
        TAG_ASC = 9
        TAG_DESC = 10

    End Enum

    Public Enum ChartType

        TwoDimensionalScatter = 0
        Pie = 1
        HorizontalBars = 2
        VerticalBars = 3

    End Enum

    Public Enum ChartSource

        SpreadsheetRange = 0
        FlowsheetObject = 1

    End Enum

End Namespace

Namespace Enums.GraphicObjects

    Public Enum ConType

        ConIn = -1
        ConOut = 1
        ConEn = 0
        ConSp = 2

    End Enum
    Public Enum ConDir
        Up
        Down
        Right
        Left
    End Enum

    Public Enum ObjectType

        NodeIn
        NodeOut
        NodeEn
        Pump
        Tank
        Vessel
        MaterialStream
        EnergyStream
        Compressor
        Expander
        TPVessel
        Cooler
        Heater
        Pipe
        Valve
        Nenhum
        GO_Table
        GO_Text
        GO_Image
        GO_FloatingTable
        OT_Adjust
        OT_Spec
        OT_Recycle
        RCT_Conversion
        RCT_Equilibrium
        RCT_Gibbs
        RCT_CSTR
        RCT_PFR
        HeatExchanger
        ShortcutColumn
        DistillationColumn
        AbsorptionColumn
        RefluxedAbsorber
        ReboiledAbsorber
        OT_EnergyRecycle
        GO_Animation
        ComponentSeparator
        OrificePlate
        CustomUO
        ExcelUO
        CapeOpenUO
        FlowsheetUO
        GO_MasterTable
        SolidSeparator
        Filter
        GO_SpreadsheetTable
        GO_Rectangle

        CompressorExpander
        HeaterCooler

        GO_Chart
        GO_InputControl

        External

    End Enum

    Public Enum ShapeIcon

        DefaultShape
        NodeIn
        NodeOut
        Pump
        Tank
        Vessel
        Compressor
        Expander
        Cooler
        Heater
        Pipe
        Valve
        RCT_Conversion
        RCT_Equilibrium
        RCT_Gibbs
        RCT_CSTR
        RCT_PFR
        HeatExchanger
        DistillationColumn
        AbsorptionColumn
        ComponentSeparator
        OrificePlate

    End Enum

    Public Enum Status
        Calculated
        Calculating
        ErrorCalculating
        Inactive
        Idle
        NotCalculated
        Modified
    End Enum

End Namespace



