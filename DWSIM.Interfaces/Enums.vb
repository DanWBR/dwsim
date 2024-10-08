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

    Public Class Dynamics

        Public Enum DynamicsSpecType
            Pressure = 0
            Flow = 1
        End Enum

        Public Enum DynamicsEventType
            ChangeProperty = 0
            RunScript = 1
        End Enum

        Public Enum DynamicsAlarmType
            LL = 0
            L = 1
            H = 2
            HH = 3
        End Enum

        Public Enum DynamicsEventTransitionType
            StepChange = 0
            LinearChange = 1
            LogChange = 3
            InverseLogChange = 4
            RandomChange = 5
        End Enum

        Public Enum DynamicsEventTransitionReferenceType
            InitialState = 0
            PreviousEvent = 1
            SpecificEvent = 2
        End Enum

    End Class

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
            Integrator = 3
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
            IntegratorStarted = 14
            IntegratorFinished = 15
            IntegratorError = 16
            IntegratorStep = 17
            IntegratorPreStep = 18
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

    Public Enum SnapshotType
        All = 0
        ObjectLayout = 1
        ObjectData = 2
        Compounds = 3
        ReactionSubsystem = 4
        PropertyPackages = 5
        Spreadsheet = 6
        SimulationSettings = 7
        WindowLayout = 8
        ObjectAddedOrRemoved = 9
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

        accel = 0
        activity = 1
        activityCoefficient = 2
        area = 3
        boilingPointTemperature = 4
        cakeresistance = 5
        cinematic_viscosity = 6
        compressibility = 7
        compressibilityFactor = 8
        deltaP = 9
        deltaT = 10
        density = 11
        diameter = 12
        distance = 13
        enthalpy = 14
        entropy = 15
        excessEnthalpy = 16
        excessEntropy = 17
        force = 18
        foulingfactor = 19
        fugacity = 20
        fugacityCoefficient = 21
        gor = 22
        head = 23
        heat_transf_coeff = 24
        heatCapacityCp = 25
        heatCapacityCv = 26
        heatflow = 27
        idealGasHeatCapacity = 28
        jouleThomsonCoefficient = 29
        kvalue = 30
        logFugacityCoefficient = 31
        logKvalue = 32
        mass = 33
        mass_conc = 34
        massflow = 35
        massfraction = 36
        mediumresistance = 37
        meltingTemperature = 38
        molar_conc = 39
        molar_enthalpy = 40
        molar_entropy = 41
        molar_volume = 42
        molarflow = 43
        molarfraction = 44
        molecularWeight = 45
        pressure = 46
        reac_rate = 47
        reac_rate_heterog = 48
        spec_vol = 49
        speedOfSound = 50
        surfaceTension = 51
        temperature = 52
        thermalConductivity = 53
        thermalConductivityOfLiquid = 54
        thermalConductivityOfVapor = 55
        thickness = 56
        time = 57
        vaporPressure = 58
        velocity = 59
        viscosity = 60
        viscosityOfLiquid = 61
        viscosityOfVapor = 62
        volume = 63
        volumetricFlow = 64
        diffusivity = 65
        none = 66
        conductance = 67
        heat = 68
        mole = 69
        emission_factor = 70
        specificpower = 71

    End Enum

    Public Enum OptimizationMethod
        Limited_Memory_BGFS = 0
        Truncated_Newton = 1
        Simplex = 2
        IPOPT = 3
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
        Gibbs_Minimization_Multiphase = 21
        Universal = 22
        Custom = 23
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

        PHFlash_MaximumTemperatureChange = 23

        PTFlash_DampingFactor = 24

        ForceEquilibriumCalculationType = 25

        ImmiscibleWaterOption = 26

        HandleSolidsInDefaultEqCalcMode = 27

        UseIOFlash = 28

        GibbsMinimizationExternalSolver = 29

        GibbsMinimizationExternalSolverConfigData = 30

        PHFlash_Use_Interpolated_Result_In_Oscillating_Temperature_Cases = 31

        PVFlash_TryIdealCalcOnFailure = 32

        '0 = Rigorous VLE, 1 = Ideal VLE, 2 = NoFlash, 3 = throw error
        FailSafeCalculationMode = 33

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
        VolumeTemperature = 9
        VolumePressure = 10
        VolumeEnthalpy = 11
        VolumeEntropy = 12
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
        Volume_and_Temperature = 6
        Volume_and_Enthalpy = 7
        Volume_and_Entropy = 8
    End Enum

    Public Enum FlowSpec
        Mass = 0
        Mole = 1
        Volumetric = 2
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

    Public Enum ForcedPhase
        None
        Vapor
        Liquid
        Solid
        GlobalDef
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

    Public Enum ReactionPhase
        Liquid
        Vapor
        Mixture
        Solid
        Liquid_Solid
        Vapor_Solid
    End Enum

    Public Enum ReactionKineticType
        Arrhenius = 0
        UserDefined = 1
    End Enum

    Public Enum ReactionKinetics
        Expression = 0
        PythonScript = 1
    End Enum

    Public Enum KOpt
        Gibbs = 0
        Expression = 1
        Constant = 2
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
        Indicators = 12
        Controllers = 13
        Switches = 14
        Inputs = 15
        None = 16
        CleanPowerSources = 17
        Electrolyzers = 18

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

    Public Enum ExtenderLevel

        MainWindow = 0
        FlowsheetWindow = 1

    End Enum

    Public Enum ExtenderCategory

        File = 0
        Edit = 1
        Settings = 2
        Tools = 3
        Utilities = 4
        Dynamics = 5
        Optimization = 6
        Results = 7
        View = 8
        Help = 9
        NewItem = 10
        FlowsheetSurfaceSelected = 11
        FlowsheetSurfaceNotSelected = 12
        ToolStrip = 13
        InitializationScript = 14

    End Enum

    Public Enum ExternalSolverCategory

        LinearSystem = 0
        NonLinearSystem = 1
        NonLinearMinimization = 2
        OrdinaryDifferentialEquations = 3

    End Enum

    Public Enum SpecCalcMode

        AfterSourceObject = 0
        BeforeTargetObject = 1
        BeforeFlowsheet = 2
        AfterFlowsheet = 3

    End Enum

    Public Enum SpecCalcMode2

        GlobalSetting = 0
        AfterSourceObject = 1
        BeforeTargetObject = 2
        BeforeFlowsheet = 3
        AfterFlowsheet = 4
        AfterObject = 5
        BeforeObject = 6

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

        AnalogGauge
        DigitalGauge
        LevelGauge

        Controller_PID

        Switch

        Input

        GO_HTMLText
        GO_Button

        AirCooler2
        WindTurbine
        HydroelectricTurbine
        SolarPanel
        PEMFuelCell
        WaterElectrolyzer
        RCT_GibbsReaktoro

        EnergyMixer
        Mixer
        Splitter

        Controller_Python

        Dummy

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

    Public Enum FontStyle
        Regular = 0
        Bold = 1
        Italic = 2
        BoldItalic = 3
    End Enum

    Public Enum PointValueType

        Temperature = 0
        Pressure = 1
        Flow = 2
        EnergyFlow = 3
        Concentration = 4
        CompoundMassFlow = 5
        CompoundMolarFlow = 6
        CompoundMassFraction = 7
        CompoundMolarFraction = 8
        MeanSolidParticleSize = 9

    End Enum

End Namespace



