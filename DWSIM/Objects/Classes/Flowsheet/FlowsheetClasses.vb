'    Main Form Auxiliary Classes
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

Imports Microsoft.MSDN.Samples.GraphicObjects
Imports DWSIM.DWSIM.ClassesBasicasTermodinamica
Imports DWSIM.DWSIM.SimulationObjects
Imports System.Linq

Namespace DWSIM.FormClasses

    Public Enum TipoAviso
        Informacao
        Aviso
        Erro
        Dica
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
    End Enum

    <System.Serializable()> Public Class UndoRedoAction

        Property ID As String = ""
        Property Name As String = ""
        Property AType As UndoRedoActionType
        Property ObjID As String = ""
        Property ObjID2 As String = ""
        Property OldValue As Object = Nothing
        Property NewValue As Object = Nothing
        Property Tag As Object = Nothing
        Property PropertyName As String = ""

    End Class


    <System.Serializable()> Public Class ClsObjectCollections

        'Declares collections for holding graphic elements of the flowsheet.
        Public MixerCollection As Dictionary(Of String, NodeInGraphic)
        Public SplitterCollection As Dictionary(Of String, NodeOutGraphic)
        Public MaterialStreamCollection As Dictionary(Of String, MaterialStreamGraphic)
        Public EnergyStreamCollection As Dictionary(Of String, EnergyStreamGraphic)
        Public PumpCollection As Dictionary(Of String, PumpGraphic)
        Public SeparatorCollection As Dictionary(Of String, VesselGraphic)
        Public CompressorCollection As Dictionary(Of String, CompressorGraphic)
        Public PipeCollection As Dictionary(Of String, PipeGraphic)
        Public ValveCollection As Dictionary(Of String, ValveGraphic)
        Public CoolerCollection As Dictionary(Of String, CoolerGraphic)
        Public HeaterCollection As Dictionary(Of String, HeaterGraphic)
        Public TankCollection As Dictionary(Of String, TankGraphic)
        Public ConnectorCollection As Dictionary(Of String, ConnectorGraphic)
        Public TPSeparatorCollection As Dictionary(Of String, TPVesselGraphic)
        Public TurbineCollection As Dictionary(Of String, TurbineGraphic)
        Public MixerENCollection As Dictionary(Of String, NodeEnGraphic)
        Public AdjustCollection As Dictionary(Of String, AdjustGraphic)
        Public SpecCollection As Dictionary(Of String, SpecGraphic)
        Public RecycleCollection As Dictionary(Of String, RecycleGraphic)
        Public ReactorConversionCollection As Dictionary(Of String, ReactorConversionGraphic)
        Public ReactorEquilibriumCollection As Dictionary(Of String, ReactorEquilibriumGraphic)
        Public ReactorGibbsCollection As Dictionary(Of String, ReactorGibbsGraphic)
        Public ReactorCSTRCollection As Dictionary(Of String, ReactorCSTRGraphic)
        Public ReactorPFRCollection As Dictionary(Of String, ReactorPFRGraphic)
        Public HeatExchangerCollection As Dictionary(Of String, HeatExchangerGraphic)
        Public ShortcutColumnCollection As Dictionary(Of String, ShorcutColumnGraphic)
        Public DistillationColumnCollection As Dictionary(Of String, DistillationColumnGraphic)
        Public AbsorptionColumnCollection As Dictionary(Of String, AbsorptionColumnGraphic)
        Public RefluxedAbsorberCollection As Dictionary(Of String, RefluxedAbsorberGraphic)
        Public ReboiledAbsorberCollection As Dictionary(Of String, ReboiledAbsorberGraphic)
        Public EnergyRecycleCollection As Dictionary(Of String, EnergyRecycleGraphic)
        Public ComponentSeparatorCollection As Dictionary(Of String, ComponentSeparatorGraphic)
        Public OrificePlateCollection As Dictionary(Of String, OrificePlateGraphic)
        Public CustomUOCollection As Dictionary(Of String, CustomUOGraphic)
        Public ExcelUOCollection As Dictionary(Of String, ExcelUOGraphic)
        Public CapeOpenUOCollection As Dictionary(Of String, CapeOpenUOGraphic)
        Public SolidsSeparatorCollection As Dictionary(Of String, SolidSeparatorGraphic)
        Public FilterCollection As Dictionary(Of String, FilterGraphic)
        Public FlowsheetUOCollection As Dictionary(Of String, FlowsheetUOGraphic)

        Public ObjectCounter As Dictionary(Of String, Integer)

        Public ObjectCollection As Dictionary(Of String, SimulationObjects_BaseClass)

        'These are collections for holding the actual unit operations instances.
        Public CLCS_ReactorConversionCollection As Dictionary(Of String, DWSIM.SimulationObjects.Reactors.Reactor_Conversion)
        Public CLCS_ReactorEquilibriumCollection As Dictionary(Of String, DWSIM.SimulationObjects.Reactors.Reactor_Equilibrium)
        Public CLCS_ReactorGibbsCollection As Dictionary(Of String, DWSIM.SimulationObjects.Reactors.Reactor_Gibbs)
        Public CLCS_ReactorCSTRCollection As Dictionary(Of String, DWSIM.SimulationObjects.Reactors.Reactor_CSTR)
        Public CLCS_ReactorPFRCollection As Dictionary(Of String, DWSIM.SimulationObjects.Reactors.Reactor_PFR)
        Public CLCS_MaterialStreamCollection As Dictionary(Of String, DWSIM.SimulationObjects.Streams.MaterialStream)
        Public CLCS_EnergyStreamCollection As Dictionary(Of String, DWSIM.SimulationObjects.Streams.EnergyStream)
        Public CLCS_PipeCollection As Dictionary(Of String, DWSIM.SimulationObjects.UnitOps.Pipe)
        Public CLCS_MixerCollection As Dictionary(Of String, DWSIM.SimulationObjects.UnitOps.Mixer)
        Public CLCS_SplitterCollection As Dictionary(Of String, DWSIM.SimulationObjects.UnitOps.Splitter)
        Public CLCS_PumpCollection As Dictionary(Of String, DWSIM.SimulationObjects.UnitOps.Pump)
        Public CLCS_CompressorCollection As Dictionary(Of String, DWSIM.SimulationObjects.UnitOps.Compressor)
        Public CLCS_ValveCollection As Dictionary(Of String, DWSIM.SimulationObjects.UnitOps.Valve)
        Public CLCS_VesselCollection As Dictionary(Of String, DWSIM.SimulationObjects.UnitOps.Vessel)
        Public CLCS_TurbineCollection As Dictionary(Of String, DWSIM.SimulationObjects.UnitOps.Expander)
        Public CLCS_EnergyMixerCollection As Dictionary(Of String, DWSIM.SimulationObjects.UnitOps.EnergyMixer)
        Public CLCS_HeaterCollection As Dictionary(Of String, DWSIM.SimulationObjects.UnitOps.Heater)
        Public CLCS_CoolerCollection As Dictionary(Of String, DWSIM.SimulationObjects.UnitOps.Cooler)
        Public CLCS_TankCollection As Dictionary(Of String, DWSIM.SimulationObjects.UnitOps.Tank)
        Public CLCS_AdjustCollection As Dictionary(Of String, DWSIM.SimulationObjects.SpecialOps.Adjust)
        Public CLCS_SpecCollection As Dictionary(Of String, DWSIM.SimulationObjects.SpecialOps.Spec)
        Public CLCS_RecycleCollection As Dictionary(Of String, DWSIM.SimulationObjects.SpecialOps.Recycle)
        Public CLCS_HeatExchangerCollection As Dictionary(Of String, DWSIM.SimulationObjects.UnitOps.HeatExchanger)
        Public CLCS_ShortcutColumnCollection As Dictionary(Of String, DWSIM.SimulationObjects.UnitOps.ShortcutColumn)
        Public CLCS_DistillationColumnCollection As Dictionary(Of String, DWSIM.SimulationObjects.UnitOps.DistillationColumn)
        Public CLCS_AbsorptionColumnCollection As Dictionary(Of String, DWSIM.SimulationObjects.UnitOps.AbsorptionColumn)
        Public CLCS_RefluxedAbsorberCollection As Dictionary(Of String, DWSIM.SimulationObjects.UnitOps.RefluxedAbsorber)
        Public CLCS_ReboiledAbsorberCollection As Dictionary(Of String, DWSIM.SimulationObjects.UnitOps.ReboiledAbsorber)
        Public CLCS_EnergyRecycleCollection As Dictionary(Of String, DWSIM.SimulationObjects.SpecialOps.EnergyRecycle)
        Public CLCS_ComponentSeparatorCollection As Dictionary(Of String, DWSIM.SimulationObjects.UnitOps.ComponentSeparator)
        Public CLCS_OrificePlateCollection As Dictionary(Of String, DWSIM.SimulationObjects.UnitOps.OrificePlate)
        Public CLCS_CustomUOCollection As Dictionary(Of String, DWSIM.SimulationObjects.UnitOps.CustomUO)
        Public CLCS_ExcelUOCollection As Dictionary(Of String, DWSIM.SimulationObjects.UnitOps.ExcelUO)
        Public CLCS_CapeOpenUOCollection As Dictionary(Of String, DWSIM.SimulationObjects.UnitOps.CapeOpenUO)
        Public CLCS_SolidsSeparatorCollection As Dictionary(Of String, DWSIM.SimulationObjects.UnitOps.SolidsSeparator)
        Public CLCS_FilterCollection As Dictionary(Of String, DWSIM.SimulationObjects.UnitOps.Filter)
        Public CLCS_FlowsheetUOCollection As Dictionary(Of String, DWSIM.SimulationObjects.UnitOps.Flowsheet)

        Public OPT_SensAnalysisCollection As List(Of DWSIM.Optimization.SensitivityAnalysisCase)
        Public OPT_OptimizationCollection As List(Of DWSIM.Optimization.OptimizationCase)

        Sub New()

            'Creates all the graphic collections.
            MixerCollection = New Dictionary(Of String, NodeInGraphic)
            SplitterCollection = New Dictionary(Of String, NodeOutGraphic)
            MaterialStreamCollection = New Dictionary(Of String, MaterialStreamGraphic)
            EnergyStreamCollection = New Dictionary(Of String, EnergyStreamGraphic)
            PumpCollection = New Dictionary(Of String, PumpGraphic)
            SeparatorCollection = New Dictionary(Of String, VesselGraphic)
            CompressorCollection = New Dictionary(Of String, CompressorGraphic)
            PipeCollection = New Dictionary(Of String, PipeGraphic)
            ValveCollection = New Dictionary(Of String, ValveGraphic)
            CoolerCollection = New Dictionary(Of String, CoolerGraphic)
            HeaterCollection = New Dictionary(Of String, HeaterGraphic)
            TankCollection = New Dictionary(Of String, TankGraphic)
            ConnectorCollection = New Dictionary(Of String, ConnectorGraphic)
            TPSeparatorCollection = New Dictionary(Of String, TPVesselGraphic)
            TurbineCollection = New Dictionary(Of String, TurbineGraphic)
            MixerENCollection = New Dictionary(Of String, NodeEnGraphic)
            AdjustCollection = New Dictionary(Of String, AdjustGraphic)
            SpecCollection = New Dictionary(Of String, SpecGraphic)
            RecycleCollection = New Dictionary(Of String, RecycleGraphic)
            ReactorConversionCollection = New Dictionary(Of String, ReactorConversionGraphic)
            ReactorEquilibriumCollection = New Dictionary(Of String, ReactorEquilibriumGraphic)
            ReactorGibbsCollection = New Dictionary(Of String, ReactorGibbsGraphic)
            ReactorCSTRCollection = New Dictionary(Of String, ReactorCSTRGraphic)
            ReactorPFRCollection = New Dictionary(Of String, ReactorPFRGraphic)
            HeatExchangerCollection = New Dictionary(Of String, HeatExchangerGraphic)
            ShortcutColumnCollection = New Dictionary(Of String, ShorcutColumnGraphic)
            DistillationColumnCollection = New Dictionary(Of String, DistillationColumnGraphic)
            AbsorptionColumnCollection = New Dictionary(Of String, AbsorptionColumnGraphic)
            RefluxedAbsorberCollection = New Dictionary(Of String, RefluxedAbsorberGraphic)
            ReboiledAbsorberCollection = New Dictionary(Of String, ReboiledAbsorberGraphic)
            EnergyRecycleCollection = New Dictionary(Of String, EnergyRecycleGraphic)
            ComponentSeparatorCollection = New Dictionary(Of String, ComponentSeparatorGraphic)
            OrificePlateCollection = New Dictionary(Of String, OrificePlateGraphic)
            CustomUOCollection = New Dictionary(Of String, CustomUOGraphic)
            ExcelUOCollection = New Dictionary(Of String, ExcelUOGraphic)
            CapeOpenUOCollection = New Dictionary(Of String, CapeOpenUOGraphic)
            SolidsSeparatorCollection = New Dictionary(Of String, SolidSeparatorGraphic)
            FilterCollection = New Dictionary(Of String, FilterGraphic)
            FlowsheetUOCollection = New Dictionary(Of String, FlowsheetUOGraphic)

            ObjectCollection = New Dictionary(Of String, SimulationObjects_BaseClass)

            'Creates all the actual unit operations collections.
            CLCS_MaterialStreamCollection = New Dictionary(Of String, DWSIM.SimulationObjects.Streams.MaterialStream)
            CLCS_EnergyStreamCollection = New Dictionary(Of String, DWSIM.SimulationObjects.Streams.EnergyStream)
            CLCS_PipeCollection = New Dictionary(Of String, DWSIM.SimulationObjects.UnitOps.Pipe)
            CLCS_MixerCollection = New Dictionary(Of String, DWSIM.SimulationObjects.UnitOps.Mixer)
            CLCS_SplitterCollection = New Dictionary(Of String, DWSIM.SimulationObjects.UnitOps.Splitter)
            CLCS_PumpCollection = New Dictionary(Of String, DWSIM.SimulationObjects.UnitOps.Pump)
            CLCS_CompressorCollection = New Dictionary(Of String, DWSIM.SimulationObjects.UnitOps.Compressor)
            CLCS_ValveCollection = New Dictionary(Of String, DWSIM.SimulationObjects.UnitOps.Valve)
            CLCS_VesselCollection = New Dictionary(Of String, DWSIM.SimulationObjects.UnitOps.Vessel)
            CLCS_TurbineCollection = New Dictionary(Of String, DWSIM.SimulationObjects.UnitOps.Expander)
            CLCS_EnergyMixerCollection = New Dictionary(Of String, DWSIM.SimulationObjects.UnitOps.EnergyMixer)
            CLCS_HeaterCollection = New Dictionary(Of String, DWSIM.SimulationObjects.UnitOps.Heater)
            CLCS_CoolerCollection = New Dictionary(Of String, DWSIM.SimulationObjects.UnitOps.Cooler)
            CLCS_TankCollection = New Dictionary(Of String, DWSIM.SimulationObjects.UnitOps.Tank)
            CLCS_AdjustCollection = New Dictionary(Of String, DWSIM.SimulationObjects.SpecialOps.Adjust)
            CLCS_SpecCollection = New Dictionary(Of String, DWSIM.SimulationObjects.SpecialOps.Spec)
            CLCS_RecycleCollection = New Dictionary(Of String, DWSIM.SimulationObjects.SpecialOps.Recycle)
            CLCS_ReactorConversionCollection = New Dictionary(Of String, DWSIM.SimulationObjects.Reactors.Reactor_Conversion)
            CLCS_ReactorEquilibriumCollection = New Dictionary(Of String, DWSIM.SimulationObjects.Reactors.Reactor_Equilibrium)
            CLCS_ReactorGibbsCollection = New Dictionary(Of String, DWSIM.SimulationObjects.Reactors.Reactor_Gibbs)
            CLCS_ReactorCSTRCollection = New Dictionary(Of String, DWSIM.SimulationObjects.Reactors.Reactor_CSTR)
            CLCS_ReactorPFRCollection = New Dictionary(Of String, DWSIM.SimulationObjects.Reactors.Reactor_PFR)
            CLCS_HeatExchangerCollection = New Dictionary(Of String, DWSIM.SimulationObjects.UnitOps.HeatExchanger)
            CLCS_ShortcutColumnCollection = New Dictionary(Of String, DWSIM.SimulationObjects.UnitOps.ShortcutColumn)
            CLCS_DistillationColumnCollection = New Dictionary(Of String, DWSIM.SimulationObjects.UnitOps.DistillationColumn)
            CLCS_AbsorptionColumnCollection = New Dictionary(Of String, DWSIM.SimulationObjects.UnitOps.AbsorptionColumn)
            CLCS_ReboiledAbsorberCollection = New Dictionary(Of String, DWSIM.SimulationObjects.UnitOps.ReboiledAbsorber)
            CLCS_RefluxedAbsorberCollection = New Dictionary(Of String, DWSIM.SimulationObjects.UnitOps.RefluxedAbsorber)
            CLCS_EnergyRecycleCollection = New Dictionary(Of String, DWSIM.SimulationObjects.SpecialOps.EnergyRecycle)
            CLCS_ComponentSeparatorCollection = New Dictionary(Of String, DWSIM.SimulationObjects.UnitOps.ComponentSeparator)
            CLCS_OrificePlateCollection = New Dictionary(Of String, DWSIM.SimulationObjects.UnitOps.OrificePlate)
            CLCS_CustomUOCollection = New Dictionary(Of String, DWSIM.SimulationObjects.UnitOps.CustomUO)
            CLCS_ExcelUOCollection = New Dictionary(Of String, DWSIM.SimulationObjects.UnitOps.ExcelUO)
            CLCS_CapeOpenUOCollection = New Dictionary(Of String, DWSIM.SimulationObjects.UnitOps.CapeOpenUO)
            CLCS_SolidsSeparatorCollection = New Dictionary(Of String, DWSIM.SimulationObjects.UnitOps.SolidsSeparator)
            CLCS_FilterCollection = New Dictionary(Of String, DWSIM.SimulationObjects.UnitOps.Filter)
            CLCS_FlowsheetUOCollection = New Dictionary(Of String, DWSIM.SimulationObjects.UnitOps.Flowsheet)

            OPT_SensAnalysisCollection = New List(Of DWSIM.Optimization.SensitivityAnalysisCase)
            OPT_OptimizationCollection = New List(Of DWSIM.Optimization.OptimizationCase)

        End Sub

        Sub InitializeCounter()

            Me.ObjectCounter = New Dictionary(Of String, Integer)

            With Me.ObjectCounter
                .Add("MSTR", Me.MaterialStreamCollection.Count)
                .Add("ESTR", Me.EnergyStreamCollection.Count)
                .Add("MIXR", Me.MixerCollection.Count)
                .Add("SPLI", Me.SplitterCollection.Count)
                .Add("VALV", Me.ValveCollection.Count)
                .Add("TANK", Me.TankCollection.Count)
                .Add("PIPE", Me.PipeCollection.Count)
                .Add("PUMP", Me.PumpCollection.Count)
                .Add("COMP", Me.CompressorCollection.Count)
                .Add("EXPN", Me.TurbineCollection.Count)
                .Add("HEAT", Me.HeaterCollection.Count)
                .Add("COOL", Me.CoolerCollection.Count)
                .Add("CSTR", Me.ReactorCSTRCollection.Count)
                .Add("PFR_", Me.ReactorPFRCollection.Count)
                .Add("REQL", Me.ReactorEquilibriumCollection.Count)
                .Add("RCON", Me.ReactorConversionCollection.Count)
                .Add("RECY", Me.RecycleCollection.Count)
                .Add("RGIB", Me.ReactorGibbsCollection.Count)
                .Add("ADJU", Me.AdjustCollection.Count)
                .Add("SPEC", Me.SpecCollection.Count)
                .Add("HXCG", Me.HeatExchangerCollection.Count)
                .Add("VESS", Me.SeparatorCollection.Count)
                .Add("VES3", Me.SeparatorCollection.Count)
                .Add("MXEN", Me.MixerENCollection.Count)
                .Add("SCOL", Me.ShortcutColumnCollection.Count)
                .Add("DCOL", Me.DistillationColumnCollection.Count)
                .Add("ACOL", Me.AbsorptionColumnCollection.Count)
                .Add("RFAB", Me.RefluxedAbsorberCollection.Count)
                .Add("RBAB", Me.ReboiledAbsorberCollection.Count)
                .Add("EREC", Me.EnergyRecycleCollection.Count)
                .Add("CSEP", Me.ComponentSeparatorCollection.Count)
                .Add("ORIF", Me.OrificePlateCollection.Count)
                .Add("CUOP", Me.CustomUOCollection.Count)
                .Add("CEOP", Me.ExcelUOCollection.Count)
                .Add("COOP", Me.CapeOpenUOCollection.Count)
                .Add("SOLS", Me.SolidsSeparatorCollection.Count)
                .Add("FILT", Me.FilterCollection.Count)
                .Add("FLST", Me.FlowsheetUOCollection.Count)
            End With

        End Sub

        Sub UpdateCounter(ByVal item As String)

            Me.ObjectCounter(item) += 1

        End Sub

    End Class

    <System.Serializable()> Public Class ClsFormOptions

        Implements XMLSerializer.Interfaces.ICustomXMLSerialization

        Public AvailableUnitSystems As New Dictionary(Of String, DWSIM.SistemasDeUnidades.Unidades)
        Public PropertyPackages As Dictionary(Of String, DWSIM.SimulationObjects.PropertyPackages.PropertyPackage)

        Public ReadOnly Property SelectedPropertyPackage() As DWSIM.SimulationObjects.PropertyPackages.PropertyPackage
            Get
                For Each pp2 As DWSIM.SimulationObjects.PropertyPackages.PropertyPackage In PropertyPackages.Values
                    Return pp2
                    Exit For
                Next
                Return Nothing
            End Get
        End Property

        Public SelectedComponents As Dictionary(Of String, DWSIM.ClassesBasicasTermodinamica.ConstantProperties)
        Public NotSelectedComponents As Dictionary(Of String, DWSIM.ClassesBasicasTermodinamica.ConstantProperties)

        Public Databases As Dictionary(Of String, String())

        Public Reactions As Dictionary(Of String, DWSIM.ClassesBasicasTermodinamica.Reaction)
        Public ReactionSets As Dictionary(Of String, DWSIM.ClassesBasicasTermodinamica.ReactionSet)
        Public SimulationMode As String = ""

        Public PetroleumAssays As Dictionary(Of String, DWSIM.Utilities.PetroleumCharacterization.Assay.Assay)

        Public SelectedUnitSystem As DWSIM.SistemasDeUnidades.Unidades

        Public NumberFormat As String = "#0.0####"
        Public FractionNumberFormat As String = "#0.0######"

        Public SempreCalcularFlashPH As Boolean = False

        Public CalculateBubbleAndDewPoints As Boolean = False

        Public ValidateEquilibriumCalc As Boolean = False
        Public UsePhaseIdentificationAlgorithm As Boolean = False

        Public SimNome As String = "simulation"
        Public SimAutor As String = "user"
        Public SimComentario As String = "comments"

        Public FilePath As String = ""

        Public BackupFileName As String = ""

        Public CalculatorActivated As Boolean = True

        Public PropertyPackageFlashAlgorithm As SimulationObjects.PropertyPackages.FlashMethod = SimulationObjects.PropertyPackages.FlashMethod.DWSIMDefault
        Public PropertyPackageIOFlashQuickMode As Boolean = True

        Public ThreePhaseFlashStabTestSeverity As Integer = 0
        Public ThreePhaseFlashStabTestCompIds As String() = New String() {}

        Public FlashValidationDGETolerancePct As Double = 0.01

        <Xml.Serialization.XmlIgnore> Public Password As String = ""
        <Xml.Serialization.XmlIgnore> Public UsePassword As Boolean = False

        Public FlowsheetSnapToGrid As Boolean = False
        Public FlowsheetQuickConnect As Boolean = False
        Public FlowsheetShowConsoleWindow As Boolean = False
        Public FlowsheetShowCOReportsWindow As Boolean = False
        Public FlowsheetShowCalculationQueue As Boolean = False
        Public FlowsheetShowWatchWindow As Boolean = False

        Public BinaryEnvelopeExpData As String = ""

        Public Key As String = ""

        Sub New()

            SelectedComponents = New Dictionary(Of String, DWSIM.ClassesBasicasTermodinamica.ConstantProperties)
            NotSelectedComponents = New Dictionary(Of String, DWSIM.ClassesBasicasTermodinamica.ConstantProperties)
            SelectedUnitSystem = New SistemasDeUnidades.UnidadesSI()
            Reactions = New Dictionary(Of String, DWSIM.ClassesBasicasTermodinamica.Reaction)
            ReactionSets = New Dictionary(Of String, DWSIM.ClassesBasicasTermodinamica.ReactionSet)
            Databases = New Dictionary(Of String, String())
            PropertyPackages = New Dictionary(Of String, DWSIM.SimulationObjects.PropertyPackages.PropertyPackage)
            PetroleumAssays = New Dictionary(Of String, DWSIM.Utilities.PetroleumCharacterization.Assay.Assay)

            With ReactionSets
                .Add("DefaultSet", New ReactionSet("DefaultSet", DWSIM.App.GetLocalString("Rxn_DefaultSetName"), DWSIM.App.GetLocalString("Rxn_DefaultSetDesc")))
            End With

        End Sub

        Public Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean Implements XMLSerializer.Interfaces.ICustomXMLSerialization.LoadData

            Dim el As XElement = (From xel As XElement In data Select xel Where xel.Name = "ThreePhaseFlashStabTestCompIds").SingleOrDefault

            If Not el Is Nothing Then Me.ThreePhaseFlashStabTestCompIds = el.Value.Split(",")

            Return XMLSerializer.XMLSerializer.Deserialize(Me, data, True)

        End Function

        Public Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement) Implements XMLSerializer.Interfaces.ICustomXMLSerialization.SaveData

            Dim elements As System.Collections.Generic.List(Of System.Xml.Linq.XElement) = XMLSerializer.XMLSerializer.Serialize(Me, True)
            Dim comps As String = ""
            For Each s As String In Me.ThreePhaseFlashStabTestCompIds
                comps += s + ","
            Next
            If comps <> "" Then
                comps = comps.Remove(comps.Length - 1, 1)
                elements.Add(New XElement("ThreePhaseFlashStabTestCompIds", comps))
            End If
            Return elements

        End Function

    End Class

    <System.Serializable()> Public Class FlowsheetState

        Public Collections As Byte()
        Public GraphicObjects As Byte()
        Public Options As Byte()
        Public WatchItems As Byte()
        Public TreeViewObjects As Byte()
        Public SpreadsheetDT1 As Byte()
        Public SpreadsheetDT2 As Byte()

        Public Snapshot As Bitmap
        Public Description As String = ""
        Public SaveDate As Date

    End Class

    <System.Serializable()> Public Class FlowsheetSolution

        Implements XMLSerializer.Interfaces.ICustomXMLSerialization

        Public Solution As Byte()
        Public ID As String = ""
        Public SaveDate As Date

        Public Function LoadData(data As List(Of XElement)) As Boolean Implements XMLSerializer.Interfaces.ICustomXMLSerialization.LoadData
            XMLSerializer.XMLSerializer.Deserialize(Me, data, True)
        End Function

        Public Function SaveData() As List(Of XElement) Implements XMLSerializer.Interfaces.ICustomXMLSerialization.SaveData
            Return XMLSerializer.XMLSerializer.Serialize(Me, True)
        End Function

    End Class

End Namespace
