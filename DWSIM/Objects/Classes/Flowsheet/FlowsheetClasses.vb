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

Imports DWSIM.DrawingTools.GraphicObjects
Imports DWSIM.Thermodynamics.BaseClasses
Imports DWSIM.DWSIM.SimulationObjects
Imports System.Linq

Namespace DWSIM.Flowsheet

    Public Enum MessageType
        Information
        Warning
        GeneralError
        Tip
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

    <System.Serializable()> Public Class ObjectCollection

        Public GraphicObjectCollection As Dictionary(Of String, GraphicObject)

        Public FlowsheetObjectCollection As Dictionary(Of String, DWSIM.SimulationObjects.UnitOperations.BaseClass)

        Public OPT_SensAnalysisCollection As List(Of DWSIM.Optimization.SensitivityAnalysisCase)

        Public OPT_OptimizationCollection As List(Of DWSIM.Optimization.OptimizationCase)

        Sub New()

            'Creates all the graphic collections.

            GraphicObjectCollection = New Dictionary(Of String, GraphicObject)

            FlowsheetObjectCollection = New Dictionary(Of String, DWSIM.SimulationObjects.UnitOperations.BaseClass)

            OPT_SensAnalysisCollection = New List(Of DWSIM.Optimization.SensitivityAnalysisCase)

            OPT_OptimizationCollection = New List(Of DWSIM.Optimization.OptimizationCase)

        End Sub

    End Class

    <System.Serializable()> Public Class FlowsheetVariables

        Implements XMLSerializer.Interfaces.ICustomXMLSerialization

        Implements Interfaces.IFlowsheetOptions

        Public AvailableUnitSystems As New Dictionary(Of String, SystemsOfUnits.Units)

        Public PropertyPackages As Dictionary(Of String, PropertyPackages.PropertyPackage)

        Public ReadOnly Property SelectedPropertyPackage() As PropertyPackages.PropertyPackage
            Get
                For Each pp2 As PropertyPackages.PropertyPackage In PropertyPackages.Values
                    Return pp2
                    Exit For
                Next
                Return Nothing
            End Get
        End Property

        Public SelectedComponents As Dictionary(Of String, BaseClasses.ConstantProperties)

        Public NotSelectedComponents As Dictionary(Of String, BaseClasses.ConstantProperties)

        Public Databases As Dictionary(Of String, String())

        Public Reactions As Dictionary(Of String, BaseClasses.Reaction)

        Public ReactionSets As Dictionary(Of String, BaseClasses.ReactionSet)

        Public SimulationMode As String = ""

        Public PetroleumAssays As Dictionary(Of String, DWSIM.Utilities.PetroleumCharacterization.Assay.Assay)

        Public SelectedUnitSystem As SystemsOfUnits.Units

        Sub New()

            SelectedComponents = New Dictionary(Of String, BaseClasses.ConstantProperties)
            NotSelectedComponents = New Dictionary(Of String, BaseClasses.ConstantProperties)
            SelectedUnitSystem = New SystemsOfUnits.SI()
            Reactions = New Dictionary(Of String, BaseClasses.Reaction)
            ReactionSets = New Dictionary(Of String, BaseClasses.ReactionSet)
            Databases = New Dictionary(Of String, String())
            PropertyPackages = New Dictionary(Of String, PropertyPackages.PropertyPackage)
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

        Public Property BackupFileName As String = "" Implements Interfaces.IFlowsheetOptions.BackupFileName

        Public Property BinaryEnvelopeExpData As String = "" Implements Interfaces.IFlowsheetOptions.BinaryEnvelopeExpData

        Public Property CalculateBubbleAndDewPoints As Boolean = False Implements Interfaces.IFlowsheetOptions.CalculateBubbleAndDewPoints

        Public Property CalculatorActivated As Boolean = False Implements Interfaces.IFlowsheetOptions.CalculatorActivated

        Public Property FilePath As String = "" Implements Interfaces.IFlowsheetOptions.FilePath

        Public Property FlashValidationDGETolerancePct As Double = 0.01 Implements Interfaces.IFlowsheetOptions.FlashValidationDGETolerancePct

        Public Property FlowsheetQuickConnect As Boolean = False Implements Interfaces.IFlowsheetOptions.FlowsheetQuickConnect

        Public Property FlowsheetShowCalculationQueue As Boolean = False Implements Interfaces.IFlowsheetOptions.FlowsheetShowCalculationQueue

        Public Property FlowsheetShowConsoleWindow As Boolean = False Implements Interfaces.IFlowsheetOptions.FlowsheetShowConsoleWindow

        Public Property FlowsheetShowCOReportsWindow As Boolean = False Implements Interfaces.IFlowsheetOptions.FlowsheetShowCOReportsWindow

        Public Property FlowsheetShowWatchWindow As Boolean = False Implements Interfaces.IFlowsheetOptions.FlowsheetShowWatchWindow

        Public Property FlowsheetSnapToGrid As Boolean = False Implements Interfaces.IFlowsheetOptions.FlowsheetSnapToGrid

        Public Property FractionNumberFormat As String = "N4" Implements Interfaces.IFlowsheetOptions.FractionNumberFormat

        Public Property Key As String = "" Implements Interfaces.IFlowsheetOptions.Key

        Public Property NumberFormat As String = "N" Implements Interfaces.IFlowsheetOptions.NumberFormat

        Public Property Password As String = "" Implements Interfaces.IFlowsheetOptions.Password

        Public Property PropertyPackageFlashAlgorithm As Integer = 0 Implements Interfaces.IFlowsheetOptions.PropertyPackageFlashAlgorithm

        Public Property SempreCalcularFlashPH As Boolean = False Implements Interfaces.IFlowsheetOptions.SempreCalcularFlashPH

        Public Property SimulationAuthor As String = "" Implements Interfaces.IFlowsheetOptions.SimulationAuthor

        Public Property SimulationComments As String = "" Implements Interfaces.IFlowsheetOptions.SimulationComments

        Public Property SimulationName As String = "" Implements Interfaces.IFlowsheetOptions.SimulationName

        Public Property ThreePhaseFlashStabTestCompIds As String() = New String() {} Implements Interfaces.IFlowsheetOptions.ThreePhaseFlashStabTestCompIds

        Public Property ThreePhaseFlashStabTestSeverity As Integer = 0 Implements Interfaces.IFlowsheetOptions.ThreePhaseFlashStabTestSeverity

        Public Property UsePassword As Boolean = False Implements Interfaces.IFlowsheetOptions.UsePassword

        Public Property UsePhaseIdentificationAlgorithm As Boolean = False Implements Interfaces.IFlowsheetOptions.UsePhaseIdentificationAlgorithm

        Public Property ValidateEquilibriumCalc As Boolean = False Implements Interfaces.IFlowsheetOptions.ValidateEquilibriumCalc

        Public Property SelectedUnitSystem1 As Interfaces.IUnitsOfMeasure Implements Interfaces.IFlowsheetOptions.SelectedUnitSystem
            Get
                Return Me.SelectedUnitSystem
            End Get
            Set(value As Interfaces.IUnitsOfMeasure)
                Me.SelectedUnitSystem = value
            End Set
        End Property

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
