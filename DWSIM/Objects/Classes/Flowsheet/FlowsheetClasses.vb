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

        Public FlowsheetObjectCollection As Dictionary(Of String, SharedClasses.UnitOperations.BaseClass)

        Public OPT_SensAnalysisCollection As List(Of DWSIM.Optimization.SensitivityAnalysisCase)

        Public OPT_OptimizationCollection As List(Of DWSIM.Optimization.OptimizationCase)

        Sub New()

            'Creates all the graphic collections.

            GraphicObjectCollection = New Dictionary(Of String, GraphicObject)

            FlowsheetObjectCollection = New Dictionary(Of String, SharedClasses.UnitOperations.BaseClass)

            OPT_SensAnalysisCollection = New List(Of DWSIM.Optimization.SensitivityAnalysisCase)

            OPT_OptimizationCollection = New List(Of DWSIM.Optimization.OptimizationCase)

        End Sub

    End Class

    <System.Serializable()> Public Class FlowsheetVariables

        Implements XMLSerializer.Interfaces.ICustomXMLSerialization

        Implements Interfaces.IFlowsheetOptions

        Public Property FlashSettings As New Dictionary(Of Interfaces.Enums.FlashMethod, Dictionary(Of Interfaces.Enums.FlashSetting, String)) Implements Interfaces.IFlowsheetOptions.FlashSettings

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

            Dim defaultsettings = Thermodynamics.PropertyPackages.Auxiliary.FlashAlgorithms.FlashAlgorithm.GetDefaultSettings

            For Each item In [Enum].GetValues(PropertyPackageFlashAlgorithm.GetType)
                FlashSettings.Add(item, defaultsettings)
            Next

        End Sub

        Public Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean Implements XMLSerializer.Interfaces.ICustomXMLSerialization.LoadData

            Dim el As XElement = (From xel As XElement In data Select xel Where xel.Name = "VisibleProperties").SingleOrDefault

            If Not el Is Nothing Then

                VisibleProperties.Clear()

                For Each xel2 As XElement In el.Elements
                    VisibleProperties.Add(xel2.@Value, New List(Of String))
                    For Each xel3 In xel2.Elements
                        VisibleProperties(xel2.@Value).Add(xel3.@Value)
                    Next
                Next

            End If

            el = (From xel As XElement In data Select xel Where xel.Name = "FlashSettings").SingleOrDefault

            If Not el Is Nothing Then

                FlashSettings.Clear()

                For Each xel2 As XElement In el.Elements
                    Dim etype = [Enum].Parse(PropertyPackageFlashAlgorithm.GetType, xel2.@Value)
                    FlashSettings.Add(etype, New Dictionary(Of Interfaces.Enums.FlashSetting, String))
                    For Each xel3 In xel2.Elements
                        Dim esname = [Enum].Parse(Interfaces.Enums.Helpers.GetEnumType("DWSIM.Interfaces.Enums.FlashSetting"), xel3.@Name)
                        FlashSettings(etype).Add(esname, xel3.@Value)
                    Next
                Next

            End If

            Return XMLSerializer.XMLSerializer.Deserialize(Me, data, True)

        End Function

        Public Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement) Implements XMLSerializer.Interfaces.ICustomXMLSerialization.SaveData

            Dim elements As System.Collections.Generic.List(Of System.Xml.Linq.XElement) = XMLSerializer.XMLSerializer.Serialize(Me, True)

            elements.Add(New XElement("VisibleProperties"))

            For Each item In VisibleProperties
                Dim xel2 = New XElement("ObjectType", New XAttribute("Value", item.Key))
                elements(elements.Count - 1).Add(xel2)
                For Each item2 In item.Value
                    xel2.Add(New XElement("PropertyID", New XAttribute("Value", item2)))
                Next
            Next

            elements.Add(New XElement("FlashSettings"))

            For Each item In FlashSettings
                Dim xel2 = New XElement("FlashType", New XAttribute("Value", item.Key.ToString))
                elements(elements.Count - 1).Add(xel2)
                For Each item2 In item.Value
                    xel2.Add(New XElement("Setting", New XAttribute("Name", item2.Key), New XAttribute("Value", item2.Value)))
                Next
            Next

            Return elements

        End Function

        Public Property BackupFileName As String = "" Implements Interfaces.IFlowsheetOptions.BackupFileName

        Public Property BinaryEnvelopeExpData As String = "" Implements Interfaces.IFlowsheetOptions.BinaryEnvelopeExpData

        Public Property FilePath As String = "" Implements Interfaces.IFlowsheetOptions.FilePath

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

        Public Property PropertyPackageFlashAlgorithm As Interfaces.Enums.FlashMethod = FlashMethod.Default_Algorithm Implements Interfaces.IFlowsheetOptions.PropertyPackageFlashAlgorithm

        Public Property SimulationAuthor As String = "" Implements Interfaces.IFlowsheetOptions.SimulationAuthor

        Public Property SimulationComments As String = "" Implements Interfaces.IFlowsheetOptions.SimulationComments

        Public Property SimulationName As String = "" Implements Interfaces.IFlowsheetOptions.SimulationName

        Public Property UsePassword As Boolean = False Implements Interfaces.IFlowsheetOptions.UsePassword

        Public Property SelectedUnitSystem1 As Interfaces.IUnitsOfMeasure Implements Interfaces.IFlowsheetOptions.SelectedUnitSystem
            Get
                Return Me.SelectedUnitSystem
            End Get
            Set(value As Interfaces.IUnitsOfMeasure)
                Me.SelectedUnitSystem = value
            End Set
        End Property

        Public Property VisibleProperties As New Dictionary(Of String, List(Of String)) Implements Interfaces.IFlowsheetOptions.VisibleProperties

    End Class

End Namespace
