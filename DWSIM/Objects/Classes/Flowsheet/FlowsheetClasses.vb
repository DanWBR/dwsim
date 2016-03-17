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
Imports DWSIM.DWSIM.Thermodynamics.BaseClasses
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

        Public AvailableUnitSystems As New Dictionary(Of String, DWSIM.SystemsOfUnits.Units)

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

        Public SelectedComponents As Dictionary(Of String, DWSIM.Thermodynamics.BaseClasses.ConstantProperties)

        Public NotSelectedComponents As Dictionary(Of String, DWSIM.Thermodynamics.BaseClasses.ConstantProperties)

        Public Databases As Dictionary(Of String, String())

        Public Reactions As Dictionary(Of String, DWSIM.Thermodynamics.BaseClasses.Reaction)
        Public ReactionSets As Dictionary(Of String, DWSIM.Thermodynamics.BaseClasses.ReactionSet)
        Public SimulationMode As String = ""

        Public PetroleumAssays As Dictionary(Of String, DWSIM.Utilities.PetroleumCharacterization.Assay.Assay)

        Public SelectedUnitSystem As DWSIM.SystemsOfUnits.Units

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

            SelectedComponents = New Dictionary(Of String, DWSIM.Thermodynamics.BaseClasses.ConstantProperties)
            NotSelectedComponents = New Dictionary(Of String, DWSIM.Thermodynamics.BaseClasses.ConstantProperties)
            SelectedUnitSystem = New SystemsOfUnits.SI()
            Reactions = New Dictionary(Of String, DWSIM.Thermodynamics.BaseClasses.Reaction)
            ReactionSets = New Dictionary(Of String, DWSIM.Thermodynamics.BaseClasses.ReactionSet)
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
