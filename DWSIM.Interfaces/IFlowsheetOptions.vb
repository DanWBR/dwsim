﻿Imports DWSIM.Interfaces.Enums

'    DWSIM Interface definitions
'    Copyright 2010-2017 Daniel Wagner O. de Medeiros
'
'    This file is part of DWSIM.
'
'    DWSIM is free software: you can redistribute it and/or modify
'    it under the terms of the GNU Lesser General Public License as published by
'    the Free Software Foundation, either version 3 of the License, or
'    (at your option) any later version.
'
'    DWSIM is distributed in the hope that it will be useful,
'    but WITHOUT ANY WARRANTY; without even the implied warranty of
'    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
'    GNU Lesser General Public License for more details.
'
'    You should have received a copy of the GNU Lesser General Public License
'    along with DWSIM.  If not, see <http://www.gnu.org/licenses/>.

''' <summary>
''' This interface defines the flowsheet settings and other properties.
''' </summary>
<InterfaceType(ComInterfaceType.InterfaceIsIDispatch)> Public Interface IFlowsheetOptions

    Property NumberFormat As String
    Property FractionNumberFormat As String

    Property SimulationName As String
    Property SimulationAuthor As String
    Property SimulationComments As String

    Property FilePath As String

    Property BackupFileName As String

    <Xml.Serialization.XmlIgnore> Property Password As String
    <Xml.Serialization.XmlIgnore> Property UsePassword As Boolean

    Property FlowsheetSnapToGrid As Boolean
    Property FlowsheetDisplayGrid As Boolean
    Property FlowsheetQuickConnect As Boolean
    Property FlowsheetShowConsoleWindow As Boolean
    Property FlowsheetShowCOReportsWindow As Boolean
    Property FlowsheetShowCalculationQueue As Boolean
    Property FlowsheetShowWatchWindow As Boolean

    Property FlowsheetControlPanelMode As Boolean

    Property Key As String

    Property SelectedUnitSystem As IUnitsOfMeasure

    Property VisibleProperties As Dictionary(Of String, List(Of String))

    Property SimultaneousAdjustSolverEnabled As Boolean

    Property SpreadsheetUseRegionalSeparator As Boolean

    Property SpreadsheetUnitLockingMode As Boolean

    Property MassBalanceCheck As WarningType

    Property EnergyBalanceCheck As WarningType

    Property MassBalanceRelativeTolerance As Double

    Property EnergyBalanceRelativeTolerance As Double

    Property DisplayFloatingPropertyTables As Boolean

    Property DisplayCornerPropertyList As Boolean

    Property DisplayCornerPropertyListPosition As ListPosition

    Property DisplayCornerPropertyListFontName As String

    Property DisplayCornerPropertyListFontSize As Integer

    Property DisplayCornerPropertyListFontColor As String

    Property DisplayCornerPropertyListPadding As Integer

    Property DefaultFloatingTableCompoundAmountBasis As Enums.CompositionBasis

    Property DisplayFloatingTableCompoundAmounts As Boolean

    Property FlowsheetMultiSelectMode As Boolean

    Property CompoundOrderingMode As CompoundOrdering

    Property SkipEquilibriumCalculationOnDefinedStreams As Boolean

End Interface
