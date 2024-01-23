'    DWSIM Interface definitions
'    Copyright 2020 Daniel Wagner O. de Medeiros
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

Public Interface IDynamicsManager

    Property Description As String

    Property ScheduleList As Dictionary(Of String, IDynamicsSchedule)

    Property CauseAndEffectMatrixList As Dictionary(Of String, IDynamicsCauseAndEffectMatrix)

    Property EventSetList As Dictionary(Of String, IDynamicsEventSet)

    Property CurrentSchedule As String

    Property IntegratorList As Dictionary(Of String, IDynamicsIntegrator)

    Function GetChartModel(fs As IFlowsheet, IntegratorID As String) As Object

    Property ToggleDynamicMode() As Action(Of Boolean)

    Property RunSchedule() As Func(Of String, Task)

    Function GetSchedule(name As String) As IDynamicsSchedule

    Function GetIntegrator(name As String) As IDynamicsIntegrator

    Function GetEventSet(name As String) As IDynamicsEventSet

    Function GetCauseAndEffectMatrix(name As String) As IDynamicsCauseAndEffectMatrix

    Function GetPropertyValuesFromEvents(fs As IFlowsheet, currenttime As DateTime, history As Dictionary(Of DateTime, XDocument), eventset As IDynamicsEventSet) As List(Of Tuple(Of String, String, Double))

End Interface
