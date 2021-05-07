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

''' <summary>
''' This interface defines the basic properties of the Dynamic Mode Integrator.
''' </summary>
<InterfaceType(ComInterfaceType.InterfaceIsIDispatch)> Public Interface IDynamicsIntegrator

    Property ID As String

    Property Description As String

    Property ShouldCalculateEquilibrium As Boolean

    Property ShouldCalculatePressureFlow As Boolean

    Property ShouldCalculateControl As Boolean

    Property IntegrationStep As TimeSpan

    Property Duration As TimeSpan

    Property RealTimeStepMs As Integer

    Property CurrentTime As DateTime

    Property CalculationRateEquilibrium As Integer

    Property CalculationRatePressureFlow As Integer

    Property CalculationRateControl As Integer

    Property RealTime As Boolean

    Property MonitoredVariableValues As Dictionary(Of Long, List(Of IDynamicsMonitoredVariable))

    Property MonitoredVariables As List(Of IDynamicsMonitoredVariable)

End Interface
