'    DWSIM Dynamics Library
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

Imports DWSIM.Interfaces

Public Class Integrator

    Implements Interfaces.IDynamicsIntegrator, ICustomXMLSerialization

    Public Property ShouldCalculateEquilibrium As Boolean Implements IDynamicsIntegrator.ShouldCalculateEquilibrium

    Public Property ShouldCalculatePressureFlow As Boolean Implements IDynamicsIntegrator.ShouldCalculatePressureFlow

    Public Property ShouldCalculateControl As Boolean Implements IDynamicsIntegrator.ShouldCalculateControl

    Public Property IntegrationStep As TimeSpan = New TimeSpan(0, 0, 5) Implements IDynamicsIntegrator.IntegrationStep

    Public Property Duration As TimeSpan = New TimeSpan(0, 10, 0) Implements IDynamicsIntegrator.Duration

    Public Property CurrentTime As Date = New Date() Implements IDynamicsIntegrator.CurrentTime

    Public Property CalculationRateEquilibrium As Integer = 10 Implements IDynamicsIntegrator.CalculationRateEquilibrium

    Public Property CalculationRatePressureFlow As Integer = 1 Implements IDynamicsIntegrator.CalculationRatePressureFlow

    Public Property CalculationRateControl As Integer = 2 Implements IDynamicsIntegrator.CalculationRateControl

    Public Property RealTime As Boolean = False Implements IDynamicsIntegrator.RealTime

    Public Property StoredSolutions As Dictionary(Of Date, List(Of XElement)) = New Dictionary(Of Date, List(Of XElement)) Implements IDynamicsIntegrator.StoredSolutions

    Public Function SaveData() As List(Of XElement) Implements ICustomXMLSerialization.SaveData
        Return XMLSerializer.XMLSerializer.Serialize(Me)
    End Function

    Public Function LoadData(data As List(Of XElement)) As Boolean Implements ICustomXMLSerialization.LoadData
        XMLSerializer.XMLSerializer.Deserialize(Me, data)
        Return True
    End Function

End Class
