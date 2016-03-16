'    Activity Coefficient Base Interface Definition
'    Copyright 2015 Daniel Wagner O. de Medeiros
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

Namespace DWSIM.SimulationObjects.PropertyPackages.Auxiliary

    Public Interface IActivityCoefficientBase

        Function CalcActivityCoefficients(ByVal T As Double, ByVal Vx As Array, ByVal otherargs As Object) As Array

        Function CalcExcessEnthalpy(ByVal T As Double, ByVal Vx As Array, ByVal otherargs As Object) As Double

        Function CalcExcessHeatCapacity(ByVal T As Double, ByVal Vx As Array, ByVal otherargs As Object) As Double

    End Interface

End Namespace
