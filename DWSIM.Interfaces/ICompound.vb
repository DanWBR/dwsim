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

''' <summary>
''' This interface defines the basic properties of a compound in a phase.
''' </summary>
<InterfaceType(ComInterfaceType.InterfaceIsIDispatch)> Public Interface ICompound

    Property ExtraProperties As Dynamic.ExpandoObject

    Property ConstantProperties As ICompoundConstantProperties

    Property lnKvalue() As Double

    Property Kvalue() As Double

    Property PetroleumFraction() As Boolean

    Property MoleFraction() As Nullable(Of Double)

    Property MassFraction() As Nullable(Of Double)

    Property Molarity() As Nullable(Of Double)

    Property Molality() As Nullable(Of Double)

    Property MolarFlow() As Nullable(Of Double)

    Property MassFlow() As Nullable(Of Double)

    Property FugacityCoeff() As Nullable(Of Double)

    Property ActivityCoeff() As Nullable(Of Double)

    Property PartialVolume() As Nullable(Of Double)

    Property PartialPressure() As Nullable(Of Double)

    Property VolumetricFlow() As Nullable(Of Double)

    Property VolumetricFraction() As Nullable(Of Double)

    Property Name() As String

    Property DiffusionCoefficient As Nullable(Of Double)

End Interface
