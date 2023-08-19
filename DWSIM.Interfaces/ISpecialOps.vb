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

Imports DWSIM.Interfaces.Enums

<InterfaceType(ComInterfaceType.InterfaceIsIDispatch)> Public Interface IRecycle

    ReadOnly Property Errors As Dictionary(Of String, Double)
    ReadOnly Property Values As Dictionary(Of String, Double)
    Property AccelerationMethod() As Enums.AccelMethod
    Sub SetOutletStreamProperties()
    Property Converged As Boolean
    Property ConvergenceHistory As IRecycleConvergenceHistory

End Interface

<InterfaceType(ComInterfaceType.InterfaceIsIDispatch)> Public Interface IRecycleConvergenceHistory

    Property Temperatura As Double
    Property Pressao As Double
    Property VazaoMassica As Double
    Property Entalpia As Double
    Property Entropia As Double
    Property Temperatura0 As Double
    Property Pressao0 As Double
    Property VazaoMassica0 As Double
    Property Entalpia0 As Double
    Property Entropia0 As Double

    Property TemperaturaE As Double
    Property PressaoE As Double
    Property VazaoMassicaE As Double
    Property EntalpiaE As Double
    Property EntropiaE As Double
    Property TemperaturaE0 As Double
    Property PressaoE0 As Double
    Property VazaoMassicaE0 As Double
    Property EntalpiaE0 As Double
    Property EntropiaE0 As Double

End Interface

<InterfaceType(ComInterfaceType.InterfaceIsIDispatch)> Public Interface ISpec

    Property SpecCalculationMode As SpecCalcMode2

    Property ReferenceObjectID As String

End Interface

<InterfaceType(ComInterfaceType.InterfaceIsIDispatch)> Public Interface IAdjust

    Property Tolerance As Double

    Property SimultaneousAdjust As Boolean

    Property Referenced As Boolean

    Property AdjustValue As Double

    Property ControlledObjectData As ISpecialOpObjectInfo

    Property ManipulatedObjectData As ISpecialOpObjectInfo

    Property ReferencedObjectData As ISpecialOpObjectInfo

End Interface

<InterfaceType(ComInterfaceType.InterfaceIsIDispatch)> Public Interface ISpecialOpObjectInfo

    Property Type As String
    Property Name As String
    Property ID As String
    Property PropertyName As String
    Property UnitsType As Enums.UnitOfMeasure
    Property Units As String

End Interface

<InterfaceType(ComInterfaceType.InterfaceIsIDispatch)> Public Interface IControllableObject

    Property ControlPanel As Object

End Interface
