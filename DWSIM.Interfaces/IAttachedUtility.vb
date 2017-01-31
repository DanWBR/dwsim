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
''' This interface defines the expected funcionality of a flowsheet utility attached to an object.
''' </summary>
<InterfaceType(ComInterfaceType.InterfaceIsIDispatch)> Public Interface IAttachedUtility

    Property ID As Integer

    Property Name As String

    Property AttachedTo As ISimulationObject

    Function GetPropertyList() As List(Of String)

    Function GetPropertyValue(pname As String) As Object

    Function GetPropertyUnits(pname As String) As String

    Sub SetPropertyValue(pname As String, pvalue As Object)

    Sub Update()

    Function GetUtilityType() As Enums.FlowsheetUtility

    Property AutoUpdate As Boolean

    Function SaveData() As Dictionary(Of String, Object)

    Sub LoadData(data As Dictionary(Of String, Object))

    Sub Initialize()

    Sub Populate()

End Interface
