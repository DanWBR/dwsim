'    DWSIM Interface definitions
'    Copyright 2020-2021 Daniel Wagner O. de Medeiros
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

Public Interface IMenuExtenderCollection

    Property ID As String

    Property Description As String

    Property DisplayText As String

    Property Category As Enums.MenuExtenderCategory

    Property Level As Enums.MenuExtenderLevel

    Property Collection As List(Of IMenuExtender)

End Interface

Public Interface IMenuExtender

    Property ID As String

    Function SetMainWindow(mainwindow As Object)

    Function SetFlowsheet(form As IFlowsheet) As Boolean

    Property DisplayText As String

    Property DisplayImage As Object

    Property InsertAtPosition As Integer

    Sub Run()

End Interface