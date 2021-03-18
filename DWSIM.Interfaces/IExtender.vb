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

Public Interface IExtenderCollection

    ReadOnly Property ID As String

    ReadOnly Property Description As String

    ReadOnly Property DisplayText As String

    ReadOnly Property Category As Enums.ExtenderCategory

    ReadOnly Property Level As Enums.ExtenderLevel

    ReadOnly Property Collection As List(Of IExtender)

End Interface

Public Interface IExtender

    ReadOnly Property ID As String

    Function SetMainWindow(mainwindow As Object)

    Function SetFlowsheet(form As IFlowsheet) As Boolean

    ReadOnly Property DisplayText As String

    ReadOnly Property DisplayImage As System.Drawing.Bitmap

    ReadOnly Property InsertAtPosition As Integer

    Sub Run()

End Interface