﻿'    DWSIM Interface definitions
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

<InterfaceType(ComInterfaceType.InterfaceIsIDispatch)> Public Interface IUndoRedoAction

Property ID As String
    Property Name As String
    Property AType As Enums.UndoRedoActionType
    Property ObjID As String
    Property ObjID2 As String
    Property OldValue As Object
    Property NewValue As Object
    Property Tag As Object
    Property PropertyName As String

End Interface
