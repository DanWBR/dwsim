'    DWSIM Interface definitions
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

''' <summary>
''' This interface defines an event raised when a new message is sent to the flowsheet log, 
''' to be catched by an object when the automation is being done through a COM association.
''' </summary>
<InterfaceType(ComInterfaceType.InterfaceIsIDispatch)>
<Guid("5E0BA6EE-9025-4C33-A896-E061F32E93BF")>
Public Interface IFlowsheetNewMessageSentEvent

    <DispId(100)> Sub NewMessageSent(message As String)

End Interface
