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

<InterfaceType(ComInterfaceType.InterfaceIsIDispatch)> Public Interface IReactionSet

    Property ID() As String

    Property Name() As String

    Property Description() As String

    ReadOnly Property Reactions() As Dictionary(Of String, IReactionSetBase)

    Sub SetReactionRank(reactionID As String, rank As Integer)

    Sub EnableReaction(reactionID As String)

    Sub DisableReaction(reactionID As String)

End Interface
