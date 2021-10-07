'    Copyright 2021 Daniel Wagner O. de Medeiros
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
'

Public Class ComponentNotFoundException

    Inherits Exception

    Public Property Base As Exception

    Public Property ProductName As String = ""

    Public Property ProductDescription As String = ""

    Public Property ProductAuthor As String = ""

    Public Property ProductContactInfo As String = ""

    Public Property ProductPage As String = ""

    Public Property ProductVersion As String = ""

    Public Property ProductAssembly As String = ""

    Sub New()

    End Sub

End Class
