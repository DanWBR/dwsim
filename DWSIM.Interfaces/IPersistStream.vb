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

Imports System.Runtime.InteropServices
Imports System.Runtime.InteropServices.ComTypes

Namespace Interfaces2

    <ComImport(), Guid("00000109-0000-0000-C000-000000000046"), ComVisible(False), InterfaceType(CShort(1))> _
    Public Interface IPersistStream
        Sub GetClassID(<Out()> ByRef pClassID As Guid)
        <PreserveSig()> _
        Function IsDirty() As Integer
        Sub Load(ByVal pStm As IStream)
        Sub Save(ByVal pStm As IStream, <[In](), MarshalAs(UnmanagedType.Bool)> ByVal fClearDirty As Boolean)
        Sub GetSizeMax(<Out()> ByRef pcbSize As Long)
    End Interface

    <ComImport(), Guid("7FD52380-4E07-101B-AE2D-08002B2EC713"), ComVisible(False), InterfaceType(CShort(1))> _
    Public Interface IPersistStreamInit
        Sub GetClassID(<Out()> ByRef pClassID As Guid)
        <PreserveSig()> _
        Function IsDirty() As Integer
        Sub Load(ByVal pStm As IStream)
        Sub Save(ByVal pStm As IStream, <[In](), MarshalAs(UnmanagedType.Bool)> ByVal fClearDirty As Boolean)
        Sub GetSizeMax(<Out()> ByRef pcbSize As Long)
        Sub InitNew()
    End Interface

End Namespace
