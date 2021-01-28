'    Miscelaneous Classes
'    Copyright 2008-2015 Daniel Wagner O. de Medeiros
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

Imports System.Runtime.Serialization
Imports System.Reflection
Imports System.Linq
Imports CapeOpen

Namespace Extras

    <CLSCompliant(True)> <System.Serializable()> Public Class NodeItem

        Private m_checked As Boolean = False
        Private m_text As String
        Private m_value As String
        Private m_unit As String
        Private m_level As Integer = 0
        Private m_parentnode As String
        Private m_key As Integer

        Public Property CustomText As String = ""

        Sub New()

        End Sub

        Sub New(ByVal texto As String, ByVal valor As String, ByVal unidade As String, ByVal key As Integer, ByVal nivel As Integer, ByVal pai As String)
            Me.m_value = valor
            Me.m_unit = unidade
            Me.m_text = texto
            Me.m_key = key
            Me.m_parentnode = pai
            Me.m_level = nivel
        End Sub

        Public Property Checked() As Boolean
            Get
                Return m_checked
            End Get
            Set(ByVal value As Boolean)
                m_checked = value
            End Set
        End Property

        Public Property Text() As String
            Get
                Return m_text
            End Get
            Set(ByVal value As String)
                m_text = value
            End Set
        End Property

        Public Property Value() As String
            Get
                Return m_value
            End Get
            Set(ByVal value As String)
                m_value = value
            End Set
        End Property

        Public Property Unit() As String
            Get
                Return m_unit
            End Get
            Set(ByVal value As String)
                m_unit = value
            End Set
        End Property

        Public Property Level() As Integer
            Get
                Return m_level
            End Get
            Set(ByVal value As Integer)
                m_level = value
            End Set
        End Property

        Public Property ParentNode() As String
            Get
                Return m_parentnode
            End Get
            Set(ByVal value As String)
                m_parentnode = value
            End Set
        End Property

        Public Property Key() As String
            Get
                Return m_key
            End Get
            Set(ByVal value As String)
                m_key = value
            End Set
        End Property

    End Class

    <System.Serializable()> Public Class WatchItem

        Implements Interfaces.ICustomXMLSerialization

        Public ObjID As String = ""
        Public PropID As String = ""
        Public ROnly As Boolean = False

        Sub New()

        End Sub

        Sub New(ByVal oid As String, ByVal pid As String, ByVal ro As Boolean)
            ObjID = oid
            PropID = pid
            ROnly = ro
        End Sub

        Public Function LoadData(data As List(Of XElement)) As Boolean Implements Interfaces.ICustomXMLSerialization.LoadData
            XMLSerializer.XMLSerializer.Deserialize(Me, data, True)
            Return True
        End Function

        Public Function SaveData() As List(Of XElement) Implements Interfaces.ICustomXMLSerialization.SaveData
            Return XMLSerializer.XMLSerializer.Serialize(Me, True)
        End Function

    End Class

End Namespace