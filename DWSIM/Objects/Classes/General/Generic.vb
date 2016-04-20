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

Imports DWSIM.DrawingTools.GraphicObjects
Imports System.Runtime.Serialization
Imports System.Reflection
Imports System.Linq
Imports CapeOpen

Namespace DWSIM.Extras

    ''' <summary>
    ''' Serialization Binder Class to enable loading of pre-3.0 simulations in DWSIM 3.0+.
    ''' </summary>
    ''' <remarks></remarks>
    Public NotInheritable Class VersionDeserializationBinder

        Inherits SerializationBinder

        Public Overrides Function BindToType(assemblyName As String, typeName As String) As Type
            If Not String.IsNullOrEmpty(assemblyName) AndAlso Not String.IsNullOrEmpty(typeName) Then
                Dim typeToDeserialize As Type = Nothing
                If typeName = "CapeOpen.ICapeParameter" Then
                    typeName = "CapeOpen.CapeParameter"
                    assemblyName = "CapeOpen, Version=1.0.0.0, Culture=neutral, PublicKeyToken=90d5303f0e924b64"
                ElseIf typeName.Contains("System.Collections.Generic.List`1[[CapeOpen.ICapeParameter") Then
                    typeName = typeName.Replace("List`1[[CapeOpen.ICapeParameter", "List`1[[CapeOpen.CapeParameter")
                    typeName = typeName.Replace("CapeOpen, Version=1.0.4118.14986, Culture=neutral, PublicKeyToken=100cc1b5ee8fe630", "CapeOpen, Version=1.0.0.0, Culture=neutral, PublicKeyToken=90d5303f0e924b64")
                ElseIf typeName.Contains("DWSIM.Auxiliary.CapeOpen.CRealParameter") Then
                    typeName = "CapeOpen.RealParameter"
                    assemblyName = "CapeOpen, Version=1.0.0.0, Culture=neutral, PublicKeyToken=90d5303f0e924b64"
                ElseIf typeName.Contains("CapeOpen.CRealParameter") Then
                    typeName = typeName.Replace("CapeOpen.CRealParameter", "CapeOpen.RealParameter")
                    typeName = typeName.Replace("CapeOpen, Version=1.0.4118.14986, Culture=neutral, PublicKeyToken=100cc1b5ee8fe630", "CapeOpen, Version=1.0.0.0, Culture=neutral, PublicKeyToken=90d5303f0e924b64")
                    assemblyName = "CapeOpen, Version=1.0.0.0, Culture=neutral, PublicKeyToken=90d5303f0e924b64"
                ElseIf typeName.Contains("CapeOpen.ICape") Then
                    typeName = typeName.Replace("CapeOpen, Version=1.0.4118.14986, Culture=neutral, PublicKeyToken=100cc1b5ee8fe630", "CapeOpen, Version=1.0.0.0, Culture=neutral, PublicKeyToken=90d5303f0e924b64")
                ElseIf typeName.Contains("CapeOpen") Then
                    typeName = typeName.Replace("CapeOpen.C", "CapeOpen.")
                    typeName = typeName.Replace("CapeOpen.ape", "CapeOpen.Cape")
                    typeName = typeName.Replace("CapeOpen, Version=1.0.4118.14986, Culture=neutral, PublicKeyToken=100cc1b5ee8fe630", "CapeOpen, Version=1.0.0.0, Culture=neutral, PublicKeyToken=90d5303f0e924b64")
                End If
                If assemblyName.Contains("DWSIM") Then
                    assemblyName = Assembly.GetExecutingAssembly().FullName
                    typeToDeserialize = Type.[GetType]([String].Format("{0}, {1}", typeName, assemblyName))
                ElseIf assemblyName.Contains("CapeOpen") Then
                    Dim assemblies As AssemblyName() = Assembly.GetExecutingAssembly().GetReferencedAssemblies()
                    assemblyName = (From a As AssemblyName In assemblies Where a.FullName.Contains("CapeOpen")).FirstOrDefault.FullName
                    typeToDeserialize = Type.[GetType]([String].Format("{0}, {1}", typeName, assemblyName))
                Else
                    typeToDeserialize = Type.[GetType]([String].Format("{0}, {1}", typeName, assemblyName))
                End If
                ' The following line of code returns the type. 
                Return typeToDeserialize
            End If
            Return Nothing
        End Function
    End Class

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

    <System.Serializable()> Public Class Annotation

        Implements XMLSerializer.Interfaces.ICustomXMLSerialization

        Public m_text As String = ""
        Public m_rtfText As String = ""

        Sub New(ByVal rtf As String, ByVal text As String)
            m_rtfText = rtf
            m_text = text
        End Sub

        Sub New()

        End Sub

        Public Property annotation() As Object
            Get
                Return m_rtfText
            End Get
            Set(ByVal value As Object)
                m_rtfText = value(0)
                m_text = value(1)
            End Set
        End Property

        Public Overrides Function ToString() As String
            Return m_text
        End Function

        Public Function LoadData(data As List(Of XElement)) As Boolean Implements XMLSerializer.Interfaces.ICustomXMLSerialization.LoadData
            XMLSerializer.XMLSerializer.Deserialize(Me, data, True)
        End Function

        Public Function SaveData() As List(Of XElement) Implements XMLSerializer.Interfaces.ICustomXMLSerialization.SaveData
            Return XMLSerializer.XMLSerializer.Serialize(Me, True)
        End Function

    End Class

    <System.Serializable()> Public Class WatchItem

        Implements XMLSerializer.Interfaces.ICustomXMLSerialization

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

        Public Function LoadData(data As List(Of XElement)) As Boolean Implements XMLSerializer.Interfaces.ICustomXMLSerialization.LoadData
            XMLSerializer.XMLSerializer.Deserialize(Me, data, True)
        End Function

        Public Function SaveData() As List(Of XElement) Implements XMLSerializer.Interfaces.ICustomXMLSerialization.SaveData
            Return XMLSerializer.XMLSerializer.Serialize(Me, True)
        End Function

    End Class

End Namespace

Namespace Auxiliary.CapeOpen

    ''' <summary>
    ''' This class if for legacy compatibility only. It should NOT be used. Use CapeOpen.RealParameter instead if necessary.
    ''' </summary>
    ''' <remarks></remarks>
    <System.Serializable()> Public Class CRealParameter
        Implements ICapeIdentification, ICapeParameter, ICapeParameterSpec, ICapeRealParameterSpec
        Dim _par As Global.CapeOpen.RealParameter
        Public Event OnParameterValueChanged(ByVal sender As Object, ByVal args As System.EventArgs)
        Sub New(ByVal name As String, ByVal value As Double, ByVal defaultvalue As Double, ByVal unit As String)
            _par = New Global.CapeOpen.RealParameter(name, value, defaultvalue, unit)
        End Sub
        Public Property ComponentDescription() As String Implements Global.CapeOpen.ICapeIdentification.ComponentDescription
            Get
                Return _par.ComponentDescription
            End Get
            Set(ByVal value As String)
                _par.ComponentDescription = value
            End Set
        End Property
        Public Property ComponentName() As String Implements Global.CapeOpen.ICapeIdentification.ComponentName
            Get
                Return _par.ComponentName
            End Get
            Set(ByVal value As String)
                _par.ComponentName = value
            End Set
        End Property
        Public Property Mode() As Global.CapeOpen.CapeParamMode Implements Global.CapeOpen.ICapeParameter.Mode
            Get
                Return _par.Mode
            End Get
            Set(ByVal value As Global.CapeOpen.CapeParamMode)
                _par.Mode = value
            End Set
        End Property
        Public Sub Reset() Implements Global.CapeOpen.ICapeParameter.Reset
            _par.Reset()
        End Sub
        Public ReadOnly Property Specification() As Object Implements Global.CapeOpen.ICapeParameter.Specification
            Get
                Return Me
            End Get
        End Property
        Public Function Validate(ByRef message As String) As Boolean Implements Global.CapeOpen.ICapeParameter.Validate
            Return _par.Validate(message)
        End Function
        Public ReadOnly Property ValStatus() As Global.CapeOpen.CapeValidationStatus Implements Global.CapeOpen.ICapeParameter.ValStatus
            Get
                Return _par.ValStatus
            End Get
        End Property
        Public Property value() As Object Implements Global.CapeOpen.ICapeParameter.value
            Get
                Return _par.SIValue
            End Get
            Set(ByVal value As Object)
                _par.SIValue = value
                RaiseEvent OnParameterValueChanged(Me, New System.EventArgs())
            End Set
        End Property
        Public ReadOnly Property Dimensionality() As Object Implements Global.CapeOpen.ICapeParameterSpec.Dimensionality
            Get
                Dim myd As ICapeParameterSpec = _par
                Return New Double() {myd.Dimensionality(0), myd.Dimensionality(1), myd.Dimensionality(2), myd.Dimensionality(3), myd.Dimensionality(4), myd.Dimensionality(5), myd.Dimensionality(6), myd.Dimensionality(7)}
            End Get
        End Property
        Public ReadOnly Property Type() As Global.CapeOpen.CapeParamType Implements Global.CapeOpen.ICapeParameterSpec.Type
            Get
                Return _par.Type
            End Get
        End Property
        Public ReadOnly Property DefaultValue() As Double Implements Global.CapeOpen.ICapeRealParameterSpec.DefaultValue
            Get
                Return _par.DefaultValue
            End Get
        End Property
        Public ReadOnly Property LowerBound() As Double Implements Global.CapeOpen.ICapeRealParameterSpec.LowerBound
            Get
                Return _par.LowerBound
            End Get
        End Property
        Public ReadOnly Property UpperBound() As Double Implements Global.CapeOpen.ICapeRealParameterSpec.UpperBound
            Get
                Return _par.UpperBound
            End Get
        End Property
        Public Function Validate1(ByVal value As Double, ByRef message As String) As Boolean Implements Global.CapeOpen.ICapeRealParameterSpec.Validate
            Return _par.Validate(value, message)
        End Function
    End Class

    <System.Serializable> Public Class CapeArrayParameter

        Inherits Global.CapeOpen.CapeParameter

        Implements Global.CapeOpen.ICapeArrayParameterSpec

        Public Property value As Object
        <NonSerialized> Private ispecs As Object
        Private numdim As Integer

        Sub New(ByVal name As String, description As String, ByVal value As Object, ispecs As Object, numdim As Integer)
            MyBase.New(name, description)
            Me.value = value
            Me.ispecs = ispecs
            Me.numdim = numdim
        End Sub

        Public ReadOnly Property ItemsSpecifications As Object Implements ICapeArrayParameterSpec.ItemsSpecifications
            Get
                Return ispecs
            End Get
        End Property

        Public ReadOnly Property NumDimensions As Integer Implements ICapeArrayParameterSpec.NumDimensions
            Get
                Return 1
            End Get
        End Property

        Public ReadOnly Property Size As Object Implements ICapeArrayParameterSpec.Size
            Get
                Return value.Length
            End Get
        End Property

        Public Overrides ReadOnly Property Type As CapeParamType
            Get
                Return CapeParamType.CAPE_ARRAY
            End Get
        End Property

        Public Overrides Sub Reset()
            value = Nothing
        End Sub

        Public Function Validate1(inputArray As Object, ByRef value As Object) As Object Implements ICapeArrayParameterSpec.Validate
            Return TypeOf inputArray Is System.Array
        End Function

        Public Overrides Function Validate(ByRef message As String) As Boolean
            Return True
        End Function

    End Class

End Namespace