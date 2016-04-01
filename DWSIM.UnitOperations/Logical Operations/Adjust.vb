'    Adjust Calculation Routines 
'    Copyright 2008 Daniel Wagner O. de Medeiros
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
Imports System.ComponentModel
Imports PropertyGridEx
Imports System.Linq
Imports DWSIM.SharedClasses.UnitOperations
Imports DWSIM.SharedClasses.SpecialOps

Namespace SharedClasses.SpecialOps

    <System.Serializable()> Public Class Adjust

        Inherits SpecialOpBaseClass

        Protected m_ManipulatedObject As BaseClass
        Protected m_ControlledObject As BaseClass
        Protected m_ReferenceObject As BaseClass

        Protected m_ManipulatedVariable As String = ""
        Protected m_ControlledVariable As String = ""
        Protected m_ReferenceVariable As String = ""

        Protected m_Status As String = ""

        Protected m_AdjustValue As Double = 1.0#

        Protected m_IsReferenced As Boolean = False
        Protected m_IsSimultAdjustEnabled As Boolean = False

        Protected m_StepSize As Double = 0.1
        Protected m_Tolerance As Double = 0.0001
        Protected m_MaxIterations As Integer = 10

        Protected m_ManipulatedObjectData As New Helpers.Adjust.ManipulatedObjectInfo
        Protected m_ControlledObjectData As New Helpers.Adjust.ControlledObjectInfo
        Protected m_ReferencedObjectData As New Helpers.Adjust.ReferenceObjectInfo

        Protected m_CV_OK As Boolean = False
        Protected m_MV_OK As Boolean = False
        Protected m_RV_OK As Boolean = False

        Protected m_minVal As Nullable(Of Double) = Nothing
        Protected m_maxVal As Nullable(Of Double) = Nothing
        Protected m_initialEstimate As Nullable(Of Double) = Nothing

        Public Property SimultaneousAdjust() As Boolean
            Get
                Return m_IsSimultAdjustEnabled
            End Get
            Set(ByVal value As Boolean)
                m_IsSimultAdjustEnabled = value
            End Set
        End Property

        Public Property InitialEstimate() As Nullable(Of Double)
            Get
                Return m_initialEstimate
            End Get
            Set(ByVal value As Nullable(Of Double))
                m_initialEstimate = value
            End Set
        End Property

        Public Property MaxVal() As Nullable(Of Double)
            Get
                Return m_maxVal
            End Get
            Set(ByVal value As Nullable(Of Double))
                m_maxVal = value
            End Set
        End Property

        Public Property MinVal() As Nullable(Of Double)
            Get
                Return m_minVal
            End Get
            Set(ByVal value As Nullable(Of Double))
                m_minVal = value
            End Set
        End Property

        Public Property RvOk() As Boolean
            Get
                Return m_RV_OK
            End Get
            Set(ByVal value As Boolean)
                m_RV_OK = value
            End Set
        End Property

        Public Property MvOk() As Boolean
            Get
                Return m_MV_OK
            End Get
            Set(ByVal value As Boolean)
                m_MV_OK = value
            End Set
        End Property

        Public Property CvOk() As Boolean
            Get
                Return m_CV_OK
            End Get
            Set(ByVal value As Boolean)
                m_CV_OK = value
            End Set
        End Property

        Public Property ManipulatedObjectData() As Helpers.Adjust.ManipulatedObjectInfo
            Get
                Return Me.m_ManipulatedObjectData
            End Get
            Set(ByVal value As Helpers.Adjust.ManipulatedObjectInfo)
                Me.m_ManipulatedObjectData = value
            End Set
        End Property

        Public Property ControlledObjectData() As Helpers.Adjust.ControlledObjectInfo
            Get
                Return Me.m_ControlledObjectData
            End Get
            Set(ByVal value As Helpers.Adjust.ControlledObjectInfo)
                Me.m_ControlledObjectData = value
            End Set
        End Property

        Public Property ReferencedObjectData() As Helpers.Adjust.ReferenceObjectInfo
            Get
                Return Me.m_ReferencedObjectData
            End Get
            Set(ByVal value As Helpers.Adjust.ReferenceObjectInfo)
                Me.m_ReferencedObjectData = value
            End Set
        End Property

        <Xml.Serialization.XmlIgnore()> Public Property ManipulatedObject() As BaseClass
            Get
                Return Me.m_ManipulatedObject
            End Get
            Set(ByVal value As BaseClass)
                Me.m_ManipulatedObject = value
            End Set
        End Property

        <Xml.Serialization.XmlIgnore()> Public Property ControlledObject() As BaseClass
            Get
                Return Me.m_ControlledObject
            End Get
            Set(ByVal value As BaseClass)
                Me.m_ControlledObject = value
            End Set
        End Property

        <Xml.Serialization.XmlIgnore()> Public Property ReferenceObject() As BaseClass
            Get
                Return Me.m_ReferenceObject
            End Get
            Set(ByVal value As BaseClass)
                Me.m_ReferenceObject = value
            End Set
        End Property

        Public Property ManipulatedVariable() As String
            Get
                Return Me.m_ManipulatedVariable
            End Get
            Set(ByVal value As String)
                Me.m_ManipulatedVariable = value
            End Set
        End Property

        Public Property ControlledVariable() As String
            Get
                Return Me.m_ControlledVariable
            End Get
            Set(ByVal value As String)
                Me.m_ControlledVariable = value
            End Set
        End Property

        Public Property ReferenceVariable() As String
            Get
                Return Me.m_ReferenceVariable
            End Get
            Set(ByVal value As String)
                Me.m_ReferenceVariable = value
            End Set
        End Property

        Public Property Status() As String
            Get
                Return Me.m_Status
            End Get
            Set(ByVal value As String)
                Me.m_Status = value
            End Set
        End Property

        Public Property AdjustValue() As Double
            Get
                Return Me.m_AdjustValue
            End Get
            Set(ByVal value As Double)
                Me.m_AdjustValue = value
            End Set
        End Property

        Public Property Referenced() As Boolean
            Get
                Return Me.m_IsReferenced
            End Get
            Set(ByVal value As Boolean)
                Me.m_IsReferenced = value
            End Set
        End Property

        Public Property StepSize() As Double
            Get
                Return Me.m_StepSize
            End Get
            Set(ByVal value As Double)
                Me.m_StepSize = value
            End Set
        End Property

        Public Property Tolerance() As Double
            Get
                Return Me.m_Tolerance
            End Get
            Set(ByVal value As Double)
                Me.m_Tolerance = value
            End Set
        End Property

        Public Property MaximumIterations() As Integer
            Get
                Return Me.m_MaxIterations
            End Get
            Set(ByVal value As Integer)
                Me.m_MaxIterations = value
            End Set
        End Property

        Public Overrides Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean

            Dim ci As Globalization.CultureInfo = Globalization.CultureInfo.InvariantCulture

            MyBase.LoadData(data)

            Dim xel As XElement

            xel = (From xel2 As XElement In data Select xel2 Where xel2.Name = "ManipulatedObjectData").SingleOrDefault

            If Not xel Is Nothing Then

                With m_ManipulatedObjectData
                    .m_ID = xel.@ID
                    .m_Name = xel.@Name
                    .m_Property = xel.@Property
                    .m_Type = xel.@Type
                End With

            End If

            xel = (From xel2 As XElement In data Select xel2 Where xel2.Name = "ControlledObjectData").SingleOrDefault

            If Not xel Is Nothing Then

                With m_ControlledObjectData
                    .m_ID = xel.@ID
                    .m_Name = xel.@Name
                    .m_Property = xel.@Property
                    .m_Type = xel.@Type
                End With

            End If

            xel = (From xel2 As XElement In data Select xel2 Where xel2.Name = "ReferencedObjectData").SingleOrDefault

            If Not xel Is Nothing Then

                With m_ReferencedObjectData
                    .m_ID = xel.@ID
                    .m_Name = xel.@Name
                    .m_Property = xel.@Property
                    .m_Type = xel.@Type
                End With

            End If

        End Function

        Public Overrides Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement)

            Dim elements As System.Collections.Generic.List(Of System.Xml.Linq.XElement) = MyBase.SaveData()
            Dim ci As Globalization.CultureInfo = Globalization.CultureInfo.InvariantCulture

            With elements
                .Add(New XElement("ManipulatedObjectData", New XAttribute("ID", m_ManipulatedObjectData.m_ID),
                                  New XAttribute("Name", m_ManipulatedObjectData.m_Name),
                                  New XAttribute("Property", m_ManipulatedObjectData.m_Property),
                                  New XAttribute("Type", m_ManipulatedObjectData.m_Type)))
                .Add(New XElement("ControlledObjectData", New XAttribute("ID", m_ControlledObjectData.m_ID),
                                  New XAttribute("Name", m_ControlledObjectData.m_Name),
                                  New XAttribute("Property", m_ControlledObjectData.m_Property),
                                  New XAttribute("Type", m_ControlledObjectData.m_Type)))
                .Add(New XElement("ReferencedObjectData", New XAttribute("ID", m_ReferencedObjectData.m_ID),
                                  New XAttribute("Name", m_ReferencedObjectData.m_Name),
                                  New XAttribute("Property", m_ReferencedObjectData.m_Property),
                                  New XAttribute("Type", m_ReferencedObjectData.m_Type)))
            End With

            Return elements

        End Function

        Public Sub New()
            MyBase.New()
        End Sub

        Public Sub New(ByVal name As String, ByVal description As String)

            MyBase.CreateNew()
            m_ManipulatedObjectData = New Helpers.Adjust.ManipulatedObjectInfo
            m_ControlledObjectData = New Helpers.Adjust.ControlledObjectInfo
            m_ReferencedObjectData = New Helpers.Adjust.ReferenceObjectInfo
            Me.ComponentName = name
            Me.ComponentDescription = description

        End Sub

        Public Overrides Function GetPropertyValue(ByVal prop As String, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As Object
            Return 0
        End Function

        Public Overloads Overrides Function GetProperties(ByVal proptype As Interfaces.Enums.PropertyType) As String()
            Dim i As Integer = 0
            Dim proplist As New ArrayList
            Return proplist.ToArray(GetType(System.String))
            proplist = Nothing
        End Function

        Public Overrides Function SetPropertyValue(ByVal prop As String, ByVal propval As Object, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As Boolean
            Return 0
        End Function

        Public Overrides Function GetPropertyUnit(ByVal prop As String, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As String
            Return 0
        End Function

    End Class

End Namespace




