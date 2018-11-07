'    Flowsheet Object Base Classes 
'    Copyright 2008-2014 Daniel Wagner O. de Medeiros
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

Imports System.Runtime.Serialization.Formatters.Binary
Imports System.Runtime.Serialization
Imports System.IO
Imports System.Linq
Imports CapeOpen
Imports System.Runtime.Serialization.Formatters
Imports System.Runtime.InteropServices.Marshal
Imports System.Runtime.InteropServices
Imports System.Text
Imports DWSIM.Interfaces.Enums.GraphicObjects
Imports DWSIM.Interfaces.Enums
Imports System.Windows.Forms
Imports DWSIM.SharedClasses

Namespace UnitOperations

    <System.Serializable()> <ComVisible(True)> Public MustInherit Class UnitOpBaseClass

        Inherits SharedClasses.UnitOperations.BaseClass

        Implements ICapeIdentification

        <NonSerialized> <Xml.Serialization.XmlIgnore> Public _pp As Interfaces.IPropertyPackage
        Public _ppid As String = ""

        Protected _capeopenmode As Boolean = False

        Public Sub New()
            MyBase.CreateNew()
        End Sub

        Public Overrides Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean

            MyBase.LoadData(data)

            Try
                Me._ppid = (From xel As XElement In data Select xel Where xel.Name = "PropertyPackage").SingleOrDefault.Value
            Catch

            End Try

            Return True

        End Function

        Public Overrides Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement)

            Dim elements As List(Of XElement) = MyBase.SaveData()

            Dim ppid As String = ""
            If _ppid <> "" Then
                ppid = _ppid
            ElseIf Not _pp Is Nothing Then
                ppid = _pp.Name
            Else
                ppid = ""
            End If
            elements.Add(New XElement("PropertyPackage", ppid))

            Return elements

        End Function

#Region "   DWSIM Specific"

        Public Overrides Function GetDebugReport() As String

            Me.DebugMode = True
            Me.DebugText = ""

            Try

                Calculate(Nothing)

                Me.DebugText += vbCrLf & vbCrLf & "Calculated OK."

            Catch ex As Exception

                Dim st As New StackTrace(ex, True)
                Dim frame As StackFrame = st.GetFrame(0)
                Dim fileName As String = Path.GetFileName(frame.GetFileName)
                Dim methodName As String = frame.GetMethod().Name
                Dim line As Integer = frame.GetFileLineNumber()

                AppendDebugLine(String.Format("ERROR: exception raised on file {0}, method {1}, line {2}:", fileName, methodName, line))
                AppendDebugLine(ex.Message.ToString)

            Finally

                Me.DebugMode = False

            End Try

            Return DebugText

        End Function

        ''' <summary>
        ''' Gets or sets the property package associated with this object.
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        <Xml.Serialization.XmlIgnore()> Public Overrides Property PropertyPackage() As Interfaces.IPropertyPackage
            Get
                If Not _pp Is Nothing Then Return _pp
                If _ppid Is Nothing Then _ppid = ""
                If FlowSheet.PropertyPackages.ContainsKey(_ppid) Then
                    Return FlowSheet.PropertyPackages(_ppid)
                Else
                    For Each pp As Interfaces.IPropertyPackage In Me.FlowSheet.PropertyPackages.Values
                        _ppid = pp.UniqueID
                        Return pp
                        Exit For
                    Next
                End If
                Return Nothing
            End Get
            Set(ByVal value As Interfaces.IPropertyPackage)
                If value IsNot Nothing Then
                    _ppid = value.UniqueID
                    _pp = value
                Else
                    _pp = Nothing
                End If
            End Set
        End Property

        ''' <summary>
        ''' Decalculates the object.
        ''' </summary>
        ''' <remarks></remarks>
        Public Overridable Overloads Sub DeCalculate()
            MyBase.DeCalculate()
        End Sub

        ''' <summary>
        ''' Decalculates the object.
        ''' </summary>
        ''' <remarks></remarks>
        Public Sub Unsolve()

            DeCalculate()

            Calculated = False

        End Sub

#End Region

#Region "   CAPE-OPEN ICapeIdentification"

        Public Overrides Property ComponentDescription() As String = "" Implements CapeOpen.ICapeIdentification.ComponentDescription

        Public Overrides Property ComponentName() As String = "" Implements CapeOpen.ICapeIdentification.ComponentName

#End Region

    End Class

    <System.Serializable()> Public MustInherit Class SpecialOpBaseClass

        Inherits SharedClasses.UnitOperations.BaseClass

        Public Overrides Property ObjectClass As SimulationObjectClass = SimulationObjectClass.Logical

        Public Sub New()
            MyBase.CreateNew()
        End Sub

    End Class

End Namespace

Namespace SpecialOps.Helpers

    <System.Serializable()> Public Class SpecialOpObjectInfo

        Implements ISpecialOpObjectInfo

        Sub New()

        End Sub

        Public Property ID As String = "" Implements ISpecialOpObjectInfo.ID

        Public Property Name As String = "" Implements ISpecialOpObjectInfo.Name

        Public Property PropertyName As String = "" Implements ISpecialOpObjectInfo.PropertyName

        Public Property ObjectType As String = "" Implements ISpecialOpObjectInfo.Type

    End Class

End Namespace

