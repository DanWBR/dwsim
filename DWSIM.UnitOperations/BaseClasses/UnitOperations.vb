'    Flowsheet Object Base Classes 
'    Copyright 2008-2020 Daniel Wagner O. de Medeiros
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

Imports System.IO
Imports CapeOpen
Imports System.Runtime.InteropServices
Imports DWSIM.Interfaces.Enums
Imports Microsoft.Scripting.Hosting
Imports DWSIM.Thermodynamics

Namespace UnitOperations

    <System.Serializable()> <ComVisible(True)> Public MustInherit Class UnitOpBaseClass

        Inherits SharedClasses.UnitOperations.BaseClass

        Implements ICapeIdentification, IUnitOperation

        <NonSerialized> <Xml.Serialization.XmlIgnore> Public _pp As Interfaces.IPropertyPackage

        Public _ppid As String = ""

        Protected _capeopenmode As Boolean = False

        Public AccumulationStream As Thermodynamics.Streams.MaterialStream

        Public Property ExternalSolverID As String = ""

        Public Property ExternalSolverConfigData As String = ""

        Public Sub New()

            MyBase.CreateNew()

        End Sub

        Public Overrides Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean

            MyBase.LoadData(data)

            Dim ael = (From xel As XElement In data Select xel Where xel.Name = "AccumulationStream").FirstOrDefault

            If Not ael Is Nothing Then
                AccumulationStream = New Thermodynamics.Streams.MaterialStream()
                AccumulationStream.LoadData(ael.Elements.ToList)
                For Each phase In AccumulationStream.Phases.Values
                    For Each comp In phase.Compounds.Values
                        comp.ConstantProperties = FlowSheet.SelectedCompounds(comp.Name)
                    Next
                Next
            End If

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

            If AccumulationStream IsNot Nothing Then
                elements.Add(New XElement("AccumulationStream", AccumulationStream.SaveData()))
            End If

            Return elements

        End Function

        Public Overrides Function GetDynamicResidenceTime() As Double
            If GetDynamicProperty("Volume") IsNot Nothing Then
                Try
                    Dim q As Double = 0.0
                    For Each inlet In GraphicObject.InputConnectors
                        If inlet.IsAttached And inlet.Type = GraphicObjects.ConType.ConIn Then
                            q += Convert.ToDouble(inlet.AttachedConnector.AttachedFrom.Owner.GetPropertyValue("PROP_MS_4"))
                        End If
                    Next
                    Dim v = Convert.ToDouble(GetDynamicProperty("Volume"))
                    Return v / q
                Catch ex As Exception
                    Return Double.NaN
                End Try
            Else
                Return Double.NaN
            End If
        End Function

        Public Overrides Function GetDynamicVolume() As Double
            If GetDynamicProperty("Volume") IsNot Nothing Then
                Return Convert.ToDouble(GetDynamicProperty("Volume"))
            Else
                Return Double.NaN
            End If
        End Function

        Public Overrides Function GetDynamicContents() As Double
            If AccumulationStream IsNot Nothing Then
                Return AccumulationStream.GetMassFlow()
            Else
                Return Double.NaN
            End If
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

        Public Property UnitsType As UnitOfMeasure = UnitOfMeasure.none Implements ISpecialOpObjectInfo.UnitsType

        Public Property Units As String = "" Implements ISpecialOpObjectInfo.Units

    End Class

End Namespace

