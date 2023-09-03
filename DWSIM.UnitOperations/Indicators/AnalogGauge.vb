'    Copyright 2020 Daniel Wagner O. de Medeiros
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

Imports DWSIM.SharedClasses
Imports DWSIM.Interfaces.Enums

Namespace UnitOperations

    <System.Serializable()> Public Class AnalogGauge

        Inherits UnitOperations.UnitOpBaseClass

        Implements Interfaces.IIndicator

        Public Overrides Property ObjectClass As SimulationObjectClass = SimulationObjectClass.Indicators

        <NonSerialized> <Xml.Serialization.XmlIgnore> Public f As EditingForm_AnalogGauge

        Public Property DecimalDigits As Integer = 2 Implements IIndicator.DecimalDigits

        Public Property IntegralDigits As Integer = 4 Implements IIndicator.IntegralDigits

        Public Property MinimumValue As Double Implements IIndicator.MinimumValue

        Public Property MaximumValue As Double = 100 Implements IIndicator.MaximumValue

        Public Property CurrentValue As Double Implements IIndicator.CurrentValue

        Public Property SelectedObjectID As String = "" Implements IIndicator.SelectedObjectID

        Public Property SelectedProperty As String = "" Implements IIndicator.SelectedProperty

        Public Property SelectedPropertyType As UnitOfMeasure = UnitOfMeasure.none Implements IIndicator.SelectedPropertyType

        Public Property SelectedPropertyUnits As String = "" Implements IIndicator.SelectedPropertyUnits

        Public Property VeryLowAlarmEnabled As Boolean = False Implements IIndicator.VeryLowAlarmEnabled

        Public Property LowAlarmEnabled As Boolean = False Implements IIndicator.LowAlarmEnabled

        Public Property HighAlarmEnabled As Boolean = False Implements IIndicator.HighAlarmEnabled

        Public Property VeryHighAlarmEnabled As Boolean = False Implements IIndicator.VeryHighAlarmEnabled

        Public Property VeryLowAlarmValue As Double Implements IIndicator.VeryLowAlarmValue

        Public Property LowAlarmValue As Double Implements IIndicator.LowAlarmValue

        Public Property HighAlarmValue As Double Implements IIndicator.HighAlarmValue

        Public Property VeryHighAlarmValue As Double Implements IIndicator.VeryHighAlarmValue

        Public Property VeryLowAlarmActive As Boolean = False Implements IIndicator.VeryLowAlarmActive

        Public Property LowAlarmActive As Boolean = False Implements IIndicator.LowAlarmActive

        Public Property HighAlarmActive As Boolean = False Implements IIndicator.HighAlarmActive

        Public Property VeryHighAlarmActive As Boolean = False Implements IIndicator.VeryHighAlarmActive

        Public Property ShowAlarms As Boolean = False Implements IIndicator.ShowAlarms

        Public Overrides ReadOnly Property SupportsDynamicMode As Boolean = True

        Public Property DisplayInPercent As Boolean = False Implements IIndicator.DisplayInPercent

        Public Sub New(ByVal name As String, ByVal description As String)

            MyBase.CreateNew()
            Me.ComponentName = name
            Me.ComponentDescription = description

        End Sub

        Public Overrides Function CloneXML() As Object
            Dim obj As ICustomXMLSerialization = New AnalogGauge()
            obj.LoadData(Me.SaveData)
            Return obj
        End Function

        Public Overrides Function CloneJSON() As Object
            Return Newtonsoft.Json.JsonConvert.DeserializeObject(Of AnalogGauge)(Newtonsoft.Json.JsonConvert.SerializeObject(Me))
        End Function

        Public Sub New()
            MyBase.New()
        End Sub

        Public Overrides Sub Calculate(Optional ByVal args As Object = Nothing)

            If GetFlowsheet.SimulationObjects.ContainsKey(SelectedObjectID) Then

                Try

                    Dim SelectedObject = GetFlowsheet.SimulationObjects.Values.Where(Function(x) x.Name = SelectedObjectID).FirstOrDefault

                    Dim currentvalue = SystemsOfUnits.Converter.ConvertFromSI(SelectedPropertyUnits, SelectedObject.GetPropertyValue(SelectedProperty))

                    VeryLowAlarmActive = currentvalue <= VeryLowAlarmValue And VeryLowAlarmEnabled

                    LowAlarmActive = currentvalue <= LowAlarmValue And LowAlarmEnabled

                    HighAlarmActive = currentvalue >= HighAlarmValue And HighAlarmEnabled

                    VeryHighAlarmActive = currentvalue >= VeryHighAlarmValue And VeryHighAlarmEnabled

                Catch ex As Exception

                End Try

            End If

        End Sub

        Public Overrides Sub DeCalculate()

        End Sub

        Public Overrides Function GetPropertyValue(ByVal prop As String, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As Object

            Try

                Dim SelectedObject = GetFlowsheet.SimulationObjects.Values.Where(Function(x) x.Name = SelectedObjectID).FirstOrDefault

                Dim currentvalue = SystemsOfUnits.Converter.ConvertFromSI(SelectedPropertyUnits, SelectedObject.GetPropertyValue(SelectedProperty))

                Return currentvalue

            Catch ex As Exception

                Return Double.NaN

            End Try

        End Function

        Public Overrides Function GetProperties(ByVal proptype As Interfaces.Enums.PropertyType) As String()

            Return New String() {"Monitored Value"}

        End Function

        Public Overrides Function SetPropertyValue(ByVal prop As String, ByVal propval As Object, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As Boolean

            Return True

        End Function

        Public Overrides Function GetPropertyUnit(ByVal prop As String, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As String

            Return SelectedPropertyUnits

        End Function

        Public Overrides Sub DisplayEditForm()

            If f Is Nothing Then
                f = New EditingForm_AnalogGauge With {.SimObject = Me}
                f.ShowHint = GlobalSettings.Settings.DefaultEditFormLocation
                f.Tag = "ObjectEditor"
                Me.FlowSheet.DisplayForm(f)
            Else
                If f.IsDisposed Then
                    f = New EditingForm_AnalogGauge With {.SimObject = Me}
                    f.ShowHint = GlobalSettings.Settings.DefaultEditFormLocation
                    f.Tag = "ObjectEditor"
                    Me.FlowSheet.DisplayForm(f)
                Else
                    f.Activate()
                End If
            End If

        End Sub

        Public Overrides Sub UpdateEditForm()
            If f IsNot Nothing Then
                If Not f.IsDisposed Then
                    f.UIThread(Sub() f.UpdateInfo())
                End If
            End If
        End Sub

        Public Overrides Function GetEditingForm() As Form
            If f Is Nothing Then
                f = New EditingForm_AnalogGauge With {.SimObject = Me}
                f.Tag = "ObjectEditor"
                Return f
            Else
                If f.IsDisposed Then
                    f = New EditingForm_AnalogGauge With {.SimObject = Me}
                    f.Tag = "ObjectEditor"
                    Return f
                Else
                    Return Nothing
                End If
            End If
        End Function

        Public Overrides Function GetIconBitmap() As Object
            Return My.Resources.analog_gauge1
        End Function

        Public Overrides Function GetDisplayDescription() As String
            Return ResMan.GetLocalString("AG_Desc")
        End Function

        Public Overrides Function GetDisplayName() As String
            Return ResMan.GetLocalString("AG_Name")
        End Function

        Public Overrides Sub CloseEditForm()
            If f IsNot Nothing Then
                If Not f.IsDisposed Then
                    f.Close()
                    f = Nothing
                End If
            End If
        End Sub

        Public Overrides ReadOnly Property MobileCompatible As Boolean
            Get
                Return False
            End Get
        End Property

    End Class

End Namespace


