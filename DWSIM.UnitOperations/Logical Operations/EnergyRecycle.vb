'    Energy Recycle Calculation Routines 
'    Copyright 2009 Daniel Wagner O. de Medeiros
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
Imports DWSIM.Thermodynamics
Imports DWSIM.Thermodynamics.Streams
Imports DWSIM.SharedClasses
Imports System.Windows.Forms
Imports DWSIM.UnitOperations.UnitOperations.Auxiliary
Imports DWSIM.Thermodynamics.BaseClasses
Imports DWSIM.Interfaces.Enums
Imports DWSIM.UnitOperations.SpecialOps.Helpers.Recycle

Namespace SpecialOps

    <System.Serializable()> Public Class EnergyRecycle

        Inherits UnitOperations.SpecialOpBaseClass

        <NonSerialized> <Xml.Serialization.XmlIgnore> Dim f As EditingForm_EnergyRecycle

        Protected m_ConvPar As ConvergenceParametersE
        Protected m_ConvHist As ConvergenceHistoryE
        Protected m_AccelMethod As AccelMethod = AccelMethod.Wegstein
        Protected m_WegPars As WegsteinParameters

        Protected m_MaxIterations As Integer = 10
        Protected m_IterationCount As Integer = 0
        Protected m_InternalCounterE As Integer = 0
        Protected m_IterationsTaken As Integer = 0

        Public Overrides Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean

            Dim ci As Globalization.CultureInfo = Globalization.CultureInfo.InvariantCulture

            MyBase.LoadData(data)
            Dim xel As XElement

            xel = (From xel2 As XElement In data Select xel2 Where xel2.Name = "ConvHist").SingleOrDefault

            If Not xel Is Nothing Then
                m_ConvHist.Energy = Double.Parse(xel.@Energy, ci)
                m_ConvHist.Energy0 = Double.Parse(xel.@Energy0, ci)
                m_ConvHist.EnergyE = Double.Parse(xel.@EnergyE, ci)
                m_ConvHist.EnergyE0 = Double.Parse(xel.@EnergyE0, ci)
            End If

            xel = (From xel2 As XElement In data Select xel2 Where xel2.Name = "WegPars").SingleOrDefault

            If Not xel Is Nothing Then
                m_WegPars.AccelDelay = Double.Parse(xel.@AccelDelay, ci)
                m_WegPars.AccelFreq = Double.Parse(xel.@AccelFreq, ci)
                m_WegPars.Qmax = Double.Parse(xel.@Qmax, ci)
                m_WegPars.Qmin = Double.Parse(xel.@Qmin, ci)
            End If
            Return True
        End Function

        Public Overrides Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement)

            Dim elements As System.Collections.Generic.List(Of System.Xml.Linq.XElement) = MyBase.SaveData()
            Dim ci As Globalization.CultureInfo = Globalization.CultureInfo.InvariantCulture

            With elements
                .Add(New XElement("ConvHist", New XAttribute("Energy", m_ConvHist.Energy),
                                  New XAttribute("EnergyE", m_ConvHist.EnergyE),
                                  New XAttribute("Energy0", m_ConvHist.Energy0),
                                 New XAttribute("EnergyE0", m_ConvHist.EnergyE0)))
                .Add(New XElement("WegPars", New XAttribute("AccelDelay", m_WegPars.AccelDelay),
                                  New XAttribute("AccelFreq", m_WegPars.AccelFreq),
                                  New XAttribute("Qmax", m_WegPars.Qmax),
                                  New XAttribute("Qmin", m_WegPars.Qmin)))
            End With

            Return elements

        End Function

        Public Property IterationsTaken() As Integer
            Get
                Return m_IterationsTaken
            End Get
            Set(ByVal value As Integer)
                m_IterationsTaken = value
            End Set
        End Property

        Public Property IterationCount() As Integer
            Get
                Return m_IterationCount
            End Get
            Set(ByVal value As Integer)
                m_IterationCount = value
            End Set
        End Property

        Public Property WegsteinParameters() As WegsteinParameters
            Get
                Return m_WegPars
            End Get
            Set(ByVal value As WegsteinParameters)
                m_WegPars = value
            End Set
        End Property

        Public Property AccelerationMethod() As AccelMethod
            Get
                Return m_AccelMethod
            End Get
            Set(ByVal value As AccelMethod)
                m_AccelMethod = value
            End Set
        End Property

        Public Property ConvergenceParameters() As ConvergenceParametersE
            Get
                Return m_ConvPar
            End Get
            Set(ByVal value As ConvergenceParametersE)
                m_ConvPar = value
            End Set
        End Property

        Public Property ConvergenceHistory() As ConvergenceHistoryE
            Get
                Return m_ConvHist
            End Get
            Set(ByVal value As ConvergenceHistoryE)
                m_ConvHist = value
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

        Public Sub New()

            MyBase.CreateNew()

            m_ConvPar = New ConvergenceParametersE
            m_ConvHist = New ConvergenceHistoryE
            m_WegPars = New WegsteinParameters

        End Sub

        Public Sub New(ByVal name As String, ByVal description As String)

            MyBase.CreateNew()

            m_ConvPar = New ConvergenceParametersE
            m_ConvHist = New ConvergenceHistoryE
            m_WegPars = New WegsteinParameters

            Me.ComponentName = name
            Me.ComponentDescription = description



        End Sub

        Public Overrides Sub Calculate(Optional ByVal args As Object = Nothing)

            If Not Me.GraphicObject.OutputConnectors(0).IsAttached Then
                Throw New Exception(FlowSheet.GetTranslatedString("NohcorrentedeEnergyFlow2"))
            ElseIf Not Me.GraphicObject.InputConnectors(0).IsAttached Then
                Throw New Exception(FlowSheet.GetTranslatedString("NohcorrentedeEnergyFlow2"))
            End If

            Dim Enew As Double

            Dim ees As Streams.EnergyStream = FlowSheet.SimulationObjects(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name)
            With ees

                Me.ConvergenceHistory.EnergyE = .EnergyFlow.GetValueOrDefault - Me.ConvergenceHistory.Energy

                Me.ConvergenceHistory.EnergyE0 = Me.ConvergenceHistory.Energy - Me.ConvergenceHistory.Energy0

                Me.ConvergenceHistory.Energy0 = Me.ConvergenceHistory.Energy

                Me.ConvergenceHistory.Energy = .EnergyFlow.GetValueOrDefault

            End With

            If Me.IterationCount <= 3 Then

SS:             Enew = Me.ConvergenceHistory.Energy

            Else

                Select Case Me.AccelerationMethod

                    Case AccelMethod.None

                        GoTo SS

                    Case AccelMethod.Wegstein

                        If Me.WegsteinParameters.AccelDelay <= Me.IterationCount + 3 Then

                            Dim sE, qE As Double
                            sE = (Me.ConvergenceHistory.EnergyE - Me.ConvergenceHistory.EnergyE0) / (Me.ConvergenceHistory.Energy - Me.ConvergenceHistory.Energy0)
                            qE = sE / (sE - 1)
                            If Me.WegsteinParameters.AccelFreq <= Me.m_InternalCounterE And Double.IsNaN(sE) = False And qE > Me.WegsteinParameters.Qmin And qE < Me.WegsteinParameters.Qmax Then
                                Enew = Me.ConvergenceHistory.EnergyE * (1 - qE) + Me.ConvergenceHistory.Energy * qE
                                Me.m_InternalCounterE = 0
                            Else
                                Enew = Me.ConvergenceHistory.Energy
                                Me.m_InternalCounterE += 1
                            End If

                        Else

                            GoTo SS

                        End If

                End Select

            End If

            'Corrente de EnergyFlow - atualizar valor da potencia (kJ/s)

            Dim es As Streams.EnergyStream = FlowSheet.SimulationObjects(Me.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Name)

            With es
                .EnergyFlow = Enew
                .GraphicObject.Calculated = True
            End With

            If Me.IterationCount >= Me.MaximumIterations Then
                Dim msgres As MsgBoxResult = MessageBox.Show(FlowSheet.GetTranslatedString("Onmeromximodeiteraes"), _
                                Me.GraphicObject.Tag & " - " & FlowSheet.GetTranslatedString("Nmeromximodeiteraesa3"), _
                                MessageBoxButtons.YesNo, MessageBoxIcon.Question)
                If msgres = MsgBoxResult.No Then
                    GoTo final
                Else
                    Me.IterationCount = 0
                End If
            End If

            Me.IterationCount += 1

            If Math.Abs(Me.ConvergenceHistory.EnergyE) > Me.ConvergenceParameters.Energy Then

            Else
final:          Me.IterationsTaken = Me.IterationCount.ToString
                Me.IterationCount = 0
            End If

        End Sub

        Public Overloads Sub DeCalculate()

            Me.IterationCount = 0

        End Sub

        Function MAX(ByVal Vv As Object)

            Dim n = UBound(Vv)
            Dim mx As Double

            If n >= 1 Then
                Dim i As Integer = 1
                mx = Vv(i - 1)
                i = 0
                Do
                    If Vv(i) > mx Then
                        mx = Vv(i)
                    End If
                    i += 1
                Loop Until i = n + 1
                Return mx
            Else
                Return Vv(0)
            End If

        End Function

        Public Overrides Function GetPropertyValue(ByVal prop As String, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As Object
            If su Is Nothing Then su = New SystemsOfUnits.SI
            Dim cv As New SystemsOfUnits.Converter
            Dim value As Double = 0
            Dim propidx As Integer = Convert.ToInt32(prop.Split("_")(2))

            Select Case propidx

                Case 0
                    'PROP_ER_0	Maximum Iterations
                    value = Me.MaximumIterations
                Case 1
                    'PROP_ER_1	Power Tolerance
                    value = SystemsOfUnits.Converter.ConvertFromSI(su.heatflow, Me.ConvergenceParameters.Energy)
                Case 2
                    'PROP_ER_2	Power Error
                    value = SystemsOfUnits.Converter.ConvertFromSI(su.heatflow, Me.ConvergenceHistory.EnergyE)
            End Select

            Return value
        End Function

        Public Overloads Overrides Function GetProperties(ByVal proptype As Interfaces.Enums.PropertyType) As String()
            Dim i As Integer = 0
            Dim proplist As New ArrayList
            Select Case proptype
                Case PropertyType.RO
                    For i = 2 To 2
                        proplist.Add("PROP_ER_" + CStr(i))
                    Next
                Case PropertyType.RW
                    For i = 0 To 2
                        proplist.Add("PROP_ER_" + CStr(i))
                    Next
                Case PropertyType.WR
                    For i = 0 To 1
                        proplist.Add("PROP_ER_" + CStr(i))
                    Next
                Case PropertyType.ALL
                    For i = 0 To 2
                        proplist.Add("PROP_ER_" + CStr(i))
                    Next
            End Select
            Return proplist.ToArray(GetType(System.String))
            proplist = Nothing
        End Function

        Public Overrides Function SetPropertyValue(ByVal prop As String, ByVal propval As Object, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As Boolean
            If su Is Nothing Then su = New SystemsOfUnits.SI
            Dim cv As New SystemsOfUnits.Converter
            Dim propidx As Integer = Convert.ToInt32(prop.Split("_")(2))

            Select Case propidx

                Case 0
                    'PROP_RY_0	Maximum Iterations
                    Me.MaximumIterations = propval
                Case 1
                    'PROP_ER_1	Power Tolerance
                    Me.ConvergenceParameters.Energy = SystemsOfUnits.Converter.ConvertToSI(su.heatflow, propval)

            End Select
            Return 1
        End Function

        Public Overrides Function GetPropertyUnit(ByVal prop As String, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As String
            If su Is Nothing Then su = New SystemsOfUnits.SI
            Dim cv As New SystemsOfUnits.Converter
            Dim value As String = ""
            Dim propidx As Integer = Convert.ToInt32(prop.Split("_")(2))

            Select Case propidx

                Case 0
                    'PROP_ER_0	Maximum Iterations
                    value = ""
                Case 1
                    'PROP_ER_1	Power Tolerance
                    value = su.heatflow
                Case 2
                    'PROP_ER_2	Power Error
                    value = su.heatflow
            End Select

            Return value
        End Function

        Public Overrides Sub DisplayEditForm()

            If f Is Nothing Then
                f = New EditingForm_EnergyRecycle With {.SimObject = Me}
                f.ShowHint = GlobalSettings.Settings.DefaultEditFormLocation
                Me.FlowSheet.DisplayForm(f)
            Else
                If f.IsDisposed Then
                    f = New EditingForm_EnergyRecycle With {.SimObject = Me}
                    f.ShowHint = GlobalSettings.Settings.DefaultEditFormLocation
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

        Public Overrides Function GetIconBitmap() As Object
            Return My.Resources.lo_enrecy_32
        End Function

        Public Overrides Function GetDisplayDescription() As String
            Return ResMan.GetLocalString("ERECY_Desc")
        End Function

        Public Overrides Function GetDisplayName() As String
            Return ResMan.GetLocalString("ERECY_Name")
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

Namespace SpecialOps.Helpers.Recycle

    <System.Serializable()> Public Class ConvergenceParametersE

        Public Energy As Double = 0.1

        Sub New()

        End Sub

    End Class

    <System.Serializable()> Public Class ConvergenceHistoryE

        Public Energy As Double = 0
        Public Energy0 As Double = 0

        Public EnergyE As Double = 0
        Public EnergyE0 As Double = 0

        Sub New()

        End Sub

    End Class

End Namespace


