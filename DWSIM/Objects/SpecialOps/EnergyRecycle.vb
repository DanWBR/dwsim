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

Imports Microsoft.MSDN.Samples.GraphicObjects
Imports DWSIM.DWSIM.SimulationObjects.SpecialOps.Helpers.Recycle
Imports System.Linq
Imports System.ComponentModel
Imports PropertyGridEx
Imports DWSIM.DWSIM.Flowsheet.FlowSheetSolver

Namespace DWSIM.SimulationObjects.SpecialOps

    <System.Serializable()> Public Class EnergyRecycle

        Inherits DWSIM.SimulationObjects.UnitOperations.UnitOpBaseClass

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

            Me.m_ComponentName = nome
            Me.m_ComponentDescription = descricao
            Me.FillNodeItems()
            Me.QTFillNodeItems()

        End Sub

        Public Overrides Sub QTFillNodeItems()

            With Me.QTNodeTableItems

                .Clear()

                .Add(0, New DWSIM.Outros.NodeItem(DWSIM.App.GetLocalString("Iteraes"), "", "", 0, 0, ""))
                .Add(1, New DWSIM.Outros.NodeItem(DWSIM.App.GetLocalString("ErroE"), "", "", 1, 0, ""))

            End With

        End Sub

        Public Overrides Sub UpdatePropertyNodes(ByVal su As SystemsOfUnits.Units, ByVal nf As String)

            Dim Conversor As New DWSIM.SystemsOfUnits.Converter
            If Me.NodeTableItems Is Nothing Then
                Me.NodeTableItems = New System.Collections.Generic.Dictionary(Of Integer, DWSIM.Outros.NodeItem)
                Me.FillNodeItems()
            End If

            Try

                For Each nti As Outros.NodeItem In Me.NodeTableItems.Values
                    nti.Value = GetPropertyValue(nti.Text, FlowSheet.Options.SelectedUnitSystem)
                    nti.Unit = GetPropertyUnit(nti.Text, FlowSheet.Options.SelectedUnitSystem)
                Next

                If Me.QTNodeTableItems Is Nothing Then
                    Me.QTNodeTableItems = New System.Collections.Generic.Dictionary(Of Integer, DWSIM.Outros.NodeItem)
                    Me.QTFillNodeItems()
                End If

                With Me.QTNodeTableItems

                    Dim valor As String

                    .Item(0).Value = Me.IterationsTaken
                    .Item(0).Unit = ""

                    valor = Format(Converter.ConvertFromSI(su.heatflow, Me.ConvergenceHistory.EnergyE), nf)
                    .Item(1).Value = valor
                    .Item(1).Unit = su.heatflow

                End With

            Catch ex As Exception

            End Try

        End Sub

        Public Overrides Function Calculate(Optional ByVal args As Object = Nothing) As Integer

            Dim form As Global.DWSIM.FormFlowsheet = Me.Flowsheet
            Dim objargs As New DWSIM.Outros.StatusChangeEventArgs

            If Not Me.GraphicObject.OutputConnectors(0).IsAttached Then
                'Call function to calculate flowsheet
                With objargs
                    .Calculado = False
                    .Nome = Me.Nome
                    .Tipo = TipoObjeto.OT_Reciclo
                End With
                CalculateFlowsheet(FlowSheet, objargs, Nothing)
                Throw New Exception(DWSIM.App.GetLocalString("Nohcorrentedeenergia2"))
            ElseIf Not Me.GraphicObject.InputConnectors(0).IsAttached Then
                'Call function to calculate flowsheet
                With objargs
                    .Calculado = False
                    .Nome = Me.Nome
                    .Tipo = TipoObjeto.OT_Reciclo
                End With
                CalculateFlowsheet(FlowSheet, objargs, Nothing)
                Throw New Exception(DWSIM.App.GetLocalString("Nohcorrentedeenergia2"))
            End If

            Dim Enew As Double

            Dim ees As DWSIM.SimulationObjects.Streams.EnergyStream = form.Collections.CLCS_EnergyStreamCollection(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name)
            With ees

                Me.ConvergenceHistory.EnergyE = .Energia.GetValueOrDefault - Me.ConvergenceHistory.Energy

                Me.ConvergenceHistory.EnergyE0 = Me.ConvergenceHistory.Energy - Me.ConvergenceHistory.Energy0

                Me.ConvergenceHistory.Energy0 = Me.ConvergenceHistory.Energy

                Me.ConvergenceHistory.Energy = .Energia.GetValueOrDefault

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

            'Corrente de energia - atualizar valor da potência (kJ/s)

            Dim es As DWSIM.SimulationObjects.Streams.EnergyStream = form.Collections.CLCS_EnergyStreamCollection(Me.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Name)

            With es
                .Energia = Enew
                .GraphicObject.Calculated = True
            End With

            If Me.IterationCount >= Me.MaximumIterations Then
                Dim msgres As MsgBoxResult = MessageBox.Show(DWSIM.App.GetLocalString("Onmeromximodeiteraes"), _
                                Me.GraphicObject.Tag & " - " & DWSIM.App.GetLocalString("Nmeromximodeiteraesa3"), _
                                MessageBoxButtons.YesNo, MessageBoxIcon.Question)
                If msgres = MsgBoxResult.No Then
                    GoTo final
                Else
                    Me.IterationCount = 0
                End If
            End If

            Me.IterationCount += 1

            If Math.Abs(Me.ConvergenceHistory.EnergyE) > Me.ConvergenceParameters.Energy Then

                'Call function to calculate flowsheet
                With objargs
                    .Calculado = True
                    .Nome = Me.Nome
                    .Tipo = TipoObjeto.OT_EnergyRecycle
                End With

                form.CalculationQueue.Enqueue(objargs)

            Else
final:          Me.IterationsTaken = Me.IterationCount.ToString
                Me.IterationCount = 0
            End If

            Me.UpdatePropertyNodes(form.Options.SelectedUnitSystem, form.Options.NumberFormat)

        End Function

        Public Overrides Function DeCalculate() As Integer

            Dim form As Global.DWSIM.FormFlowsheet = Me.Flowsheet

            Me.IterationCount = 0

        End Function

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

        Public Overrides Sub PopulatePropertyGrid(ByVal pgrid As PropertyGridEx.PropertyGridEx, ByVal su As SystemsOfUnits.Units)

            Dim Conversor As New DWSIM.SystemsOfUnits.Converter

            With pgrid

                .PropertySort = PropertySort.Categorized
                .ShowCustomProperties = True
                .Item.Clear()

                Dim ent, saida As String
                If Me.GraphicObject.InputConnectors(0).IsAttached = True Then
                    ent = Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Tag
                Else
                    ent = ""
                End If
                If Me.GraphicObject.OutputConnectors(0).IsAttached = True Then
                    saida = Me.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Tag
                Else
                    saida = ""
                End If

                .Item.Add(DWSIM.App.GetLocalString("Correntedeentrada"), ent, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .CustomEditor = New DWSIM.Editors.Streams.UIInputESSelector
                End With

                .Item.Add(DWSIM.App.GetLocalString("Correntedesada"), saida, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .CustomEditor = New DWSIM.Editors.Streams.UIOutputESSelector
                End With

                .Item.Add(DWSIM.App.GetLocalString("Mtododeacelerao"), Me, "AccelerationMethod", False, DWSIM.App.GetLocalString("Configuraes2"), DWSIM.App.GetLocalString("Mtododeaceleraodacon"), True)

                If Me.AccelerationMethod = DWSIM.SimulationObjects.SpecialOps.Helpers.Recycle.AccelMethod.Wegstein Then
                    Dim cpc As New CustomPropertyCollection
                    cpc.Add(DWSIM.App.GetLocalString("Atrasonaacelerao"), Me.WegsteinParameters, "AccelDelay", False, DWSIM.App.GetLocalString("ParmetrosWegstein"), "", True)
                    cpc.Add(DWSIM.App.GetLocalString("Ferqunciadeacelerao"), Me.WegsteinParameters, "AccelFreq", False, DWSIM.App.GetLocalString("ParmetrosWegstein"), "", True)
                    cpc.Add(DWSIM.App.GetLocalString("Qmnimo"), Me.WegsteinParameters, "Qmin", False, DWSIM.App.GetLocalString("Wegstein"), "", True)
                    cpc.Add(DWSIM.App.GetLocalString("Qmximo"), Me.WegsteinParameters, "Qmax", False, DWSIM.App.GetLocalString("Wegstein"), "", True)
                    .Item.Add(DWSIM.App.GetLocalString("ParmetrosWegstein"), cpc, True, DWSIM.App.GetLocalString("Configuraes2"), DWSIM.App.GetLocalString("ParmetrosdomtododeWe"))
                    With .Item(.Item.Count - 1)
                        .IsBrowsable = True
                        .BrowsableLabelStyle = BrowsableTypeConverter.LabelStyle.lsEllipsis
                    End With
                End If

                .Item.Add(DWSIM.App.GetLocalString("NmeroMximodeIteraes"), Me, "MaximumIterations", False, DWSIM.App.GetLocalString("Configuraes2"), DWSIM.App.GetLocalString("Nmeromximodeiteraesd"), True)

                Dim valor As Double

                valor = Format(Converter.ConvertFromSI(su.heatflow, Me.ConvergenceParameters.Energy), FlowSheet.Options.NumberFormat)
                .Item.Add(FT(DWSIM.App.GetLocalString("Energia"), su.heatflow), valor, False, DWSIM.App.GetLocalString("Parmetrosdeconvergn3"), "", True)

                .Item.Add(DWSIM.App.GetLocalString("Iteraesnecessrias"), Me, "IterationsTaken", True, DWSIM.App.GetLocalString("Resultados4"), DWSIM.App.GetLocalString("Nmerodeiteraesusadas"), True)
                valor = Format(Converter.ConvertFromSI(su.deltaT, Me.ConvergenceHistory.EnergyE), FlowSheet.Options.NumberFormat)
                .Item.Add(FT(DWSIM.App.GetLocalString("Erronaenergia"), su.heatflow), valor, True, DWSIM.App.GetLocalString("Resultados4"), DWSIM.App.GetLocalString("Diferenaentreosvalor"), True)

                If Not Me.Annotation Is Nothing Then
                    .Item.Add(DWSIM.App.GetLocalString("Anotaes"), Me, "Annotation", False, DWSIM.App.GetLocalString("Outros"), DWSIM.App.GetLocalString("Cliquenobotocomretic"), True)
                    With .Item(.Item.Count - 1)
                        .IsBrowsable = False
                        .CustomEditor = New DWSIM.Editors.Annotation.UIAnnotationEditor
                    End With
                End If

                .ExpandAllGridItems()

            End With

        End Sub

        Public Overrides Function GetPropertyValue(ByVal prop As String, Optional ByVal su As SystemsOfUnits.Units = Nothing) As Object
            If su Is Nothing Then su = New DWSIM.SystemsOfUnits.SI
            Dim cv As New DWSIM.SystemsOfUnits.Converter
            Dim value As Double = 0
            Dim propidx As Integer = CInt(prop.Split("_")(2))

            Select Case propidx

                Case 0
                    'PROP_ER_0	Maximum Iterations
                    value = Me.MaximumIterations
                Case 1
                    'PROP_ER_1	Power Tolerance
                    value = Converter.ConvertFromSI(su.heatflow, Me.ConvergenceParameters.Energy)
                Case 2
                    'PROP_ER_2	Power Error
                    value = Converter.ConvertFromSI(su.heatflow, Me.ConvergenceHistory.EnergyE)
            End Select

            Return value
        End Function

        Public Overloads Overrides Function GetProperties(ByVal proptype As DWSIM.SimulationObjects.UnitOperations.BaseClass.PropertyType) As String()
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

        Public Overrides Function SetPropertyValue(ByVal prop As String, ByVal propval As Object, Optional ByVal su As DWSIM.SystemsOfUnits.Units = Nothing) As Object
            If su Is Nothing Then su = New DWSIM.SystemsOfUnits.SI
            Dim cv As New DWSIM.SystemsOfUnits.Converter
            Dim propidx As Integer = CInt(prop.Split("_")(2))

            Select Case propidx

                Case 0
                    'PROP_RY_0	Maximum Iterations
                    Me.MaximumIterations = propval
                Case 1
                    'PROP_ER_1	Power Tolerance
                    Me.ConvergenceParameters.Energy = Converter.ConvertToSI(su.heatflow, propval)

            End Select
            Return 1
        End Function

        Public Overrides Function GetPropertyUnit(ByVal prop As String, Optional ByVal su As SystemsOfUnits.Units = Nothing) As Object
            If su Is Nothing Then su = New DWSIM.SystemsOfUnits.SI
            Dim cv As New DWSIM.SystemsOfUnits.Converter
            Dim value As String = ""
            Dim propidx As Integer = CInt(prop.Split("_")(2))

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
    End Class

End Namespace

Namespace DWSIM.SimulationObjects.SpecialOps.Helpers.Recycle

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


