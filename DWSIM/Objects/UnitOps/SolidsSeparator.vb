'    Solids Separator Calculation Routines 
'    Copyright 2013 Daniel Wagner O. de Medeiros
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
Imports DWSIM.DWSIM.ClassesBasicasTermodinamica
Imports DWSIM.DWSIM.SimulationObjects.Streams
Imports DWSIM.DWSIM.SimulationObjects.UnitOps.Auxiliary
Imports DWSIM.DWSIM.Flowsheet.FlowsheetSolver

Namespace DWSIM.SimulationObjects.UnitOps

    <System.Serializable()> Public Class SolidsSeparator

        Inherits SimulationObjects_UnitOpBaseClass

        Public Overrides Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean
            Return MyBase.LoadData(data)
        End Function

        Public Overrides Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement)

            Dim elements As System.Collections.Generic.List(Of System.Xml.Linq.XElement) = MyBase.SaveData()
            Dim ci As Globalization.CultureInfo = Globalization.CultureInfo.InvariantCulture

            Return elements

        End Function

        'Public Property EnergyImb() As Double

        Public Property SeparationEfficiency() As Double = 100.0#
        Public Property LiquidSeparationEfficiency() As Double = 100.0#

        Public Sub New()
            MyBase.New()
        End Sub

        Public Sub New(ByVal nome As String, ByVal descricao As String)

            MyBase.CreateNew()
            Me.m_ComponentName = nome
            Me.m_ComponentDescription = descricao
            Me.FillNodeItems()
            Me.QTFillNodeItems()
            Me.ShowQuickTable = True

        End Sub

        Public Overrides Function Calculate(Optional ByVal args As Object = Nothing) As Integer

            Dim form As FormFlowsheet = Me.FlowSheet
            Dim objargs As New DWSIM.Outros.StatusChangeEventArgs

            If Not Me.GraphicObject.InputConnectors(0).IsAttached Then
                'Call function to calculate flowsheet
                With objargs
                    .Calculado = False
                    .Nome = Me.Nome
                    .Tipo = TipoObjeto.SolidSeparator
                End With
                CalculateFlowsheet(FlowSheet, objargs, Nothing)
                Throw New Exception(DWSIM.App.GetLocalString("Verifiqueasconexesdo"))
            ElseIf Not Me.GraphicObject.OutputConnectors(0).IsAttached Then
                'Call function to calculate flowsheet
                With objargs
                    .Calculado = False
                    .Nome = Me.Nome
                    .Tipo = TipoObjeto.SolidSeparator
                End With
                CalculateFlowsheet(FlowSheet, objargs, Nothing)
                Throw New Exception(DWSIM.App.GetLocalString("Verifiqueasconexesdo"))
            ElseIf Not Me.GraphicObject.OutputConnectors(1).IsAttached Then
                'Call function to calculate flowsheet
                With objargs
                    .Calculado = False
                    .Nome = Me.Nome
                    .Tipo = TipoObjeto.SolidSeparator
                End With
                CalculateFlowsheet(FlowSheet, objargs, Nothing)
                Throw New Exception(DWSIM.App.GetLocalString("Verifiqueasconexesdo"))
            End If

            Dim instr, outstr1, outstr2 As Streams.MaterialStream
            instr = FlowSheet.Collections.ObjectCollection(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name)
            outstr1 = FlowSheet.Collections.ObjectCollection(Me.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Name)
            outstr2 = FlowSheet.Collections.ObjectCollection(Me.GraphicObject.OutputConnectors(1).AttachedConnector.AttachedTo.Name)

            Dim W As Double = instr.Fases(0).SPMProperties.massflow.GetValueOrDefault
            Dim Wsin As Double = instr.Fases(7).SPMProperties.massflow.GetValueOrDefault
            Dim Wlin As Double = instr.Fases(1).SPMProperties.massflow.GetValueOrDefault
            Dim Wvin As Double = instr.Fases(2).SPMProperties.massflow.GetValueOrDefault
            Dim sse, lse As Double
            sse = Me.SeparationEfficiency / 100
            lse = Me.LiquidSeparationEfficiency / 100
            Dim Wsout As Double = sse * Wsin + (1 - lse) * Wlin
            Dim Wlvout As Double = (1 - sse) * Wsin + lse * Wlin + Wvin

            Dim mw As Double

            Dim cp As ConnectionPoint

            cp = Me.GraphicObject.OutputConnectors(0)
            If cp.IsAttached Then
                outstr1 = form.Collections.CLCS_MaterialStreamCollection(cp.AttachedConnector.AttachedTo.Name)
                With outstr1
                    .ClearAllProps()
                    .Fases(0).SPMProperties.massflow = Wlvout
                    Dim comp As DWSIM.ClassesBasicasTermodinamica.Substancia
                    For Each comp In .Fases(0).Componentes.Values
                        comp.MassFlow = (1 - sse) * instr.Fases(7).Componentes(comp.Nome).MassFlow + instr.Fases(2).Componentes(comp.Nome).MassFlow
                        comp.MassFlow += lse * (instr.Fases(3).Componentes(comp.Nome).MassFlow + instr.Fases(4).Componentes(comp.Nome).MassFlow)
                        comp.FracaoMassica = comp.MassFlow / Wlvout
                    Next
                    mw = 0.0#
                    For Each comp In .Fases(0).Componentes.Values
                        mw += comp.FracaoMassica / comp.ConstantProperties.Molar_Weight
                    Next
                    For Each comp In .Fases(0).Componentes.Values
                        comp.FracaoMolar = comp.FracaoMassica / comp.ConstantProperties.Molar_Weight / mw
                    Next
                    For Each comp In .Fases(0).Componentes.Values
                        comp.MolarFlow = comp.MassFlow / comp.ConstantProperties.Molar_Weight / 1000
                    Next
                End With
            End If

            cp = Me.GraphicObject.OutputConnectors(1)
            If cp.IsAttached Then
                outstr2 = form.Collections.CLCS_MaterialStreamCollection(cp.AttachedConnector.AttachedTo.Name)
                With outstr2
                    .ClearAllProps()
                    .Fases(0).SPMProperties.massflow = Wsout
                    Dim comp As DWSIM.ClassesBasicasTermodinamica.Substancia
                    For Each comp In .Fases(0).Componentes.Values
                        comp.MassFlow = sse * instr.Fases(7).Componentes(comp.Nome).MassFlow.GetValueOrDefault + (1 - lse) * (instr.Fases(3).Componentes(comp.Nome).MassFlow + instr.Fases(4).Componentes(comp.Nome).MassFlow)
                        comp.FracaoMassica = If(Wsout > 0.0#, comp.MassFlow / Wsout, 0.0#)
                    Next
                    mw = 0.0#
                    For Each comp In .Fases(0).Componentes.Values
                        mw += comp.FracaoMassica / comp.ConstantProperties.Molar_Weight
                    Next
                    For Each comp In .Fases(0).Componentes.Values
                        comp.FracaoMolar = If(mw > 0.0#, comp.FracaoMassica / comp.ConstantProperties.Molar_Weight / mw, 0.0#)
                    Next
                    For Each comp In .Fases(0).Componentes.Values
                        comp.MolarFlow = comp.MassFlow / comp.ConstantProperties.Molar_Weight / 1000
                    Next
                End With
            End If

            'pass conditions

            outstr1.Fases(0).SPMProperties.temperature = InStr.Fases(0).SPMProperties.temperature.GetValueOrDefault
            outstr1.Fases(0).SPMProperties.pressure = InStr.Fases(0).SPMProperties.pressure.GetValueOrDefault
            outstr2.Fases(0).SPMProperties.temperature = InStr.Fases(0).SPMProperties.temperature.GetValueOrDefault
            outstr2.Fases(0).SPMProperties.pressure = InStr.Fases(0).SPMProperties.pressure.GetValueOrDefault

            'call the flowsheet calculator

            With objargs
                .Calculado = True
                .Nome = Me.Nome
                .Tag = Me.GraphicObject.Tag
                .Tipo = Me.GraphicObject.TipoObjeto
            End With

            form.CalculationQueue.Enqueue(objargs)

        End Function

        Public Overrides Function DeCalculate() As Integer

            Dim form As Global.DWSIM.FormFlowsheet = Me.FlowSheet

            Dim j As Integer = 0

            Dim ms As DWSIM.SimulationObjects.Streams.MaterialStream
            Dim cp As ConnectionPoint

            cp = Me.GraphicObject.OutputConnectors(0)
            If cp.IsAttached Then
                ms = form.Collections.CLCS_MaterialStreamCollection(cp.AttachedConnector.AttachedTo.Name)
                With ms
                    .Fases(0).SPMProperties.temperature = Nothing
                    .Fases(0).SPMProperties.pressure = Nothing
                    .Fases(0).SPMProperties.enthalpy = Nothing
                    Dim comp As DWSIM.ClassesBasicasTermodinamica.Substancia
                    j = 0
                    For Each comp In .Fases(0).Componentes.Values
                        comp.FracaoMolar = 0
                        comp.FracaoMassica = 0
                        j += 1
                    Next
                    .Fases(0).SPMProperties.massflow = Nothing
                    .Fases(0).SPMProperties.massfraction = 1
                    .Fases(0).SPMProperties.molarfraction = 1
                    .GraphicObject.Calculated = False
                End With
            End If

            cp = Me.GraphicObject.OutputConnectors(1)
            If cp.IsAttached Then
                ms = form.Collections.CLCS_MaterialStreamCollection(cp.AttachedConnector.AttachedTo.Name)
                With ms
                    .Fases(0).SPMProperties.temperature = Nothing
                    .Fases(0).SPMProperties.pressure = Nothing
                    .Fases(0).SPMProperties.enthalpy = Nothing
                    Dim comp As DWSIM.ClassesBasicasTermodinamica.Substancia
                    j = 0
                    For Each comp In .Fases(0).Componentes.Values
                        comp.FracaoMolar = 0
                        comp.FracaoMassica = 0
                        j += 1
                    Next
                    .Fases(0).SPMProperties.massflow = Nothing
                    .Fases(0).SPMProperties.massfraction = 1
                    .Fases(0).SPMProperties.molarfraction = 1
                    .GraphicObject.Calculated = False
                End With
            End If

            'Call function to calculate flowsheet
            Dim objargs As New DWSIM.Outros.StatusChangeEventArgs
            With objargs
                .Calculado = False
                .Nome = Me.Nome
                .Tipo = TipoObjeto.SolidSeparator
            End With

            form.CalculationQueue.Enqueue(objargs)

        End Function

        Public Overloads Overrides Sub UpdatePropertyNodes(ByVal su As SistemasDeUnidades.Unidades, ByVal nf As String)

            Dim Conversor As New DWSIM.SistemasDeUnidades.Conversor
            If Me.NodeTableItems Is Nothing Then
                Me.NodeTableItems = New System.Collections.Generic.Dictionary(Of Integer, DWSIM.Outros.NodeItem)
                Me.FillNodeItems()
            End If

            For Each nti As Outros.NodeItem In Me.NodeTableItems.Values
                nti.Value = GetPropertyValue(nti.Text, FlowSheet.Options.SelectedUnitSystem)
                nti.Unit = GetPropertyUnit(nti.Text, FlowSheet.Options.SelectedUnitSystem)
            Next

            If Me.QTNodeTableItems Is Nothing Then
                Me.QTNodeTableItems = New System.Collections.Generic.Dictionary(Of Integer, DWSIM.Outros.NodeItem)
                Me.QTFillNodeItems()
            End If

        End Sub

        Public Overrides Sub QTFillNodeItems()

            With Me.QTNodeTableItems

                .Clear()

            End With

        End Sub

        Public Overrides Sub PropertyValueChanged(ByVal s As Object, ByVal e As System.Windows.Forms.PropertyValueChangedEventArgs)

            MyBase.PropertyValueChanged(s, e)

            If FlowSheet.Options.CalculatorActivated Then

                'Call function to calculate flowsheet
                Dim objargs As New DWSIM.Outros.StatusChangeEventArgs
                With objargs
                    .Tag = Me.GraphicObject.Tag
                    .Calculado = False
                    .Nome = Me.GraphicObject.Name
                    .Tipo = Me.GraphicObject.TipoObjeto
                    .Emissor = "PropertyGrid"
                End With

                If Me.IsSpecAttached = True And Me.SpecVarType = DWSIM.SimulationObjects.SpecialOps.Helpers.Spec.TipoVar.Fonte Then FlowSheet.Collections.CLCS_SpecCollection(Me.AttachedSpecId).Calculate()
                FlowSheet.CalculationQueue.Enqueue(objargs)

            End If

        End Sub

        Public Overrides Sub PopulatePropertyGrid(ByVal pgrid As PropertyGridEx.PropertyGridEx, ByVal su As SistemasDeUnidades.Unidades)

            Dim Conversor As New DWSIM.SistemasDeUnidades.Conversor

            With pgrid

                .PropertySort = PropertySort.Categorized
                .ShowCustomProperties = True
                .Item.Clear()

                MyBase.PopulatePropertyGrid(pgrid, su)

                Dim ent, saida1, saida2 As String
                If Me.GraphicObject.InputConnectors(0).IsAttached = True Then
                    ent = Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Tag
                Else
                    ent = ""
                End If
                If Me.GraphicObject.OutputConnectors(0).IsAttached = True Then
                    saida1 = Me.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Tag
                Else
                    saida1 = ""
                End If
                If Me.GraphicObject.OutputConnectors(1).IsAttached = True Then
                    saida2 = Me.GraphicObject.OutputConnectors(1).AttachedConnector.AttachedTo.Tag
                Else
                    saida2 = ""
                End If

                .Item.Add(DWSIM.App.GetLocalString("Correntedeentrada"), ent, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .CustomEditor = New DWSIM.Editors.Streams.UIInputMSSelector
                End With

                .Item.Add(DWSIM.App.GetLocalString("OutletStream1"), saida1, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .CustomEditor = New DWSIM.Editors.Streams.UIOutputMSSelector
                End With

                .Item.Add(DWSIM.App.GetLocalString("OutletStream2"), saida2, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .CustomEditor = New DWSIM.Editors.Streams.UIOutputMSSelector
                End With

                .Item.Add(DWSIM.App.GetLocalString("SolidSepEfficiency"), Me, "SeparationEfficiency", False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("SolidSepEfficiencyDesc"), True)
                .Item.Add(DWSIM.App.GetLocalString("LiquidSepEfficiency"), Me, "LiquidSeparationEfficiency", False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("LiquidSepEfficiencyDesc"), True)

                If Me.IsSpecAttached = True Then
                    .Item.Add(DWSIM.App.GetLocalString("ObjetoUtilizadopor"), FlowSheet.Collections.ObjectCollection(Me.AttachedSpecId).GraphicObject.Tag, True, DWSIM.App.GetLocalString("Miscelnea2"), "", True)
                    .Item.Add(DWSIM.App.GetLocalString("Utilizadocomo"), Me.SpecVarType, True, DWSIM.App.GetLocalString("Miscelnea3"), "", True)
                End If

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

        Public Overrides Function GetPropertyValue(ByVal prop As String, Optional ByVal su As SistemasDeUnidades.Unidades = Nothing) As Object
            If su Is Nothing Then su = New DWSIM.SistemasDeUnidades.UnidadesSI
            Dim cv As New DWSIM.SistemasDeUnidades.Conversor
            Dim value As Double = 0
            Dim propidx As Integer = CInt(prop.Split("_")(2))

            Select Case propidx
                Case 1
                    value = Me.SeparationEfficiency
                Case 2
                    value = Me.LiquidSeparationEfficiency
            End Select

            Return value

        End Function

        Public Overloads Overrides Function GetProperties(ByVal proptype As SimulationObjects_BaseClass.PropertyType) As String()
            Dim i As Integer = 0
            Dim proplist As New ArrayList
            Select Case proptype
                Case PropertyType.RW
                    'For i = 0 To 0
                    '    proplist.Add("PROP_SS_" + CStr(i))
                    'Next
                Case PropertyType.WR
                    For i = 1 To 2
                        proplist.Add("PROP_SS_" + CStr(i))
                    Next
                Case PropertyType.ALL
                    For i = 1 To 2
                        proplist.Add("PROP_SS_" + CStr(i))
                    Next
                Case PropertyType.RO
                    'For i = 0 To 0
                    '    proplist.Add("PROP_SS_" + CStr(i))
                    'Next
            End Select
            Return proplist.ToArray(GetType(System.String))
            proplist = Nothing
        End Function

        Public Overrides Function SetPropertyValue(ByVal prop As String, ByVal propval As Object, Optional ByVal su As DWSIM.SistemasDeUnidades.Unidades = Nothing) As Object
            If su Is Nothing Then su = New DWSIM.SistemasDeUnidades.UnidadesSI
            Dim cv As New DWSIM.SistemasDeUnidades.Conversor
            Dim propidx As Integer = CInt(prop.Split("_")(2))

            Select Case propidx
                Case 1
                    'PROP_SS_1	Solid Separation Efficiency
                    Me.SeparationEfficiency = propval
                Case 2
                    'PROP_SS_2	Liquid Separation Efficiency
                    Me.LiquidSeparationEfficiency = propval
            End Select

            Return 1

        End Function

        Public Overrides Function GetPropertyUnit(ByVal prop As String, Optional ByVal su As SistemasDeUnidades.Unidades = Nothing) As Object
            'If su Is Nothing Then su = New DWSIM.SistemasDeUnidades.UnidadesSI
            'Dim cv As New DWSIM.SistemasDeUnidades.Conversor
            Dim value As String = "%"
            'Dim propidx As Integer = CInt(prop.Split("_")(2))

            Return value
        End Function
    End Class

End Namespace
