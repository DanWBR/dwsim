'    Energy Mixer Calculation Routines 
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

Imports Microsoft.MSDN.Samples.GraphicObjects
Imports DWSIM.DWSIM.Flowsheet.FlowSheetSolver

Namespace DWSIM.SimulationObjects.UnitOps

    <System.Serializable()> Public Class EnergyMixer

        Inherits SimulationObjects_UnitOpBaseClass

        Public Sub New()
            MyBase.New()
        End Sub

        Public Sub New(ByVal nome As String, ByVal descricao As String)

            MyBase.CreateNew()
            Me.m_ComponentName = nome
            Me.m_ComponentDescription = descricao
            Me.FillNodeItems()
            Me.QTFillNodeItems()
            Me.ShowQuickTable = False

        End Sub

        Public Overrides Function Calculate(Optional ByVal args As Object = Nothing) As Integer

            Dim form As Global.DWSIM.FormFlowsheet = Me.Flowsheet
            Dim objargs As New DWSIM.Outros.StatusChangeEventArgs

            If Not Me.GraphicObject.OutputConnectors(0).IsAttached Then
                'Call function to calculate flowsheet
                With objargs
                    .Calculado = False
                    .Nome = Me.Nome
                    .Tipo = TipoObjeto.NodeEn
                End With
                CalculateFlowsheet(FlowSheet, objargs, Nothing)
                Throw New Exception(DWSIM.App.GetLocalString("Verifiqueasconexesdo"))
            End If

            Me.PropertyPackage.CurrentMaterialStream = form.Collections.CLCS_MaterialStreamCollection(Me.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Name)

            Dim H, Hs, T, W, P, DQ As Double
            H = 0
            Hs = 0
            T = 0
            W = 0
            P = 0
            DQ = 0
            Dim ms As DWSIM.SimulationObjects.Streams.MaterialStream
            Dim cp As ConnectionPoint
            For Each cp In Me.GraphicObject.InputConnectors
                If cp.IsAttached Then
                    If cp.AttachedConnector.AttachedFrom.Calculated = False Then Throw New Exception(DWSIM.App.GetLocalString("Umaoumaiscorrentesna"))
                    If cp.AttachedConnector.AttachedFrom.TipoObjeto = TipoObjeto.EnergyStream Then

                        DQ = form.Collections.CLCS_EnergyStreamCollection(cp.AttachedConnector.AttachedFrom.Name).Energia.GetValueOrDefault

                    Else

                        ms = form.Collections.CLCS_MaterialStreamCollection(cp.AttachedConnector.AttachedFrom.Name)
                        If ms.Fases(0).SPMProperties.pressure.GetValueOrDefault < P Then
                            P = ms.Fases(0).SPMProperties.pressure
                        ElseIf P = 0 Then
                            P = ms.Fases(0).SPMProperties.pressure
                        End If
                        T = ms.Fases(0).SPMProperties.temperature
                        W += ms.Fases(0).SPMProperties.massflow.GetValueOrDefault
                        H = W * ms.Fases(0).SPMProperties.enthalpy.GetValueOrDefault

                    End If

                End If
            Next
            Hs = (H + DQ) / W

            Dim n As Integer = form.Collections.CLCS_MaterialStreamCollection(Me.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Name).Fases(0).Componentes.Count
            Dim Vw(n - 1) As Double
            For Each cp In Me.GraphicObject.InputConnectors
                If cp.IsAttached And Not cp.AttachedConnector.AttachedFrom.TipoObjeto = TipoObjeto.EnergyStream Then
                    ms = form.Collections.CLCS_MaterialStreamCollection(cp.AttachedConnector.AttachedFrom.Name)
                    Dim comp As DWSIM.ClassesBasicasTermodinamica.Substancia
                    Dim i As Integer = 0
                    For Each comp In ms.Fases(0).Componentes.Values
                        Vw(i) += comp.FracaoMassica.GetValueOrDefault * ms.Fases(0).SPMProperties.massflow.GetValueOrDefault
                        i += 1
                    Next
                End If
            Next

            With form.Collections.CLCS_MaterialStreamCollection(Me.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Name)
                Dim i As Integer = 0
                Dim comp As DWSIM.ClassesBasicasTermodinamica.Substancia
                For Each comp In .Fases(0).Componentes.Values
                    comp.FracaoMassica = Vw(i) / W
                    i += 1
                Next
                Dim mass_div_mm As Double = 0
                Dim sub1 As DWSIM.ClassesBasicasTermodinamica.Substancia
                For Each sub1 In .Fases(0).Componentes.Values
                    mass_div_mm += sub1.FracaoMassica.GetValueOrDefault / sub1.ConstantProperties.Molar_Weight
                    i += 1
                Next
                i = 0
                For Each sub1 In .Fases(0).Componentes.Values
                    sub1.FracaoMolar = sub1.FracaoMassica.GetValueOrDefault / sub1.ConstantProperties.Molar_Weight / mass_div_mm
                    i += 1
                Next
                .Fases(0).SPMProperties.temperature = T
                .Fases(0).SPMProperties.enthalpy = Hs
                .Fases(0).SPMProperties.pressure = P
                .Fases(0).SPMProperties.massflow = W
                .Fases(0).SPMProperties.molarfraction = 1
                .Fases(0).SPMProperties.massfraction = 1
                Me.PropertyPackage.CurrentMaterialStream = form.Collections.CLCS_MaterialStreamCollection(Me.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Name)
                Dim tmp As Object = Me.PropertyPackage.DW_CalcEquilibrio_ISOL(PropertyPackages.FlashSpec.P, PropertyPackages.FlashSpec.H, P, Hs, T)
                T = tmp(2)
                .Fases(0).SPMProperties.temperature = T
            End With

            'Call function to calculate flowsheet
            With objargs
                .Calculado = True
                .Nome = Me.Nome
                .Tag = Me.GraphicObject.Tag
                .Tipo = TipoObjeto.NodeEn
            End With

            form.CalculationQueue.Enqueue(objargs)

        End Function

        Public Overrides Function DeCalculate() As Integer

            Dim form As Global.DWSIM.FormFlowsheet = Me.Flowsheet

            If Me.GraphicObject.OutputConnectors(0).IsAttached Then


                'Zerar valores da corrente de matéria conectada a jusante
                With form.Collections.CLCS_MaterialStreamCollection(Me.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Name)
                    .Fases(0).SPMProperties.temperature = Nothing
                    .Fases(0).SPMProperties.pressure = Nothing
                    .Fases(0).SPMProperties.molarfraction = 1
                    Dim comp As DWSIM.ClassesBasicasTermodinamica.Substancia
                    Dim i As Integer = 0
                    For Each comp In .Fases(0).Componentes.Values
                        comp.FracaoMolar = 0
                        i += 1
                    Next
                    .Fases(0).SPMProperties.massflow = Nothing
                    .Fases(0).SPMProperties.molarflow = Nothing
                    .GraphicObject.Calculated = False
                End With

            End If

            'Call function to calculate flowsheet
            Dim objargs As New DWSIM.Outros.StatusChangeEventArgs
            With objargs
                .Calculado = False
                .Nome = Me.Nome
                .Tipo = TipoObjeto.NodeEn
            End With

            form.CalculationQueue.Enqueue(objargs)

        End Function

        Public Overloads Overrides Sub UpdatePropertyNodes(ByVal su As SistemasDeUnidades.Unidades, ByVal nf As String)

        End Sub

        Public Overrides Sub QTFillNodeItems()

        End Sub

        Public Overrides Sub PopulatePropertyGrid(ByVal pgrid As PropertyGridEx.PropertyGridEx, ByVal su As SistemasDeUnidades.Unidades)

        End Sub

        Public Overrides Function GetPropertyValue(ByVal prop As String, Optional ByVal su As SistemasDeUnidades.Unidades = Nothing) As Object
            Return 0

        End Function

        Public Overloads Overrides Function GetProperties(ByVal proptype As SimulationObjects_BaseClass.PropertyType) As String()
            Dim i As Integer = 0
            Dim proplist As New ArrayList
            Return proplist.ToArray(GetType(System.String))
            proplist = Nothing
        End Function

        Public Overrides Function SetPropertyValue(ByVal prop As String, ByVal propval As Object, Optional ByVal su As DWSIM.SistemasDeUnidades.Unidades = Nothing) As Object
            Return 0

        End Function

        Public Overrides Function GetPropertyUnit(ByVal prop As String, Optional ByVal su As SistemasDeUnidades.Unidades = Nothing) As Object
            Return 0

        End Function
    End Class

End Namespace
