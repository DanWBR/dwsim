Imports DWSIM.Thermodynamics.Streams
Imports DWSIM.UnitOperations
Imports DWSIM.SharedClasses
Imports System.Text

Public Class EditorTooltips

    Public Shared Sub Update(obj As Interfaces.ISimulationObject, flowsheet As Interfaces.IFlowsheet)

        Dim su = flowsheet.FlowsheetOptions.SelectedUnitSystem
        Dim nf = flowsheet.FlowsheetOptions.NumberFormat

        If TypeOf obj Is MaterialStream Then

            Dim ms As MaterialStream = DirectCast(obj, MaterialStream)

            Dim editor = ms.f

            If editor IsNot Nothing Then

                Dim T = ms.Phases(0).Properties.temperature.GetValueOrDefault
                Dim ttTemp As New StringBuilder

                Dim units = su.GetUnitSet(UnitOfMeasure.temperature)
                units.Remove(su.temperature)

                For Each u In units
                    ttTemp.AppendLine(T.ConvertFromSI(u).ToString(nf) + " " + u)
                Next

                editor.ToolTipValues.SetToolTip(editor.tbTemp, ttTemp.ToString)

                Dim P = ms.Phases(0).Properties.pressure.GetValueOrDefault
                Dim ttP As New StringBuilder

                units = su.GetUnitSet(UnitOfMeasure.pressure)
                units.Remove(su.pressure)

                For Each u In units
                    ttP.AppendLine(P.ConvertFromSI(u).ToString(nf) + " " + u)
                Next

                editor.ToolTipValues.SetToolTip(editor.tbPressure, ttP.ToString)

                Dim W = ms.Phases(0).Properties.massflow.GetValueOrDefault
                Dim ttW As New StringBuilder

                units = su.GetUnitSet(UnitOfMeasure.massflow)
                units.Remove(su.massflow)

                For Each u In units
                    ttW.AppendLine(W.ConvertFromSI(u).ToString(nf) + " " + u)
                Next

                editor.ToolTipValues.SetToolTip(editor.tbMassFlow, ttW.ToString)

                Dim M = ms.Phases(0).Properties.molarflow.GetValueOrDefault
                Dim ttM As New StringBuilder

                units = su.GetUnitSet(UnitOfMeasure.molarflow)
                units.Remove(su.molarflow)

                For Each u In units
                    ttM.AppendLine(M.ConvertFromSI(u).ToString(nf) + " " + u)
                Next

                editor.ToolTipValues.SetToolTip(editor.tbMoleFlow, ttM.ToString)

                Dim Q = ms.Phases(0).Properties.volumetric_flow.GetValueOrDefault
                Dim ttQ As New StringBuilder

                units = su.GetUnitSet(UnitOfMeasure.volumetricFlow)
                units.Remove(su.volumetricFlow)

                For Each u In units
                    ttQ.AppendLine(Q.ConvertFromSI(u).ToString(nf) + " " + u)
                Next

                editor.ToolTipValues.SetToolTip(editor.tbVolFlow, ttQ.ToString)

                Dim H = ms.Phases(0).Properties.enthalpy.GetValueOrDefault
                Dim ttH As New StringBuilder

                units = su.GetUnitSet(UnitOfMeasure.enthalpy)
                units.Remove(su.enthalpy)

                For Each u In units
                    ttH.AppendLine(H.ConvertFromSI(u).ToString(nf) + " " + u)
                Next

                editor.ToolTipValues.SetToolTip(editor.tbEnth, ttH.ToString)

                Dim S = ms.Phases(0).Properties.entropy.GetValueOrDefault
                Dim ttS As New StringBuilder

                units = su.GetUnitSet(UnitOfMeasure.entropy)
                units.Remove(su.entropy)

                For Each u In units
                    ttS.AppendLine(S.ConvertFromSI(u).ToString(nf) + " " + u)
                Next

                editor.ToolTipValues.SetToolTip(editor.tbEntr, ttS.ToString)

            End If

        ElseIf TypeOf obj Is Cooler Then

            Dim uo As Cooler = DirectCast(obj, Cooler)

            Dim editor = uo.f

            If editor IsNot Nothing Then

                Dim prop As Double, tb As TextBox, units As List(Of String), text As StringBuilder

                tb = editor.tbHeatingChange
                prop = uo.DeltaQ.GetValueOrDefault
                text = New StringBuilder
                units = su.GetUnitSet(UnitOfMeasure.heatflow)
                units.Remove(su.heatflow)
                For Each u In units
                    text.AppendLine(prop.ConvertFromSI(u).ToString(nf) + " " + u)
                Next
                editor.ToolTipValues.SetToolTip(tb, text.ToString)

                tb = editor.tbOutletTemperature
                prop = uo.OutletTemperature.GetValueOrDefault
                text = New StringBuilder
                units = su.GetUnitSet(UnitOfMeasure.temperature)
                units.Remove(su.temperature)
                For Each u In units
                    text.AppendLine(prop.ConvertFromSI(u).ToString(nf) + " " + u)
                Next
                editor.ToolTipValues.SetToolTip(tb, text.ToString)

                tb = editor.tbPressureDrop
                prop = uo.DeltaP.GetValueOrDefault
                text = New StringBuilder
                units = su.GetUnitSet(UnitOfMeasure.deltaP)
                units.Remove(su.deltaP)
                For Each u In units
                    text.AppendLine(prop.ConvertFromSI(u).ToString(nf) + " " + u)
                Next
                editor.ToolTipValues.SetToolTip(tb, text.ToString)

                tb = editor.tbTemperatureChange
                prop = uo.DeltaT.GetValueOrDefault
                text = New StringBuilder
                units = su.GetUnitSet(UnitOfMeasure.deltaT)
                units.Remove(su.deltaT)
                For Each u In units
                    text.AppendLine(prop.ConvertFromSI(u).ToString(nf) + " " + u)
                Next
                editor.ToolTipValues.SetToolTip(tb, text.ToString)

            End If

        ElseIf TypeOf obj Is Heater Then

            Dim uo As Heater = DirectCast(obj, Heater)

            Dim editor = uo.f

            If editor IsNot Nothing Then

                Dim prop As Double, tb As TextBox, units As List(Of String), text As StringBuilder

                tb = editor.tbHeatingChange
                prop = uo.DeltaQ.GetValueOrDefault
                text = New StringBuilder
                units = su.GetUnitSet(UnitOfMeasure.heatflow)
                units.Remove(su.heatflow)
                For Each u In units
                    text.AppendLine(prop.ConvertFromSI(u).ToString(nf) + " " + u)
                Next
                editor.ToolTipValues.SetToolTip(tb, text.ToString)

                tb = editor.tbOutletTemperature
                prop = uo.OutletTemperature.GetValueOrDefault
                text = New StringBuilder
                units = su.GetUnitSet(UnitOfMeasure.temperature)
                units.Remove(su.temperature)
                For Each u In units
                    text.AppendLine(prop.ConvertFromSI(u).ToString(nf) + " " + u)
                Next
                editor.ToolTipValues.SetToolTip(tb, text.ToString)

                tb = editor.tbPressureDrop
                prop = uo.DeltaP.GetValueOrDefault
                text = New StringBuilder
                units = su.GetUnitSet(UnitOfMeasure.deltaP)
                units.Remove(su.deltaP)
                For Each u In units
                    text.AppendLine(prop.ConvertFromSI(u).ToString(nf) + " " + u)
                Next
                editor.ToolTipValues.SetToolTip(tb, text.ToString)

                tb = editor.tbTemperatureChange
                prop = uo.DeltaT.GetValueOrDefault
                text = New StringBuilder
                units = su.GetUnitSet(UnitOfMeasure.deltaT)
                units.Remove(su.deltaT)
                For Each u In units
                    text.AppendLine(prop.ConvertFromSI(u).ToString(nf) + " " + u)
                Next
                editor.ToolTipValues.SetToolTip(tb, text.ToString)

            End If

        ElseIf TypeOf obj Is Compressor Then

            Dim uo As Compressor = DirectCast(obj, Compressor)

            Dim editor = uo.f

            If editor IsNot Nothing Then

                Dim prop As Double, tb As TextBox, units As List(Of String), text As StringBuilder

                tb = editor.tbPower
                prop = uo.DeltaQ
                text = New StringBuilder
                units = su.GetUnitSet(UnitOfMeasure.heatflow)
                units.Remove(su.heatflow)
                For Each u In units
                    text.AppendLine(prop.ConvertFromSI(u).ToString(nf) + " " + u)
                Next
                editor.ToolTipValues.SetToolTip(tb, text.ToString)

                tb = editor.tbTemp
                prop = uo.OutletTemperature
                text = New StringBuilder
                units = su.GetUnitSet(UnitOfMeasure.temperature)
                units.Remove(su.temperature)
                For Each u In units
                    text.AppendLine(prop.ConvertFromSI(u).ToString(nf) + " " + u)
                Next
                editor.ToolTipValues.SetToolTip(tb, text.ToString)

                tb = editor.tbPressureDrop
                prop = uo.DeltaP
                text = New StringBuilder
                units = su.GetUnitSet(UnitOfMeasure.deltaP)
                units.Remove(su.deltaP)
                For Each u In units
                    text.AppendLine(prop.ConvertFromSI(u).ToString(nf) + " " + u)
                Next
                editor.ToolTipValues.SetToolTip(tb, text.ToString)

                tb = editor.tbOutletPressure
                prop = uo.POut
                text = New StringBuilder
                units = su.GetUnitSet(UnitOfMeasure.pressure)
                units.Remove(su.pressure)
                For Each u In units
                    text.AppendLine(prop.ConvertFromSI(u).ToString(nf) + " " + u)
                Next
                editor.ToolTipValues.SetToolTip(tb, text.ToString)

                tb = editor.tbDeltaT
                prop = uo.DeltaT
                text = New StringBuilder
                units = su.GetUnitSet(UnitOfMeasure.deltaT)
                units.Remove(su.deltaT)
                For Each u In units
                    text.AppendLine(prop.ConvertFromSI(u).ToString(nf) + " " + u)
                Next
                editor.ToolTipValues.SetToolTip(tb, text.ToString)

            End If

        ElseIf TypeOf obj Is Expander Then

            Dim uo As Expander = DirectCast(obj, Expander)

            Dim editor = uo.f

            If editor IsNot Nothing Then

                Dim prop As Double, tb As TextBox, units As List(Of String), text As StringBuilder

                tb = editor.tbPower
                prop = uo.DeltaQ
                text = New StringBuilder
                units = su.GetUnitSet(UnitOfMeasure.heatflow)
                units.Remove(su.heatflow)
                For Each u In units
                    text.AppendLine(prop.ConvertFromSI(u).ToString(nf) + " " + u)
                Next
                editor.ToolTipValues.SetToolTip(tb, text.ToString)

                tb = editor.tbTemp
                prop = uo.OutletTemperature
                text = New StringBuilder
                units = su.GetUnitSet(UnitOfMeasure.temperature)
                units.Remove(su.temperature)
                For Each u In units
                    text.AppendLine(prop.ConvertFromSI(u).ToString(nf) + " " + u)
                Next
                editor.ToolTipValues.SetToolTip(tb, text.ToString)

                tb = editor.tbPressureDrop
                prop = uo.DeltaP
                text = New StringBuilder
                units = su.GetUnitSet(UnitOfMeasure.deltaP)
                units.Remove(su.deltaP)
                For Each u In units
                    text.AppendLine(prop.ConvertFromSI(u).ToString(nf) + " " + u)
                Next
                editor.ToolTipValues.SetToolTip(tb, text.ToString)

                tb = editor.tbOutletPressure
                prop = uo.POut
                text = New StringBuilder
                units = su.GetUnitSet(UnitOfMeasure.pressure)
                units.Remove(su.pressure)
                For Each u In units
                    text.AppendLine(prop.ConvertFromSI(u).ToString(nf) + " " + u)
                Next
                editor.ToolTipValues.SetToolTip(tb, text.ToString)

                tb = editor.tbDeltaT
                prop = uo.DeltaT
                text = New StringBuilder
                units = su.GetUnitSet(UnitOfMeasure.deltaT)
                units.Remove(su.deltaT)
                For Each u In units
                    text.AppendLine(prop.ConvertFromSI(u).ToString(nf) + " " + u)
                Next
                editor.ToolTipValues.SetToolTip(tb, text.ToString)

            End If

        ElseIf TypeOf obj Is Valve Then

            Dim uo As Valve = DirectCast(obj, Valve)

            Dim editor = uo.f

            If editor IsNot Nothing Then

                Dim prop As Double, tb As TextBox, units As List(Of String), text As StringBuilder

                tb = editor.tbPressureDrop
                prop = uo.DeltaP.GetValueOrDefault
                text = New StringBuilder
                units = su.GetUnitSet(UnitOfMeasure.deltaP)
                units.Remove(su.deltaP)
                For Each u In units
                    text.AppendLine(prop.ConvertFromSI(u).ToString(nf) + " " + u)
                Next
                editor.ToolTipValues.SetToolTip(tb, text.ToString)

                tb = editor.tbOutletPressure
                prop = uo.OutletPressure.GetValueOrDefault
                text = New StringBuilder
                units = su.GetUnitSet(UnitOfMeasure.pressure)
                units.Remove(su.pressure)
                For Each u In units
                    text.AppendLine(prop.ConvertFromSI(u).ToString(nf) + " " + u)
                Next
                editor.ToolTipValues.SetToolTip(tb, text.ToString)

            End If

        ElseIf TypeOf obj Is HeatExchanger Then

            Dim uo As HeatExchanger = DirectCast(obj, HeatExchanger)

            Dim editor = uo.f

            If editor IsNot Nothing Then

                Dim prop As Double, tb As TextBox, units As List(Of String), text As StringBuilder

                tb = editor.tbHeat
                prop = uo.Q.GetValueOrDefault
                text = New StringBuilder
                units = su.GetUnitSet(UnitOfMeasure.heatflow)
                units.Remove(su.heatflow)
                For Each u In units
                    text.AppendLine(prop.ConvertFromSI(u).ToString(nf) + " " + u)
                Next
                editor.ToolTipValues.SetToolTip(tb, text.ToString)

                tb = editor.tbHeatLoss
                prop = uo.HeatLoss
                text = New StringBuilder
                units = su.GetUnitSet(UnitOfMeasure.heatflow)
                units.Remove(su.heatflow)
                For Each u In units
                    text.AppendLine(prop.ConvertFromSI(u).ToString(nf) + " " + u)
                Next
                editor.ToolTipValues.SetToolTip(tb, text.ToString)

                tb = editor.tbColdFluidOutletT
                prop = uo.ColdSideOutletTemperature
                text = New StringBuilder
                units = su.GetUnitSet(UnitOfMeasure.temperature)
                units.Remove(su.temperature)
                For Each u In units
                    text.AppendLine(prop.ConvertFromSI(u).ToString(nf) + " " + u)
                Next
                editor.ToolTipValues.SetToolTip(tb, text.ToString)

                tb = editor.tbHotFluidOutletT
                prop = uo.HotSideOutletTemperature
                text = New StringBuilder
                units = su.GetUnitSet(UnitOfMeasure.temperature)
                units.Remove(su.temperature)
                For Each u In units
                    text.AppendLine(prop.ConvertFromSI(u).ToString(nf) + " " + u)
                Next
                editor.ToolTipValues.SetToolTip(tb, text.ToString)

                tb = editor.tbColdFluidPDrop
                prop = uo.ColdSidePressureDrop
                text = New StringBuilder
                units = su.GetUnitSet(UnitOfMeasure.deltaP)
                units.Remove(su.deltaP)
                For Each u In units
                    text.AppendLine(prop.ConvertFromSI(u).ToString(nf) + " " + u)
                Next
                editor.ToolTipValues.SetToolTip(tb, text.ToString)

                tb = editor.tbHotFluidPDrop
                prop = uo.HotSidePressureDrop
                text = New StringBuilder
                units = su.GetUnitSet(UnitOfMeasure.deltaP)
                units.Remove(su.deltaP)
                For Each u In units
                    text.AppendLine(prop.ConvertFromSI(u).ToString(nf) + " " + u)
                Next
                editor.ToolTipValues.SetToolTip(tb, text.ToString)

                tb = editor.tbArea
                prop = uo.Area.GetValueOrDefault
                text = New StringBuilder
                units = su.GetUnitSet(UnitOfMeasure.area)
                units.Remove(su.area)
                For Each u In units
                    text.AppendLine(prop.ConvertFromSI(u).ToString(nf) + " " + u)
                Next
                editor.ToolTipValues.SetToolTip(tb, text.ToString)

                tb = editor.tbMITA
                prop = uo.MITA
                text = New StringBuilder
                units = su.GetUnitSet(UnitOfMeasure.deltaT)
                units.Remove(su.deltaT)
                For Each u In units
                    text.AppendLine(prop.ConvertFromSI(u).ToString(nf) + " " + u)
                Next
                editor.ToolTipValues.SetToolTip(tb, text.ToString)

                tb = editor.tbOverallU
                prop = uo.OverallCoefficient
                text = New StringBuilder
                units = su.GetUnitSet(UnitOfMeasure.heat_transf_coeff)
                units.Remove(su.heat_transf_coeff)
                For Each u In units
                    text.AppendLine(prop.ConvertFromSI(u).ToString(nf) + " " + u)
                Next
                editor.ToolTipValues.SetToolTip(tb, text.ToString)

            End If

        ElseIf TypeOf obj Is Pump Then

            Dim uo As Pump = DirectCast(obj, Pump)

            Dim editor = uo.f

            If editor IsNot Nothing Then

                Dim prop As Double, tb As TextBox, units As List(Of String), text As StringBuilder

                tb = editor.tbHeatingChange
                prop = uo.DeltaQ.GetValueOrDefault
                text = New StringBuilder
                units = su.GetUnitSet(UnitOfMeasure.heatflow)
                units.Remove(su.heatflow)
                For Each u In units
                    text.AppendLine(prop.ConvertFromSI(u).ToString(nf) + " " + u)
                Next
                editor.ToolTipValues.SetToolTip(tb, text.ToString)

                tb = editor.tbOutletTemperature
                prop = uo.OutletTemperature
                text = New StringBuilder
                units = su.GetUnitSet(UnitOfMeasure.temperature)
                units.Remove(su.temperature)
                For Each u In units
                    text.AppendLine(prop.ConvertFromSI(u).ToString(nf) + " " + u)
                Next
                editor.ToolTipValues.SetToolTip(tb, text.ToString)

                tb = editor.tbPressureIncr
                prop = uo.DeltaP.GetValueOrDefault
                text = New StringBuilder
                units = su.GetUnitSet(UnitOfMeasure.deltaP)
                units.Remove(su.deltaP)
                For Each u In units
                    text.AppendLine(prop.ConvertFromSI(u).ToString(nf) + " " + u)
                Next
                editor.ToolTipValues.SetToolTip(tb, text.ToString)

                tb = editor.tbOutletPressure
                prop = uo.Pout
                text = New StringBuilder
                units = su.GetUnitSet(UnitOfMeasure.deltaP)
                units.Remove(su.deltaP)
                For Each u In units
                    text.AppendLine(prop.ConvertFromSI(u).ToString(nf) + " " + u)
                Next
                editor.ToolTipValues.SetToolTip(tb, text.ToString)

            End If

        ElseIf TypeOf obj Is Pipe Then

            Dim uo As Pipe = DirectCast(obj, Pipe)

            Dim editor = uo.f

            If editor IsNot Nothing Then

                Dim prop As Double, tb As TextBox, units As List(Of String), text As StringBuilder

                tb = editor.tbOutletTemperature
                prop = uo.OutletTemperature
                text = New StringBuilder
                units = su.GetUnitSet(UnitOfMeasure.temperature)
                units.Remove(su.temperature)
                For Each u In units
                    text.AppendLine(prop.ConvertFromSI(u).ToString(nf) + " " + u)
                Next
                editor.ToolTipValues.SetToolTip(tb, text.ToString)

                tb = editor.tbOutletPressure
                prop = uo.OutletPressure
                text = New StringBuilder
                units = su.GetUnitSet(UnitOfMeasure.deltaP)
                units.Remove(su.deltaP)
                For Each u In units
                    text.AppendLine(prop.ConvertFromSI(u).ToString(nf) + " " + u)
                Next
                editor.ToolTipValues.SetToolTip(tb, text.ToString)

            End If

        End If

    End Sub

End Class
