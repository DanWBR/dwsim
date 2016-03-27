'    Centrifugal Pump Calculation Routines 
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

Imports DWSIM.DrawingTools.GraphicObjects
Imports System.Globalization
Imports System.Reflection
Imports System.Linq
Imports DWSIM.Interfaces.Enums.GraphicObjects
Imports DWSIM.UnitOperations.Auxiliary.PipingOps
Imports DWSIM.Thermodynamics.MathEx.Interpolation

Namespace Auxiliary.PipingOps

    Public Enum CurveType
        Head
        Power
        NPSHr
        Efficiency
        SystemHead
        None
    End Enum

    <System.Serializable()> Public Class Curve

        Implements XMLSerializer.Interfaces.ICustomXMLSerialization

        Protected _x As ArrayList
        Protected _y As ArrayList
        Protected _xunit As String = ""
        Protected _yunit As String = ""
        Protected _type As CurveType
        Protected _id As String = ""
        Protected _name As String = ""
        Protected _enabled As Boolean = True

        Sub New(ByVal id As String, ByVal name As String, ByVal x As ArrayList, ByVal xunit As String, ByVal y As ArrayList, ByVal yunit As String, ByVal type As CurveType)
            Me.New()
            Me.ID = id
            Me.Name = name
            Me.x = x
            Me.y = y
            Me.xunit = xunit
            Me.yunit = yunit
            Me.CvType = type
        End Sub

        Sub New(ByVal id As String, ByVal name As String, ByVal type As CurveType)
            Me.New()
            Me.ID = id
            Me.Name = name
            Me.CvType = type
        End Sub

        Sub New()
            _x = New ArrayList
            _y = New ArrayList
        End Sub

        Public Property Enabled() As Boolean
            Get
                Return _enabled
            End Get
            Set(ByVal value As Boolean)
                _enabled = value
            End Set
        End Property

        Public Property Name() As String
            Get
                Return _name
            End Get
            Set(ByVal value As String)
                _name = value
            End Set
        End Property

        Public Property ID() As String
            Get
                Return _id
            End Get
            Set(ByVal value As String)
                _id = value
            End Set
        End Property

        Public Property CvType() As CurveType
            Get
                Return _type
            End Get
            Set(ByVal value As CurveType)
                _type = value
            End Set
        End Property

        Public Property yunit() As String
            Get
                Return _yunit
            End Get
            Set(ByVal value As String)
                _yunit = value
            End Set
        End Property

        Public Property xunit() As String
            Get
                Return _xunit
            End Get
            Set(ByVal value As String)
                _xunit = value
            End Set
        End Property

        Public Property y() As ArrayList
            Get
                Return _y
            End Get
            Set(ByVal value As ArrayList)
                _y = value
            End Set
        End Property

        Public Property x() As ArrayList
            Get
                Return _x
            End Get
            Set(ByVal value As ArrayList)
                _x = value
            End Set
        End Property

        Public Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean Implements XMLSerializer.Interfaces.ICustomXMLSerialization.LoadData

            Return XMLSerializer.XMLSerializer.Deserialize(Me, data)

        End Function

        Public Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement) Implements XMLSerializer.Interfaces.ICustomXMLSerialization.SaveData

            Return XMLSerializer.XMLSerializer.Serialize(Me)

        End Function

    End Class

End Namespace

Namespace UnitOperations

    <System.Serializable()> Public Class Pump

        Inherits SharedClasses.UnitOperations.UnitOpBaseClass

        Public Enum CalculationMode
            Delta_P = 0
            OutletPressure = 1
            EnergyStream = 2
            Curves = 3
            Power = 4
        End Enum

        Protected m_dp As Nullable(Of Double)
        Protected m_dt As Nullable(Of Double)
        Protected m_DQ As Nullable(Of Double)

        Protected m_eta As Nullable(Of Double) = 75
        Protected m_npsh As Nullable(Of Double)
        Protected m_pout As Double = 0

        Protected m_ignorephase As Boolean = True

        Protected m_FixOnDeltaP As Boolean = True
        Protected m_cmode As CalculationMode = CalculationMode.Delta_P

        Protected m_curves As New Dictionary(Of String, UnitOperations.Auxiliary.PipingOps.Curve)

        Protected _curvehead As Double
        Protected _curveeff As Double
        Protected _curvenpsh As Double
        Protected _curvepower As Double
        Protected _curvesyshead As Double
        Protected _curveflow As Double

        Public Overrides Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean

            MyBase.LoadData(data)

            m_curves = New Dictionary(Of String, UnitOperations.Auxiliary.PipingOps.Curve)

            For Each xel As XElement In (From xel2 As XElement In data Select xel2 Where xel2.Name = "Curves").Elements.ToList
                Dim cv As New Curve()
                cv.LoadData(xel.Elements.ToList)
                m_curves.Add(cv.Name, cv)
            Next

            Return True

        End Function

        Public Overrides Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement)

            Dim elements As System.Collections.Generic.List(Of System.Xml.Linq.XElement) = MyBase.SaveData()
            Dim ci As Globalization.CultureInfo = Globalization.CultureInfo.InvariantCulture

            With elements
                .Add(New XElement("Curves"))
                For Each kvp As KeyValuePair(Of String, Curve) In m_curves
                    .Item(.Count - 1).Add(New XElement("Curve", New XAttribute("ID", kvp.Key), kvp.Value.SaveData.ToArray()))
                Next
            End With

            Return elements

        End Function

        Public Property CurveFlow() As Double
            Get
                Return _curveflow
            End Get
            Set(ByVal value As Double)
                _curveflow = value
            End Set
        End Property

        Public Property CurveSysHead() As Double
            Get
                Return _curvesyshead
            End Get
            Set(ByVal value As Double)
                _curvesyshead = value
            End Set
        End Property

        Public Property CurvePower() As Double
            Get
                Return _curvepower
            End Get
            Set(ByVal value As Double)
                _curvepower = value
            End Set
        End Property

        Public Property CurveNPSHr() As Double
            Get
                Return _curvenpsh
            End Get
            Set(ByVal value As Double)
                _curvenpsh = value
            End Set
        End Property

        Public Property CurveEff() As Double
            Get
                Return _curveeff
            End Get
            Set(ByVal value As Double)
                _curveeff = value
            End Set
        End Property

        Public Property CurveHead() As Double
            Get
                Return _curvehead
            End Get
            Set(ByVal value As Double)
                _curvehead = value
            End Set
        End Property

        Public Property Curves() As Dictionary(Of String, UnitOperations.Auxiliary.PipingOps.Curve)
            Get
                If m_curves Is Nothing Then
                    m_curves = New Dictionary(Of String, UnitOperations.Auxiliary.PipingOps.Curve)
                    CreateCurves()
                End If
                Return m_curves
            End Get
            Set(ByVal value As Dictionary(Of String, UnitOperations.Auxiliary.PipingOps.Curve))
                m_curves = value
            End Set
        End Property

        Public Property Pout() As Double
            Get
                Return m_pout
            End Get
            Set(ByVal value As Double)
                m_pout = value
            End Set
        End Property

        Public Property CalcMode() As CalculationMode
            Get
                Return m_cmode
            End Get
            Set(ByVal value As CalculationMode)
                m_cmode = value
            End Set
        End Property

        Public Property FixOnDeltaP() As Boolean
            Get
                Return m_FixOnDeltaP
            End Get
            Set(ByVal value As Boolean)
                m_FixOnDeltaP = value
            End Set
        End Property

        Public Property IgnorePhase() As Boolean
            Get
                Return m_ignorephase
            End Get
            Set(ByVal value As Boolean)
                m_ignorephase = value
            End Set
        End Property

        Public Sub New()

        End Sub

        Public Sub New(ByVal name As String, ByVal description As String)

            MyBase.CreateNew()
            Me.ComponentName = name
            Me.ComponentDescription = description


            Me.CreateCurves()
        End Sub

        Public Property NPSH() As Nullable(Of Double)
            Get
                Return m_npsh
            End Get
            Set(ByVal value As Nullable(Of Double))
                m_npsh = value
            End Set
        End Property

        Public Property Eficiencia() As Nullable(Of Double)
            Get
                Return m_eta
            End Get
            Set(ByVal value As Nullable(Of Double))
                m_eta = value
            End Set
        End Property

        Public Property DeltaP() As Nullable(Of Double)
            Get
                Return m_dp
            End Get
            Set(ByVal value As Nullable(Of Double))
                m_dp = value
            End Set
        End Property

        Public Property DeltaT() As Nullable(Of Double)
            Get
                Return m_dt
            End Get
            Set(ByVal value As Nullable(Of Double))
                m_dt = value
            End Set
        End Property

        Public Property DeltaQ() As Nullable(Of Double)
            Get
                Return m_DQ
            End Get
            Set(ByVal value As Nullable(Of Double))
                m_DQ = value
            End Set
        End Property

        Sub CreateCurves()

            If Not Me.Curves.ContainsKey("NPSH") Then
                Me.Curves.Add("NPSH", New Curve(Guid.NewGuid().ToString, "NPSH", CurveType.NPSHr))
            End If
            If Not Me.Curves.ContainsKey("HEAD") Then
                Me.Curves.Add("HEAD", New Curve(Guid.NewGuid().ToString, "HEAD", CurveType.Head))
            End If
            If Not Me.Curves.ContainsKey("EFF") Then
                Me.Curves.Add("EFF", New Curve(Guid.NewGuid().ToString, "EFF", CurveType.Efficiency))
            End If
            If Not Me.Curves.ContainsKey("POWER") Then
                Me.Curves.Add("POWER", New Curve(Guid.NewGuid().ToString, "POWER", CurveType.Power))
            End If
            If Not Me.Curves.ContainsKey("SYSTEM") Then
                Me.Curves.Add("SYSTEM", New Curve(Guid.NewGuid().ToString, "SYSTEM", CurveType.SystemHead))
            End If

        End Sub

        Public Overrides Function Calculate(Optional ByVal args As Object = Nothing) As Integer

            Dim form As Interfaces.IFlowsheet = Me.FlowSheet

            If Not Me.GraphicObject.InputConnectors(1).IsAttached Then
                Throw New Exception(Me.FlowSheet.GetTranslatedString("NohcorrentedeEnergyFlow4"))
            ElseIf Not Me.GraphicObject.OutputConnectors(0).IsAttached Then
                Throw New Exception(Me.FlowSheet.GetTranslatedString("Verifiqueasconexesdo"))
            ElseIf Not Me.GraphicObject.InputConnectors(0).IsAttached Then
                Throw New Exception(Me.FlowSheet.GetTranslatedString("Verifiqueasconexesdo"))
            End If

            Me.PropertyPackage.CurrentMaterialStream = Me.FlowSheet.SimulationObjects(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name)

            Me.PropertyPackage.CurrentMaterialStream.Validate()

            Dim Ti, Pi, Hi, Wi, rho_li, qli, qvi, ei, ein, T2, P2, H2 As Double

            qvi = Me.FlowSheet.SimulationObjects(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name).Phases(2).Properties.volumetric_flow.GetValueOrDefault
            If qvi > 0 And Not Me.IgnorePhase Then Throw New Exception(Me.FlowSheet.GetTranslatedString("ExisteumaPhasevaporna"))

            Ti = Me.FlowSheet.SimulationObjects(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name).Phases(0).Properties.temperature.GetValueOrDefault
            Pi = Me.FlowSheet.SimulationObjects(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name).Phases(0).Properties.pressure.GetValueOrDefault
            rho_li = Me.FlowSheet.SimulationObjects(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name).Phases(0).Properties.density.GetValueOrDefault
            qli = Me.FlowSheet.SimulationObjects(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name).Phases(1).Properties.volumetric_flow.GetValueOrDefault
            Hi = Me.FlowSheet.SimulationObjects(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name).Phases(0).Properties.enthalpy.GetValueOrDefault
            Wi = Me.FlowSheet.SimulationObjects(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name).Phases(0).Properties.massflow.GetValueOrDefault
            ei = Hi * Wi
            ein = ei

            If DebugMode Then AppendDebugLine(String.Format("Property Package: {0}", Me.PropertyPackage.Tag))
            If DebugMode Then AppendDebugLine(String.Format("Flash Algorithm: {0}", Me.PropertyPackage.FlashBase.GetType.Name))
            If DebugMode Then AppendDebugLine(String.Format("Input variables: T = {0} K, P = {1} Pa, H = {2} kJ/kg, W = {3} kg/s, Liquid Flow = {4} m3/s", Ti, Pi, Hi, Wi, qli))

            If DebugMode Then AppendDebugLine("Calculation mode: " & CalcMode.ToString)

            Select Case Me.CalcMode

                Case CalculationMode.Curves

                    Dim cv As New SystemsOfUnits.Converter

                    Dim cnpsh, chead, ceff, cpower, csystem As Curve

                    If DebugMode Then AppendDebugLine(String.Format("Creating curves..."))

                    Me.CreateCurves()

                    cnpsh = Me.Curves("NPSH")
                    chead = Me.Curves("HEAD")
                    ceff = Me.Curves("EFF")
                    cpower = Me.Curves("POWER")
                    csystem = Me.Curves("SYSTEM")

                    Dim xhead, yhead, xnpsh, ynpsh, xeff, yeff, xpower, ypower, xsystem, ysystem As New ArrayList

                    Dim i As Integer

                    For i = 0 To chead.x.Count - 1
                        If Double.TryParse(chead.x(i), New Double) And Double.TryParse(chead.y(i), New Double) Then
                            xhead.Add(SystemsOfUnits.Converter.ConvertToSI(chead.xunit, chead.x(i)))
                            yhead.Add(SystemsOfUnits.Converter.ConvertToSI(chead.yunit, chead.y(i)))
                        End If
                    Next
                    For i = 0 To cnpsh.x.Count - 1
                        If Double.TryParse(cnpsh.x(i), New Double) And Double.TryParse(cnpsh.y(i), New Double) Then
                            xnpsh.Add(SystemsOfUnits.Converter.ConvertToSI(cnpsh.xunit, cnpsh.x(i)))
                            ynpsh.Add(SystemsOfUnits.Converter.ConvertToSI(cnpsh.yunit, cnpsh.y(i)))
                        End If
                    Next
                    For i = 0 To ceff.x.Count - 1
                        If Double.TryParse(ceff.x(i), New Double) And Double.TryParse(ceff.y(i), New Double) Then
                            xeff.Add(SystemsOfUnits.Converter.ConvertToSI(ceff.xunit, ceff.x(i)))
                            If ceff.yunit = "%" Then
                                yeff.Add(ceff.y(i) / 100)
                            Else
                                yeff.Add(ceff.y(i))
                            End If
                        End If
                    Next
                    For i = 0 To cpower.x.Count - 1
                        If Double.TryParse(cpower.x(i), New Double) And Double.TryParse(cpower.y(i), New Double) Then
                            xpower.Add(SystemsOfUnits.Converter.ConvertToSI(cpower.xunit, cpower.x(i)))
                            ypower.Add(SystemsOfUnits.Converter.ConvertToSI(cpower.yunit, cpower.y(i)))
                        End If
                    Next
                    For i = 0 To csystem.x.Count - 1
                        If Double.TryParse(csystem.x(i), New Double) And Double.TryParse(csystem.y(i), New Double) Then
                            xsystem.Add(SystemsOfUnits.Converter.ConvertToSI(csystem.xunit, csystem.x(i)))
                            ysystem.Add(SystemsOfUnits.Converter.ConvertToSI(csystem.yunit, csystem.y(i)))
                        End If
                    Next

                    Dim w() As Double

                    If DebugMode Then AppendDebugLine(String.Format("Getting operating point..."))

                    'get operating points
                    Dim head, npshr, eff, power, syshead As Double

                    'head
                    ReDim w(xhead.Count)
                    ratinterpolation.buildfloaterhormannrationalinterpolant(xhead.ToArray(GetType(Double)), xhead.Count, 0.5, w)
                    head = polinterpolation.barycentricinterpolation(xhead.ToArray(GetType(Double)), yhead.ToArray(GetType(Double)), w, xhead.Count, qli)

                    If DebugMode Then AppendDebugLine(String.Format("Head: {0} m", head))

                    Me.CurveHead = head

                    'npshr
                    If Me.Curves("NPSH").Enabled Then
                        ReDim w(xnpsh.Count)
                        ratinterpolation.buildfloaterhormannrationalinterpolant(xnpsh.ToArray(GetType(Double)), xnpsh.Count, 0.5, w)
                        npshr = polinterpolation.barycentricinterpolation(xnpsh.ToArray(GetType(Double)), ynpsh.ToArray(GetType(Double)), w, xnpsh.Count, qli)
                    Else
                        npshr = 0
                    End If

                    If DebugMode Then AppendDebugLine(String.Format("NPSHr: {0} m", npshr))

                    Me.CurveNPSHr = npshr

                    'efficiency
                    If Me.Curves("EFF").Enabled Then
                        ReDim w(xeff.Count)
                        ratinterpolation.buildfloaterhormannrationalinterpolant(xeff.ToArray(GetType(Double)), xeff.Count, 0.5, w)
                        eff = polinterpolation.barycentricinterpolation(xeff.ToArray(GetType(Double)), yeff.ToArray(GetType(Double)), w, xeff.Count, qli)
                    Else
                        eff = Me.Eficiencia.GetValueOrDefault / 100
                    End If

                    If DebugMode Then AppendDebugLine(String.Format("Efficiency: {0} %", eff * 100))

                    Me.CurveEff = eff * 100

                    'syshead
                    If Me.Curves("SYSTEM").Enabled Then
                        ReDim w(xsystem.Count)
                        ratinterpolation.buildfloaterhormannrationalinterpolant(xsystem.ToArray(GetType(Double)), xsystem.Count, 0.5, w)
                        syshead = polinterpolation.barycentricinterpolation(xsystem.ToArray(GetType(Double)), ysystem.ToArray(GetType(Double)), w, xsystem.Count, qli)
                    Else
                        syshead = head
                    End If

                    If DebugMode Then AppendDebugLine(String.Format("System Head: {0} m", syshead))

                    Me.CurveSysHead = syshead

                    'we need -> head, power, eff, to calculate P2, H2, T2

                    P2 = Pi + syshead * 9.81 * rho_li
                    CheckSpec(P2, True, "outlet pressure")

                    Me.DeltaP = P2 - Pi

                    If DebugMode Then AppendDebugLine(String.Format("Outlet Pressure: {0} Pa", P2))

                    Dim tmp As Object

                    'power
                    If Me.Curves("POWER").Enabled Then
                        ReDim w(xpower.Count)
                        ratinterpolation.buildfloaterhormannrationalinterpolant(xpower.ToArray(GetType(Double)), xpower.Count, 0.5, w)
                        power = polinterpolation.barycentricinterpolation(xpower.ToArray(GetType(Double)), ypower.ToArray(GetType(Double)), w, xpower.Count, qli)
                        H2 = Hi + power * eff / Wi
                    Else
                        power = Wi * 9.81 * syshead / eff / 1000
                        H2 = Hi + power * eff / Wi
                    End If

                    If DebugMode Then AppendDebugLine(String.Format("Power: {0} kW", power))

                    CheckSpec(power, True, "power")

                    Me.CurvePower = power

                    Me.DeltaQ = power

                    If DebugMode Then AppendDebugLine(String.Format("Doing a PH flash to calculate outlet temperature... P = {0} Pa, H = {1} kJ/[kg.K]", P2, H2))

                    tmp = Me.PropertyPackage.DW_CalcEquilibrio_ISOL(PropertyPackages.FlashSpec.P, PropertyPackages.FlashSpec.H, P2, H2, Ti)
                    T2 = tmp(2)

                    If DebugMode Then AppendDebugLine(String.Format("Calculated outlet temperature T2 = {0} K", T2))

                    CheckSpec(T2, True, "outlet temperature")

                    Me.DeltaT = T2 - Ti

                    Me.Eficiencia = eff * 100

                    Me.NPSH = (Pi - Me.PropertyPackage.DW_CalcPVAP_ISOL(Ti)) / (rho_li * 9.81)

                    Me.CurveFlow = qli

                    'Corrente de EnergyFlow - atualizar valor da potência (kJ/s)
                    With Me.FlowSheet.SimulationObjects(Me.GraphicObject.InputConnectors(1).AttachedConnector.AttachedFrom.Name)
                        .EnergyFlow = Me.DeltaQ.GetValueOrDefault
                        .GraphicObject.Calculated = True
                    End With

                Case CalculationMode.EnergyStream

                    Dim tmp As Object

                    'Corrente de EnergyFlow - pegar valor do DH
                    With Me.FlowSheet.SimulationObjects(Me.GraphicObject.InputConnectors(1).AttachedConnector.AttachedFrom.Name)
                        Me.DeltaQ = .EnergyFlow.GetValueOrDefault 'Wi * (H2 - Hi) / (Me.Eficiencia.GetValueOrDefault / 100)
                    End With

                    H2 = Hi + Me.DeltaQ.GetValueOrDefault * (Me.Eficiencia.GetValueOrDefault / 100) / Wi
                    CheckSpec(H2, False, "outlet enthalpy")

                    P2 = Pi + (H2 - Hi) * rho_li * 1000
                    CheckSpec(P2, True, "outlet pressure")

                    DeltaP = P2 - Pi

                    If DebugMode Then AppendDebugLine(String.Format("Doing a PH flash to calculate outlet temperature... P = {0} Pa, H = {1} kJ/[kg.K]", P2, H2))

                    tmp = Me.PropertyPackage.DW_CalcEquilibrio_ISOL(PropertyPackages.FlashSpec.P, PropertyPackages.FlashSpec.H, P2, H2, Ti)
                    T2 = tmp(2)
                    CheckSpec(T2, True, "outlet temperature")

                    If DebugMode Then AppendDebugLine(String.Format("Calculated outlet temperature T2 = {0} K", T2))

                    Me.DeltaT = T2 - Ti

                    Dim Pbub As Double '= Me.PropertyPackage.DW_CalcPVAP_ISOL(Ti)

                    If DebugMode Then AppendDebugLine(String.Format("Doing a bubble point flash to calculate NPSH... T = {0} K, VF = 0", Ti))

                    Try
                        Pbub = Me.PropertyPackage.DW_CalcEquilibrio_ISOL(PropertyPackages.FlashSpec.T, PropertyPackages.FlashSpec.VAP, Ti, 0.0#, Pi)(3)
                        If DebugMode Then AppendDebugLine(String.Format("Calculated bubble pressure: {0} Pa", Pbub))
                        Me.NPSH = (Pi - Pbub) / (rho_li * 9.81)
                    Catch ex As Exception
                        Me.NPSH = Double.PositiveInfinity
                    End Try

                Case CalculationMode.Power

                    Dim tmp As Object

                    'Corrente de EnergyFlow - pegar valor do DH
                    With Me.FlowSheet.SimulationObjects(Me.GraphicObject.InputConnectors(1).AttachedConnector.AttachedFrom.Name)
                        .EnergyFlow = Me.DeltaQ
                    End With

                    H2 = Hi + Me.DeltaQ.GetValueOrDefault * (Me.Eficiencia.GetValueOrDefault / 100) / Wi
                    CheckSpec(H2, False, "outlet enthalpy")

                    P2 = Pi + (H2 - Hi) * rho_li * 1000
                    CheckSpec(P2, True, "outlet pressure")

                    DeltaP = P2 - Pi

                    If DebugMode Then AppendDebugLine(String.Format("Doing a PH flash to calculate outlet temperature... P = {0} Pa, H = {1} kJ/[kg.K]", P2, H2))

                    tmp = Me.PropertyPackage.DW_CalcEquilibrio_ISOL(PropertyPackages.FlashSpec.P, PropertyPackages.FlashSpec.H, P2, H2, Ti)
                    T2 = tmp(2)
                    CheckSpec(T2, True, "outlet temperature")

                    If DebugMode Then AppendDebugLine(String.Format("Calculated outlet temperature T2 = {0} K", T2))

                    Me.DeltaT = T2 - Ti

                    If DebugMode Then AppendDebugLine(String.Format("Doing a bubble point flash to calculate NPSH... P = {0} Pa, VF = 0", P2))

                    Dim Pbub As Double '= Me.PropertyPackage.DW_CalcPVAP_ISOL(Ti)

                    Try
                        Pbub = Me.PropertyPackage.DW_CalcEquilibrio_ISOL(PropertyPackages.FlashSpec.T, PropertyPackages.FlashSpec.VAP, Ti, 0.0#, Pi)(3)
                        If DebugMode Then AppendDebugLine(String.Format("Calculated bubble pressure: {0} Pa", Pbub))
                        Me.NPSH = (Pi - Pbub) / (rho_li * 9.81)
                    Catch ex As Exception
                        Me.NPSH = Double.PositiveInfinity
                    End Try

                Case CalculationMode.Delta_P

                    qvi = Me.FlowSheet.SimulationObjects(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name).Phases(2).Properties.volumetric_flow.GetValueOrDefault.ToString

                    If qvi > 0 And Not Me.IgnorePhase Then Throw New Exception(Me.FlowSheet.GetTranslatedString("ExisteumaPhasevaporna"))

                    Me.PropertyPackage.CurrentMaterialStream = Me.FlowSheet.SimulationObjects(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name)
                    P2 = Pi + Me.DeltaP.GetValueOrDefault
                    CheckSpec(P2, True, "outlet pressure")

                    Me.DeltaQ = (P2 - Pi) / rho_li / 1000 / (Me.Eficiencia.GetValueOrDefault / 100) * Wi

                    H2 = Hi + Me.DeltaQ / Wi
                    CheckSpec(H2, False, "outlet enthalpy")

                    If DebugMode Then AppendDebugLine(String.Format("Doing a PH flash to calculate outlet temperature... P = {0} Pa, H = {1} kJ/[kg.K]", P2, H2))

                    Dim tmp = Me.PropertyPackage.DW_CalcEquilibrio_ISOL(PropertyPackages.FlashSpec.P, PropertyPackages.FlashSpec.H, P2, H2, 0.0#)
                    T2 = tmp(2)
                    CheckSpec(T2, True, "outlet temperature")

                    If DebugMode Then AppendDebugLine(String.Format("Calculated outlet temperature T2 = {0} K", T2))

                    Me.DeltaT = T2 - Ti

                    Dim Pbub As Double '= Me.PropertyPackage.DW_CalcPVAP_ISOL(Ti)

                    If DebugMode Then AppendDebugLine(String.Format("Doing a bubble point flash to calculate NPSH... P = {0} Pa, VF = 0", P2))

                    Try
                        Pbub = Me.PropertyPackage.DW_CalcEquilibrio_ISOL(PropertyPackages.FlashSpec.T, PropertyPackages.FlashSpec.VAP, Ti, 0.0#, Pi)(3)
                        If DebugMode Then AppendDebugLine(String.Format("Calculated bubble pressure: {0} Pa", Pbub))
                        Me.NPSH = (Pi - Pbub) / (rho_li * 9.81)
                    Catch ex As Exception
                        Me.NPSH = Double.PositiveInfinity
                    End Try

                    'Corrente de EnergyFlow - atualizar valor da potência (kJ/s)
                    With Me.FlowSheet.SimulationObjects(Me.GraphicObject.InputConnectors(1).AttachedConnector.AttachedFrom.Name)
                        .EnergyFlow = Me.DeltaQ.GetValueOrDefault
                        .GraphicObject.Calculated = True
                    End With

                Case CalculationMode.OutletPressure

                    Me.PropertyPackage.CurrentMaterialStream = Me.FlowSheet.SimulationObjects(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name)

                    P2 = Me.Pout
                    CheckSpec(P2, True, "outlet pressure")

                    Me.DeltaP = P2 - Pi

                    Me.DeltaQ = (P2 - Pi) / rho_li / 1000 / (Me.Eficiencia.GetValueOrDefault / 100) * Wi

                    H2 = Hi + Me.DeltaQ / Wi
                    CheckSpec(H2, False, "outlet enthalpy")

                    If DebugMode Then AppendDebugLine(String.Format("Doing a PH flash to calculate outlet temperature... P = {0} Pa, H = {1} kJ/[kg.K]", P2, H2))

                    Dim tmp = Me.PropertyPackage.DW_CalcEquilibrio_ISOL(PropertyPackages.FlashSpec.P, PropertyPackages.FlashSpec.H, P2, H2, Ti)
                    T2 = tmp(2)
                    CheckSpec(T2, True, "outlet temperature")

                    If DebugMode Then AppendDebugLine(String.Format("Calculated outlet temperature T2 = {0} K", T2))

                    Me.DeltaT = T2 - Ti

                    Dim Pbub As Double '= Me.PropertyPackage.DW_CalcPVAP_ISOL(Ti)

                    If DebugMode Then AppendDebugLine(String.Format("Doing a bubble point flash to calculate NPSH... P = {0} Pa, VF = 0", P2))

                    Try
                        Pbub = Me.PropertyPackage.DW_CalcEquilibrio_ISOL(PropertyPackages.FlashSpec.T, PropertyPackages.FlashSpec.VAP, Ti, 0.0#, Pi)(3)
                        If DebugMode Then AppendDebugLine(String.Format("Calculated bubble pressure: {0} Pa", Pbub))
                        Me.NPSH = (Pi - Pbub) / (rho_li * 9.81)
                    Catch ex As Exception
                        Me.NPSH = Double.PositiveInfinity
                    End Try

                    'Corrente de EnergyFlow - atualizar valor da potência (kJ/s)
                    With Me.FlowSheet.SimulationObjects(Me.GraphicObject.InputConnectors(1).AttachedConnector.AttachedFrom.Name)
                        .EnergyFlow = Me.DeltaQ.GetValueOrDefault
                        .GraphicObject.Calculated = True
                    End With

            End Select

            If Not DebugMode Then

                'Atribuir valores à corrente de matéria conectada à jusante
                Dim omstr As IMaterialStream = Me.FlowSheet.SimulationObjects(Me.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Name)
                With omstr
                    .Phases(0).Properties.temperature = T2
                    .Phases(0).Properties.pressure = P2
                    .Phases(0).Properties.enthalpy = H2
                    Dim comp As Interfaces.ICompound
                    Dim i As Integer = 0
                    For Each comp In .Phases(0).Compounds.Values
                        comp.MoleFraction = Me.FlowSheet.SimulationObjects(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name).Phases(0).Compounds(comp.Name).MoleFraction
                        comp.MassFraction = Me.FlowSheet.SimulationObjects(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name).Phases(0).Compounds(comp.Name).MassFraction
                        i += 1
                    Next
                    .Phases(0).Properties.massflow = Me.FlowSheet.SimulationObjects(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name).Phases(0).Properties.massflow.GetValueOrDefault
                    .SpecType = Interfaces.Enums.StreamSpec.Pressure_and_Enthalpy
                End With

                'Call function to calculate flowsheet
                With objargs
                    .Calculated = True
                    .Name = Me.Name
                    .Tag = Me.GraphicObject.Tag
                    .ObjectType = ObjectType.Pump
                End With

                form.CalculationQueue.Enqueue(objargs)

            Else

                AppendDebugLine("Calculation finished successfully.")

            End If

        End Function

        Public Overrides Function DeCalculate() As Integer

            Dim form As IFlowsheet = Me.FlowSheet

            If Me.GraphicObject.OutputConnectors(0).IsAttached Then

                'Zerar valores da corrente de matéria conectada a jusante
                With Me.FlowSheet.SimulationObjects(Me.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Name)
                    .Phases(0).Properties.temperature = Nothing
                    .Phases(0).Properties.pressure = Nothing
                    .Phases(0).Properties.molarfraction = 1
                    .Phases(0).Properties.massfraction = 1
                    .Phases(0).Properties.enthalpy = Nothing
                    Dim comp As Interfaces.ICompound
                    Dim i As Integer = 0
                    For Each comp In .Phases(0).Compounds.Values
                        comp.MoleFraction = 0
                        comp.MassFraction = 0
                        i += 1
                    Next
                    .Phases(0).Properties.massflow = Nothing
                    .Phases(0).Properties.molarflow = Nothing
                    .GraphicObject.Calculated = False
                End With

            End If

            'Corrente de EnergyFlow - atualizar valor da potência (kJ/s)
            If Me.GraphicObject.EnergyConnector.IsAttached Then
                With Me.FlowSheet.SimulationObjects(Me.GraphicObject.EnergyConnector.AttachedConnector.AttachedTo.Name)
                    .EnergyFlow = Nothing
                    .GraphicObject.Calculated = False
                End With
            End If

            'Call function to calculate flowsheet
            Dim objargs As New DWSIM.Extras.StatusChangeEventArgs
            With objargs
                .Calculated = False
                .Name = Me.Name
                .ObjectType = ObjectType.Pump
            End With

            form.CalculationQueue.Enqueue(objargs)

        End Function

        Public Overrides Sub PopulatePropertyGrid(ByVal pgrid As PropertyGridEx.PropertyGridEx, ByVal su As SystemsOfUnits.Units)

            Dim Conversor As New SystemsOfUnits.Converter

            With pgrid

                .PropertySort = PropertySort.Categorized
                .ShowCustomProperties = True
                .Item.Clear()

                MyBase.PopulatePropertyGrid(pgrid, su)

                Dim ent, saida, energ As String
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

                If Me.GraphicObject.InputConnectors(1).IsAttached = True Then
                    energ = Me.GraphicObject.InputConnectors(1).AttachedConnector.AttachedFrom.Tag
                Else
                    energ = ""
                End If

                .Item.Add(Me.FlowSheet.GetTranslatedString("Correntedeentrada"), ent, False, Me.FlowSheet.GetTranslatedString("Conexes1"), "", True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .CustomEditor = New DWSIM.Editors.Streams.UIInputMSSelector
                End With

                .Item.Add(Me.FlowSheet.GetTranslatedString("Correntedesada"), saida, False, Me.FlowSheet.GetTranslatedString("Conexes1"), "", True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .CustomEditor = New DWSIM.Editors.Streams.UIOutputMSSelector
                End With

                .Item.Add(Me.FlowSheet.GetTranslatedString("CorrentedeEnergia"), energ, False, Me.FlowSheet.GetTranslatedString("Conexes1"), "", True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .CustomEditor = New DWSIM.Editors.Streams.UIInputESSelector
                End With

                .Item.Add(Me.FlowSheet.GetTranslatedString("PumpCalcMode"), Me, "CalcMode", False, Me.FlowSheet.GetTranslatedString("Parmetrosdeclculo2"), "", True)
                .Item(.Item.Count - 1).Tag2 = "CalcMode"

                Select Case Me.CalcMode
                    Case CalculationMode.Curves
                        .Item.Add(Me.FlowSheet.GetTranslatedString("PumpSetupCurves"), Me, "Curves", False, Me.FlowSheet.GetTranslatedString("Parmetrosdeclculo2"), Me.FlowSheet.GetTranslatedString("Diferenadepressoentr"), True)
                        With .Item(.Item.Count - 1)
                            .DefaultValue = Nothing
                            .CustomEditor = New DWSIM.Editors.Pump.UIPumpCurvesEditor
                        End With
                        If Me.Curves.ContainsKey("EFF") Then
                            If Not Me.Curves("EFF").Enabled Then
                                .Item.Add(Me.FlowSheet.GetTranslatedString("Eficincia0100"), Me, "Eficiencia", False, Me.FlowSheet.GetTranslatedString("Parmetrosdeclculo2"), Me.FlowSheet.GetTranslatedString("Eficinciaglobaldabom"), True)
                                With .Item(.Item.Count - 1)
                                    .Tag2 = "PROP_PI_1"
                                    .DefaultValue = Nothing
                                    .DefaultType = GetType(Nullable(Of Double))
                                End With
                            End If
                        End If
                    Case CalculationMode.Delta_P
                        Dim valor = Format(SystemsOfUnits.Converter.ConvertFromSI(su.deltaP, Me.DeltaP.GetValueOrDefault), FlowSheet.Options.NumberFormat)
                        .Item.Add(FT("Delta P", su.deltaP), Double.Parse(valor), False, Me.FlowSheet.GetTranslatedString("Parmetrosdeclculo2"), Me.FlowSheet.GetTranslatedString("Diferenadepressoentr"), True)
                        With .Item(.Item.Count - 1)
                            .CustomTypeConverter = New System.ComponentModel.StringConverter
                            .Tag2 = "PROP_PI_0"
                            .Tag = New Object() {FlowSheet.Options.NumberFormat, su.deltaP, "DP"}
                            .CustomEditor = New DWSIM.Editors.Generic.UIUnitConverter
                        End With
                        .Item.Add(Me.FlowSheet.GetTranslatedString("Eficincia0100"), Me, "Eficiencia", False, Me.FlowSheet.GetTranslatedString("Parmetrosdeclculo2"), Me.FlowSheet.GetTranslatedString("Eficinciaglobaldabom"), True)
                        With .Item(.Item.Count - 1)
                            .CustomTypeConverter = New System.ComponentModel.StringConverter
                            .Tag2 = "PROP_PI_1"
                            .DefaultValue = Nothing
                            .DefaultType = GetType(Nullable(Of Double))
                        End With
                    Case CalculationMode.EnergyStream
                        .Item.Add(Me.FlowSheet.GetTranslatedString("Eficincia0100"), Me, "Eficiencia", False, Me.FlowSheet.GetTranslatedString("Parmetrosdeclculo2"), Me.FlowSheet.GetTranslatedString("Eficinciaglobaldabom"), True)
                        With .Item(.Item.Count - 1)
                            .CustomTypeConverter = New System.ComponentModel.StringConverter
                            .Tag2 = "PROP_PI_1"
                            .DefaultValue = Nothing
                            .DefaultType = GetType(Nullable(Of Double))
                        End With
                    Case CalculationMode.Power
                        Dim valor = Format(SystemsOfUnits.Converter.ConvertFromSI(su.heatflow, Me.DeltaQ.GetValueOrDefault), FlowSheet.Options.NumberFormat)
                        .Item.Add(FT(Me.FlowSheet.GetTranslatedString("EnergyFlownecessria"), su.heatflow), valor, False, Me.FlowSheet.GetTranslatedString("Parmetrosdeclculo2"), Me.FlowSheet.GetTranslatedString("Potnciarequeridapela"), True)
                        With .Item(.Item.Count - 1)
                            .CustomTypeConverter = New System.ComponentModel.StringConverter
                            .Tag2 = "PROP_PI_3"
                            .Tag = New Object() {FlowSheet.Options.NumberFormat, su.pressure, "E"}
                            .CustomEditor = New DWSIM.Editors.Generic.UIUnitConverter
                        End With
                    Case CalculationMode.OutletPressure
                        Dim valor = Format(SystemsOfUnits.Converter.ConvertFromSI(su.pressure, Me.Pout), FlowSheet.Options.NumberFormat)
                        .Item.Add(FT(Me.FlowSheet.GetTranslatedString("Pressoajusante"), su.pressure), valor, False, Me.FlowSheet.GetTranslatedString("Parmetrosdeclculo2"), Me.FlowSheet.GetTranslatedString("Diferenadepressoentr"), True)
                        With .Item(.Item.Count - 1)
                            .CustomTypeConverter = New System.ComponentModel.StringConverter
                            .Tag = New Object() {FlowSheet.Options.NumberFormat, su.pressure, "P"}
                            .CustomEditor = New DWSIM.Editors.Generic.UIUnitConverter
                        End With
                        .Item.Add(Me.FlowSheet.GetTranslatedString("Eficincia0100"), Me, "Eficiencia", False, Me.FlowSheet.GetTranslatedString("Parmetrosdeclculo2"), Me.FlowSheet.GetTranslatedString("Eficinciaglobaldabom"), True)
                        With .Item(.Item.Count - 1)
                            .Tag2 = "PROP_PI_1"
                            .DefaultValue = Nothing
                            .DefaultType = GetType(Nullable(Of Double))
                        End With
                End Select

                .Item.Add(Me.FlowSheet.GetTranslatedString("IgnorarVapornaEntrad"), Me, "IgnorePhase", False, Me.FlowSheet.GetTranslatedString("Parmetrosdeclculo2"), Me.FlowSheet.GetTranslatedString("SelecioLiquidrueparaign"), True)
                With .Item(.Item.Count - 1)
                    .DefaultType = GetType(Boolean)
                End With

                If Me.GraphicObject.Calculated Then
                    Select Case Me.CalcMode
                        Case CalculationMode.Curves
                            .Item.Add(Me.FlowSheet.GetTranslatedString("PumpViewCurves"), Me, "Curves", False, Me.FlowSheet.GetTranslatedString("Resultados3"), Me.FlowSheet.GetTranslatedString("Diferenadepressoentr"), True)
                            With .Item(.Item.Count - 1)
                                .DefaultValue = Nothing
                                .CustomEditor = New DWSIM.Editors.Pump.UIPumpCurvesEditor
                            End With
                            .Item.Add(FT(Me.FlowSheet.GetTranslatedString("PumpCurveHead"), su.head), Format(SystemsOfUnits.Converter.ConvertFromSI(su.head, Me.CurveHead), FlowSheet.Options.NumberFormat), True, Me.FlowSheet.GetTranslatedString("Resultados3"), Me.FlowSheet.GetTranslatedString("Diferenadetemperatur"), True)
                            .Item.Add(Me.FlowSheet.GetTranslatedString("PumpCurveEfficiency"), Format(Me.CurveEff, FlowSheet.Options.NumberFormat), True, Me.FlowSheet.GetTranslatedString("Resultados3"), Me.FlowSheet.GetTranslatedString("Diferenadetemperatur"), True)
                            .Item.Add(FT(Me.FlowSheet.GetTranslatedString("PumpCurvePower"), su.heatflow), Format(SystemsOfUnits.Converter.ConvertFromSI(su.heatflow, Me.CurvePower), FlowSheet.Options.NumberFormat), True, Me.FlowSheet.GetTranslatedString("Resultados3"), Me.FlowSheet.GetTranslatedString("Diferenadetemperatur"), True)
                            Dim valor = Format(SystemsOfUnits.Converter.ConvertFromSI(su.deltaP, Me.DeltaP.GetValueOrDefault), FlowSheet.Options.NumberFormat)
                            .Item.Add(FT("Delta P", su.deltaP), valor, True, Me.FlowSheet.GetTranslatedString("Resultados3"), Me.FlowSheet.GetTranslatedString("Diferenadepressoentr"), True)
                            With .Item(.Item.Count - 1)
                                .DefaultValue = Nothing
                                .DefaultType = GetType(Nullable(Of Double))
                            End With
                            .Item.Add(FT(Me.FlowSheet.GetTranslatedString("DeltaT2"), su.deltaT), Format(SystemsOfUnits.Converter.ConvertFromSI(su.deltaT, Me.DeltaT.GetValueOrDefault), FlowSheet.Options.NumberFormat), True, Me.FlowSheet.GetTranslatedString("Resultados3"), Me.FlowSheet.GetTranslatedString("Diferenadetemperatur"), True)
                            With .Item(.Item.Count - 1)
                                .DefaultValue = Nothing
                                .DefaultType = GetType(Nullable(Of Double))
                            End With
                            .Item.Add(FT(Me.FlowSheet.GetTranslatedString("PumpCurveNPSHr"), su.head), Format(SystemsOfUnits.Converter.ConvertFromSI(su.head, Me.CurveNPSHr), FlowSheet.Options.NumberFormat), True, Me.FlowSheet.GetTranslatedString("Resultados3"), Me.FlowSheet.GetTranslatedString("Diferenadetemperatur"), True)
                        Case CalculationMode.Delta_P
                            .Item.Add(FT(Me.FlowSheet.GetTranslatedString("DeltaT2"), su.deltaT), Format(SystemsOfUnits.Converter.ConvertFromSI(su.deltaT, Me.DeltaT.GetValueOrDefault), FlowSheet.Options.NumberFormat), True, Me.FlowSheet.GetTranslatedString("Resultados3"), Me.FlowSheet.GetTranslatedString("Diferenadetemperatur"), True)
                            With .Item(.Item.Count - 1)
                                .DefaultValue = Nothing
                                .DefaultType = GetType(Nullable(Of Double))
                            End With
                            .Item.Add(FT(Me.FlowSheet.GetTranslatedString("EnergyFlownecessria"), su.heatflow), Format(SystemsOfUnits.Converter.ConvertFromSI(su.heatflow, Me.DeltaQ.GetValueOrDefault), FlowSheet.Options.NumberFormat), True, Me.FlowSheet.GetTranslatedString("Resultados3"), Me.FlowSheet.GetTranslatedString("Potnciarequeridapela"), True)
                            With .Item(.Item.Count - 1)
                                .DefaultValue = Nothing
                                .DefaultType = GetType(Nullable(Of Double))
                            End With
                        Case CalculationMode.EnergyStream, CalculationMode.Power
                            Dim valor = Format(SystemsOfUnits.Converter.ConvertFromSI(su.deltaP, Me.DeltaP.GetValueOrDefault), FlowSheet.Options.NumberFormat)
                            .Item.Add(FT("Delta P", su.deltaP), valor, True, Me.FlowSheet.GetTranslatedString("Resultados3"), Me.FlowSheet.GetTranslatedString("Diferenadepressoentr"), True)
                            With .Item(.Item.Count - 1)
                                .DefaultValue = Nothing
                                .DefaultType = GetType(Nullable(Of Double))
                            End With
                            .Item.Add(FT(Me.FlowSheet.GetTranslatedString("DeltaT2"), su.deltaT), Format(SystemsOfUnits.Converter.ConvertFromSI(su.deltaT, Me.DeltaT.GetValueOrDefault), FlowSheet.Options.NumberFormat), True, Me.FlowSheet.GetTranslatedString("Resultados3"), Me.FlowSheet.GetTranslatedString("Diferenadetemperatur"), True)
                            With .Item(.Item.Count - 1)
                                .DefaultValue = Nothing
                                .DefaultType = GetType(Nullable(Of Double))
                            End With
                        Case CalculationMode.OutletPressure
                            Dim valor = Format(SystemsOfUnits.Converter.ConvertFromSI(su.deltaP, Me.DeltaP.GetValueOrDefault), FlowSheet.Options.NumberFormat)
                            .Item.Add(FT("Delta P", su.deltaP), valor, True, Me.FlowSheet.GetTranslatedString("Resultados3"), Me.FlowSheet.GetTranslatedString("Diferenadepressoentr"), True)
                            With .Item(.Item.Count - 1)
                                .DefaultValue = Nothing
                                .DefaultType = GetType(Nullable(Of Double))
                            End With
                            .Item.Add(FT(Me.FlowSheet.GetTranslatedString("DeltaT2"), su.deltaT), Format(SystemsOfUnits.Converter.ConvertFromSI(su.deltaT, Me.DeltaT.GetValueOrDefault), FlowSheet.Options.NumberFormat), True, Me.FlowSheet.GetTranslatedString("Resultados3"), Me.FlowSheet.GetTranslatedString("Diferenadetemperatur"), True)
                            With .Item(.Item.Count - 1)
                                .DefaultValue = Nothing
                                .DefaultType = GetType(Nullable(Of Double))
                            End With
                            .Item.Add(FT(Me.FlowSheet.GetTranslatedString("EnergyFlownecessria"), su.heatflow), Format(SystemsOfUnits.Converter.ConvertFromSI(su.heatflow, Me.DeltaQ.GetValueOrDefault), FlowSheet.Options.NumberFormat), True, Me.FlowSheet.GetTranslatedString("Resultados3"), Me.FlowSheet.GetTranslatedString("Potnciarequeridapela"), True)
                            With .Item(.Item.Count - 1)
                                .DefaultValue = Nothing
                                .DefaultType = GetType(Nullable(Of Double))
                            End With
                    End Select
                End If
                .Item.Add(FT(Me.FlowSheet.GetTranslatedString("PumpNPSHd"), su.head), Format(SystemsOfUnits.Converter.ConvertFromSI(su.head, Me.NPSH.GetValueOrDefault), FlowSheet.Options.NumberFormat), True, Me.FlowSheet.GetTranslatedString("Resultados3"), Me.FlowSheet.GetTranslatedString("Potnciarequeridapela"), True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .DefaultType = GetType(Nullable(Of Double))
                End With

                If Me.GraphicObject.Calculated = False Then
                    .Item.Add(Me.FlowSheet.GetTranslatedString("Mensagemdeerro"), Me, "ErrorMessage", True, Me.FlowSheet.GetTranslatedString("Miscelnea4"), Me.FlowSheet.GetTranslatedString("Mensagemretornadaqua"), True)
                    With .Item(.Item.Count - 1)
                        .DefaultType = GetType(System.String)
                    End With
                End If

            End With

        End Sub

        Public Overrides Function GetPropertyValue(ByVal prop As String, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As Object

            If su Is Nothing Then su = New SystemsOfUnits.SI
            Dim cv As New SystemsOfUnits.Converter
            Dim value As Double = 0
            Dim propidx As Integer = Convert.ToInt32(prop.Split("_")(2))

            Select Case propidx

                Case 0
                    'PROP_PU_0	Pressure Increase (Head)
                    value = SystemsOfUnits.Converter.ConvertFromSI(su.deltaP, Me.DeltaP.GetValueOrDefault)
                Case 1
                    'PROP_PU_1(Efficiency)
                    value = Me.Eficiencia.GetValueOrDefault
                Case 2
                    'PROP_PU_2(Delta - T)
                    value = SystemsOfUnits.Converter.ConvertFromSI(su.deltaT, Me.DeltaT.GetValueOrDefault)
                Case 3
                    'PROP_PU_3	Power Required
                    value = SystemsOfUnits.Converter.ConvertFromSI(su.heatflow, Me.DeltaQ.GetValueOrDefault)

            End Select

            Return value


        End Function

        Public Overloads Overrides Function GetProperties(ByVal proptype As Interfaces.Enums.PropertyType) As String()
            Dim i As Integer = 0
            Dim proplist As New ArrayList
            Select Case proptype
                Case PropertyType.RO
                    For i = 2 To 2
                        proplist.Add("PROP_PU_" + CStr(i))
                    Next
                Case PropertyType.RW
                    For i = 0 To 3
                        proplist.Add("PROP_PU_" + CStr(i))
                    Next
                Case PropertyType.WR
                    For i = 0 To 3
                        proplist.Add("PROP_PU_" + CStr(i))
                    Next
                Case PropertyType.ALL
                    For i = 0 To 3
                        proplist.Add("PROP_PU_" + CStr(i))
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
                    'PROP_PU_0	Pressure Increase (Head)
                    Me.DeltaP = SystemsOfUnits.Converter.ConvertToSI(su.deltaP, propval)
                Case 1
                    'PROP_PU_1(Efficiency)
                    Me.Eficiencia = propval
                Case 3
                    Me.DeltaQ = SystemsOfUnits.Converter.ConvertToSI(su.heatflow, propval)
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
                    'PROP_PU_0	Pressure Increase (Head)
                    value = su.deltaP
                Case 1
                    'PROP_PU_1(Efficiency)
                    value = ""
                Case 2
                    'PROP_PU_2(Delta - T)
                    value = su.deltaT
                Case 3
                    'PROP_PU_3	Power Required
                    value = su.heatflow

            End Select

            Return value

        End Function

    End Class

End Namespace

