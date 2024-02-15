'    CoolProp Property Package
'    Copyright 2014 Daniel Wagner O. de Medeiros
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
Imports System.Linq
Imports System.Reflection
Imports DWSIM.Interfaces.Enums
Imports DWSIM.MathOps.MathEx

Namespace PropertyPackages


    <System.Runtime.InteropServices.Guid(CoolPropIncompressibleMixturePropertyPackage.ClassId)>
    <System.Serializable()> Public Class CoolPropIncompressibleMixturePropertyPackage

        Inherits PropertyPackages.PropertyPackage

        Public Shadows Const ClassId As String = "1F5B0263-E936-40d5-BA5B-FFAB11595E54"

        Public Property SoluteName As String = "LiBr"

        Public Property SoluteCompound As String = "Lithium Bromide"

        Public Property SolventCompound As String = "Water"

        Public SolutionDataList As New Dictionary(Of String, SolutionData)

        <System.Serializable()> Public Class SolutionData
            Public Name As String
            Public Description As String
            Public Tmin, Tmax, Tbase, xmin, xmax As Double
        End Class

        Public Sub New(ByVal comode As Boolean)
            MyBase.New(comode)
            ReadData()
        End Sub

        Public Sub New()
            MyBase.New()
            Me.IsConfigurable = True
            Me._packagetype = PropertyPackages.PackageType.Miscelaneous
            ReadData()

            With PropertyMethodsInfo
                .Vapor_Fugacity = "Ideal"
                .Vapor_Thermal_Conductivity = "CoolProp"
                .Vapor_Viscosity = "CoolProp"
                .Vapor_Enthalpy_Entropy_CpCv = "CoolProp"
                .Vapor_Density = "CoolProp"
                .Liquid_Fugacity = "Vapor Pressure"
                .Liquid_Enthalpy_Entropy_CpCv = "CoolProp"
                .Liquid_ThermalConductivity = "CoolProp"
                .Liquid_Viscosity = "CoolProp"
            End With

        End Sub

        Public Overrides Sub DisplayEditingForm()
            Dim f As New FormConfigCoolPropIncompMixture
            f.pp = Me
            f.ShowDialog()
        End Sub

        Public Overrides Function GetEditingForm() As Form

            Return New FormConfigCoolPropIncompMixture() With {.pp = Me}

        End Function

        Public Overrides Function SaveData() As List(Of XElement)
            Dim elements = XMLSerializer.XMLSerializer.Serialize(Me)
            elements.AddRange(MyBase.SaveData)
            Return elements
        End Function

        Public Overrides Function LoadData(data As List(Of XElement)) As Boolean
            XMLSerializer.XMLSerializer.Deserialize(Me, data)
            MyBase.LoadData(data)
        End Function

        Sub ReadData()
            Dim contents As String = ""
            Dim ic = Globalization.CultureInfo.InvariantCulture
            Using filestr As Stream = Assembly.GetAssembly(Me.GetType).GetManifestResourceStream("DWSIM.Thermodynamics.CoolPropIncompMixtures.txt")
                Using t As New StreamReader(filestr)
                    contents = t.ReadToEnd()
                End Using
            End Using
            For Each l As String In contents.Split(New Char() {vbLf, vbCr, vbCrLf})
                If l <> "" Then SolutionDataList.Add(l.Split(vbTab)(0), New SolutionData With {.Name = l.Split(vbTab)(0), .Description = l.Split(vbTab)(1),
                                                                .Tmin = Double.Parse(l.Split(vbTab)(3), ic) + 273.15, .Tmax = Double.Parse(l.Split(vbTab)(4), ic) + 273.15,
                                                                .Tbase = Double.Parse(l.Split(vbTab)(5), ic), .xmin = Double.Parse(l.Split(vbTab)(6), ic),
                                                                .xmax = Double.Parse(l.Split(vbTab)(7), ic)})
            Next
        End Sub

        Function GetCoolPropName(massfraction As Double) As String
            Return "INCOMP::" & SoluteName & "[" & massfraction.ToString("N4", Globalization.CultureInfo.InvariantCulture) & "]"
        End Function

        Function GetXmax() As Double
            Return SolutionDataList(SoluteName).xmax
        End Function

        Private Sub SetCPDebugLevel()

            If GlobalSettings.Settings.InspectorEnabled Then
                CoolProp.set_debug_level(100000)
            Else
                CoolProp.set_debug_level(0)
            End If

        End Sub

        Public Overrides ReadOnly Property FlashBase() As Auxiliary.FlashAlgorithms.FlashAlgorithm
            Get
                FlashAlgorithm = New Auxiliary.FlashAlgorithms.CoolPropIncompressibleMixture
                Return FlashAlgorithm
            End Get
        End Property

#Region "    DWSIM Functions"

        Private Sub WriteWarningMessage(message As String)
            Select Case Settings.DebugLevel
                Case 0
                    'do nothing
                Case Else
                    Console.WriteLine(message)
            End Select
        End Sub

        Public Overrides Function AUX_IS_SINGLECOMP(Phase As Phase) As Boolean
            Return True
        End Function

        Public Overrides Function AUX_CONDTG(T As Double, P As Double) As Double

            Dim Tb As Double
            Dim Tmin = CoolProp.Props1SI(SolventCompound, "TMIN")
            Dim Tmax = CoolProp.Props1SI(SolventCompound, "TMAX")
            Dim Pmin = CoolProp.Props1SI(SolventCompound, "PMIN")
            Dim Pmax = CoolProp.Props1SI(SolventCompound, "PMAX")
            Dim Tc = CoolProp.Props1SI(SolventCompound, "TCRIT")
            If T < Tc Then Tb = CoolProp.PropsSI("T", "P", P, "Q", 1, SolventCompound) Else Tb = Tc
            If T > Tb And Math.Abs(T - Tb) > 0.01 Then
                Return CoolProp.PropsSI("L", "T", T, "P", P, SolventCompound)
            Else
                Dim x1, x2, x3, x4, x5, p1, p2, p3, p4, p5 As Double
                x1 = Tb + (Tmax - Tb) * 0.2
                x2 = Tb + (Tmax - Tb) * 0.4
                x3 = Tb + (Tmax - Tb) * 0.6
                x4 = Tb + (Tmax - Tb) * 0.8
                x5 = Tb + (Tmax - Tb) * 0.9
                p1 = CoolProp.PropsSI("L", "T", x1, "P", P, SolventCompound)
                p2 = CoolProp.PropsSI("L", "T", x2, "P", P, SolventCompound)
                p3 = CoolProp.PropsSI("L", "T", x3, "P", P, SolventCompound)
                p4 = CoolProp.PropsSI("L", "T", x4, "P", P, SolventCompound)
                p5 = CoolProp.PropsSI("L", "T", x5, "P", P, SolventCompound)
                Return MathNet.Numerics.Interpolate.Linear(New Double() {x1, x2, x3, x4, x5}, New Double() {p1, p2, p3, p4, p5}).Interpolate(T)
            End If

        End Function

        Public Function AUX_PVAPi2(x As Double, T As Double) As Double

            Return CoolProp.PropsSI("P", "T", T, "Q", 0, GetCoolPropName(x))

        End Function

        Public Function AUX_TSAT(P As Double, x As Double) As Double

            Dim Tmin = GetTmin(x)
            Dim Tmax = CoolProp.Props1SI(GetCoolPropName(x), "TMAX")
            Dim xmax = SolutionDataList(SoluteName).xmax
            Dim xmin = SolutionDataList(SoluteName).xmin

            Dim bs As New MathEx.BrentOpt.BrentMinimize
            bs.DefineFuncDelegate(Function(t)
                                      Dim psat As Double
                                      If x < xmax Then
                                          psat = CoolProp.PropsSI("P", "T", t, "Q", 0, GetCoolPropName(x))
                                          If psat = 0.0 Then
                                              Throw New Exception(String.Format("Error calculating saturation pressure at {0} K for {1} - CoolProp returned Psat = 0 Pa", t, GetCoolPropName(x)))
                                          End If
                                      Else
                                          Dim x1, x2, x3, x4, x5, p1, p2, p3, p4, p5 As Double
                                          x1 = xmin + (xmax - xmin) * 0.2
                                          x2 = xmin + (xmax - xmin) * 0.4
                                          x3 = xmin + (xmax - xmin) * 0.6
                                          x4 = xmin + (xmax - xmin) * 0.8
                                          x5 = xmin + (xmax - xmin) * 0.9
                                          p1 = CoolProp.PropsSI("P", "T", t, "Q", 0, GetCoolPropName(x1))
                                          p2 = CoolProp.PropsSI("P", "T", t, "Q", 0, GetCoolPropName(x2))
                                          p3 = CoolProp.PropsSI("P", "T", t, "Q", 0, GetCoolPropName(x3))
                                          p4 = CoolProp.PropsSI("P", "T", t, "Q", 0, GetCoolPropName(x4))
                                          p5 = CoolProp.PropsSI("P", "T", t, "Q", 0, GetCoolPropName(x5))
                                          psat = MathNet.Numerics.Interpolate.Linear(New Double() {x1, x2, x3, x4, x5}, New Double() {p1, p2, p3, p4, p5}).Interpolate(x)
                                      End If
                                      Return (P - psat) ^ 2
                                  End Function)

            Dim Tsat As Double = 0.0
            Dim fval As Double

            fval = bs.brentoptimize(Tmin, Tmax, 0.1, Tsat)

            Return Tsat

        End Function

        Public Function AUX_CONDTL2(T As Double, P As Double, phaseid As Integer) As Double

            Dim x = CurrentMaterialStream.Phases(phaseid).Compounds(SoluteCompound).MassFraction.GetValueOrDefault

            Try
                Return CoolProp.PropsSI("L", "T", T, "P", P, GetCoolPropName(x))
            Catch ex As Exception
                Return 0.0
            End Try

        End Function

        Public Function AUX_LIQDENS2(T As Double, P As Double) As Double

            Dim x = CurrentMaterialStream.Phases(3).Compounds(SoluteCompound).MassFraction.GetValueOrDefault

            Try
                Return CoolProp.PropsSI("D", "T", T, "P", P, GetCoolPropName(x))
            Catch ex As Exception
                Return 0.0
            End Try

        End Function

        Public Overrides Function AUX_SURFTM(T As Double) As Double

            Try
                Dim x = CurrentMaterialStream.Phases(3).Compounds(SoluteCompound).MassFraction.GetValueOrDefault
                Return CoolProp.PropsSI("I", "T", T, "P", 101325, GetCoolPropName(x))
            Catch ex As Exception
                Return 0.0
            End Try

        End Function

        Public Function AUX_VAPVISCMIX(T As Double, P As Double, MM As Double) As Double

            Dim Tb As Double
            Dim Tmin = CoolProp.Props1SI(SolventCompound, "TMIN")
            Dim Tmax = CoolProp.Props1SI(SolventCompound, "TMAX")
            Dim Pmin = CoolProp.Props1SI(SolventCompound, "PMIN")
            Dim Pmax = CoolProp.Props1SI(SolventCompound, "PMAX")
            Dim Tc = CoolProp.Props1SI(SolventCompound, "TCRIT")
            If T < Tc Then Tb = CoolProp.PropsSI("V", "P", P, "Q", 1, SolventCompound) Else Tb = Tc
            If T > Tb And Math.Abs(T - Tb) > 0.01 Then
                Return CoolProp.PropsSI("V", "T", T, "P", P, SolventCompound)
            Else
                Dim x1, x2, x3, x4, x5, p1, p2, p3, p4, p5 As Double
                x1 = Tb + (Tmax - Tb) * 0.2
                x2 = Tb + (Tmax - Tb) * 0.4
                x3 = Tb + (Tmax - Tb) * 0.6
                x4 = Tb + (Tmax - Tb) * 0.8
                x5 = Tb + (Tmax - Tb) * 0.9
                p1 = CoolProp.PropsSI("V", "T", x1, "P", P, SolventCompound)
                p2 = CoolProp.PropsSI("V", "T", x2, "P", P, SolventCompound)
                p3 = CoolProp.PropsSI("V", "T", x3, "P", P, SolventCompound)
                p4 = CoolProp.PropsSI("V", "T", x4, "P", P, SolventCompound)
                p5 = CoolProp.PropsSI("V", "T", x5, "P", P, SolventCompound)
                Return MathNet.Numerics.Interpolate.Linear(New Double() {x1, x2, x3, x4, x5}, New Double() {p1, p2, p3, p4, p5}).Interpolate(T)
            End If

        End Function

        Public Function AUX_LIQVISCMIX(T As Double, P As Double, phaseid As Integer) As Double

            Dim x = CurrentMaterialStream.Phases(phaseid).Compounds(SoluteCompound).MassFraction.GetValueOrDefault

            Try
                Return CoolProp.PropsSI("V", "T", T, "P", P, GetCoolPropName(x))
            Catch ex As Exception
                Return 0.0
            End Try

        End Function

        Public Overrides Function AUX_VAPDENS(ByVal T As Double, ByVal P As Double) As Double

            Dim Tb As Double
            Dim Tmin = CoolProp.Props1SI(SolventCompound, "TMIN")
            Dim Tmax = CoolProp.Props1SI(SolventCompound, "TMAX")
            Dim Pmin = CoolProp.Props1SI(SolventCompound, "PMIN")
            Dim Pmax = CoolProp.Props1SI(SolventCompound, "PMAX")
            Dim Tc = CoolProp.Props1SI(SolventCompound, "TCRIT")
            If T < Tc Then Tb = CoolProp.PropsSI("T", "P", P, "Q", 1, SolventCompound) Else Tb = Tc
            If T > Tb And Math.Abs(T - Tb) > 0.01 Then
                Return CoolProp.PropsSI("D", "T", T, "P", P, SolventCompound)
            Else
                Dim x1, x2, x3, x4, x5, p1, p2, p3, p4, p5 As Double
                x1 = Tb + (Tmax - Tb) * 0.2
                x2 = Tb + (Tmax - Tb) * 0.4
                x3 = Tb + (Tmax - Tb) * 0.6
                x4 = Tb + (Tmax - Tb) * 0.8
                x5 = Tb + (Tmax - Tb) * 0.9
                p1 = CoolProp.PropsSI("D", "T", x1, "P", P, SolventCompound)
                p2 = CoolProp.PropsSI("D", "T", x2, "P", P, SolventCompound)
                p3 = CoolProp.PropsSI("D", "T", x3, "P", P, SolventCompound)
                p4 = CoolProp.PropsSI("D", "T", x4, "P", P, SolventCompound)
                p5 = CoolProp.PropsSI("D", "T", x5, "P", P, SolventCompound)
                Return MathNet.Numerics.Interpolate.Linear(New Double() {x1, x2, x3, x4, x5}, New Double() {p1, p2, p3, p4, p5}).Interpolate(T)
            End If

        End Function

        Public Overrides Sub DW_CalcCompPartialVolume(ByVal phase As Phase, ByVal T As Double, ByVal P As Double)

            For Each subst As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                subst.PartialVolume = 0.0#
            Next

        End Sub

        Public Overrides Function DW_CalcCp_ISOL(ByVal Phase1 As Phase, ByVal T As Double, ByVal P As Double) As Double

            Dim Vz = CurrentMaterialStream.GetPhaseComposition(RET_PHASEID(Phase1))

            Dim H1, H2 As Double

            Select Case Phase1
                Case Phase.Aqueous, Phase.Liquid, Phase.Liquid1, Phase.Liquid2, Phase.Liquid3
                    H1 = DW_CalcEnthalpy(Vz, T, P, State.Liquid)
                    H2 = DW_CalcEnthalpy(Vz, T + 0.001, P, State.Liquid)
                Case Phase.Vapor
                    H1 = DW_CalcEnthalpy(Vz, T, P, State.Vapor)
                    H2 = DW_CalcEnthalpy(Vz, T + 0.001, P, State.Vapor)
                Case Phase.Solid
                    H1 = DW_CalcEnthalpy(Vz, T, P, State.Solid)
                    H2 = DW_CalcEnthalpy(Vz, T + 0.001, P, State.Solid)
            End Select

            Return (H2 - H1) / 0.001

        End Function

        Public Overrides Function DW_CalcCv_ISOL(ByVal Phase1 As Phase, ByVal T As Double, ByVal P As Double) As Double

            Return DW_CalcCp_ISOL(Phase1, T, P)

        End Function

        Public Overrides Function DW_CalcEnthalpy(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double

            Dim Vxw = AUX_CONVERT_MOL_TO_MASS(Vx)
            Dim x = Vxw(Array.IndexOf(RET_VNAMES(), SoluteCompound))

            Dim Tmin = GetTmin(x)
            Dim Tmax = CoolProp.Props1SI(GetCoolPropName(x), "TMAX")
            Dim xmax = SolutionDataList(SoluteName).xmax
            Dim xmin = SolutionDataList(SoluteName).xmin

            If st = State.Liquid Then
                If x < xmax Then
                    Try
                        Return CoolProp.PropsSI("H", "T", T, "P", P, GetCoolPropName(x)) / 1000
                    Catch ex As Exception
                        Dim Tsat = AUX_TSAT(P, x)
                        Dim x1, x2, x3, x4, x5, p1, p2, p3, p4, p5 As Double
                        x1 = Tmin + (Tsat - Tmin) * 0.9
                        x2 = Tmin + (Tsat - Tmin) * 0.8
                        x3 = Tmin + (Tsat - Tmin) * 0.7
                        x4 = Tmin + (Tsat - Tmin) * 0.6
                        x5 = Tmin + (Tsat - Tmin) * 0.5
                        p1 = CoolProp.PropsSI("H", "T", x1, "P", P, GetCoolPropName(x)) / 1000
                        p2 = CoolProp.PropsSI("H", "T", x2, "P", P, GetCoolPropName(x)) / 1000
                        p3 = CoolProp.PropsSI("H", "T", x3, "P", P, GetCoolPropName(x)) / 1000
                        p4 = CoolProp.PropsSI("H", "T", x4, "P", P, GetCoolPropName(x)) / 1000
                        p5 = CoolProp.PropsSI("H", "T", x5, "P", P, GetCoolPropName(x)) / 1000
                        Return MathNet.Numerics.Interpolate.Linear(New Double() {x1, x2, x3, x4, x5}, New Double() {p1, p2, p3, p4, p5}).Interpolate(T)
                    End Try
                Else
                    Try
                        Dim x1, x2, x3, x4, x5, p1, p2, p3, p4, p5 As Double
                        x1 = xmin + (xmax - xmin) * 0.2
                        x2 = xmin + (xmax - xmin) * 0.4
                        x3 = xmin + (xmax - xmin) * 0.6
                        x4 = xmin + (xmax - xmin) * 0.8
                        x5 = xmin + (xmax - xmin) * 0.9
                        p1 = CoolProp.PropsSI("H", "T", T, "P", P, GetCoolPropName(x1)) / 1000
                        p2 = CoolProp.PropsSI("H", "T", T, "P", P, GetCoolPropName(x2)) / 1000
                        p3 = CoolProp.PropsSI("H", "T", T, "P", P, GetCoolPropName(x3)) / 1000
                        p4 = CoolProp.PropsSI("H", "T", T, "P", P, GetCoolPropName(x4)) / 1000
                        p5 = CoolProp.PropsSI("H", "T", T, "P", P, GetCoolPropName(x5)) / 1000
                        Return MathNet.Numerics.Interpolate.Linear(New Double() {x1, x2, x3, x4, x5}, New Double() {p1, p2, p3, p4, p5}).Interpolate(x)
                    Catch ex As Exception
                        Dim Tsat = AUX_TSAT(P, x)
                        Dim x1, x2, x3, x4, x5, p1, p2, p3, p4, p5 As Double
                        x1 = Tmin + (Tsat - Tmin) * 0.9
                        x2 = Tmin + (Tsat - Tmin) * 0.8
                        x3 = Tmin + (Tsat - Tmin) * 0.7
                        x4 = Tmin + (Tsat - Tmin) * 0.6
                        x5 = Tmin + (Tsat - Tmin) * 0.5
                        p1 = CoolProp.PropsSI("H", "T", x1, "P", P, GetCoolPropName(xmax)) / 1000
                        p2 = CoolProp.PropsSI("H", "T", x2, "P", P, GetCoolPropName(xmax)) / 1000
                        p3 = CoolProp.PropsSI("H", "T", x3, "P", P, GetCoolPropName(xmax)) / 1000
                        p4 = CoolProp.PropsSI("H", "T", x4, "P", P, GetCoolPropName(xmax)) / 1000
                        p5 = CoolProp.PropsSI("H", "T", x5, "P", P, GetCoolPropName(xmax)) / 1000
                        Return MathNet.Numerics.Interpolate.Linear(New Double() {x1, x2, x3, x4, x5}, New Double() {p1, p2, p3, p4, p5}).Interpolate(T)
                    End Try
                End If
            Else
                Dim Tsat = CoolProp.PropsSI("T", "P", P, "Q", 1.0, SolventCompound)
                Dim Hb As Double = DW_CalcEnthalpy(Vx, T, P, State.Liquid)
                Dim Hvap As Double = (CoolProp.PropsSI("H", "P", P, "Q", 1.0, SolventCompound) - CoolProp.PropsSI("H", "P", P, "Q", 0.0, SolventCompound)) / 1000
                Return Hb + Hvap
            End If

        End Function

        Public Overrides Function DW_CalcEnthalpyDeparture(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double

            Return DW_CalcEnthalpy(Vx, T, P, st)

        End Function

        Public Overrides Function DW_CalcEntropy(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double

            Dim Vxw = AUX_CONVERT_MOL_TO_MASS(Vx)
            Dim x = Vxw(Array.IndexOf(RET_VNAMES(), SoluteCompound))

            Dim Tmin = GetTmin(x)
            Dim Tmax = CoolProp.Props1SI(GetCoolPropName(x), "TMAX")

            If st = State.Liquid Then
                Try
                    Return CoolProp.PropsSI("S", "T", T, "P", P, GetCoolPropName(x)) / 1000
                Catch ex As Exception
                    Dim Tsat = AUX_TSAT(P, x)
                    Dim x1, x2, x3, x4, x5, p1, p2, p3, p4, p5 As Double
                    x1 = Tmin + (Tsat - Tmin) * 0.9
                    x2 = Tmin + (Tsat - Tmin) * 0.8
                    x3 = Tmin + (Tsat - Tmin) * 0.7
                    x4 = Tmin + (Tsat - Tmin) * 0.6
                    x5 = Tmin + (Tsat - Tmin) * 0.5
                    p1 = CoolProp.PropsSI("S", "T", x1, "P", P, GetCoolPropName(x)) / 1000
                    p2 = CoolProp.PropsSI("S", "T", x2, "P", P, GetCoolPropName(x)) / 1000
                    p3 = CoolProp.PropsSI("S", "T", x3, "P", P, GetCoolPropName(x)) / 1000
                    p4 = CoolProp.PropsSI("S", "T", x4, "P", P, GetCoolPropName(x)) / 1000
                    p5 = CoolProp.PropsSI("S", "T", x5, "P", P, GetCoolPropName(x)) / 1000
                    Return MathNet.Numerics.Interpolate.Linear(New Double() {x1, x2, x3, x4, x5}, New Double() {p1, p2, p3, p4, p5}).Interpolate(T)
                End Try
            Else
                Dim Tsat = CoolProp.PropsSI("T", "P", P, "Q", 1.0, SolventCompound)
                Dim Sb As Double = DW_CalcEntropy(Vx, T, P, State.Liquid)
                Dim Svap As Double = (CoolProp.PropsSI("S", "P", P, "Q", 1.0, SolventCompound) - CoolProp.PropsSI("S", "P", P, "Q", 0.0, SolventCompound)) / 1000
                Return Sb + Svap
            End If

        End Function

        Public Overrides Function DW_CalcEntropyDeparture(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double

            Return DW_CalcEntropy(Vx, T, P, st)

        End Function

        Public Overrides Function DW_CalcFugCoeff(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double()

            Return RET_UnitaryVector()

        End Function

        Public Overrides Function DW_CalcK_ISOL(ByVal Phase1 As Phase, ByVal T As Double, ByVal P As Double) As Double

            Return 0.0#

        End Function

        Public Overrides Function DW_CalcMassaEspecifica_ISOL(ByVal Phase1 As Phase, ByVal T As Double, ByVal P As Double, Optional ByVal Pvp As Double = 0.0) As Double

            Return 0.0#

        End Function

        Public Overrides Function DW_CalcMM_ISOL(ByVal Phase1 As Phase, ByVal T As Double, ByVal P As Double) As Double

            Return Me.AUX_MMM(Phase1)

        End Function

        Public Overrides Sub DW_CalcPhaseProps(ByVal Phase As Phase)

            SetCPDebugLevel()

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "DW_CalcPhaseProps", ComponentName & String.Format(" (Phase Properties - {0})", [Enum].GetName(Phase.GetType, Phase)), "Property Package Phase Properties Calculation Routine")

            IObj?.Paragraphs.Add("This is the routine responsible for the calculation of phase properties of the currently associated Material Stream.")

            IObj?.Paragraphs.Add("Specified Phase: " & [Enum].GetName(Phase.GetType, Phase))

            Dim result As Double
            Dim dwpl As Phase

            Dim T, P As Double
            Dim phasemolarfrac As Double = Nothing
            Dim overallmolarflow As Double = Nothing

            Dim phaseID As Integer
            T = Me.CurrentMaterialStream.Phases(0).Properties.temperature.GetValueOrDefault
            P = Me.CurrentMaterialStream.Phases(0).Properties.pressure.GetValueOrDefault

            Select Case Phase
                Case PropertyPackages.Phase.Mixture
                    phaseID = 0
                    dwpl = PropertyPackages.Phase.Mixture
                Case PropertyPackages.Phase.Vapor
                    phaseID = 2
                    dwpl = PropertyPackages.Phase.Vapor
                Case PropertyPackages.Phase.Liquid1
                    phaseID = 3
                    dwpl = PropertyPackages.Phase.Liquid1
                Case PropertyPackages.Phase.Liquid2
                    phaseID = 4
                    dwpl = PropertyPackages.Phase.Liquid2
                Case PropertyPackages.Phase.Liquid3
                    phaseID = 5
                    dwpl = PropertyPackages.Phase.Liquid3
                Case PropertyPackages.Phase.Liquid
                    phaseID = 1
                    dwpl = PropertyPackages.Phase.Liquid
                Case PropertyPackages.Phase.Aqueous
                    phaseID = 6
                    dwpl = PropertyPackages.Phase.Aqueous
                Case PropertyPackages.Phase.Solid
                    phaseID = 7
                    dwpl = PropertyPackages.Phase.Solid
            End Select

            If phaseID > 0 Then

                overallmolarflow = Me.CurrentMaterialStream.Phases(0).Properties.molarflow.GetValueOrDefault
                phasemolarfrac = Me.CurrentMaterialStream.Phases(phaseID).Properties.molarfraction.GetValueOrDefault
                result = overallmolarflow * phasemolarfrac
                Me.CurrentMaterialStream.Phases(phaseID).Properties.molarflow = result
                result = result * Me.AUX_MMM(Phase) / 1000
                Me.CurrentMaterialStream.Phases(phaseID).Properties.massflow = result
                IObj?.SetCurrent
                Me.DW_CalcCompVolFlow(phaseID)
                IObj?.SetCurrent
                Me.DW_CalcCompFugCoeff(Phase)
            End If

            If phaseID = 3 Or phaseID = 4 Or phaseID = 5 Or phaseID = 6 Then


                IObj?.SetCurrent
                Me.CurrentMaterialStream.Phases(phaseID).Properties.density = AUX_LIQDENS2(T, P)

                IObj?.SetCurrent
                result = Me.DW_CalcEnthalpy(RET_VMOL(dwpl), T, P, State.Liquid)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy = result

                IObj?.SetCurrent
                result = Me.DW_CalcEntropy(RET_VMOL(dwpl), T, P, State.Liquid)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy = result

                IObj?.SetCurrent
                result = P / (Me.AUX_LIQDENS2(T, P) * 8.314 * T) / 1000 * AUX_MMM(Phase)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.compressibilityFactor = result

                IObj?.SetCurrent
                result = Me.DW_CalcCp_ISOL(Phase, T, P)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCp = result

                IObj?.SetCurrent
                result = Me.DW_CalcCv_ISOL(Phase, T, P)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCv = result

                result = Me.AUX_MMM(Phase)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight = result

                IObj?.SetCurrent
                result = Me.DW_CalcEnthalpy(RET_VMOL(dwpl), T, P, State.Liquid) * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_enthalpy = result

                IObj?.SetCurrent
                result = Me.DW_CalcEntropy(RET_VMOL(dwpl), T, P, State.Liquid) * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_entropy = result

                IObj?.SetCurrent
                result = Me.AUX_CONDTL2(T, P, phaseID)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.thermalConductivity = result

                IObj?.SetCurrent
                result = Me.AUX_LIQVISCMIX(T, P, phaseID)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.viscosity = result

                Me.CurrentMaterialStream.Phases(phaseID).Properties.kinematic_viscosity = result / Me.CurrentMaterialStream.Phases(phaseID).Properties.density.Value

            ElseIf phaseID = 2 Then

                IObj?.SetCurrent
                result = Me.AUX_VAPDENS(T, P)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.density = result

                IObj?.SetCurrent
                result = Me.DW_CalcEnthalpy(RET_VMOL(dwpl), T, P, State.Vapor)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy = result

                IObj?.SetCurrent
                result = Me.DW_CalcEntropy(RET_VMOL(dwpl), T, P, State.Vapor)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy = result

                IObj?.SetCurrent
                result = P / (Me.AUX_VAPDENS(T, P) * 8.314 * T) / 1000 * AUX_MMM(Phase)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.compressibilityFactor = result

                IObj?.SetCurrent
                result = Me.DW_CalcCp_ISOL(Phase, T, P)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCp = result

                IObj?.SetCurrent
                result = Me.DW_CalcCv_ISOL(Phase, T, P)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCv = result

                IObj?.SetCurrent
                result = Me.AUX_MMM(Phase)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight = result

                IObj?.SetCurrent
                result = Me.DW_CalcEnthalpy(RET_VMOL(dwpl), T, P, State.Vapor) * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_enthalpy = result

                IObj?.SetCurrent
                result = Me.DW_CalcEntropy(RET_VMOL(dwpl), T, P, State.Vapor) * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_entropy = result

                IObj?.SetCurrent
                result = Me.AUX_CONDTG(T, P)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.thermalConductivity = result

                IObj?.SetCurrent
                result = Me.AUX_VAPVISCMIX(T, P, Me.AUX_MMM(Phase))
                Me.CurrentMaterialStream.Phases(phaseID).Properties.viscosity = result
                Me.CurrentMaterialStream.Phases(phaseID).Properties.kinematic_viscosity = result / Me.CurrentMaterialStream.Phases(phaseID).Properties.density.GetValueOrDefault

            ElseIf phaseID = 1 Then

                IObj?.SetCurrent
                DW_CalcLiqMixtureProps()


            Else

                IObj?.SetCurrent
                DW_CalcOverallProps()

            End If


            If phaseID > 0 Then
                result = overallmolarflow * phasemolarfrac * Me.AUX_MMM(Phase) / 1000 / Me.CurrentMaterialStream.Phases(phaseID).Properties.density.GetValueOrDefault
                Me.CurrentMaterialStream.Phases(phaseID).Properties.volumetric_flow = result
            End If

            IObj?.Close()

        End Sub

        Public Overrides Sub DW_CalcProp(ByVal [property] As String, ByVal phase As Phase)

            Dim result As Double = 0.0#
            Dim resultObj As Object = Nothing
            Dim phaseID As Integer = -1
            Dim state As String = ""
            Dim fstate As State = PropertyPackages.State.Solid

            Dim T, P As Double
            T = Me.CurrentMaterialStream.Phases(0).Properties.temperature.GetValueOrDefault
            P = Me.CurrentMaterialStream.Phases(0).Properties.pressure.GetValueOrDefault

            Select Case phase
                Case Phase.Vapor
                    state = "V"
                    fstate = PropertyPackages.State.Vapor
                Case Phase.Liquid, Phase.Liquid1, Phase.Liquid2, Phase.Liquid3, Phase.Aqueous
                    state = "L"
                    fstate = PropertyPackages.State.Liquid
                Case Phase.Solid
                    state = "S"
                    fstate = PropertyPackages.State.Solid
            End Select

            Select Case phase
                Case PropertyPackages.Phase.Mixture
                    phaseID = 0
                Case PropertyPackages.Phase.Vapor
                    phaseID = 2
                Case PropertyPackages.Phase.Liquid1
                    phaseID = 3
                Case PropertyPackages.Phase.Liquid2
                    phaseID = 4
                Case PropertyPackages.Phase.Liquid3
                    phaseID = 5
                Case PropertyPackages.Phase.Liquid
                    phaseID = 1
                Case PropertyPackages.Phase.Aqueous
                    phaseID = 6
                Case PropertyPackages.Phase.Solid
                    phaseID = 7
            End Select

            Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight = Me.AUX_MMM(phase)

            Select Case [property].ToLower
                Case "isothermalcompressibility", "bulkmodulus", "joulethomsoncoefficient", "speedofsound", "internalenergy", "gibbsenergy", "helmholtzenergy"
                    CalcAdditionalPhaseProperties(phaseID)
                Case "compressibilityfactor"
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.compressibilityFactor = result
                Case "heatcapacity", "heatcapacitycp"
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCp = result
                Case "heatcapacitycv"
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCv = result
                Case "enthalpy", "enthalpynf"
                    result = Me.DW_CalcEnthalpy(RET_VMOL(phase), T, P, phase)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy = result
                    result = result * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_enthalpy = result
                Case "entropy", "entropynf"
                    result = Me.DW_CalcEntropy(RET_VMOL(phase), T, P, phase)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy = result
                    result = result * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_entropy = result
                Case "excessenthalpy"
                    result = Me.DW_CalcEnthalpyDeparture(RET_VMOL(phase), T, P, phase)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.excessEnthalpy = result
                Case "excessentropy"
                    result = Me.DW_CalcEntropyDeparture(RET_VMOL(phase), T, P, phase)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.excessEntropy = result
                Case "enthalpyf"
                    Dim entF As Double = 0.0#
                    result = Me.DW_CalcEnthalpy(RET_VMOL(phase), T, P, phase)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpyF = result + entF
                    result = result * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_enthalpyF = result
                Case "entropyf"
                    Dim entF As Double = 0.0#
                    result = Me.DW_CalcEntropy(RET_VMOL(phase), T, P, phase)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.entropyF = result + entF
                    result = Me.CurrentMaterialStream.Phases(phaseID).Properties.entropyF.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_entropyF = result
                Case "viscosity"
                    If state = "L" Then
                        result = Me.AUX_LIQVISCm(T, P, phaseID)
                    Else
                        result = Me.AUX_VAPVISCMIX(T, P, Me.AUX_MMM(phase))
                    End If
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.viscosity = result
                Case "thermalconductivity"
                    If state = "L" Then
                        result = Me.AUX_CONDTL(T, phaseID)
                    Else
                        result = Me.AUX_CONDTG(T, P)
                    End If
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.thermalConductivity = result
                Case "fugacity", "fugacitycoefficient", "logfugacitycoefficient", "activity", "activitycoefficient"
                    Me.DW_CalcCompFugCoeff(phase)
                Case "volume", "density"
                    If state = "L" Then
                        result = Me.AUX_LIQDENS2(T, P)
                    Else
                        result = Me.AUX_VAPDENS(T, P)
                    End If
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.density = result
                Case "surfacetension"
                    Me.CurrentMaterialStream.Phases(0).Properties.surfaceTension = Me.AUX_SURFTM(T)
                Case Else
                    Dim ex As Exception = New CapeOpen.CapeThrmPropertyNotAvailableException
                    ThrowCAPEException(ex, "Error", ex.Message, "ICapeThermoMaterial", ex.Source, ex.StackTrace, "CalcSinglePhaseProp/CalcTwoPhaseProp/CalcProp", ex.GetHashCode)
            End Select

        End Sub

        Public Overrides Function DW_CalcPVAP_ISOL(ByVal T As Double) As Double

            Return 0.0#

        End Function

        Public Overrides Function DW_CalcTensaoSuperficial_ISOL(ByVal Phase1 As Phase, ByVal T As Double, ByVal P As Double) As Double

            Return Me.AUX_SURFTM(T)

        End Function

        Public Overrides Sub DW_CalcTwoPhaseProps(ByVal Phase1 As Phase, ByVal Phase2 As Phase)

            Dim T As Double

            T = Me.CurrentMaterialStream.Phases(0).Properties.temperature.GetValueOrDefault
            Me.CurrentMaterialStream.Phases(0).Properties.surfaceTension = Me.AUX_SURFTM(T)

        End Sub

        Public Overrides Function DW_CalcViscosidadeDinamica_ISOL(ByVal Phase1 As Phase, ByVal T As Double, ByVal P As Double) As Double

            If Phase1 = Phase.Liquid Then
                Return Me.AUX_LIQVISCm(T, P)
            ElseIf Phase1 = Phase.Vapor Then
                Return Me.AUX_VAPVISCm(T, Me.AUX_VAPDENS(T, P), Me.AUX_MMM(Phase.Vapor))
            End If

        End Function

        Public Overrides Function SupportsComponent(ByVal comp As Interfaces.ICompoundConstantProperties) As Boolean

            Return IsCompoundSupported(comp.Name)

        End Function

        Public Overrides Function DW_CalcEnergyFlowMistura_ISOL(T As Double, P As Double) As Double

        End Function

#End Region

#Region "    Auxiliary Functions"

        Function IsCompoundSupported(compname As String) As Boolean

            Return True

        End Function

#End Region

        Public Overrides ReadOnly Property MobileCompatible As Boolean
            Get
                Return False
            End Get
        End Property

        Public Overrides Function AUX_Z(Vx() As Double, T As Double, P As Double, state As PhaseName) As Double

            Return 0.0

        End Function

        Private Function GetTmin(x As Double) As Double

            Dim t = CoolProp.Props1SI(GetCoolPropName(x), "T_FREEZE")
            Try
                Dim psat = CoolProp.PropsSI("P", "T", t, "Q", 0, GetCoolPropName(x))
                Return t
            Catch ex As Exception
                Return CoolProp.Props1SI(GetCoolPropName(x), "TMIN")
            End Try

        End Function

    End Class

End Namespace


