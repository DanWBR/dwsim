'    Copyright 2010-2016 Daniel Wagner O. de Medeiros, Gregor Reichert
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

Imports DWSIM.MathOps.MathEx

Namespace PropertyPackages.Auxiliary.FlashAlgorithms

    ''' <summary>
    ''' The Flash algorithms in this class are based on the Nested Loops approach to solve equilibrium calculations.
    ''' </summary>
    ''' <remarks></remarks>
    <System.Serializable()> Public Class SteamTables

        Inherits FlashAlgorithm

        Dim etol As Double = 0.000001
        Dim itol As Double = 0.000001
        Dim maxit_i As Integer = 100
        Dim maxit_e As Integer = 100
        Dim Hv0, Hvid, Hlid, Hf, Hv, Hl As Double
        Dim Sv0, Svid, Slid, Sf, Sv, Sl As Double

        Dim spp As SteamTablesPropertyPackage

        Public Overrides ReadOnly Property InternalUseOnly As Boolean
            Get
                Return True
            End Get
        End Property

        Sub New()
            MyBase.New()
            FlashSettings(Interfaces.Enums.FlashSetting.CalculateBubbleAndDewPoints) = True
        End Sub

        Public Property LimitVaporFraction As Boolean = True

        Public Overrides ReadOnly Property AlgoType As Interfaces.Enums.FlashMethod
            Get
                Return Interfaces.Enums.FlashMethod.SteamTables
            End Get
        End Property

        Public Overrides ReadOnly Property Description As String
            Get
                If GlobalSettings.Settings.CurrentCulture = "pt-BR" Then
                    Return "Algoritmo Flash para Tabelas de Vapor."
                Else
                    Return "Flash Algorithm for Steam Tables."
                End If
            End Get
        End Property

        Public Overrides ReadOnly Property Name As String
            Get
                Return "Steam Tables VLE"
            End Get
        End Property

        Public Overrides Function Flash_PT(ByVal Vz As Double(), ByVal P As Double, ByVal T As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim Psat, vf, lf As Double

            spp = PP

            With spp.m_iapws97
                Psat = .pSatW(T)
                If T > 273.15 And Psat = -1 Then Psat = 1.0E+20
                If P / 100000 > Psat Then
                    vf = 0.0#
                Else
                    vf = 1.0#
                End If
            End With
            lf = 1 - vf

            Return New Object() {lf, vf, Vz.Clone, Vz.Clone, 0, 0.0#, PP.RET_NullVector, 0.0#, PP.RET_NullVector}

        End Function

        Public Overrides Function Flash_PH(ByVal Vz As Double(), ByVal P As Double, ByVal H As Double, ByVal Tref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim vf, lf, T As Double

            spp = PP

            Dim brentsolverT As New BrentOpt.Brent
            brentsolverT.DefineFuncDelegate(AddressOf spp.EnthalpyTx)

            With spp.m_iapws97

                Dim Tsat = .tSatW(P / 100000)
                Dim Tcrit = 647.1

                If Tsat > Tcrit Then

                    'supercritical

                    T = brentsolverT.BrentOpt2(647.1, 2000, 5, 0.001, 100, Function(Tx)

                                                                               Return H - .enthalpyW(Tx, P / 100000)

                                                                           End Function)

                    Dim dens = .densW(T, P / 100000)

                    If dens > 322.0 Then
                        vf = 0.0
                    Else
                        vf = 1.0
                    End If

                Else

                    Hl = .enthalpySatLiqPW(P / 100000)
                    Hv = .enthalpySatVapPW(P / 100000)
                    Sl = .entropySatLiqPW(P / 100000)
                    Sv = .entropySatVapPW(P / 100000)

                    If H < Hl Then
                        vf = 0.0#
                    ElseIf H > Hv Then
                        vf = 1.0#
                    Else
                        vf = (H - Hl) / (Hv - Hl)
                    End If

                    If vf > 0.0 And vf < 1.0 Then
                        T = .tSatW(P / 100000)
                    Else
                        spp.LoopVarF = H
                        spp.LoopVarX = P / 100000
                        T = brentsolverT.BrentOpt(273.15, 2000, 100, 0.0001, 1000, Nothing)
                    End If

                End If

            End With

            lf = 1 - vf

            Return New Object() {lf, vf, Vz.Clone, Vz.Clone, T, 0.0#, Vz.Clone, 0.0#, PP.RET_NullVector, 0.0#, PP.RET_NullVector}

        End Function

        Public Overrides Function Flash_PS(ByVal Vz As Double(), ByVal P As Double, ByVal S As Double, ByVal Tref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim vf, lf, T As Double

            spp = PP

            Dim brentsolverT As New BrentOpt.Brent
            brentsolverT.DefineFuncDelegate(AddressOf spp.EntropyTx)

            With spp.m_iapws97

                Dim Tsat = .tSatW(P / 100000)
                Dim Tcrit = 647.1

                If Tsat > Tcrit Then

                    'supercritical

                    T = brentsolverT.BrentOpt2(273.15, 2000, 5, 0.001, 100, Function(Tx)

                                                                                Return S - .entropyW(Tx, P / 100000)

                                                                            End Function)

                    Dim dens = .densW(T, P / 100000)

                    If dens > 322.0 Then
                        vf = 0.0
                    Else
                        vf = 1.0
                    End If

                Else

                    Sl = .entropySatLiqPW(P / 100000)
                    Sv = .entropySatVapPW(P / 100000)

                    If S < Sl Then
                        vf = 0
                    ElseIf S > Sv Then
                        vf = 1
                    Else
                        vf = (S - Sl) / (Sv - Sl)
                    End If

                    If vf > 0.0 And vf < 1.0 Then
                        T = .tSatW(P / 100000)
                    Else
                        spp.LoopVarF = S
                        spp.LoopVarX = P / 100000
                        T = brentsolverT.BrentOpt(273.15, 2000, 100, 0.0001, 1000, Nothing)
                    End If

                End If

            End With

            lf = 1 - vf

            Return New Object() {lf, vf, Vz.Clone, Vz.Clone, T, 0, Vz.Clone, 0.0#, PP.RET_NullVector, 0.0#, PP.RET_NullVector}

        End Function

        Public Overrides Function Flash_TV(ByVal Vz As Double(), ByVal T As Double, ByVal V As Double, ByVal Pref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim vf, lf, P As Double

            spp = PP

            With spp.m_iapws97
                P = .pSatW(T) * 100000
            End With
            lf = 1 - vf

            Return New Object() {lf, vf, Vz.Clone, Vz.Clone, P, 0, Vz.Clone, 0.0#, PP.RET_NullVector, 0.0#, PP.RET_NullVector}

        End Function

        Public Overrides Function Flash_PV(ByVal Vz As Double(), ByVal P As Double, ByVal V As Double, ByVal Tref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim lf, T As Double

            spp = PP

            With spp.m_iapws97
                T = .tSatW(P / 100000)
            End With
            lf = 1 - V

            Return New Object() {lf, V, Vz.Clone, Vz.Clone, T, 0, Vz.Clone, 0.0#, PP.RET_NullVector, 0.0#, PP.RET_NullVector}

        End Function

        Public Overrides ReadOnly Property MobileCompatible As Boolean
            Get
                Return False
            End Get
        End Property
    End Class

End Namespace
