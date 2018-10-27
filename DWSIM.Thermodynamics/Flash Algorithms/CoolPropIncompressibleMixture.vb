'    Copyright 2018 Daniel Wagner O. de Medeiros
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

    <System.Serializable()> Public Class CoolPropIncompressibleMixture

        Inherits FlashAlgorithm

        Dim spp As CoolPropIncompressibleMixturePropertyPackage

        Public Overrides ReadOnly Property InternalUseOnly As Boolean
            Get
                Return True
            End Get
        End Property

        Sub New()
            MyBase.New()
        End Sub

        Public Overrides ReadOnly Property AlgoType As Interfaces.Enums.FlashMethod
            Get
                Return Interfaces.Enums.FlashMethod.CoolProp_IncompressibleMixtures
            End Get
        End Property

        Public Overrides ReadOnly Property Description As String
            Get
                Return "CoolProp Incompressible Mixtures"
            End Get
        End Property

        Public Overrides ReadOnly Property Name As String
            Get
                Return "CoolProp Incompressible Mixtures"
            End Get
        End Property

        Public Overrides Function Flash_PT(ByVal Vz As Double(), ByVal P As Double, ByVal T As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim Psat, vf, lf As Double

            spp = PP

            Dim Vxw = spp.AUX_CONVERT_MOL_TO_MASS(Vz)
            Dim x = Vxw(Array.IndexOf(spp.RET_VNAMES(), spp.SoluteCompound))

            Psat = spp.AUX_PVAPi2(x, T)

            If P > Psat Then
                vf = 0.0#
            Else
                vf = 1.0#
            End If
            lf = 1 - vf

            Return New Object() {lf, vf, Vz.Clone, Vz.Clone, 0, 0.0#, PP.RET_NullVector, 0.0#, PP.RET_NullVector}

        End Function

        Public Overrides Function Flash_PH(ByVal Vz As Double(), ByVal P As Double, ByVal H As Double, ByVal Tref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim vf, lf, T, Tmin, Tmax, Tant, Hl, Hv, mwl, mwv As Double

            spp = PP

            Dim si = Array.IndexOf(spp.RET_VNAMES(), spp.SoluteCompound)
            Dim nsi = Array.IndexOf(spp.RET_VNAMES(), spp.SolventCompound)

            Tmin = CoolProp.Props1SI(spp.GetCoolPropName(0.0), "TMIN")
            Tmax = CoolProp.Props1SI(spp.GetCoolPropName(0.0), "TMAX")

            Dim Vxw = spp.AUX_CONVERT_MOL_TO_MASS(Vz)
            Dim x = Vxw(si)

            Dim Vx = Vz.Clone
            Dim Vy = spp.RET_NullVector

            With spp

                T = Tref

                Vy(nsi) = 1.0
                Vx(nsi) = Vz(nsi)
                Vx(si) = 1.0 - Vx(nsi)

                Do

                    Hl = spp.DW_CalcEnthalpy(Vx, T, P, State.Liquid)
                    Hv = spp.DW_CalcEnthalpy(Vy, T, P, State.Vapor)

                    If H < Hl Then
                        vf = 0.0#
                    ElseIf H > Hv Then
                        vf = 1.0#
                    Else
                        vf = (H - Hl) / (Hv - Hl)
                    End If

                    lf = 1 - vf

                    Vy(nsi) = 1.0
                    Vx(nsi) = (Vz(nsi) - vf) / lf
                    Vx(si) = 1.0 - Vx(nsi)

                    mwv = spp.AUX_MMM(Vy)
                    mwl = spp.AUX_MMM(Vx)

                    Tant = T
                    If vf <> 0.0# And vf <> 1.0# Then
                        Dim bs As New BrentOpt.BrentMinimize
                        bs.DefineFuncDelegate(Function(tx)
                                                  Hl = spp.DW_CalcEnthalpy(Vx, tx, P, State.Liquid)
                                                  Hv = spp.DW_CalcEnthalpy(Vy, tx, P, State.Vapor)
                                                  Return (H - vf * mwv / (vf * mwv + lf * mwl) * Hv - lf * mwl / (vf * mwv + lf * mwl) * Hl) ^ 2
                                              End Function)
                        Dim fmin As Double
                        fmin = bs.brentoptimize(Tmin, Tmax, 0.0001, T)
                    Else
                        Dim brentsolverT As New BrentOpt.Brent
                        brentsolverT.DefineFuncDelegate(AddressOf spp.EnthalpyTx)
                        .LoopVarF = H
                        .LoopVarP = P
                        .LoopVarX = x
                        T = brentsolverT.BrentOpt(Tmin, Tmax, 100, 0.0001, 1000, Nothing)
                    End If

                Loop Until Math.Abs(T - Tant) < 0.01

            End With

            Return New Object() {lf, vf, Vx, Vy, T, 0.0#, Vz.Clone, 0.0#, PP.RET_NullVector, 0.0#, PP.RET_NullVector}

        End Function

        Public Overrides Function Flash_PS(ByVal Vz As Double(), ByVal P As Double, ByVal S As Double, ByVal Tref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim vf, lf, T, Tmin, Tmax, Sl, Sv As Double

            spp = PP

            Dim si = Array.IndexOf(spp.RET_VNAMES(), spp.SoluteCompound)
            Dim nsi = Array.IndexOf(spp.RET_VNAMES(), spp.SolventCompound)

            Tmin = CoolProp.Props1SI(spp.GetCoolPropName(0.0), "TMIN")
            Tmax = CoolProp.Props1SI(spp.GetCoolPropName(0.0), "TMAX")

            Dim Vxw = spp.AUX_CONVERT_MOL_TO_MASS(Vz)
            Dim x = Vxw(si)

            Dim brentsolverT As New BrentOpt.Brent
            brentsolverT.DefineFuncDelegate(AddressOf spp.EntropyTx)

            With spp

                T = Tref
                Sl = .EntropySatLiqP(P, T, x)
                Sv = .EntropySatVapP(P, T, x)

                If S < Sl Then
                    vf = 0.0#
                ElseIf S > Sv Then
                    vf = 1.0#
                Else
                    vf = (S - Sl) / (Sv - Sl)
                End If

                If vf <> 0.0# And vf <> 1.0# Then
                    T = CoolProp.PropsSI("T", "P", P, "S", S * 1000, spp.GetCoolPropName(x))
                Else
                    .LoopVarF = S
                    .LoopVarP = P
                    .LoopVarX = x
                    T = brentsolverT.BrentOpt(Tmin, Tmax, 100, 0.0001, 1000, Nothing)
                End If

            End With

            lf = 1 - vf

            Dim Vx = Vz.Clone
            Dim Vy = spp.RET_NullVector

            Vy(si) = 1.0
            Vx(si) = (Vz(si) - vf) / lf
            Vx(nsi) = 1 - Vx(si)

            Return New Object() {lf, vf, Vx, Vy, T, 0.0#, Vz.Clone, 0.0#, PP.RET_NullVector, 0.0#, PP.RET_NullVector}

        End Function

        Public Overrides Function Flash_TV(ByVal Vz As Double(), ByVal T As Double, ByVal V As Double, ByVal Pref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim vf, lf, P As Double

            spp = PP

            vf = V

            Dim si = Array.IndexOf(spp.RET_VNAMES(), spp.SoluteCompound)
            Dim nsi = Array.IndexOf(spp.RET_VNAMES(), spp.SolventCompound)

            Dim Vxw = spp.AUX_CONVERT_MOL_TO_MASS(Vz)
            Dim x = Vxw(si)

            P = CoolProp.PropsSI("P", "T", T, "Q", vf, spp.GetCoolPropName(x))

            lf = 1 - vf

            Dim Vx = Vz.Clone
            Dim Vy = spp.RET_NullVector

            Vy(si) = 1.0
            Vx(si) = (Vz(si) - vf) / lf
            Vx(nsi) = 1 - Vx(si)

            Return New Object() {lf, vf, Vx, Vy, P, 0.0#, Vz.Clone, 0.0#, PP.RET_NullVector, 0.0#, PP.RET_NullVector}

        End Function

        Public Overrides Function Flash_PV(ByVal Vz As Double(), ByVal P As Double, ByVal V As Double, ByVal Tref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim vf, lf, T As Double

            spp = PP

            vf = V

            Dim si = Array.IndexOf(spp.RET_VNAMES(), spp.SoluteCompound)
            Dim nsi = Array.IndexOf(spp.RET_VNAMES(), spp.SolventCompound)

            Dim Vxw = spp.AUX_CONVERT_MOL_TO_MASS(Vz)
            Dim x = Vxw(si)

            T = CoolProp.PropsSI("T", "P", P, "Q", vf, spp.GetCoolPropName(x))

            lf = 1 - vf

            Dim Vx = Vz.Clone
            Dim Vy = spp.RET_NullVector

            Vy(si) = 1.0
            Vx(si) = (Vz(si) - vf) / lf
            Vx(nsi) = 1 - Vx(si)

            Return New Object() {lf, vf, Vx, Vy, T, 0.0#, Vz.Clone, 0.0#, PP.RET_NullVector, 0.0#, PP.RET_NullVector}

        End Function

        Public Overrides ReadOnly Property MobileCompatible As Boolean
            Get
                Return False
            End Get
        End Property

    End Class

End Namespace
