'    Copyright 2018 Daniel Wagner O. de Medeiros
'
'    This file is part of DWSIM.
'
'    DWSIM is free software: you can redistribute it and/or modify
'    it under the terms of the GNU Lesser General Public License as published by
'    the Free Software Foundation, either version 3 of the License, or
'    (at your option) any later version.
'
'    DWSIM is distributed in the hope that it will be useful,
'    but WITHOUT ANY WARRANTY; without even the implied warranty of
'    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
'    GNU Lesser General Public License for more details.
'
'    You should have received a copy of the GNU Lesser General Public License
'    along with DWSIM.  If not, see <http://www.gnu.org/licenses/>.

Imports DWSIM.MathOps.MathEx

Namespace PropertyPackages.Auxiliary.FlashAlgorithms

    <System.Serializable()> Public Class CoolPropIncompressiblePure

        Inherits FlashAlgorithm

        Dim spp As CoolPropIncompressiblePurePropertyPackage

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
                Return Interfaces.Enums.FlashMethod.CoolProp_Incompressibles
            End Get
        End Property

        Public Overrides ReadOnly Property Description As String
            Get
                Return "CoolProp Incompressibles"
            End Get
        End Property

        Public Overrides ReadOnly Property Name As String
            Get
                Return "CoolProp Incompressibles"
            End Get
        End Property

        Public Overrides Function Flash_PT(ByVal Vz As Double(), ByVal P As Double, ByVal T As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Return New Object() {1.0, 0.0, Vz.Clone, Vz.Clone, 0, 0.0#, PP.RET_NullVector, 0.0#, PP.RET_NullVector}

        End Function

        Public Overrides Function Flash_PH(ByVal Vz As Double(), ByVal P As Double, ByVal H As Double, ByVal Tref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim T, Tmin, Tmax As Double

            spp = PP

            Tmin = spp.FluidDataList(spp.FluidName).Tmin
            Tmax = spp.FluidDataList(spp.FluidName).Tmax

            Dim brentsolverT As New BrentOpt.Brent
            brentsolverT.DefineFuncDelegate(AddressOf spp.EnthalpyTx)

            spp.LoopVarF = H
            spp.LoopVarP = P
            T = brentsolverT.BrentOpt(Tmin, Tmax, 100, 0.0001, 1000, Nothing)

            Return New Object() {1.0, 0.0, Vz.Clone, Vz.Clone, T, 0.0#, Vz.Clone, 0.0#, PP.RET_NullVector, 0.0#, PP.RET_NullVector}

        End Function

        Public Overrides Function Flash_PS(ByVal Vz As Double(), ByVal P As Double, ByVal S As Double, ByVal Tref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim T, Tmin, Tmax As Double

            spp = PP

            Tmin = spp.FluidDataList(spp.FluidName).Tmin
            Tmax = spp.FluidDataList(spp.FluidName).Tmax

            Dim brentsolverT As New BrentOpt.Brent
            brentsolverT.DefineFuncDelegate(AddressOf spp.EntropyTx)

            spp.LoopVarF = S
            spp.LoopVarP = P
            T = brentsolverT.BrentOpt(Tmin, Tmax, 100, 0.0001, 1000, Nothing)

            Return New Object() {1.0, 0.0, Vz.Clone, Vz.Clone, T, 0.0#, Vz.Clone, 0.0#, PP.RET_NullVector, 0.0#, PP.RET_NullVector}


        End Function

        Public Overrides Function Flash_TV(ByVal Vz As Double(), ByVal T As Double, ByVal V As Double, ByVal Pref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            spp = PP

            Return New Object() {1.0, 0.0, Vz.Clone, Vz.Clone, CoolProp.Props1SI(spp.FluidName, "PMAX"), 0, Vz.Clone, 0.0#, PP.RET_NullVector, 0.0#, PP.RET_NullVector}

        End Function

        Public Overrides Function Flash_PV(ByVal Vz As Double(), ByVal P As Double, ByVal V As Double, ByVal Tref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            spp = PP

            Return New Object() {1.0, 0.0, Vz.Clone, Vz.Clone, CoolProp.Props1SI(spp.FluidName, "PMAX"), 0, Vz.Clone, 0.0#, PP.RET_NullVector, 0.0#, PP.RET_NullVector}

        End Function

        Public Overrides ReadOnly Property MobileCompatible As Boolean
            Get
                Return False
            End Get
        End Property
    End Class

End Namespace

