'    Single Compound Flash Algorithm
'    Copyright 2021 Daniel Wagner O. de Medeiros  
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

Imports System.Math
Imports DWSIM.MathOps.MathEx.BrentOpt
Imports DWSIM.SharedClasses
Imports MathNet.Numerics

Namespace PropertyPackages.Auxiliary.FlashAlgorithms

    <System.Serializable()> Public Class SingleCompFlash

        Inherits FlashAlgorithm

        Dim Hf, Sf As Double

        Sub New()
            MyBase.New()
            Order = 1
        End Sub

        Public Overrides ReadOnly Property AlgoType As Interfaces.Enums.FlashMethod
            Get
                Return Interfaces.Enums.FlashMethod.Custom
            End Get
        End Property

        Public Overrides ReadOnly Property Description As String = "Single Compound Flash"

        Public Overrides ReadOnly Property Name As String = "Single Compound Flash"

        Public Overrides Function Flash_PT(ByVal Vz As Double(), ByVal P As Double, ByVal T As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            If T < PP.RET_VTF(0) Then
                Return New Object() {0.0, 0.0, Vz, Vz, 0, 0.0#, PP.RET_NullVector, 1.0, Vz, Vz}
            Else
                Dim Pvap = PP.AUX_PVAPi(0, T)
                If Pvap > P Then
                    Return New Object() {0.0, 1.0, Vz, Vz, 0, 0.0#, PP.RET_NullVector, 0.0, Vz, Vz}
                Else
                    Return New Object() {1.0, 0.0, Vz, Vz, 0, 0.0#, PP.RET_NullVector, 1.0, Vz, Vz}
                End If
            End If

        End Function

        Public Overrides Function Flash_PH(ByVal Vz As Double(), ByVal P As Double, ByVal H As Double, ByVal Tref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim Tsat = PP.AUX_TSATi(P, 0)
            Dim Tsub = PP.AUX_TSUBLi(0, P)
            Dim Tfus = PP.RET_VTF(0)

            Dim HsatV = PP.DW_CalcEnthalpy(Vz, Tsat, P, State.Vapor)
            Dim HsatL = PP.DW_CalcEnthalpy(Vz, Tsat, P, State.Liquid)





            'Return New Object() {L1, V, Vx1, Vy, T, ecount, Ki, L2, Vx2, Sx, Vs}

        End Function

        Public Overrides Function Flash_PS(ByVal Vz As Double(), ByVal P As Double, ByVal S As Double, ByVal Tref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object



            'Return New Object() {L1, V, Vx1, Vy, T, ecount, Ki, L2, Vx2, Sx, Vs}

        End Function

        Public Overrides Function Flash_TV(ByVal Vz As Double(), ByVal T As Double, ByVal V As Double, ByVal Pref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Throw New Exception("Unsupported Flash Specification (TVF)")

        End Function

        Public Overrides Function Flash_PV(ByVal Vz As Double(), ByVal P As Double, ByVal V As Double, ByVal Tref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Throw New Exception("Unsupported Flash Specification (PVF)")

        End Function

        Function OBJ_FUNC_PH_FLASH(ByVal Type As String, ByVal X As Double, ByVal P As Double, ByVal Vz() As Double, ByVal PP As PropertyPackages.PropertyPackage, ByVal ReuseKi As Boolean, ByVal Ki() As Double) As Object

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "Flash_PH", "PH Flash Objective Function (Error)", "Pressure-Enthalpy Flash Algorithm Objective Function (Error) Calculation")

            IObj?.Paragraphs.Add("This routine calculates the current error between calculated and specified enthalpies.")

            IObj?.SetCurrent()

            Dim n As Integer = Vz.Length - 1
            Dim L1, L2, V, Vx1(), Vx2(), Vy(), Sx, Vs(), T As Double

            Dim tmp = Me.Flash_PT(Vz, P, X, PP, ReuseKi, Ki)
            L1 = tmp(0)
            V = tmp(1)
            Vx1 = tmp(2)
            Vy = tmp(3)
            L2 = tmp(5)
            Vx2 = tmp(6)
            Sx = tmp(7)
            Vs = tmp(8)
            T = X

            Dim _Hv, _Hl1, _Hl2, _Hs As Double

            _Hv = 0.0#
            _Hl1 = 0.0#
            _Hl2 = 0.0#
            _Hs = 0.0

            If V > 0 Then _Hv = PP.DW_CalcEnthalpy(Vy, T, P, State.Vapor)
            If L1 > 0 Then _Hl1 = PP.DW_CalcEnthalpy(Vx1, T, P, State.Liquid)
            If L2 > 0 Then _Hl2 = PP.DW_CalcEnthalpy(Vx2, T, P, State.Liquid)
            If Sx > 0 Then _Hs = PP.DW_CalcEnthalpy(Vs, T, P, State.Solid)

            Dim mmg, mml, mml2, mms As Double
            mmg = PP.AUX_MMM(Vy)
            mml = PP.AUX_MMM(Vx1)
            mml2 = PP.AUX_MMM(Vx2)
            mms = PP.AUX_MMM(Vs)

            Dim herr = Hf - (mmg * V / (mmg * V + mml * L1 + mml2 * L2 + mms * Sx)) * _Hv -
                (mml * L1 / (mmg * V + mml * L1 + mml2 * L2 + mms * Sx)) * _Hl1 -
                (mml2 * L2 / (mmg * V + mml * L1 + mml2 * L2 + mms * Sx)) * _Hl2 -
                (mms * Sx / (mmg * V + mml * L1 + mml2 * L2 + mms * Sx)) * _Hs

            OBJ_FUNC_PH_FLASH = {herr, T, V, L1, Vy, Vx1}

            IObj?.Paragraphs.Add(String.Format("Specified Enthalpy: {0} kJ/kg", Hf))

            IObj?.Paragraphs.Add(String.Format("Current Error: {0} kJ/kg", herr))

            IObj?.Close()

            WriteDebugInfo("PH Flash [NL]: Current T = " & T & ", Current H Error = " & herr)

            If Not PP.CurrentMaterialStream.Flowsheet Is Nothing Then PP.CurrentMaterialStream.Flowsheet.CheckStatus()

        End Function

        Function OBJ_FUNC_PS_FLASH(ByVal Type As String, ByVal X As Double, ByVal P As Double, ByVal Vz() As Double, ByVal PP As PropertyPackages.PropertyPackage, ByVal ReuseKi As Boolean, ByVal Ki() As Double) As Object

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "Flash_PS", "PS Flash Objective Function (Error)", "Pressure-Entropy Flash Algorithm Objective Function (Error) Calculation")

            IObj?.Paragraphs.Add("This routine calculates the current error between calculated and specified entropies.")

            IObj?.SetCurrent()

            Dim n = Vz.Length - 1
            Dim L1, L2, V, Vx1(), Vx2(), Vy(), Sx, Vs(), T As Double

            Dim tmp = Me.Flash_PT(Vz, P, X, PP, ReuseKi, Ki)
            L1 = tmp(0)
            V = tmp(1)
            Vx1 = tmp(2)
            Vy = tmp(3)
            L2 = tmp(5)
            Vx2 = tmp(6)
            Sx = tmp(7)
            Vs = tmp(8)
            T = X

            Dim _Sv, _Sl1, _Sl2, _Ss As Double

            _Sv = 0.0#
            _Sl1 = 0.0#
            _Sl2 = 0.0#
            _Ss = 0.0

            If V > 0 Then _Sv = PP.DW_CalcEntropy(Vy, T, P, State.Vapor)
            If L1 > 0 Then _Sl1 = PP.DW_CalcEntropy(Vx1, T, P, State.Liquid)
            If L2 > 0 Then _Sl2 = PP.DW_CalcEntropy(Vx2, T, P, State.Liquid)
            If Sx > 0 Then _Ss = PP.DW_CalcEntropy(Vs, T, P, State.Solid)

            Dim mmg, mml, mml2, mms As Double

            mmg = PP.AUX_MMM(Vy)
            mml = PP.AUX_MMM(Vx1)
            mml2 = PP.AUX_MMM(Vx2)
            mms = PP.AUX_MMM(Vs)

            Dim serr = Sf - (mmg * V / (mmg * V + mml * L1 + mml2 * L2 + mms * Sx)) * _Sv -
                (mml * L1 / (mmg * V + mml * L1 + mml2 * L2 + mms * Sx)) * _Sl1 -
                (mml2 * L2 / (mmg * V + mml * L1 + mml2 * L2 + mms * Sx)) * _Sl2 -
                (mms * Sx / (mmg * V + mml * L1 + mml2 * L2 + mms * Sx)) * _Ss

            OBJ_FUNC_PS_FLASH = {serr, T, V, L1, Vy, Vx1}

            IObj?.Paragraphs.Add(String.Format("Specified Entropy: {0} kJ/[kg.K]", Sf))

            IObj?.Paragraphs.Add(String.Format("Current Error: {0} kJ/[kg.K]", serr))

            IObj?.Close()

            WriteDebugInfo("PS Flash [NL]: Current T = " & T & ", Current S Error = " & serr)

            If Not PP.CurrentMaterialStream.Flowsheet Is Nothing Then PP.CurrentMaterialStream.Flowsheet.CheckStatus()

        End Function

        Function Herror(ByVal type As String, ByVal X As Double, ByVal P As Double, ByVal Vz() As Double, ByVal PP As PropertyPackages.PropertyPackage, ByVal ReuseKi As Boolean, ByVal Ki() As Double) As Object
            Return OBJ_FUNC_PH_FLASH(type, X, P, Vz, PP, ReuseKi, Ki)
        End Function

        Function Serror(ByVal type As String, ByVal X As Double, ByVal P As Double, ByVal Vz() As Double, ByVal PP As PropertyPackages.PropertyPackage, ByVal ReuseKi As Boolean, ByVal Ki() As Double) As Object
            Return OBJ_FUNC_PS_FLASH(type, X, P, Vz, PP, ReuseKi, Ki)
        End Function

        Public Overrides ReadOnly Property MobileCompatible As Boolean
            Get
                Return False
            End Get
        End Property

    End Class

End Namespace
