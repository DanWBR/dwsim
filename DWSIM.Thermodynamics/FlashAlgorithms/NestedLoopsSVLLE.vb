'    DWSIM Nested Loops Global Flash Algorithm (SVLLE)
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

Imports System.Math

Namespace PropertyPackages.Auxiliary.FlashAlgorithms

    Public Class NestedLoopsSVLLE

        Inherits FlashAlgorithm

        Dim nl1 As New NestedLoops
        Dim nl2 As New NestedLoops3PV3
        Dim nl3 As New NestedLoopsSLE With {.SolidSolution = False}

        Public Sub New()
            MyBase.New
            Order = 0
        End Sub
        Public Sub ClearEstimates()
            nl2?.ClearEstimates()
        End Sub

        Public Overrides ReadOnly Property AlgoType As Interfaces.Enums.FlashMethod
            Get
                Return Interfaces.Enums.FlashMethod.Nested_Loops_SVLLE
            End Get
        End Property

        Public Overrides ReadOnly Property Description As String
            Get
                Return "Global Equilibrium Flash Algorithm, can calculate equilibria between one solid, one vapor and two liquid phases (SVLLE)."
            End Get
        End Property

        Public Overrides ReadOnly Property Name As String
            Get
                Return "Nested Loops SVLLE"
            End Get
        End Property

        Public Overrides ReadOnly Property MobileCompatible As Boolean
            Get
                Return False
            End Get
        End Property

        Public Overrides Function Flash_PT(Vz() As Double, P As Double, T As Double, PP As PropertyPackage, Optional ReuseKI As Boolean = False, Optional PrevKi() As Double = Nothing) As Object

            nl1.FlashSettings = FlashSettings
            nl2.FlashSettings = FlashSettings

            Dim d1, d2 As Date, dt As TimeSpan

            d1 = Date.Now

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "Flash_PT", Name & " (PT Flash)", "Pressure-Temperature Flash Algorithm Routine", True)

            IObj?.Paragraphs.Add(String.Format("<h2>Input Parameters</h2>"))

            IObj?.Paragraphs.Add(String.Format("Temperature: {0} K", T))
            IObj?.Paragraphs.Add(String.Format("Pressure: {0} Pa", P))
            IObj?.Paragraphs.Add(String.Format("Compounds: {0}", PP.RET_VNAMES.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Mole Fractions: {0}", Vz.ToMathArrayString))

            Dim V, L1, L2, S, Vy(), Vx1(), Vx2(), Vs() As Double

            Dim result As Object

            Dim names = PP.RET_VNAMES

            Dim n = Vz.Length - 1

            If PP.ForcedSolids.Count > 0 Then

                'we have forced solids

                Dim Vzns As Double() = Vz.Clone
                Vs = PP.RET_NullVector
                For Each item In PP.ForcedSolids
                    Dim index = names.ToList.IndexOf(item)
                    Vs(index) = Vz(index)
                    Vzns(index) = 0.0
                Next
                S = Vs.Sum
                Vzns = Vzns.NormalizeY
                Vs = Vs.NormalizeY

                IObj?.SetCurrent

                result = nl1.Flash_PT(Vzns, P, T, PP)

                L1 = result(0) * (1 - S)
                V = result(1) * (1 - S)
                Vx1 = result(2)
                Vy = result(3)
                L2 = result(5) * (1 - S)
                Vx2 = result(6)

            Else

                result = nl1.Flash_PT(Vz, P, T, PP)

                L1 = result(0)
                V = result(1)
                Vx1 = result(2)
                Vy = result(3)
                L2 = result(5)
                Vx2 = result(6)

            End If

            If L1 > 0.001 Then

                IObj?.SetCurrent

                Dim nonsolids = Vz.Count - PP.ForcedSolids.Count

                If nonsolids > 1 Then

                    Dim lps = GetPhaseSplitEstimates(T, P, L1, Vx1, PP)

                    L1 = lps(0)
                    Vx1 = lps(1)
                    L2 = lps(2)
                    Vx2 = lps(3)

                    If L2 > 0.0 Then

                        result = nl2.Flash_PT_3P(Vz, V, L1, L2, Vy, Vx1, Vx2, P, T, PP)

                    End If

                    IObj?.SetCurrent

                    L1 = result(0) * (1 - S)
                    V = result(1) * (1 - S)
                    Vx1 = result(2)
                    Vy = result(3)
                    L2 = result(5) * (1 - S)
                    Vx2 = result(6)

                    If L1 = 0.0 And L2 > 0.0 Then
                        L1 = L2
                        L2 = 0.0
                        Vx1 = Vx2
                        Vx2 = PP.RET_NullVector
                    End If

                End If

                IObj?.SetCurrent

                If PP.RET_VTF.SumY > 0.0 And S = 0.0 Then

                    result = nl3.Flash_SL(Vx1, P, T, PP)

                    IObj?.SetCurrent

                    'Return New Object() {L, 1 - L, 0.0#, Vx, Vs, L - L_old, ecount, d2 - d1}

                    S = result(1) * L1
                    L1 = result(0) * L1

                    Vx1 = result(3)
                    Vs = result(4)

                End If

                If L2 > 0.0 Then

                    If PP.RET_VTF.SumY > 0.0 OrElse PP.ForcedSolids.Count > 0 Then

                        result = nl3.Flash_SL(Vx2, P, T, PP)

                        IObj?.SetCurrent

                        'Return New Object() {L, 1 - L, 0.0#, Vx, Vs, L - L_old, ecount, d2 - d1}

                        Vx2 = result(3)
                        Vs = Vs.MultiplyConstY(S).AddY(DirectCast(result(4), Double()).MultiplyConstY(result(1))).NormalizeY()

                        S = S + result(1) * L2
                        L2 = result(0) * L2

                    End If

                End If

            ElseIf S = 0.0 Then

                IObj?.SetCurrent

                'Return New Object() {L, V, Vx, Vy, ecount, 0.0#, PP.RET_NullVector, 0.0#, PP.RET_NullVector}

                result = nl3.Flash_PT(Vz, P, T, PP)

                IObj?.SetCurrent

                L1 = result(0)
                V = result(1)
                Vx1 = result(2)
                Vy = result(3)
                Vs = result(8)
                S = result(7)

            End If

            d2 = Date.Now

            dt = d2 - d1

            IObj?.Paragraphs.Add("PT Flash [NL-SVLLE]: Converged successfully. Time taken: " & dt.TotalMilliseconds & " ms")

            IObj?.Close()

            Dim Ki1(n), Ki2(n) As Double

            Ki1 = PP.DW_CalcKvalue(Vx1, Vy, T, P)
            If L2 > 0.0 Then Ki2 = PP.DW_CalcKvalue(Vx2, Vy, T, P)

            Return New Object() {L1, V, Vx1, Vy, 0, L2, Vx2, S, Vs, Ki1, Ki2}

        End Function

        Public Overrides Function Flash_PH(ByVal Vz As Double(), ByVal P As Double, ByVal H As Double, ByVal Tref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim nl = New NestedLoops
            nl.FlashSettings = FlashSettings
            nl.PTFlashFunction = AddressOf Flash_PT

            Return nl.Flash_PH(Vz, P, H, Tref, PP, ReuseKI, PrevKi)

        End Function

        Public Overrides Function Flash_PS(ByVal Vz As Double(), ByVal P As Double, ByVal S As Double, ByVal Tref As Double, ByVal PP As PropertyPackages.PropertyPackage, Optional ByVal ReuseKI As Boolean = False, Optional ByVal PrevKi As Double() = Nothing) As Object

            Dim nl = New NestedLoops
            nl.FlashSettings = FlashSettings
            nl.PTFlashFunction = AddressOf Flash_PT

            Return nl.Flash_PS(Vz, P, S, Tref, PP, ReuseKI, PrevKi)

        End Function

        Public Overrides Function Flash_PV(Vz() As Double, P As Double, V As Double, Tref As Double, PP As PropertyPackage, Optional ReuseKI As Boolean = False, Optional PrevKi() As Double = Nothing) As Object

            If PP.ForcedSolids.Count > 0 Then

                'we have forced solids

                PP.Flowsheet?.ShowMessage("Warning: when compounds are marked as forced solids, partial or full vaporization calculations are done solids-free. Specified and calculated vapor fractions won't match.", Interfaces.IFlowsheet.MessageType.Warning)

                Dim names = PP.RET_VNAMES
                Dim Vs = PP.RET_NullVector
                Dim Vzns As Double() = Vz.Clone
                Dim S As Double = 0.0
                For Each item In PP.ForcedSolids
                    Dim index = names.ToList.IndexOf(item)
                    Vs(index) = Vz(index)
                    Vzns(index) = 0.0
                Next
                S = Vs.Sum
                Vs = Vs.NormalizeY
                Vzns = Vzns.NormalizeY

                'Return New Object() {L, V, Vx, Vy, T, ecount, Ki, 0.0#, PP.RET_NullVector, 0.0#, PP.RET_NullVector}

                Dim result = nl2.Flash_PV(Vzns, P, V, Tref, PP, ReuseKI, PrevKi)

                Dim T, L1, L2, Vx(), Vx2(), Vy() As Double

                L1 = result(0)
                Vx = result(2)
                Vy = result(3)
                T = result(4)
                L2 = result(7)
                Vx2 = result(8)

                Return New Object() {L1 * (1 - S), V * (1 - S), Vx, Vy, T, result(5), result(6), L2 * (1 - S), Vx2, S, Vs}

            Else

                Return nl2.Flash_PV(Vz, P, V, Tref, PP, ReuseKI, PrevKi)

            End If

        End Function

        Public Overrides Function Flash_TV(Vz() As Double, T As Double, V As Double, Pref As Double, PP As PropertyPackage, Optional ReuseKI As Boolean = False, Optional PrevKi() As Double = Nothing) As Object
            Return nl2.Flash_TV(Vz, T, V, Pref, PP, ReuseKI, PrevKi)
        End Function

    End Class

End Namespace


