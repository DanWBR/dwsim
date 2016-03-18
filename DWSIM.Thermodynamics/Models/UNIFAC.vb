'    UNIFAC Property Package 
'    Copyright 2008-2011 Daniel Wagner O. de Medeiros
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

Imports Microsoft.VisualBasic.FileIO
Imports DWSIM.Thermodynamics.MathEx
Imports System.Linq

Namespace PropertyPackages.Auxiliary

    <System.Serializable()> Public Class Unifac

        Implements Auxiliary.IActivityCoefficientBase

        Public UnifGroups As UnifacGroups

        Sub New()

            UnifGroups = New UnifacGroups

        End Sub

        Function ID2Group(ByVal id As Integer) As String

            For Each group As UnifacGroup In Me.UnifGroups.Groups.Values
                If group.Secondary_Group = id Then Return group.GroupName
            Next

            Return ""

        End Function

        Function Group2ID(ByVal groupname As String) As String

            For Each group As UnifacGroup In Me.UnifGroups.Groups.Values
                If group.GroupName = groupname Then
                    Return group.Secondary_Group
                End If
            Next

            Return 0

        End Function

        Function GAMMA_MR(ByVal T As Double, ByVal Vx As Double(), ByVal VQ As Double(), ByVal VR As Double(), ByVal VEKI As List(Of Dictionary(Of Integer, Double))) As Double()

            CheckParameters(VEKI)

            Dim i, m, k As Integer

            Dim n = UBound(Vx)

            Dim Vgammac(n), Vgammar(n), Vgamma(n) As Double
            Dim Q(n), R(n), j(n), L(n), val As Double

            Dim teta, s As New Dictionary(Of Integer, Double)
            Dim beta As New List(Of Dictionary(Of Integer, Double))

            i = 0
            For Each item In VEKI
                beta.Add(New Dictionary(Of Integer, Double))
                For Each item2 In VEKI
                    For Each item3 In item2
                        If Not beta(i).ContainsKey(item3.Key) Then beta(i).Add(item3.Key, 0.0#)
                    Next
                Next
                i += 1
            Next

            Dim ids As Integer() = beta(0).Keys.ToArray

            Dim n2 As Integer = ids.Length - 1

            For i = 0 To n
                For m = 0 To n2
                    For k = 0 To n2
                        If VEKI(i).ContainsKey(ids(k)) Then beta(i)(ids(m)) += VEKI(i)(ids(k)) * TAU(ids(k), ids(m), T)
                    Next
                Next
            Next

            i = 0
            For Each item In VEKI
                For Each item2 In item
                    For Each item3 In item
                    Next
                Next
                i += 1
            Next

            Dim soma_xq = 0.0#
            i = 0
            Do
                Q(i) = VQ(i)
                soma_xq = soma_xq + Vx(i) * Q(i)
                i = i + 1
            Loop Until i = n + 1

            i = 0
            For Each item In VEKI
                For Each item2 In item
                    val = Vx(i) * Q(i) * VEKI(i)(item2.Key) / soma_xq
                    If Not teta.ContainsKey(item2.Key) Then teta.Add(item2.Key, val) Else teta(item2.Key) += val
                Next
                i += 1
            Next

            For Each item In teta
                For Each item2 In teta
                    val = teta(item2.Key) * TAU(item2.Key, item.Key, T)
                    If Not s.ContainsKey(item.Key) Then s.Add(item.Key, val) Else s(item.Key) += val
                Next
            Next

            Dim soma_xr = 0.0#
            i = 0
            Do
                R(i) = VR(i)
                soma_xr = soma_xr + Vx(i) * R(i)
                i = i + 1
            Loop Until i = n + 1

            i = 0
            Do
                j(i) = R(i) / soma_xr
                L(i) = Q(i) / soma_xq
                Vgammac(i) = 1 - j(i) + Math.Log(j(i)) - 5 * Q(i) * (1 - j(i) / L(i) + Math.Log(j(i) / L(i)))
                k = 0
                Dim tmpsum = 0.0#
                For Each item2 In teta
                    If VEKI(i).ContainsKey(item2.Key) Then
                        tmpsum += item2.Value * beta(i)(item2.Key) / s(item2.Key) - VEKI(i)(item2.Key) * Math.Log(beta(i)(item2.Key) / s(item2.Key))
                    Else
                        tmpsum += item2.Value * beta(i)(item2.Key) / s(item2.Key)
                    End If
                Next
                Vgammar(i) = Q(i) * (1 - tmpsum)
                Vgamma(i) = Math.Exp(Vgammac(i) + Vgammar(i))
                If Vgamma(i) = 0 Then Vgamma(i) = 0.000001
                i = i + 1
            Loop Until i = n + 1

            Return Vgamma

        End Function

        Sub CheckParameters(ByVal VEKI)

            Dim ids As New ArrayList

            For Each item In VEKI
                For Each item2 In item
                    If item(item2.Key) <> 0.0# And Not ids.Contains(item2.Key) Then ids.Add(item2.Key)
                Next
            Next

            For Each id1 As Integer In ids
                For Each id2 As Integer In ids
                    If id1 <> id2 Then
                        Dim g1, g2 As Integer
                        g1 = Me.UnifGroups.Groups(id1).PrimaryGroup
                        g2 = Me.UnifGroups.Groups(id2).PrimaryGroup
                        If Me.UnifGroups.InteracParam.ContainsKey(g1) Then
                            If Not Me.UnifGroups.InteracParam(g1).ContainsKey(g2) Then
                                If Me.UnifGroups.InteracParam.ContainsKey(g2) Then
                                    If Not Me.UnifGroups.InteracParam(g2).ContainsKey(g1) Then
                                        Throw New Exception("UNIFAC Error: Could not find interaction parameter for groups " & Me.UnifGroups.Groups(id1 + 1).GroupName & " / " & _
                                                            Me.UnifGroups.Groups(id2 + 1).GroupName & ". Activity coefficient calculation will give you inconsistent results for this system.")
                                    End If
                                End If
                            End If
                        Else
                            If Me.UnifGroups.InteracParam.ContainsKey(g2) Then
                                If Not Me.UnifGroups.InteracParam(g2).ContainsKey(g1) Then
                                    Throw New Exception("UNIFAC Error: Could not find interaction parameter for groups " & Me.UnifGroups.Groups(id1 + 1).GroupName & " / " & _
                                                        Me.UnifGroups.Groups(id2 + 1).GroupName & ". Activity coefficient calculation will give you inconsistent results for this system.")
                                End If
                            Else
                                Throw New Exception("UNIFAC Error: Could not find interaction parameter for groups " & Me.UnifGroups.Groups(id1 + 1).GroupName & " / " & _
                                                    Me.UnifGroups.Groups(id2 + 1).GroupName & ". Activity coefficient calculation will give you inconsistent results for this system.")
                            End If
                        End If
                    End If
                Next
            Next

        End Sub

        Function TAU(ByVal group_1, ByVal group_2, ByVal T)

            Dim g1, g2 As Integer
            Dim res As Double
            g1 = Me.UnifGroups.Groups(group_1).PrimaryGroup
            g2 = Me.UnifGroups.Groups(group_2).PrimaryGroup

            If Me.UnifGroups.InteracParam.ContainsKey(g1) Then
                If Me.UnifGroups.InteracParam(g1).ContainsKey(g2) Then
                    res = Me.UnifGroups.InteracParam(g1)(g2)
                Else
                    res = 0
                End If
            Else
                res = 0
            End If

            Return Math.Exp(-res / T)

        End Function

        Function RET_Ri(ByVal VN As Dictionary(Of Integer, Double)) As Double

            Dim res As Double
            For Each kvp In VN
                res += Me.UnifGroups.Groups(kvp.Key).R * VN(kvp.Key)
            Next

            Return res

        End Function

        Function RET_Qi(ByVal VN As Dictionary(Of Integer, Double)) As Double

            Dim res As Double
            For Each kvp In VN
                res += Me.UnifGroups.Groups(kvp.Key).Q * VN(kvp.Key)
            Next

            Return res

        End Function

        Function RET_EKI(ByVal VN As Dictionary(Of Integer, Double), ByVal Q As Double) As Dictionary(Of Integer, Double)

            Dim res As New Dictionary(Of Integer, Double)
            For Each kvp In VN
                res.Add(kvp.Key, Me.UnifGroups.Groups(kvp.Key).Q * VN(kvp.Key) / Q)
            Next

            Return res

        End Function

        Function RET_VN(ByVal cp As BaseClasses.ConstantProperties) As Dictionary(Of Integer, Double)

            Dim res As New Dictionary(Of Integer, Double)
            For Each s As String In cp.UNIFACGroups.Collection.Keys
                res.Add(s, cp.UNIFACGroups.Collection(s))
            Next

            Return res

        End Function

        Function DLNGAMMA_DT(ByVal T As Double, ByVal Vx As Array, ByVal VQ As Double(), ByVal VR As Double(), ByVal VEKI As List(Of Dictionary(Of Integer, Double))) As Array

            Dim gamma1, gamma2 As Double()

            Dim epsilon As Double = 0.001

            gamma1 = GAMMA_MR(T, Vx, VQ, VR, VEKI)
            gamma2 = GAMMA_MR(T + epsilon, Vx, VQ, VR, VEKI)

            Dim dgamma(gamma1.Length - 1) As Double

            For i As Integer = 0 To Vx.Length - 1
                dgamma(i) = (gamma2(i) - gamma1(i)) / (epsilon)
            Next

            Return dgamma

        End Function

        Function HEX_MIX(ByVal T As Double, ByVal Vx As Array, ByVal VQ As Double(), ByVal VR As Double(), ByVal VEKI As List(Of Dictionary(Of Integer, Double))) As Double

            Dim dgamma As Double() = DLNGAMMA_DT(T, Vx, VQ, VR, VEKI)

            Dim hex As Double = 0.0#

            For i As Integer = 0 To Vx.Length - 1
                hex += -8.314 * T ^ 2 * Vx(i) * dgamma(i)
            Next

            Return hex 'kJ/kmol

        End Function

        Function CPEX_MIX(ByVal T As Double, ByVal Vx As Array, ByVal VQ As Double(), ByVal VR As Double(), ByVal VEKI As List(Of Dictionary(Of Integer, Double))) As Double

            Dim hex1, hex2, cpex As Double

            Dim epsilon As Double = 0.001

            hex1 = HEX_MIX(T, Vx, VQ, VR, VEKI)
            hex2 = HEX_MIX(T + epsilon, Vx, VQ, VR, VEKI)

            cpex = (hex2 - hex1) / epsilon

            Return cpex 'kJ/kmol.K

        End Function


        Public Function CalcActivityCoefficients(T As Double, Vx As Array, otherargs As Object) As Array Implements IActivityCoefficientBase.CalcActivityCoefficients

            Return GAMMA_MR(T, Vx, otherargs(0), otherargs(1), otherargs(2))

        End Function

        Public Function CalcExcessEnthalpy(T As Double, Vx As Array, otherargs As Object) As Double Implements IActivityCoefficientBase.CalcExcessEnthalpy

            Return HEX_MIX(T, Vx, otherargs(0), otherargs(1), otherargs(2))

        End Function

        Public Function CalcExcessHeatCapacity(T As Double, Vx As Array, otherargs As Object) As Double Implements IActivityCoefficientBase.CalcExcessHeatCapacity

            Return CPEX_MIX(T, Vx, otherargs(0), otherargs(1), otherargs(2))

        End Function

    End Class

    <System.Serializable()> Public Class UnifacLL

        Implements Auxiliary.IActivityCoefficientBase

        Public UnifGroups As UnifacGroupsLL

        Sub New()

            UnifGroups = New UnifacGroupsLL

        End Sub

        Function ID2Group(ByVal id As Integer) As String

            For Each group As UnifacGroup In Me.UnifGroups.Groups.Values
                If group.Secondary_Group = id Then Return group.GroupName
            Next

            Return ""

        End Function

        Function Group2ID(ByVal groupname As String) As String

            For Each group As UnifacGroup In Me.UnifGroups.Groups.Values
                If group.GroupName = groupname Then
                    Return group.Secondary_Group
                End If
            Next

            Return 0

        End Function

        Function GAMMA(ByVal T, ByVal Vx, ByVal VQ, ByVal VR, ByVal VEKI, ByVal index)

            CheckParameters(VEKI)

            Dim Q(), R(), j(), L()
            Dim i, k, m As Integer

            Dim n = UBound(Vx)
            Dim n2 = UBound(VEKI, 2)

            Dim beta(,), teta(n2), s(n2), Vgammac(), Vgammar(), Vgamma(), b(,)
            ReDim Preserve beta(n, n2), Vgammac(n), Vgammar(n), Vgamma(n), b(n, n2)
            ReDim Q(n), R(n), j(n), L(n)

            i = 0
            Do
                k = 0
                Do
                    beta(i, k) = 0
                    m = 0
                    Do
                        beta(i, k) = beta(i, k) + VEKI(i, m) * TAU(m, k, T)
                        m = m + 1
                    Loop Until m = n2 + 1
                    k = k + 1
                Loop Until k = n2 + 1
                i = i + 1
            Loop Until i = n + 1

            Dim soma_xq = 0.0#
            i = 0
            Do
                Q(i) = VQ(i)
                soma_xq = soma_xq + Vx(i) * Q(i)
                i = i + 1
            Loop Until i = n + 1

            k = 0
            Do
                i = 0
                Do
                    teta(k) = teta(k) + Vx(i) * Q(i) * VEKI(i, k)
                    i = i + 1
                Loop Until i = n + 1
                teta(k) = teta(k) / soma_xq
                k = k + 1
            Loop Until k = n2 + 1

            k = 0
            Do
                m = 0
                Do
                    s(k) = s(k) + teta(m) * TAU(m, k, T)
                    m = m + 1
                Loop Until m = n2 + 1
                k = k + 1
            Loop Until k = n2 + 1

            Dim soma_xr = 0.0#
            i = 0
            Do
                R(i) = VR(i)
                soma_xr = soma_xr + Vx(i) * R(i)
                i = i + 1
            Loop Until i = n + 1

            i = 0
            Do
                j(i) = R(i) / soma_xr
                L(i) = Q(i) / soma_xq
                Vgammac(i) = 1 - j(i) + Math.Log(j(i)) - 5 * Q(i) * (1 - j(i) / L(i) + Math.Log(j(i) / L(i)))
                k = 0
                Dim tmpsum = 0.0#
                Do
                    tmpsum = tmpsum + teta(k) * beta(i, k) / s(k) - VEKI(i, k) * Math.Log(beta(i, k) / s(k))
                    k = k + 1
                Loop Until k = n2 + 1
                Vgammar(i) = Q(i) * (1 - tmpsum)
                Vgamma(i) = Math.Exp(Vgammac(i) + Vgammar(i))
                If Vgamma(i) = 0 Then Vgamma(i) = 0.000001
                i = i + 1
            Loop Until i = n + 1

            i = 1
            k = 1
            GAMMA = Vgamma(index)

        End Function

        Function GAMMA_MR(ByVal T, ByVal Vx, ByVal VQ, ByVal VR, ByVal VEKI)

            CheckParameters(VEKI)

            Dim i, k, m As Integer

            Dim n = UBound(Vx)
            Dim n2 = UBound(VEKI, 2)

            Dim teta(n2), s(n2) As Double
            Dim beta(n, n2), Vgammac(n), Vgammar(n), Vgamma(n), b(n, n2) As Double
            Dim Q(n), R(n), j(n), L(n) As Double

            i = 0
            Do
                k = 0
                Do
                    beta(i, k) = 0
                    m = 0
                    Do
                        beta(i, k) = beta(i, k) + VEKI(i, m) * TAU(m, k, T)
                        m = m + 1
                    Loop Until m = n2 + 1
                    k = k + 1
                Loop Until k = n2 + 1
                i = i + 1
            Loop Until i = n + 1

            Dim soma_xq = 0.0#
            i = 0
            Do
                Q(i) = VQ(i)
                soma_xq = soma_xq + Vx(i) * Q(i)
                i = i + 1
            Loop Until i = n + 1

            k = 0
            Do
                i = 0
                Do
                    teta(k) = teta(k) + Vx(i) * Q(i) * VEKI(i, k)
                    i = i + 1
                Loop Until i = n + 1
                teta(k) = teta(k) / soma_xq
                k = k + 1
            Loop Until k = n2 + 1

            k = 0
            Do
                m = 0
                Do
                    s(k) = s(k) + teta(m) * TAU(m, k, T)
                    m = m + 1
                Loop Until m = n2 + 1
                k = k + 1
            Loop Until k = n2 + 1

            Dim soma_xr = 0.0#
            i = 0
            Do
                R(i) = VR(i)
                soma_xr = soma_xr + Vx(i) * R(i)
                i = i + 1
            Loop Until i = n + 1

            i = 0
            Do
                j(i) = R(i) / soma_xr
                L(i) = Q(i) / soma_xq
                Vgammac(i) = 1 - j(i) + Math.Log(j(i)) - 5 * Q(i) * (1 - j(i) / L(i) + Math.Log(j(i) / L(i)))
                k = 0
                Dim tmpsum = 0.0#
                Do
                    tmpsum = tmpsum + teta(k) * beta(i, k) / s(k) - VEKI(i, k) * Math.Log(beta(i, k) / s(k))
                    k = k + 1
                Loop Until k = n2 + 1
                Vgammar(i) = Q(i) * (1 - tmpsum)
                Vgamma(i) = Math.Exp(Vgammac(i) + Vgammar(i))
                If Vgamma(i) = 0 Then Vgamma(i) = 0.000001
                i = i + 1
            Loop Until i = n + 1

            Return Vgamma

        End Function

        Sub CheckParameters(ByVal VEKI)

            Dim ids As New ArrayList

            For Each item In VEKI
                For Each item2 In item
                    If item(item2.Key) <> 0.0# And Not ids.Contains(item2.Key) Then ids.Add(item2.Key)
                Next
            Next

            For Each id1 As Integer In ids
                For Each id2 As Integer In ids
                    If id1 <> id2 Then
                        Dim g1, g2 As Integer
                        g1 = Me.UnifGroups.Groups(id1).PrimaryGroup
                        g2 = Me.UnifGroups.Groups(id2).PrimaryGroup
                        If Me.UnifGroups.InteracParam.ContainsKey(g1) Then
                            If Not Me.UnifGroups.InteracParam(g1).ContainsKey(g2) Then
                                If Me.UnifGroups.InteracParam.ContainsKey(g2) Then
                                    If Not Me.UnifGroups.InteracParam(g2).ContainsKey(g1) Then
                                        Throw New Exception("UNIFAC Error: Could not find interaction parameter for groups " & Me.UnifGroups.Groups(id1 + 1).GroupName & " / " & _
                                                            Me.UnifGroups.Groups(id2 + 1).GroupName & ". Activity coefficient calculation will give you inconsistent results for this system.")
                                    End If
                                End If
                            End If
                        Else
                            If Me.UnifGroups.InteracParam.ContainsKey(g2) Then
                                If Not Me.UnifGroups.InteracParam(g2).ContainsKey(g1) Then
                                    Throw New Exception("UNIFAC Error: Could not find interaction parameter for groups " & Me.UnifGroups.Groups(id1 + 1).GroupName & " / " & _
                                                        Me.UnifGroups.Groups(id2 + 1).GroupName & ". Activity coefficient calculation will give you inconsistent results for this system.")
                                End If
                            Else
                                Throw New Exception("UNIFAC Error: Could not find interaction parameter for groups " & Me.UnifGroups.Groups(id1 + 1).GroupName & " / " & _
                                                    Me.UnifGroups.Groups(id2 + 1).GroupName & ". Activity coefficient calculation will give you inconsistent results for this system.")
                            End If
                        End If
                    End If
                Next
            Next

        End Sub

        Function TAU(ByVal group_1, ByVal group_2, ByVal T)

            Dim g1, g2 As Integer
            Dim res As Double
            g1 = Me.UnifGroups.Groups(group_1 + 1).PrimaryGroup
            g2 = Me.UnifGroups.Groups(group_2 + 1).PrimaryGroup

            If Me.UnifGroups.InteracParam.ContainsKey(g1) Then
                If Me.UnifGroups.InteracParam(g1).ContainsKey(g2) Then
                    res = Me.UnifGroups.InteracParam(g1)(g2)
                Else
                    res = 0
                End If
            Else
                res = 0
            End If

            Return Math.Exp(-res / T)

        End Function


        Function RET_Ri(ByVal VN As Dictionary(Of Integer, Double)) As Double

            Dim i As Integer = 0
            Dim res As Double

            For Each kvp In VN
                res += Me.UnifGroups.Groups(kvp.Key).R * VN(kvp.Key)
                i += 1
            Next

            Return res

        End Function

        Function RET_Qi(ByVal VN As Dictionary(Of Integer, Double)) As Double

            Dim i As Integer = 0
            Dim res As Double

            For Each kvp In VN
                res += Me.UnifGroups.Groups(kvp.Key).Q * VN(kvp.Key)
                i += 1
            Next

            Return res

        End Function

        Function RET_EKI(ByVal VN As Dictionary(Of Integer, Double), ByVal Q As Double) As Dictionary(Of Integer, Double)

            Dim i As Integer = 0
            Dim res As New Dictionary(Of Integer, Double)

            For Each kvp In VN
                res.Add(kvp.Key, Me.UnifGroups.Groups(kvp.Key).Q * VN(kvp.Key) / Q)
                i += 1
            Next

            Return res

        End Function

        Function RET_VN(ByVal cp As BaseClasses.ConstantProperties) As Dictionary(Of Integer, Double)

            Dim i As Integer = 0
            Dim res As New Dictionary(Of Integer, Double)
            Dim added As Boolean = False

            res.Clear()

            For Each group As UnifacGroup In Me.UnifGroups.Groups.Values
                For Each s As String In cp.MODFACGroups.Collection.Keys
                    If s = group.Secondary_Group Then
                        res.Add(group.Secondary_Group, cp.MODFACGroups.Collection(s))
                        Exit For
                    End If
                Next
            Next

            Return res

        End Function

        Function DLNGAMMA_DT(ByVal T As Double, ByVal Vx As Array, ByVal VQ As Double(), ByVal VR As Double(), ByVal VEKI As List(Of Dictionary(Of Integer, Double))) As Array

            Dim gamma1, gamma2 As Double()

            Dim epsilon As Double = 0.001

            gamma1 = GAMMA_MR(T, Vx, VQ, VR, VEKI)
            gamma2 = GAMMA_MR(T + epsilon, Vx, VQ, VR, VEKI)

            Dim dgamma(gamma1.Length - 1) As Double

            For i As Integer = 0 To Vx.Length - 1
                dgamma(i) = (gamma2(i) - gamma1(i)) / (epsilon)
            Next

            Return dgamma

        End Function

        Function HEX_MIX(ByVal T As Double, ByVal Vx As Array, ByVal VQ As Double(), ByVal VR As Double(), ByVal VEKI As List(Of Dictionary(Of Integer, Double))) As Double

            Dim dgamma As Double() = DLNGAMMA_DT(T, Vx, VQ, VR, VEKI)

            Dim hex As Double = 0.0#

            For i As Integer = 0 To Vx.Length - 1
                hex += -8.314 * T ^ 2 * Vx(i) * dgamma(i)
            Next

            Return hex 'kJ/kmol

        End Function

        Function CPEX_MIX(ByVal T As Double, ByVal Vx As Array, ByVal VQ As Double(), ByVal VR As Double(), ByVal VEKI As List(Of Dictionary(Of Integer, Double))) As Double

            Dim hex1, hex2, cpex As Double

            Dim epsilon As Double = 0.001

            hex1 = HEX_MIX(T, Vx, VQ, VR, VEKI)
            hex2 = HEX_MIX(T + epsilon, Vx, VQ, VR, VEKI)

            cpex = (hex2 - hex1) / epsilon

            Return cpex 'kJ/kmol.K

        End Function

        Public Function CalcActivityCoefficients(T As Double, Vx As Array, otherargs As Object) As Array Implements IActivityCoefficientBase.CalcActivityCoefficients

            Return GAMMA_MR(T, Vx, otherargs(0), otherargs(1), otherargs(2))

        End Function

        Public Function CalcExcessEnthalpy(T As Double, Vx As Array, otherargs As Object) As Double Implements IActivityCoefficientBase.CalcExcessEnthalpy

            Return HEX_MIX(T, Vx, otherargs(0), otherargs(1), otherargs(2))

        End Function

        Public Function CalcExcessHeatCapacity(T As Double, Vx As Array, otherargs As Object) As Double Implements IActivityCoefficientBase.CalcExcessHeatCapacity

            Return CPEX_MIX(T, Vx, otherargs(0), otherargs(1), otherargs(2))

        End Function

    End Class

    <System.Serializable()> Public Class UnifacGroups

        Public InteracParam As System.Collections.Generic.Dictionary(Of Integer, System.Collections.Generic.Dictionary(Of Integer, Double))
        Protected m_groups As System.Collections.Generic.Dictionary(Of Integer, UnifacGroup)

        Sub New()

            Dim pathsep = System.IO.Path.DirectorySeparatorChar

            m_groups = New System.Collections.Generic.Dictionary(Of Integer, UnifacGroup)
            InteracParam = New System.Collections.Generic.Dictionary(Of Integer, System.Collections.Generic.Dictionary(Of Integer, Double))

            Dim cult As Globalization.CultureInfo = New Globalization.CultureInfo("en-US")

            Dim filename As String = My.Application.Info.DirectoryPath & pathsep & "data" & pathsep & "unifac.txt"
            Dim fields As String()
            Dim delimiter As String = ","
            Using parser As New TextFieldParser(filename)
                parser.SetDelimiters(delimiter)
                fields = parser.ReadFields()
                fields = parser.ReadFields()
                While Not parser.EndOfData
                    fields = parser.ReadFields()
                    Me.Groups.Add(fields(1), New UnifacGroup(fields(2), fields(3), fields(0), fields(1), Double.Parse(fields(4), cult), Double.Parse(fields(5), cult)))
                End While
            End Using

            filename = My.Application.Info.DirectoryPath & pathsep & "data" & pathsep & "unifac_ip.txt"
            Using parser As New TextFieldParser(filename)
                delimiter = vbTab
                parser.SetDelimiters(delimiter)
                fields = parser.ReadFields()
                While Not parser.EndOfData
                    fields = parser.ReadFields()
                    If Not Me.InteracParam.ContainsKey(fields(0)) Then
                        Me.InteracParam.Add(fields(0), New System.Collections.Generic.Dictionary(Of Integer, Double))
                        Me.InteracParam(fields(0)).Add(fields(2), Double.Parse(fields(4), cult))
                    Else
                        If Not Me.InteracParam(fields(0)).ContainsKey(fields(2)) Then
                            Me.InteracParam(fields(0)).Add(fields(2), Double.Parse(fields(4), cult))
                        Else
                            Me.InteracParam(fields(0))(fields(2)) = Double.Parse(fields(4), cult)
                        End If
                    End If
                    If Not Me.InteracParam.ContainsKey(fields(2)) Then
                        Me.InteracParam.Add(fields(2), New System.Collections.Generic.Dictionary(Of Integer, Double))
                        Me.InteracParam(fields(2)).Add(fields(0), Double.Parse(fields(5), cult))
                    Else
                        If Not Me.InteracParam(fields(2)).ContainsKey(fields(0)) Then
                            Me.InteracParam(fields(2)).Add(fields(0), Double.Parse(fields(5), cult))
                        Else
                            Me.InteracParam(fields(2))(fields(0)) = Double.Parse(fields(5), cult)
                        End If
                    End If
                End While
            End Using

        End Sub

        Public ReadOnly Property Groups() As System.Collections.Generic.Dictionary(Of Integer, UnifacGroup)
            Get
                Return m_groups
            End Get
        End Property

    End Class

    <System.Serializable()> Public Class UnifacGroupsLL

        Public InteracParam As System.Collections.Generic.Dictionary(Of Integer, System.Collections.Generic.Dictionary(Of Integer, Double))
        Protected m_groups As System.Collections.Generic.Dictionary(Of Integer, UnifacGroup)

        Sub New()

            Dim pathsep = System.IO.Path.DirectorySeparatorChar

            m_groups = New System.Collections.Generic.Dictionary(Of Integer, UnifacGroup)
            InteracParam = New System.Collections.Generic.Dictionary(Of Integer, System.Collections.Generic.Dictionary(Of Integer, Double))

            Dim cult As Globalization.CultureInfo = New Globalization.CultureInfo("en-US")

            Dim filename As String = My.Application.Info.DirectoryPath & pathsep & "data" & pathsep & "unifac.txt"
            Dim fields As String()
            Dim delimiter As String = ","
            Using parser As New TextFieldParser(filename)
                parser.SetDelimiters(delimiter)
                fields = parser.ReadFields()
                fields = parser.ReadFields()
                While Not parser.EndOfData
                    fields = parser.ReadFields()
                    Me.Groups.Add(fields(1), New UnifacGroup(fields(2), fields(3), fields(0), fields(1), Double.Parse(fields(4), cult), Double.Parse(fields(5), cult)))
                End While
            End Using

            filename = My.Application.Info.DirectoryPath & pathsep & "data" & pathsep & "unifac_ll_ip.txt"
            Using parser As New TextFieldParser(filename)
                delimiter = ","
                parser.SetDelimiters(delimiter)
                While Not parser.EndOfData
                    fields = parser.ReadFields()
                    If Not Me.InteracParam.ContainsKey(fields(0)) Then
                        Me.InteracParam.Add(fields(0), New System.Collections.Generic.Dictionary(Of Integer, Double))
                        Me.InteracParam(fields(0)).Add(fields(2), Double.Parse(fields(4), cult))
                    Else
                        If Not Me.InteracParam(fields(0)).ContainsKey(fields(2)) Then
                            Me.InteracParam(fields(0)).Add(fields(2), Double.Parse(fields(4), cult))
                        Else
                            Me.InteracParam(fields(0))(fields(2)) = Double.Parse(fields(4), cult)
                        End If
                    End If
                    If Not Me.InteracParam.ContainsKey(fields(2)) Then
                        Me.InteracParam.Add(fields(2), New System.Collections.Generic.Dictionary(Of Integer, Double))
                        Me.InteracParam(fields(2)).Add(fields(0), Double.Parse(fields(5), cult))
                    Else
                        If Not Me.InteracParam(fields(2)).ContainsKey(fields(0)) Then
                            Me.InteracParam(fields(2)).Add(fields(0), Double.Parse(fields(5), cult))
                        Else
                            Me.InteracParam(fields(2))(fields(0)) = Double.Parse(fields(5), cult)
                        End If
                    End If
                End While
            End Using



        End Sub

        Public ReadOnly Property Groups() As System.Collections.Generic.Dictionary(Of Integer, UnifacGroup)
            Get
                Return m_groups
            End Get
        End Property

    End Class

    <System.Serializable()> Public Class UnifacGroup

        Protected m_primarygroupname As String
        Protected m_groupname As String

        Protected m_main_group As Integer
        Protected m_secondary_group As Integer

        Protected m_r As Double
        Protected m_q As Double
        Public Property PrimGroupName() As String
            Get
                Return m_primarygroupname
            End Get
            Set(ByVal value As String)
                m_primarygroupname = value
            End Set
        End Property
        Public Property GroupName() As String
            Get
                Return m_groupname
            End Get
            Set(ByVal value As String)
                m_groupname = value
            End Set
        End Property

        Public Property PrimaryGroup() As String
            Get
                Return m_main_group
            End Get
            Set(ByVal value As String)
                m_main_group = value
            End Set
        End Property

        Public Property Secondary_Group() As String
            Get
                Return m_secondary_group
            End Get
            Set(ByVal value As String)
                m_secondary_group = value
            End Set
        End Property

        Public Property R() As Double
            Get
                Return m_r
            End Get
            Set(ByVal value As Double)
                m_r = value
            End Set
        End Property

        Public Property Q() As Double
            Get
                Return m_q
            End Get
            Set(ByVal value As Double)
                m_q = value
            End Set
        End Property

        Sub New(ByVal MainGroupName As String, ByVal Name As String, ByVal PrimGroup As String, ByVal SecGroup As String, ByVal R As Double, ByVal Q As Double)
            Me.PrimGroupName = MainGroupName
            Me.GroupName = name
            Me.PrimaryGroup = PrimGroup
            Me.Secondary_Group = SecGroup
            Me.R = R
            Me.Q = Q
        End Sub

        Sub New()
        End Sub

    End Class

End Namespace