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
Imports DWSIM.MathOps.MathEx
Imports System.Linq

Namespace PropertyPackages.Auxiliary

    <System.Serializable()> Public Class Unifac

        Implements Auxiliary.IActivityCoefficientBase

        Public UnifGroups As UnifacGroups

        Sub New()

            UnifGroups = New UnifacGroups(False)

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

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "GAMMA_MR", "UNIFAC Activity Coefficient", "UNIFAC Activity Coefficient Calculation Routine")

            IObj?.SetCurrent

            IObj?.Paragraphs.Add("The UNIFAC method is based on the UNIQUAC equation. When applied to a solution of groups, the equations are written in the form:")

            IObj?.Paragraphs.Add("<m>\ln\gamma_{i}^{C}=1-J_{i}+\ln J_{i}-5q_{i}(1-J_{i}/L_{i}+\ln J_{i}/L_{i})<m>")

            IObj?.Paragraphs.Add("<m>\ln\gamma_{i}^{R}=q_{i}(1-\sum_{k}(\theta_{k}\beta_{ik}/s_{k})-e_{ki}ln\beta_{ik}/s_{k})<m>")

            IObj?.Paragraphs.Add("Furthermore, the following definitions apply:")

            IObj?.Paragraphs.Add("<m>r_{i}=\sum_{k}\nu_{k}^{(i)}R_{k}<m>")

            IObj?.Paragraphs.Add("<m>q_{i}=\sum_{k}\nu_{k}^{(i)}Q_{k}<m>")

            IObj?.Paragraphs.Add("<m>e_{ki}=(\nu_{k}^{(i)}Q_{k})/q_{i}<m>")

            IObj?.Paragraphs.Add("<m>\beta_{ik}=\sum_{m}e_{mk}\tau_{mk}<m>")

            IObj?.Paragraphs.Add("<m>\theta_{k}=(\sum_{i}x_{i}q_{i}e_{ki})/(\sum_{i}x_{j}q_{j})<m>")

            IObj?.Paragraphs.Add("<m>s_{k}=\sum_{m}\theta_{m}\tau_{mk}<m>")

            IObj?.Paragraphs.Add("<m>s_{i}=\sum_{l}\theta_{l}\tau_{li}<m>")

            IObj?.Paragraphs.Add("<m>\tau_{mk}=\exp(-a_{mk})/T<m>")

            IObj?.Paragraphs.Add("The i subscript identify the species, and j is an index that goes 
                                through all the species. The k subscript identify the subgroups, 
                                and m is an index that goes through all the subgroups. The 
                                parameter <mi>\nu_{k}^{(i)}<mi> is the number of the k subgroup in a 
                                molecule of the i species. The subgroup parameter values <mi>R_{k}<mi> 
                                and <mi>Q_{k}<mi> and the interaction parameters <mi>-a_{mk}<mi> are obtained in 
                                the literature. ")

            IObj?.Paragraphs.Add("<h3> Modified UNIFAC (Dortmund) model</h3>")

            IObj?.Paragraphs.Add("The UNIFAC model, despite being widely used in various 
                                applications, has some limitations which are, in some way, 
                                inherent to the model. Some of these limitations are:")

            IObj?.Paragraphs.Add("1. UNIFAC is unable to distinguish between some types of isomers. ")

            IObj?.Paragraphs.Add("2. The <m>\gamma-\phi<m> approach limits the use of UNIFAC for 
                                applications under the pressure range of 10-15 atm. ")

            IObj?.Paragraphs.Add("3. The temperature is limited within the range of approximately 
                                275-425 K. ")

            IObj?.Paragraphs.Add("4. Non-condensable gases and supercritical components are not 
                                included. ")

            IObj?.Paragraphs.Add("5. Proximity effects are not taken into account. ")

            IObj?.Paragraphs.Add("6. The parameters of liquid-liquid equilibrium are different from 
                                those of vapor-liquid equilibrium. ")

            IObj?.Paragraphs.Add("7. Polymers are not included. ")

            IObj?.Paragraphs.Add("8. Electrolytes are not included.")

            IObj?.Paragraphs.Add("Some of these limitations can be overcome. The insensitivity of 
                                some types of isomers can be eliminated through a careful choice 
                                of the groups used to represent the molecules. The fact that the 
                                parameters for the liquid-liquid equilibrium are different from 
                                those for the vapor-liquid equilibrium seems not to have a 
                                theoretical solution at this time. One solution is to use both 
                                data from both equiibria to determine the parameters as a 
                                modified UNIFAC model. The limitations on the pressure and 
                                temperature can be overcome if the UNIFAC model is used with 
                                equations of state, which carry with them the dependencies of 
                                pressure and temperature.")

            IObj?.Paragraphs.Add("These limitations of the original UNIFAC model have led several 
                                authors to propose changes in both combinatorial and the residual 
                                parts. To modify the combinatorial part, the basis is the 
                                suggestion given by Kikic et al. (1980) in the sense that the 
                                Staverman-Guggenheim correction on the original term of 
                                Flory-Huggins is very small and can, in most cases, be neglected. 
                                As a result, this correction was empirically removed from the 
                                UNIFAC model. Among these modifications, the proposed by Gmehling 
                                and coworkers [Weidlich and Gmehling, 1986; Weidlich and 
                                Gmehling, 1987; Gmehling et al., 1993], known as the model 
                                UNIFAC-Dortmund, is one of the most promising. In this model, the 
                                combinatorial part of the original UNIFAC is replaced by:")

            IObj?.Paragraphs.Add("<m>\ln\gamma_{i}^{C}=1-J_{i}+\ln J_{i}-5q_{i}(1-J_{i}/L_{i}+\ln J_{i}/L_{i})<m>")

            IObj?.Paragraphs.Add("<m>J_{i}=r_{i}^{3/4}/(\sum_{j}r_{j}^{3/4}x_{j})<m>")

            IObj?.Paragraphs.Add("where the remaining quantities is defined the same way as in the original UNIFAC. 
                                Thus, the correction in-Staverman Guggenheim is empirically taken 
                                from the template. It is important to note that the in the 
                                UNIFAC-Dortmund model, the quantities <mi>R_{k}<m> and <mi>Q_{k}<m> are no 
                                longer calculated on the volume and surface area of Van der Waals 
                                forces, as proposed by Bondi (1968), but are additional 
                                adjustable parameters of the model.")

            IObj?.Paragraphs.Add("The residual part is still given by the solution for groups, just 
                                as in the original UNIFAC, but now the parameters of group 
                                interaction are considered temperature dependent, according to:")

            IObj?.Paragraphs.Add("<m>\tau_{mk}=\exp(-a_{mk}^{(0)}+a_{mk}^{(1)}T+a_{mk}^{(2)}T^{2})/T<m>")

            IObj?.Paragraphs.Add("These parameters must be estimated from experimental phase 
                                equilibrium data. Gmehling et al. (1993) presented an array of 
                                parameters for 45 major groups, adjusted using data from the 
                                vapor-liquid equilibrium, excess enthalpies, activity 
                                coefficients at infinite dilution and liquid-liquid equilibrium. 
                                enthalpy and entropy of liquid and vapor.")

            IObj?.Paragraphs.Add("<h3>Modified UNIFAC (NIST) model</h3>")

            IObj?.Paragraphs.Add("This model [7] is similar to the Modified UNIFAC (Dortmund), with 
                                new modified UNIFAC parameters reported for 89 main groups and 
                                984 group–group interactions using critically evaluated phase 
                                equilibrium data including vapor–liquid equilibrium (VLE), 
                                liquid–liquid equilibrium (LLE), solid–liquid equilibrium (SLE), 
                                excess enthalpy (HE), infinite dilution activity coefficient 
                                (AINF) and excess heat capacity (CPE) data. A new algorithmic 
                                framework for quality assessment of phase equilibrium data was 
                                applied for qualifying the consistency of data and screening out 
                                possible erroneous data. Substantial improvement over previous 
                                versions of UNIFAC is observed due to inclusion of experimental 
                                data from recent publications and proper weighting based on a 
                                quality assessment procedure. The systems requiring further 
                                verification of phase equilibrium data were identified where 
                                insufficient number of experimental data points is available or 
                                where existing data are conflicting.")

            IObj?.Paragraphs.Add(String.Format("<h2>Input Parameters</h2>"))

            IObj?.Paragraphs.Add(String.Format("Temperature: {0} K", T))
            IObj?.Paragraphs.Add(String.Format("Mole Fractions: {0}", Vx.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Q: {0}", VQ.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("R: {0}", VR.ToMathArrayString))

            IObj?.Paragraphs.Add(String.Format("<h2>Calculated Intermediate Parameters</h2>"))

            CheckParameters(VEKI)

            Dim i, m, k As Integer

            Dim n = Vx.Length - 1

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

            IObj?.Paragraphs.Add(String.Format("<h2>Results</h2>"))

            IObj?.Paragraphs.Add(String.Format("ln gamma c: {0}", Vgammac.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("ln gamma r: {0}", Vgammar.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("ln gamma : {0}", Vgamma.LogY.ToMathArrayString))

            IObj?.Paragraphs.Add(String.Format("Activity Coefficients: {0}", Vgamma.ToMathArrayString))

            IObj?.Close()

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
                        If g1 <> g2 Then
                            If Me.UnifGroups.InteracParam.ContainsKey(g1) Then
                                If Not Me.UnifGroups.InteracParam(g1).ContainsKey(g2) Then
                                    If Me.UnifGroups.InteracParam.ContainsKey(g2) Then
                                        If Not Me.UnifGroups.InteracParam(g2).ContainsKey(g1) Then
                                            Throw New Exception("UNIFAC Error: Could not find interaction parameter for groups " & Me.UnifGroups.Groups(id1).GroupName & " / " &
                                                            Me.UnifGroups.Groups(id2).GroupName & ". Activity coefficient calculation will give you inconsistent results for this system.")
                                        End If
                                    End If
                                End If
                            Else
                                If Me.UnifGroups.InteracParam.ContainsKey(g2) Then
                                    If Not Me.UnifGroups.InteracParam(g2).ContainsKey(g1) Then
                                        Throw New Exception("UNIFAC Error: Could not find interaction parameter for groups " & Me.UnifGroups.Groups(id1 + 1).GroupName & " / " &
                                                        Me.UnifGroups.Groups(id2).GroupName & ". Activity coefficient calculation will give you inconsistent results for this system.")
                                    End If
                                Else
                                    Throw New Exception("UNIFAC Error: Could not find interaction parameter for groups " & Me.UnifGroups.Groups(id1 + 1).GroupName & " / " &
                                                    Me.UnifGroups.Groups(id2).GroupName & ". Activity coefficient calculation will give you inconsistent results for this system.")
                                End If
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

        Function RET_VN(ByVal cp As Interfaces.ICompoundConstantProperties) As Dictionary(Of Integer, Double)

            Dim res As New Dictionary(Of Integer, Double)
            For Each s As String In cp.UNIFACGroups.Keys
                res.Add(s, cp.UNIFACGroups(s))
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

        Inherits Unifac

        Sub New()

            UnifGroups = New UnifacGroups(True)

        End Sub

    End Class

    <System.Serializable()> Public Class UnifacGroups

        Public InteracParam As System.Collections.Generic.Dictionary(Of Integer, System.Collections.Generic.Dictionary(Of Integer, Double))
        Protected m_groups As System.Collections.Generic.Dictionary(Of Integer, UnifacGroup)

        Sub New(ll As Boolean)

            Dim pathsep = System.IO.Path.DirectorySeparatorChar

            m_groups = New System.Collections.Generic.Dictionary(Of Integer, UnifacGroup)
            InteracParam = New System.Collections.Generic.Dictionary(Of Integer, System.Collections.Generic.Dictionary(Of Integer, Double))

            Dim cult As Globalization.CultureInfo = New Globalization.CultureInfo("en-US")

            Dim fields As String()
            Dim delimiter As String = ","
            Using filestr As IO.Stream = System.Reflection.Assembly.GetAssembly(Me.GetType).GetManifestResourceStream("DWSIM.Thermodynamics.unifac.txt")
                Using parser As New TextFieldParser(filestr)
                    parser.SetDelimiters(delimiter)
                    fields = parser.ReadFields()
                    fields = parser.ReadFields()
                    While Not parser.EndOfData
                        fields = parser.ReadFields()
                        Me.Groups.Add(fields(1), New UnifacGroup(fields(2), fields(3), fields(0), fields(1), Double.Parse(fields(4), cult), Double.Parse(fields(5), cult)))
                    End While
                End Using
            End Using

            Using filestr As IO.Stream = System.Reflection.Assembly.GetAssembly(Me.GetType).GetManifestResourceStream("DWSIM.Thermodynamics.unifac_ip.txt")
                    Using parser As New TextFieldParser(filestr)
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
                End Using

            If ll Then

                Using filestr As IO.Stream = System.Reflection.Assembly.GetAssembly(Me.GetType).GetManifestResourceStream("DWSIM.Thermodynamics.unifac_ll_ip.txt")
                    Using parser As New TextFieldParser(filestr)
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
                End Using

            End If

            'load user database interactions
            If Not GlobalSettings.Settings.UserInteractionsDatabases Is Nothing Then
                For Each IPDBPath As String In GlobalSettings.Settings.UserInteractionsDatabases
                    Dim Interactions As BaseClasses.InteractionParameter()
                    Dim IP As BaseClasses.InteractionParameter
                    Try
                        Interactions = Databases.UserIPDB.ReadInteractions(IPDBPath, "UNIFAC")
                        For Each IP In Interactions
                            If Not Me.InteracParam.ContainsKey(IP.Comp1) Then
                                Me.InteracParam.Add(IP.Comp1, New System.Collections.Generic.Dictionary(Of Integer, Double))
                            End If
                            If Not Me.InteracParam.ContainsKey(IP.Comp2) Then
                                Me.InteracParam.Add(IP.Comp2, New System.Collections.Generic.Dictionary(Of Integer, Double))
                            End If
                            If Not Me.InteracParam(IP.Comp1).ContainsKey(IP.Comp2) Then
                                Me.InteracParam(IP.Comp1).Add(IP.Comp2, 0)
                            End If
                            If Not Me.InteracParam(IP.Comp2).ContainsKey(IP.Comp1) Then
                                Me.InteracParam(IP.Comp2).Add(IP.Comp1, 0)
                            End If
                            Me.InteracParam(IP.Comp1)(IP.Comp2) = IP.Parameters("aij")
                            Me.InteracParam(IP.Comp2)(IP.Comp1) = IP.Parameters("aji")
                        Next
                    Catch ex As Exception
                        Console.WriteLine(ex.ToString)
                    End Try
                Next
            End If

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