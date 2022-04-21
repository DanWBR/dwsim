'    UNIQUAC Property Package 
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

Imports System.Collections.Generic
Imports FileHelpers
Imports System.Threading.Tasks
Imports System.IO

Namespace PropertyPackages.Auxiliary

    <DelimitedRecord(";")> <IgnoreFirst()> <System.Serializable()>
    Public Class UNIQUAC_IPData

        Implements ICloneable

        Public ID1 As Integer = -1
        Public ID2 As Integer = -1
        Public A12 As Double = 0
        Public A21 As Double = 0
        Public comment As String = ""
        <FieldHidden()> Public B12 As Double = 0
        <FieldHidden()> Public B21 As Double = 0
        <FieldHidden()> Public C12 As Double = 0
        <FieldHidden()> Public C21 As Double = 0
        <FieldHidden()> Public Name1 As String = ""
        <FieldHidden()> Public Name2 As String = ""

        Public Function Clone() As Object Implements System.ICloneable.Clone

            Dim newclass As New UNIQUAC_IPData
            With newclass
                .ID1 = Me.ID1
                .ID2 = Me.ID2
                .A12 = Me.A12
                .A21 = Me.A21
                .B12 = Me.B12
                .B21 = Me.B21
                .C12 = Me.C12
                .C21 = Me.C21
                .Name1 = Name1
                .Name2 = Name2
                .comment = Me.comment
            End With
            Return newclass
        End Function

        Public Function CloneToLIQUAC() As LIQUAC2_IPData

            Dim newclass As New LIQUAC2_IPData
            With newclass
                .ID1 = Me.ID1
                .ID2 = Me.ID2
                .Group1 = .ID1
                .Group2 = .ID2
                .A12 = Me.A12
                .A21 = Me.A21
            End With
            Return newclass
        End Function

    End Class

    <System.Serializable()> Public Class UNIQUAC

        Implements IActivityCoefficientBase

        Private _ip As Dictionary(Of String, Dictionary(Of String, UNIQUAC_IPData))

        Public ReadOnly Property InteractionParameters() As Dictionary(Of String, Dictionary(Of String, UNIQUAC_IPData))
            Get
                Return _ip
            End Get
        End Property

        Sub New()

            _ip = New Dictionary(Of String, Dictionary(Of String, UNIQUAC_IPData))

            Dim pathsep As Char = System.IO.Path.DirectorySeparatorChar

            Dim uniquacip As UNIQUAC_IPData
            Dim uniquacipc() As UNIQUAC_IPData
            Dim uniquacipc2() As UNIQUAC_IPData
            Dim fh1 As New FileHelperEngine(Of UNIQUAC_IPData)

            Using filestr As Stream = System.Reflection.Assembly.GetAssembly(Me.GetType).GetManifestResourceStream("DWSIM.Thermodynamics.uniquac.dat")
                Using t As New StreamReader(filestr)
                    uniquacipc = fh1.ReadStream(t)
                End Using
            End Using

            Using filestr As Stream = System.Reflection.Assembly.GetAssembly(Me.GetType).GetManifestResourceStream("DWSIM.Thermodynamics.uniquacip.dat")
                Using t As New StreamReader(filestr)
                    uniquacipc2 = fh1.ReadStream(t)
                End Using
            End Using

            Dim csdb As New ChemSepHelper.ChemSepIDConverter

            'load UNIQUAC.DAT database interactions
            For Each uniquacip In uniquacipc
                If Me.InteractionParameters.ContainsKey(csdb.GetDWSIMName(uniquacip.ID1)) Then
                    If Not Me.InteractionParameters(csdb.GetDWSIMName(uniquacip.ID1)).ContainsKey(csdb.GetDWSIMName(uniquacip.ID2)) Then
                        Me.InteractionParameters(csdb.GetDWSIMName(uniquacip.ID1)).Add(csdb.GetDWSIMName(uniquacip.ID2), uniquacip.Clone)
                    End If
                    If Not Me.InteractionParameters(csdb.GetDWSIMName(uniquacip.ID1)).ContainsKey(csdb.GetCSName(uniquacip.ID2)) Then
                        Me.InteractionParameters(csdb.GetDWSIMName(uniquacip.ID1)).Add(csdb.GetCSName(uniquacip.ID2), uniquacip.Clone)
                    End If
                Else
                    Me.InteractionParameters.Add(csdb.GetDWSIMName(uniquacip.ID1), New Dictionary(Of String, UNIQUAC_IPData))
                    Me.InteractionParameters(csdb.GetDWSIMName(uniquacip.ID1)).Add(csdb.GetDWSIMName(uniquacip.ID2), uniquacip.Clone)
                    If Not Me.InteractionParameters(csdb.GetDWSIMName(uniquacip.ID1)).ContainsKey(csdb.GetCSName(uniquacip.ID2)) Then
                        Me.InteractionParameters(csdb.GetDWSIMName(uniquacip.ID1)).Add(csdb.GetCSName(uniquacip.ID2), uniquacip.Clone)
                    End If
                End If
            Next
            For Each uniquacip In uniquacipc
                If Me.InteractionParameters.ContainsKey(csdb.GetCSName(uniquacip.ID1)) Then
                    If Not Me.InteractionParameters(csdb.GetCSName(uniquacip.ID1)).ContainsKey(csdb.GetCSName(uniquacip.ID2)) Then
                        Me.InteractionParameters(csdb.GetCSName(uniquacip.ID1)).Add(csdb.GetCSName(uniquacip.ID2), uniquacip.Clone)
                    End If
                    If Not Me.InteractionParameters(csdb.GetCSName(uniquacip.ID1)).ContainsKey(csdb.GetDWSIMName(uniquacip.ID2)) Then
                        Me.InteractionParameters(csdb.GetCSName(uniquacip.ID1)).Add(csdb.GetDWSIMName(uniquacip.ID2), uniquacip.Clone)
                    End If
                Else
                    Me.InteractionParameters.Add(csdb.GetCSName(uniquacip.ID1), New Dictionary(Of String, UNIQUAC_IPData))
                    Me.InteractionParameters(csdb.GetCSName(uniquacip.ID1)).Add(csdb.GetCSName(uniquacip.ID2), uniquacip.Clone)
                    If Not Me.InteractionParameters(csdb.GetCSName(uniquacip.ID1)).ContainsKey(csdb.GetDWSIMName(uniquacip.ID2)) Then
                        Me.InteractionParameters(csdb.GetCSName(uniquacip.ID1)).Add(csdb.GetDWSIMName(uniquacip.ID2), uniquacip.Clone)
                    End If
                End If
            Next

            'load UNIQUACIP.DAT database interactions
            'For Each uniquacip In uniquacipc2
            '    uniquacip.A12 *= 1.98721
            '    uniquacip.A21 *= 1.98721
            'Next

            'For Each uniquacip In uniquacipc2
            '    If Me.InteractionParameters.ContainsKey(csdb.GetDWSIMName(uniquacip.ID1)) Then
            '        If Not Me.InteractionParameters(csdb.GetDWSIMName(uniquacip.ID1)).ContainsKey(csdb.GetDWSIMName(uniquacip.ID2)) Then
            '            Me.InteractionParameters(csdb.GetDWSIMName(uniquacip.ID1)).Add(csdb.GetDWSIMName(uniquacip.ID2), uniquacip.Clone)
            '        End If
            '        If Not Me.InteractionParameters(csdb.GetDWSIMName(uniquacip.ID1)).ContainsKey(csdb.GetCSName(uniquacip.ID2)) Then
            '            Me.InteractionParameters(csdb.GetDWSIMName(uniquacip.ID1)).Add(csdb.GetCSName(uniquacip.ID2), uniquacip.Clone)
            '        End If
            '    Else
            '        Me.InteractionParameters.Add(csdb.GetDWSIMName(uniquacip.ID1), New Dictionary(Of String, UNIQUAC_IPData))
            '        Me.InteractionParameters(csdb.GetDWSIMName(uniquacip.ID1)).Add(csdb.GetDWSIMName(uniquacip.ID2), uniquacip.Clone)
            '        If Not Me.InteractionParameters(csdb.GetDWSIMName(uniquacip.ID1)).ContainsKey(csdb.GetCSName(uniquacip.ID2)) Then
            '            Me.InteractionParameters(csdb.GetDWSIMName(uniquacip.ID1)).Add(csdb.GetCSName(uniquacip.ID2), uniquacip.Clone)
            '        End If
            '    End If
            'Next
            'For Each uniquacip In uniquacipc2
            '    If Me.InteractionParameters.ContainsKey(csdb.GetCSName(uniquacip.ID1)) Then
            '        If Not Me.InteractionParameters(csdb.GetCSName(uniquacip.ID1)).ContainsKey(csdb.GetCSName(uniquacip.ID2)) Then
            '            Me.InteractionParameters(csdb.GetCSName(uniquacip.ID1)).Add(csdb.GetCSName(uniquacip.ID2), uniquacip.Clone)
            '        End If
            '        If Not Me.InteractionParameters(csdb.GetCSName(uniquacip.ID1)).ContainsKey(csdb.GetDWSIMName(uniquacip.ID2)) Then
            '            Me.InteractionParameters(csdb.GetCSName(uniquacip.ID1)).Add(csdb.GetDWSIMName(uniquacip.ID2), uniquacip.Clone)
            '        End If
            '    Else
            '        Me.InteractionParameters.Add(csdb.GetCSName(uniquacip.ID1), New Dictionary(Of String, UNIQUAC_IPData))
            '        Me.InteractionParameters(csdb.GetCSName(uniquacip.ID1)).Add(csdb.GetCSName(uniquacip.ID2), uniquacip.Clone)
            '        If Not Me.InteractionParameters(csdb.GetCSName(uniquacip.ID1)).ContainsKey(csdb.GetDWSIMName(uniquacip.ID2)) Then
            '            Me.InteractionParameters(csdb.GetCSName(uniquacip.ID1)).Add(csdb.GetDWSIMName(uniquacip.ID2), uniquacip.Clone)
            '        End If
            '    End If
            'Next

            'load user database interactions
            If Not GlobalSettings.Settings.UserInteractionsDatabases Is Nothing Then
                For Each IPDBPath As String In GlobalSettings.Settings.UserInteractionsDatabases
                    Dim Interactions As BaseClasses.InteractionParameter()
                    Dim IP As BaseClasses.InteractionParameter
                    Try
                        Interactions = Databases.UserIPDB.ReadInteractions(IPDBPath, "UNIQUAC")
                        For Each IP In Interactions
                            Dim IPD As New UNIQUAC_IPData
                            IPD.A12 = IP.Parameters.Item("A12")
                            IPD.A21 = IP.Parameters.Item("A21")
                            IPD.comment = IP.Description
                            If IP.Parameters.ContainsKey("B12") Then IPD.B12 = IP.Parameters.Item("B12")
                            If IP.Parameters.ContainsKey("B21") Then IPD.B21 = IP.Parameters.Item("B21")
                            If IP.Parameters.ContainsKey("C12") Then IPD.C12 = IP.Parameters.Item("C12")
                            If IP.Parameters.ContainsKey("C21") Then IPD.C21 = IP.Parameters.Item("C21")

                            If Me.InteractionParameters.ContainsKey(IP.Comp1) Then
                                If Me.InteractionParameters(IP.Comp1).ContainsKey(IP.Comp2) Then
                                Else
                                    Me.InteractionParameters(IP.Comp1).Add(IP.Comp2, IPD.Clone)
                                End If
                            Else
                                Me.InteractionParameters.Add(IP.Comp1, New Dictionary(Of String, UNIQUAC_IPData))
                                Me.InteractionParameters(IP.Comp1).Add(IP.Comp2, IPD.Clone)
                            End If
                        Next
                    Catch ex As Exception
                        Console.WriteLine(ex.ToString)
                    End Try
                Next
            End If

            uniquacip = Nothing
            uniquacipc = Nothing
            uniquacipc2 = Nothing
            fh1 = Nothing

            Dim pars = New ChemSepIPDReader().ReadUNIQUACIPD()

            For Each IP In pars
                If Not Me.InteractionParameters.ContainsKey(IP.Name1) Then
                    Me.InteractionParameters.Add(IP.Name1, New Dictionary(Of String, UNIQUAC_IPData))
                    Me.InteractionParameters(IP.Name1).Add(IP.Name2, IP)
                ElseIf Not Me.InteractionParameters(IP.Name1).ContainsKey(IP.Name2) Then
                    Me.InteractionParameters(IP.Name1).Add(IP.Name2, IP)
                End If
            Next

        End Sub

        Function GAMMA(ByVal T As Double, ByVal Vx As Double(), ByVal Vids As String(), ByVal VQ As Double(), ByVal VR As Double(), ByVal index As Integer)

            Return GAMMA_MR(T, Vx, Vids, VQ, VR)(index)

        End Function

        Function GAMMA_MR(ByVal T As Double, ByVal Vx As Double(), ByVal Vids As String(), ByVal VQ As Double(), ByVal VR As Double())

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "GAMMA_MR", "UNIQUAC Activity Coefficient", "UNIQUAC Activity Coefficient Calculation Routine")

            IObj?.SetCurrent

            IObj?.Paragraphs.Add("The UNIQUAC equation considers <mi>g\equiv G^{E}/{RT}</mi> formed by two 
                                additive parts, one combinatorial term <mi>g^{C}</mi> to take into account 
                                the size of the molecules, and one residual term <mi>g^{R}</mi>, which 
                                take into account the interactions between molecules:")

            IObj?.Paragraphs.Add("<m>g\equiv g^{C}+g^{R}</m>")

            IObj?.Paragraphs.Add("The <mi>g^{C}</mi> function contains only pure species parameters, while 
                                the <mi>g^{R}</mi> function incorporates two binary parameters for each 
                                pair of molecules. For a multicomponent system, ")

            IObj?.Paragraphs.Add("<m>g^{C}=\sum_{i}x_{i}\ln\phi_{i}/x_{i}+5\sum_{i}q_{i}x_{i}\ln\theta_{i}/\phi_{i}</m>")

            IObj?.Paragraphs.Add("and")

            IObj?.Paragraphs.Add("<m>g^{R}=-\sum_{i}q_{i}x_{i}\ln(\sum_{j}\theta_{j}\tau_{j}i)</m>")

            IObj?.Paragraphs.Add("where ")

            IObj?.Paragraphs.Add("<m>\phi_{i}\equiv(x_{i}r_{i})/(\sum_{j}x_{j}r_{j})</m>")

            IObj?.Paragraphs.Add("and")

            IObj?.Paragraphs.Add("<m>\theta_{i}\equiv(x_{i}q_{i})/(\sum_{j}x_{j}q_{j})</m>")

            IObj?.Paragraphs.Add("The i subscript indicates the species, and j is an index that 
                                represents all the species, i included. All sums are over all the 
                                species. Note that <mi>\tau_{ij}\neq\tau_{ji}</mi>. When <mi>i=j</mi>, <mi>\tau_{ii}=\tau_{jj}=1</mi>. ")

            IObj?.Paragraphs.Add("In these equations, <mi>r_{i}</mi> (a relative molecular volume) and <mi>q_{i}</mi>
                                 (a relative molecular surface area) are pure species parameters. 
                                The influence of temperature in <mi>g</mi> enters by means of the <mi>\tau_{ij}</mi>
                                 parameters, which are temperature-dependent:")

            IObj?.Paragraphs.Add("<m>\tau_{ij}=\exp(u_{ij}-u_{jj})/{RT}</m>")

            IObj?.Paragraphs.Add("This way, the UNIQUAC parameters are values of <mi>(u_{ij}-u_{jj})</mi>.")

            IObj?.Paragraphs.Add("An expression for <mi>\gamma_{i}</mi> is found through the application of 
                                the following relation:")

            IObj?.Paragraphs.Add("<m>\ln\gamma_{i}=[\partial nG^{E}/{RT}/(\partial n_{i})]_{(P,T,n_{j\neq i})}</m>")

            IObj?.Paragraphs.Add("The result is represented by the following equations:")

            IObj?.Paragraphs.Add("<m>\ln\gamma_{i}=\ln\gamma_{i}^{C}+\ln\gamma_{i}^{R}</m>")

            IObj?.Paragraphs.Add("<m>\ln\gamma_{i}^{C}=1-J_{i}+\ln J_{i}-5q_{i}(1-J_{i}/L_{i}+\ln J_{i}/L_{i})</m>")

            IObj?.Paragraphs.Add("<m>\ln\gamma_{i}^{R}=q_{i}(1-\ln s_{i}-\sum_{j}\theta_{j}\tau_{ij}/s_{j})</m>")

            IObj?.Paragraphs.Add("where")

            IObj?.Paragraphs.Add("<m>J_{i}=r_{i}/(\sum_{j}r_{j}x_{j})</m>")

            IObj?.Paragraphs.Add("<m>L=q_{i}/(\sum_{j}q_{j}x_{j})</m>")

            IObj?.Paragraphs.Add("<m>s_{i}=\sum_{l}\theta_{l}\tau_{li}</m>")

            IObj?.Paragraphs.Add("Again the i subscript identify the species, j and l are indexes 
                                which represent all the species, including i. all sums are over 
                                all the species, and <mi>\tau_{ij}=1</mi> for <mi>i=j</mi>. The parameters values <mi>(u_{ij}-u_{jj})</mi>
                                 are found by regression of binary VLE/LLE data.")

            IObj?.Paragraphs.Add(String.Format("<h2>Input Parameters</h2>"))

            IObj?.Paragraphs.Add(String.Format("Temperature: {0} K", T))
            IObj?.Paragraphs.Add(String.Format("Mole Fractions: {0}", Vx.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Compound IDs: {0}", Vids.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Q: {0}", VQ.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("R: {0}", VR.ToMathArrayString))

            IObj?.Paragraphs.Add(String.Format("<h2>Calculated Intermediate Parameters</h2>"))

            Dim doparallel As Boolean = Settings.EnableParallelProcessing

            Dim n As Integer = Vx.Length - 1

            Dim tau_ij(n)(), tau_ji(n)(), a12(n)(), a21(n)(), b12(n)(), b21(n)(), c12(n)(), c21(n)() As Double
            Dim teta(n), fi(n), l(n), S(n), lngc(n), lngr(n), lng(n), g(n), sum1(n), sum2 As Double
            Dim z As Double = 10.0#
            Dim r, q As Double

            Dim i, j As Integer

            For i = 0 To n
                Array.Resize(tau_ij(i), n + 1)
                Array.Resize(tau_ji(i), n + 1)
                Array.Resize(a12(i), n + 1)
                Array.Resize(a21(i), n + 1)
                Array.Resize(b12(i), n + 1)
                Array.Resize(b21(i), n + 1)
                Array.Resize(c12(i), n + 1)
                Array.Resize(c21(i), n + 1)
            Next
            i = 0
            Do
                j = 0
                Do
                    If Me.InteractionParameters.ContainsKey(Vids(i)) Then
                        If Me.InteractionParameters(Vids(i)).ContainsKey(Vids(j)) Then
                            a12(i)(j) = Me.InteractionParameters(Vids(i))(Vids(j)).A12
                            a21(i)(j) = Me.InteractionParameters(Vids(i))(Vids(j)).A21
                            b12(i)(j) = Me.InteractionParameters(Vids(i))(Vids(j)).B12
                            b21(i)(j) = Me.InteractionParameters(Vids(i))(Vids(j)).B21
                            c12(i)(j) = Me.InteractionParameters(Vids(i))(Vids(j)).C12
                            c21(i)(j) = Me.InteractionParameters(Vids(i))(Vids(j)).C21
                        Else
                            If Me.InteractionParameters.ContainsKey(Vids(j)) Then
                                If Me.InteractionParameters(Vids(j)).ContainsKey(Vids(i)) Then
                                    a12(i)(j) = Me.InteractionParameters(Vids(j))(Vids(i)).A21
                                    a21(i)(j) = Me.InteractionParameters(Vids(j))(Vids(i)).A12
                                    b12(i)(j) = Me.InteractionParameters(Vids(j))(Vids(i)).B21
                                    b21(i)(j) = Me.InteractionParameters(Vids(j))(Vids(i)).B12
                                    c12(i)(j) = Me.InteractionParameters(Vids(j))(Vids(i)).C21
                                    c21(i)(j) = Me.InteractionParameters(Vids(j))(Vids(i)).C12
                                End If
                            End If
                        End If
                    ElseIf Me.InteractionParameters.ContainsKey(Vids(j)) Then
                        If Me.InteractionParameters(Vids(j)).ContainsKey(Vids(i)) Then
                            a12(i)(j) = Me.InteractionParameters(Vids(j))(Vids(i)).A21
                            a21(i)(j) = Me.InteractionParameters(Vids(j))(Vids(i)).A12
                            b12(i)(j) = Me.InteractionParameters(Vids(j))(Vids(i)).B21
                            b21(i)(j) = Me.InteractionParameters(Vids(j))(Vids(i)).B12
                            c12(i)(j) = Me.InteractionParameters(Vids(j))(Vids(i)).C21
                            c21(i)(j) = Me.InteractionParameters(Vids(j))(Vids(i)).C12
                        End If
                    End If
                    j = j + 1
                Loop Until j = n + 1
                i = i + 1
            Loop Until i = n + 1

            If doparallel And n > 10 Then
                Parallel.For(0, n + 1, Sub(ip)
                                           tau_ij(ip) = a12(ip).NegateY.AddY(b12(ip).MultiplyConstY(T).AddY(c12(ip).MultiplyConstY(T ^ 2))).MultiplyConstY(1 / (1.98721 * T)).ExpY
                                           tau_ji(ip) = a21(ip).NegateY.AddY(b21(ip).MultiplyConstY(T).AddY(c21(ip).MultiplyConstY(T ^ 2))).MultiplyConstY(1 / (1.98721 * T)).ExpY
                                       End Sub)
            Else
                For i = 0 To n
                    tau_ij(i) = a12(i).NegateY.AddY(b12(i).MultiplyConstY(T).AddY(c12(i).MultiplyConstY(T ^ 2))).MultiplyConstY(1 / (1.98721 * T)).ExpY
                    tau_ji(i) = a21(i).NegateY.AddY(b21(i).MultiplyConstY(T).AddY(c21(i).MultiplyConstY(T ^ 2))).MultiplyConstY(1 / (1.98721 * T)).ExpY
                Next
            End If

            IObj?.Paragraphs.Add(String.Format("tau_ij: {0}", tau_ij.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("tau_ji: {0}", tau_ji.ToMathArrayString))

            r = Vx.MultiplyY(VR).SumY
            q = Vx.MultiplyY(VQ).SumY

            fi = Vx.MultiplyY(VR).MultiplyConstY(1 / r)
            teta = Vx.MultiplyY(VQ).MultiplyConstY(1 / q)
            l = VR.SubtractY(VQ).MultiplyConstY(z / 2).SubtractY(VR.AddConstY(-1))

            If doparallel And n > 10 Then
                Parallel.For(0, n + 1, Sub(ip)
                                           S(ip) = teta.MultiplyY(tau_ji(ip)).SumY
                                       End Sub)
            Else
                For i = 0 To n
                    S(i) = teta.MultiplyY(tau_ji(i)).SumY
                Next
            End If

            If doparallel And n > 10 Then
                Parallel.For(0, n + 1, Sub(ip)
                                           sum1(ip) = teta.MultiplyY(tau_ij(ip).DivideY(S)).SumY
                                       End Sub)
            Else
                For i = 0 To n
                    sum1(i) = teta.MultiplyY(tau_ij(i).DivideY(S)).SumY
                Next
            End If

            sum2 = Vx.MultiplyY(l).SumY

            lngc = VR.MultiplyConstY(-1 / r).AddConstY(1).AddY(VR.MultiplyConstY(1 / r).LogY.AddY(VQ.MultiplyConstY(-z / 2).MultiplyY(fi.DivideY(teta).NegateY.AddConstY(1).AddY(fi.DivideY(teta).LogY))))
            lngr = VQ.MultiplyY(S.LogY.NegateY.SubtractY(sum1).AddConstY(1))
            lng = lngc.AddY(lngr)
            g = lng.ExpY

            IObj?.Paragraphs.Add(String.Format("<h2>Results</h2>"))

            IObj?.Paragraphs.Add(String.Format("ln gc: {0}", lngc.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("ln gr: {0}", lngr.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("ln g: {0}", lng.ToMathArrayString))

            IObj?.Paragraphs.Add(String.Format("Activity Coefficients: {0}", g.ToMathArrayString))

            IObj?.Close()

            Return g

        End Function

        Function DLNGAMMA_DT(ByVal T As Double, ByVal Vx As Array, ByVal Vids As Array, ByVal VQ As Array, ByVal VR As Array) As Array

            Dim gamma1, gamma2 As Double()

            Dim epsilon As Double = 0.001

            gamma1 = GAMMA_MR(T, Vx, Vids, VQ, VR)
            gamma2 = GAMMA_MR(T + epsilon, Vx, Vids, VQ, VR)

            Dim dgamma(gamma1.Length - 1) As Double

            For i As Integer = 0 To Vx.Length - 1
                dgamma(i) = (gamma2(i) - gamma1(i)) / (epsilon)
            Next

            Return dgamma

        End Function

        Function HEX_MIX(ByVal T As Double, ByVal Vx As Array, ByVal Vids As Array, ByVal VQ As Array, ByVal VR As Array) As Double

            Dim dgamma As Double() = DLNGAMMA_DT(T, Vx, Vids, VQ, VR)

            Dim hex As Double = 0.0#

            For i As Integer = 0 To Vx.Length - 1
                hex += -8.314 * T ^ 2 * Vx(i) * dgamma(i)
            Next

            Return hex 'kJ/kmol

        End Function

        Function CPEX_MIX(ByVal T As Double, ByVal Vx As Array, ByVal Vids As Array, ByVal VQ As Array, ByVal VR As Array) As Double

            Dim hex1, hex2, cpex As Double

            Dim epsilon As Double = 0.001

            hex1 = HEX_MIX(T, Vx, Vids, VQ, VR)
            hex2 = HEX_MIX(T + epsilon, Vx, Vids, VQ, VR)

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

End Namespace
