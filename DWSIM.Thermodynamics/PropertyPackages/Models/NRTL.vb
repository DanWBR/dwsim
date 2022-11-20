'    NRTL Property Package 
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

Namespace PropertyPackages.Auxiliary

    <DelimitedRecord(";")> <IgnoreFirst()> <System.Serializable()> _
    Public Class NRTL_IPData

        Implements ICloneable

        <FieldConverter(GetType(ObjectConverter))> Public ID1 As Object = ""
        <FieldConverter(GetType(ObjectConverter))> Public ID2 As Object = ""
        Public A12 As Double = 0
        Public A21 As Double = 0
        Public alpha12 As Double = 0
        Public comment As String = ""
        <FieldHidden()> Public B12 As Double = 0
        <FieldHidden()> Public B21 As Double = 0
        <FieldHidden()> Public C12 As Double = 0
        <FieldHidden()> Public C21 As Double = 0

        Public Function Clone() As Object Implements System.ICloneable.Clone

            Dim newclass As New NRTL_IPData
            With newclass
                .ID1 = Me.ID1
                .ID2 = Me.ID2
                .A12 = Me.A12
                .A21 = Me.A21
                .B12 = Me.B12
                .B21 = Me.B21
                .C12 = Me.C12
                .C21 = Me.C21
                .alpha12 = Me.alpha12
                .comment = Me.comment
            End With
            Return newclass
        End Function

    End Class

    <System.Serializable()> Public Class ObjectConverter

        Inherits ConverterBase

        Public Overrides Function StringToField(ByVal from As String) As Object
            Return [from]
        End Function


        Public Overrides Function FieldToString(ByVal fieldValue As Object) As String

            Return fieldValue.ToString

        End Function

    End Class

    <System.Serializable()> Public Class NRTL

        Implements IActivityCoefficientBase

        Private _ip As Dictionary(Of String, Dictionary(Of String, NRTL_IPData))

        Public Property InteractionParameters() As Dictionary(Of String, Dictionary(Of String, NRTL_IPData))
            Get
                Return _ip
            End Get
            Set(value As Dictionary(Of String, Dictionary(Of String, NRTL_IPData)))
                _ip = value
            End Set
        End Property

        Sub New()

            _ip = New Dictionary(Of String, Dictionary(Of String, NRTL_IPData))

            Dim pathsep As Char = System.IO.Path.DirectorySeparatorChar

            Dim nrtlip As NRTL_IPData
            Dim nrtlipc() As NRTL_IPData
            Dim fh1 As New FileHelperEngine(Of NRTL_IPData)

            Using filestr As IO.Stream = System.Reflection.Assembly.GetAssembly(Me.GetType).GetManifestResourceStream("DWSIM.Thermodynamics.nrtl.dat")
                Using t As New IO.StreamReader(filestr)
                    nrtlipc = fh1.ReadStream(t)
                End Using
            End Using

            Dim csdb As New ChemSepHelper.ChemSepIDConverter

            'load ChemSep database interactions
            For Each nrtlip In nrtlipc
                If Me.InteractionParameters.ContainsKey(csdb.GetDWSIMName(nrtlip.ID1)) Then
                    If Not Me.InteractionParameters(csdb.GetDWSIMName(nrtlip.ID1)).ContainsKey(csdb.GetDWSIMName(nrtlip.ID2)) Then
                        Me.InteractionParameters(csdb.GetDWSIMName(nrtlip.ID1)).Add(csdb.GetDWSIMName(nrtlip.ID2), nrtlip.Clone)
                    End If
                    If Not Me.InteractionParameters(csdb.GetDWSIMName(nrtlip.ID1)).ContainsKey(csdb.GetCSName(nrtlip.ID2)) Then
                        Me.InteractionParameters(csdb.GetDWSIMName(nrtlip.ID1)).Add(csdb.GetCSName(nrtlip.ID2), nrtlip.Clone)
                    End If
                Else
                    Me.InteractionParameters.Add(csdb.GetDWSIMName(nrtlip.ID1), New Dictionary(Of String, NRTL_IPData))
                    Me.InteractionParameters(csdb.GetDWSIMName(nrtlip.ID1)).Add(csdb.GetDWSIMName(nrtlip.ID2), nrtlip.Clone)
                    If Not Me.InteractionParameters(csdb.GetDWSIMName(nrtlip.ID1)).ContainsKey(csdb.GetCSName(nrtlip.ID2)) Then
                        Me.InteractionParameters(csdb.GetDWSIMName(nrtlip.ID1)).Add(csdb.GetCSName(nrtlip.ID2), nrtlip.Clone)
                    End If
                End If
            Next

            For Each nrtlip In nrtlipc
                If Me.InteractionParameters.ContainsKey(csdb.GetCSName(nrtlip.ID1)) Then
                    If Not Me.InteractionParameters(csdb.GetCSName(nrtlip.ID1)).ContainsKey(csdb.GetCSName(nrtlip.ID2)) Then
                        Me.InteractionParameters(csdb.GetCSName(nrtlip.ID1)).Add(csdb.GetCSName(nrtlip.ID2), nrtlip.Clone)
                    End If
                    If Not Me.InteractionParameters(csdb.GetCSName(nrtlip.ID1)).ContainsKey(csdb.GetDWSIMName(nrtlip.ID2)) Then
                        Me.InteractionParameters(csdb.GetCSName(nrtlip.ID1)).Add(csdb.GetDWSIMName(nrtlip.ID2), nrtlip.Clone)
                    End If
                Else
                    Me.InteractionParameters.Add(csdb.GetCSName(nrtlip.ID1), New Dictionary(Of String, NRTL_IPData))
                    Me.InteractionParameters(csdb.GetCSName(nrtlip.ID1)).Add(csdb.GetCSName(nrtlip.ID2), nrtlip.Clone)
                    If Not Me.InteractionParameters(csdb.GetCSName(nrtlip.ID1)).ContainsKey(csdb.GetDWSIMName(nrtlip.ID2)) Then
                        Me.InteractionParameters(csdb.GetCSName(nrtlip.ID1)).Add(csdb.GetDWSIMName(nrtlip.ID2), nrtlip.Clone)
                    End If
                End If
            Next

            'load user database interactions
            If Not GlobalSettings.Settings.UserInteractionsDatabases Is Nothing Then
                For Each IPDBPath As String In GlobalSettings.Settings.UserInteractionsDatabases
                    Dim Interactions As BaseClasses.InteractionParameter()
                    Dim IP As BaseClasses.InteractionParameter
                    Try
                        Interactions = Databases.UserIPDB.ReadInteractions(IPDBPath, "NRTL")
                        For Each IP In Interactions
                            Dim IPD As New NRTL_IPData
                            IPD.A12 = Convert.ToDouble(IP.Parameters.Item("A12"), System.Globalization.CultureInfo.InvariantCulture)
                            IPD.A21 = Convert.ToDouble(IP.Parameters.Item("A21"), System.Globalization.CultureInfo.InvariantCulture)
                            IPD.alpha12 = Convert.ToDouble(IP.Parameters.Item("alpha12"), System.Globalization.CultureInfo.InvariantCulture)
                            IPD.comment = IP.Description
                            If IP.Parameters.ContainsKey("B12") Then IPD.B12 = Convert.ToDouble(IP.Parameters.Item("B12"), System.Globalization.CultureInfo.InvariantCulture)
                            If IP.Parameters.ContainsKey("B21") Then IPD.B21 = Convert.ToDouble(IP.Parameters.Item("B21"), System.Globalization.CultureInfo.InvariantCulture)
                            If IP.Parameters.ContainsKey("C12") Then IPD.C12 = Convert.ToDouble(IP.Parameters.Item("C12"), System.Globalization.CultureInfo.InvariantCulture)
                            If IP.Parameters.ContainsKey("C21") Then IPD.C21 = Convert.ToDouble(IP.Parameters.Item("C21"), System.Globalization.CultureInfo.InvariantCulture)

                            If Me.InteractionParameters.ContainsKey(IP.Comp1) Then
                                If Me.InteractionParameters(IP.Comp1).ContainsKey(IP.Comp2) Then
                                Else
                                    Me.InteractionParameters(IP.Comp1).Add(IP.Comp2, IPD.Clone)
                                End If
                            Else
                                Me.InteractionParameters.Add(IP.Comp1, New Dictionary(Of String, NRTL_IPData))
                                Me.InteractionParameters(IP.Comp1).Add(IP.Comp2, IPD.Clone)
                            End If
                        Next
                    Catch ex As Exception
                        Console.WriteLine(ex.ToString)
                    End Try
                Next
            End If

            'load biodiesel database interactions
            Using filestr As IO.Stream = System.Reflection.Assembly.GetAssembly(Me.GetType).GetManifestResourceStream("DWSIM.Thermodynamics.biod_intparm_nrtl.dat")
                Using t As New IO.StreamReader(filestr)
                    nrtlipc = fh1.ReadStream(t)
                End Using
            End Using

            For Each nrtlip In nrtlipc
                If Me.InteractionParameters.ContainsKey(nrtlip.ID1) Then
                    If Me.InteractionParameters((nrtlip.ID1)).ContainsKey((nrtlip.ID2)) Then
                    Else
                        Me.InteractionParameters((nrtlip.ID1)).Add((nrtlip.ID2), nrtlip.Clone)
                    End If
                Else
                    Me.InteractionParameters.Add((nrtlip.ID1), New Dictionary(Of String, NRTL_IPData))
                    Me.InteractionParameters((nrtlip.ID1)).Add((nrtlip.ID2), nrtlip.Clone)
                End If
            Next

            nrtlip = Nothing
            nrtlipc = Nothing
            fh1 = Nothing

            Dim pars = New ChemSepIPDReader().ReadNRTLIPD()

            For Each nrtlip In pars
                If Not Me.InteractionParameters.ContainsKey(nrtlip.ID1) Then
                    Me.InteractionParameters.Add(nrtlip.ID1, New Dictionary(Of String, NRTL_IPData))
                    Me.InteractionParameters(nrtlip.ID1).Add(nrtlip.ID2, nrtlip)
                ElseIf Not Me.InteractionParameters(nrtlip.ID1).ContainsKey(nrtlip.ID2) Then
                    Me.InteractionParameters(nrtlip.ID1).Add(nrtlip.ID2, nrtlip)
                End If
            Next

        End Sub

        Function GAMMA(ByVal T As Double, ByVal Vx As Array, ByVal Vids As Array, ByVal index As Integer) As Double

            Return GAMMA_MR(T, Vx, Vids)(index)

        End Function

        Function GAMMA_MR(ByVal T As Double, ByVal Vx As Double(), ByVal Vids As String()) As Double()

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "GAMMA_MR", "NRTL Activity Coefficient", "Non-Random-Two-Liquid Activity Coefficient Calculation Routine")

            IObj?.SetCurrent

            IObj?.Paragraphs.Add("Wilson (1964) presented a model relating <mi>g^{E}</mi> to the molar 
                                fraction, based mainly on molecular considerations, using the 
                                concept of local composition. Basically, the concept of local 
                                composition states that the composition of the system in the 
                                vicinity of a given molecule is not equal to the overall 
                                composition of the system, because of intermolecular forces.")

            IObj?.Paragraphs.Add("Wilson's equation provides a good representation of the Gibbs' 
                                excess free energy for a variety of mixtures, and is particularly 
                                useful in solutions of polar compounds or with a tendency to 
                                association in apolar solvents, where Van Laar's equation or 
                                Margules' one are not sufficient. Wilson's equation has the 
                                advantage of being easily extended to multicomponent solutions 
                                but has two disadvantages: first, the less important, is that the 
                                equations are not applicable to systems where the logarithms of 
                                activity coefficients, when plotted as a function of x, show a 
                                maximum or a minimum. However, these systems are not common. The 
                                second, a little more serious, is that the model of Wilson is not 
                                able to predict limited miscibility, that is, it is not useful 
                                for LLE calculations.")

            IObj?.Paragraphs.Add("Renon and Prausnitz developed the NRTL equation (Non-Random, 
                                Two-Liquid) based on the concept of local composition but, unlike 
                                Wilson's model, the NRTL model is applicable to systems of 
                                partial miscibility. The model equation is:")

            IObj?.Paragraphs.Add("<m>\ln\gamma_{i}=\frac{\underset{j=1}{\overset{n}{\sum}}\tau_{ji}x_{j}G_{ji}}{\underset{k=1}{\overset{n}{\sum}}x_{k}G_{ki}}+\underset{j=1}{\overset{n}{\sum}}\frac{x_{j}G_{ij}}{\underset{k=1}{\overset{n}{\sum}}x_{k}G_{kj}}(\tau_{ij}-\frac{\underset{m=1}{\overset{n}{\sum}}\tau_{mj}x_{m}G_{mj}}{\underset{k=1}{\overset{n}{\sum}}x_{k}G_{kj}}),</m>")

            IObj?.Paragraphs.Add("<m>G_{ij}=exp(-\tau_{ij}\alpha_{ij}),</m>")

            IObj?.Paragraphs.Add("<m>\tau_{ij}=a_{ij}/{RT},</m>")

            IObj?.Paragraphs.Add("where")

            IObj?.Paragraphs.Add("<mi>\gamma_{i}</mi> Activity coefficient of component i")

            IObj?.Paragraphs.Add("<mi>x_{i}</mi> Molar fraction of component i")

            IObj?.Paragraphs.Add("<mi>a_{ij}</mi> Interaction parameter between i-j <mi>(a_{ij}\neq a_{ji})</mi> (cal/mol)")

            IObj?.Paragraphs.Add("<mi>T</mi> Temperature (K)")

            IObj?.Paragraphs.Add("<mi>\alpha_{ij}</mi> non-randomness parameter for the i-j pair <mi>(\alpha_{ij}=\alpha_{ji})</mi>")

            IObj?.Paragraphs.Add("The significance of <mi>G_{ij}</mi> is similar to <mi>\Lambda_{ij}</mi> from 
                                Wilson's equation, that is, they are characteristic energy 
                                parameters of the ij interaction. The parameter is related to the 
                                non-randomness of the mixture, i.e. that the components in the 
                                mixture are not randomly distributed but follow a pattern 
                                dictated by the local composition. When it is zero, the mixture 
                                is completely random, and the equation is reduced to the 
                                two-suffix Margules equation.")

            IObj?.Paragraphs.Add("For ideal or moderately ideal systems, the NRTL model does not 
                                offer much advantage over Van Laar and three-suffix Margules, but 
                                for strongly non-ideal systems, this equation can provide a good 
                                representation of experimental data, although good quality data 
                                is necessary to estimate the three required parameters.")

            IObj?.Paragraphs.Add(String.Format("<h2>Input Parameters</h2>"))

            IObj?.Paragraphs.Add(String.Format("Temperature: {0} K", T))
            IObj?.Paragraphs.Add(String.Format("Mole Fractions: {0}", Vx.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Compound IDs: {0}", Vids.ToMathArrayString))

            IObj?.Paragraphs.Add(String.Format("<h2>Calculated Intermediate Parameters</h2>"))

            Dim doparallel As Boolean = Settings.EnableParallelProcessing

            Dim n As Integer = Vx.Length - 1

            Dim Gij(n)(), tau_ij(n)(), Gji(n)(), tau_ji(n)(), alpha12(n)() As Double
            Dim S(n), C(n) As Double
            Dim Vg(n), lnVg(n) As Double

            Dim i, j As Integer

            For i = 0 To n
                Array.Resize(Gij(i), n + 1)
                Array.Resize(Gji(i), n + 1)
                Array.Resize(tau_ij(i), n + 1)
                Array.Resize(tau_ji(i), n + 1)
                Array.Resize(alpha12(i), n + 1)
            Next

            i = 0
            Do
                j = 0
                Do
                    If Me.InteractionParameters.ContainsKey(Vids(i)) Then
                        If Me.InteractionParameters(Vids(i)).ContainsKey(Vids(j)) Then
                            tau_ij(i)(j) = (Me.InteractionParameters(Vids(i))(Vids(j)).A12 + Me.InteractionParameters(Vids(i))(Vids(j)).B12 * T + Me.InteractionParameters(Vids(i))(Vids(j)).C12 * T ^ 2) / (1.98721 * T)
                            tau_ji(i)(j) = (Me.InteractionParameters(Vids(i))(Vids(j)).A21 + Me.InteractionParameters(Vids(i))(Vids(j)).B21 * T + Me.InteractionParameters(Vids(i))(Vids(j)).C21 * T ^ 2) / (1.98721 * T)
                            alpha12(i)(j) = Me.InteractionParameters(Vids(i))(Vids(j)).alpha12
                        Else
                            If Me.InteractionParameters.ContainsKey(Vids(j)) Then
                                If Me.InteractionParameters(Vids(j)).ContainsKey(Vids(i)) Then
                                    tau_ji(i)(j) = (Me.InteractionParameters(Vids(j))(Vids(i)).A12 + Me.InteractionParameters(Vids(j))(Vids(i)).B12 * T + Me.InteractionParameters(Vids(j))(Vids(i)).C12 * T ^ 2) / (1.98721 * T)
                                    tau_ij(i)(j) = (Me.InteractionParameters(Vids(j))(Vids(i)).A21 + Me.InteractionParameters(Vids(j))(Vids(i)).B21 * T + Me.InteractionParameters(Vids(j))(Vids(i)).C21 * T ^ 2) / (1.98721 * T)
                                    alpha12(i)(j) = Me.InteractionParameters(Vids(j))(Vids(i)).alpha12
                                Else
                                    tau_ij(i)(j) = 0.0#
                                    tau_ji(i)(j) = 0.0#
                                    alpha12(i)(j) = 0.0#
                                End If
                            Else
                                tau_ij(i)(j) = 0.0#
                                tau_ji(i)(j) = 0.0#
                                alpha12(i)(j) = 0.0#
                            End If
                        End If
                    ElseIf Me.InteractionParameters.ContainsKey(Vids(j)) Then
                        If Me.InteractionParameters(Vids(j)).ContainsKey(Vids(i)) Then
                            tau_ji(i)(j) = (Me.InteractionParameters(Vids(j))(Vids(i)).A12 + Me.InteractionParameters(Vids(j))(Vids(i)).B12 * T + Me.InteractionParameters(Vids(j))(Vids(i)).C12 * T ^ 2) / (1.98721 * T)
                            tau_ij(i)(j) = (Me.InteractionParameters(Vids(j))(Vids(i)).A21 + Me.InteractionParameters(Vids(j))(Vids(i)).B21 * T + Me.InteractionParameters(Vids(j))(Vids(i)).C21 * T ^ 2) / (1.98721 * T)
                            alpha12(i)(j) = Me.InteractionParameters(Vids(j))(Vids(i)).alpha12
                        Else
                            tau_ij(i)(j) = 0.0#
                            tau_ji(i)(j) = 0.0#
                            alpha12(i)(j) = 0.0#
                        End If
                    Else
                        tau_ij(i)(j) = 0.0#
                        tau_ji(i)(j) = 0.0#
                        alpha12(i)(j) = 0.0#
                    End If
                    j = j + 1
                Loop Until j = n + 1
                i = i + 1
            Loop Until i = n + 1

            If doparallel And n > 10 Then
                Parallel.For(0, n + 1, Sub(ip)
                                           Gij(ip) = alpha12(ip).NegateY.MultiplyY(tau_ij(ip)).ExpY
                                           Gji(ip) = alpha12(ip).NegateY.MultiplyY(tau_ji(ip)).ExpY
                                       End Sub)
            Else
                For i = 0 To n
                    Gij(i) = alpha12(i).NegateY.MultiplyY(tau_ij(i)).ExpY
                    Gji(i) = alpha12(i).NegateY.MultiplyY(tau_ji(i)).ExpY
                Next
            End If


            IObj?.Paragraphs.Add(String.Format("Gij: {0}", Gij.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Gji: {0}", Gji.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("tau_ij: {0}", tau_ij.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("tau_ji: {0}", tau_ji.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("alpha12: {0}", alpha12.ToMathArrayString))

            If doparallel And n > 10 Then
                Parallel.For(0, n + 1, Sub(ip)
                                           S(ip) = Vx.MultiplyY(Gji(ip)).SumY
                                           C(ip) = Vx.MultiplyY(Gji(ip)).MultiplyY(tau_ji(ip)).SumY
                                       End Sub)
            Else
                For i = 0 To n
                    S(i) = Vx.MultiplyY(Gji(i)).SumY
                    C(i) = Vx.MultiplyY(Gji(i)).MultiplyY(tau_ji(i)).SumY
                Next
            End If

            IObj?.Paragraphs.Add(String.Format("S: {0}", S.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("C: {0}", C.ToMathArrayString))

            lnVg = C.DivideY(S)

            If doparallel And n > 10 Then
                Parallel.For(0, n + 1, Sub(ip)
                                           lnVg(ip) += Vx.MultiplyY(Gij(ip)).MultiplyY(tau_ij(ip).SubtractY(C.DivideY(S))).DivideY(S).SumY
                                       End Sub)
            Else
                For i = 0 To n
                    lnVg(i) += Vx.MultiplyY(Gij(i)).MultiplyY(tau_ij(i).SubtractY(C.DivideY(S))).DivideY(S).SumY
                Next
            End If

            Vg = lnVg.ExpY

            IObj?.Paragraphs.Add(String.Format("<h2>Results</h2>"))

            IObj?.Paragraphs.Add(String.Format("Activity Coefficients: {0}", Vg.ToMathArrayString))

            IObj?.Close()

            Return Vg

        End Function

        Function DLNGAMMA_DT(ByVal T As Double, ByVal Vx As Array, ByVal Vids As Array) As Array

            Dim gamma1, gamma2 As Double()

            Dim epsilon As Double = 0.001

            gamma1 = GAMMA_MR(T, Vx, Vids)
            gamma2 = GAMMA_MR(T + epsilon, Vx, Vids)

            Dim dgamma(gamma1.Length - 1) As Double

            For i As Integer = 0 To Vx.Length - 1
                dgamma(i) = (gamma2(i) - gamma1(i)) / (epsilon)
            Next

            Return dgamma

        End Function

        Function HEX_MIX(ByVal T As Double, ByVal Vx As Array, ByVal Vids As Array) As Double

            Dim dgamma As Double() = DLNGAMMA_DT(T, Vx, Vids)

            Dim hex As Double = 0.0#

            For i As Integer = 0 To Vx.Length - 1
                hex += -8.314 * T ^ 2 * Vx(i) * dgamma(i)
            Next

            Return hex 'kJ/kmol

        End Function

        Function CPEX_MIX(ByVal T As Double, ByVal Vx As Array, ByVal Vids As Array) As Double

            Dim hex1, hex2, cpex As Double

            Dim epsilon As Double = 0.001

            hex1 = HEX_MIX(T, Vx, Vids)
            hex2 = HEX_MIX(T + epsilon, Vx, Vids)

            cpex = (hex2 - hex1) / epsilon

            Return cpex 'kJ/kmol.K

        End Function

        Public Function CalcActivityCoefficients(T As Double, Vx As Array, otherargs As Object) As Array Implements IActivityCoefficientBase.CalcActivityCoefficients

            Return GAMMA_MR(T, Vx, otherargs)

        End Function

        Public Function CalcExcessEnthalpy(T As Double, Vx As Array, otherargs As Object) As Double Implements IActivityCoefficientBase.CalcExcessEnthalpy

            Return HEX_MIX(T, Vx, otherargs)

        End Function

        Public Function CalcExcessHeatCapacity(T As Double, Vx As Array, otherargs As Object) As Double Implements IActivityCoefficientBase.CalcExcessHeatCapacity

            Return CPEX_MIX(T, Vx, otherargs)

        End Function

    End Class

End Namespace
