Imports DWSIM.Interfaces
Imports DWSIM.Interfaces.Enums
Imports DWSIM.Thermodynamics.PropertyPackages

Namespace DWSIM.Thermodynamics.AdvancedEOS

    Public Class SoaveRedlichKwongAdvancedPropertyPackage

        Inherits SRKPropertyPackage

        Private TInternal As Double = 0.0
        Private PInternal As Double = 0.0

        Private ec As New Ciloci.Flee.ExpressionContext

        Public KijExpressions As New Dictionary(Of String, String)

        Public Sub New()

            ComponentName = "Soave-Redlich-Kwong (SRK) Advanced"
            ComponentDescription = "Soave-Redlich-Kwong EOS with T/P-dependent Interaction Parameters"

            IsConfigurable = True

            ec.Imports.AddType(GetType(System.Math))
            ec.Options.ParseCulture = Globalization.CultureInfo.InvariantCulture

        End Sub

        Public Overrides Function ReturnInstance(typename As String) As Object

            Return New SoaveRedlichKwongAdvancedPropertyPackage()

        End Function

        Public Overrides Sub DisplayEditingForm()

            If GlobalSettings.Settings.CAPEOPENMode Then
                Dim f As New FormConfig() With {._pp = Me, ._comps = _selectedcomps.ToDictionary(Of String, Interfaces.ICompoundConstantProperties)(Function(k) k.Key, Function(k) k.Value)}
                f.ShowDialog()
            Else
                Dim f As New FormConfig() With {._form = Me.Flowsheet, ._pp = Me, ._comps = Flowsheet.SelectedCompounds}
                f.ShowDialog()
            End If

        End Sub

        Public Overrides Function RET_VKij() As Double(,)

            Dim vn As String() = RET_VNAMES()
            Dim n As Integer = vn.Length - 1

            Dim val(Me.CurrentMaterialStream.Phases(0).Compounds.Count - 1, Me.CurrentMaterialStream.Phases(0).Compounds.Count - 1) As Double
            Dim i As Integer = 0
            Dim l As Integer = 0

            For i = 0 To n
                For l = 0 To n
                    Dim kval = KIJ2(vn(i), vn(l))
                    If kval = 0.0 Then
                        val(i, l) = KIJ(vn(i), vn(l))
                    Else
                        val(i, l) = kval
                    End If
                Next
            Next

            Return val

        End Function

        Public Function KIJ2(id1 As String, id2 As String) As Double

            ec.Variables.Clear()
            ec.Variables.Add("P", PInternal)
            ec.Variables.Add("T", TInternal)

            Dim result As Double = 0.0

            Dim pair = id1 + "/" + id2
            Dim pair2 = id2 + "/" + id1

            Try
                If KijExpressions.ContainsKey(pair) Then
                    result = ec.CompileGeneric(Of Double)(KijExpressions(pair)).Evaluate()
                ElseIf KijExpressions.ContainsKey(pair2) Then
                    result = ec.CompileGeneric(Of Double)(KijExpressions(pair2)).Evaluate()
                Else
                    Return 0.0
                End If
                Return result
            Catch ex As Exception
                Flowsheet?.ShowMessage(String.Format("PR/SRK Adv: Error evaluating kij expression for {0}/{1}: {2}", id1, id2, ex.Message), IFlowsheet.MessageType.GeneralError)
                Return 0.0
            End Try

        End Function

        Public Function KIJ(ByVal id1 As String, ByVal id2 As String) As Double

            If Me.m_pr.InteractionParameters.ContainsKey(id1) Then
                If Me.m_pr.InteractionParameters(id1).ContainsKey(id2) Then
                    Return m_pr.InteractionParameters(id1)(id2).kij
                Else
                    If Me.m_pr.InteractionParameters.ContainsKey(id2) Then
                        If Me.m_pr.InteractionParameters(id2).ContainsKey(id1) Then
                            Return m_pr.InteractionParameters(id2)(id1).kij
                        Else
                            Return 0.0
                        End If
                    Else
                        Return 0.0
                    End If
                End If
            Else
                Return 0.0
            End If

        End Function

        Public Overrides Function AUX_LIQDENS(T As Double, Optional P As Double = 0, Optional Pvp As Double = 0, Optional phaseid As Integer = 3, Optional FORCE_EOS As Boolean = False) As Double
            TInternal = T
            PInternal = P
            Return MyBase.AUX_LIQDENS(T, P, Pvp, phaseid, FORCE_EOS)
        End Function

        Public Overrides Function AUX_LIQDENS(T As Double, Vx As Array, Optional P As Double = 0, Optional Pvp As Double = 0, Optional FORCE_EOS As Boolean = False) As Double
            TInternal = T
            PInternal = P
            Return MyBase.AUX_LIQDENS(T, Vx, P, Pvp, FORCE_EOS)
        End Function

        Public Overrides Function AUX_VAPDENS(T As Double, P As Double) As Double
            TInternal = T
            PInternal = P
            Return MyBase.AUX_VAPDENS(T, P)
        End Function

        Public Overrides Function AUX_Z(Vx() As Double, T As Double, P As Double, state As PhaseName) As Double
            TInternal = T
            PInternal = P
            Return MyBase.AUX_Z(Vx, T, P, state)
        End Function

        Public Overrides Function CalcSpeedOfSound(p As IPhase) As Double
            TInternal = CurrentMaterialStream.Phases(0).Properties.temperature.GetValueOrDefault
            PInternal = CurrentMaterialStream.Phases(0).Properties.pressure.GetValueOrDefault
            Return MyBase.CalcSpeedOfSound(p)
        End Function

        Public Overrides Sub DW_CalcPhaseProps(Phase As Phase)
            TInternal = CurrentMaterialStream.Phases(0).Properties.temperature.GetValueOrDefault
            PInternal = CurrentMaterialStream.Phases(0).Properties.pressure.GetValueOrDefault
            MyBase.DW_CalcPhaseProps(Phase)
        End Sub

        Public Overrides Sub DW_CalcCompPartialVolume(phase As Phase, T As Double, P As Double)
            TInternal = T
            PInternal = P
            MyBase.DW_CalcCompPartialVolume(phase, T, P)
        End Sub

        Public Overrides Function DW_CalcCp_ISOL(Phase1 As Phase, T As Double, P As Double) As Double
            TInternal = T
            PInternal = P
            Return MyBase.DW_CalcCp_ISOL(Phase1, T, P)
        End Function

        Public Overrides Function DW_CalcCv_ISOL(Phase1 As Phase, T As Double, P As Double) As Double
            TInternal = T
            PInternal = P
            Return MyBase.DW_CalcCv_ISOL(Phase1, T, P)
        End Function

        Public Overrides Function DW_CalcEnthalpy(Vx As Array, T As Double, P As Double, st As State) As Double
            TInternal = T
            PInternal = P
            Return MyBase.DW_CalcEnthalpy(Vx, T, P, st)
        End Function

        Public Overrides Function DW_CalcEntropy(Vx As Array, T As Double, P As Double, st As State) As Double
            TInternal = T
            PInternal = P
            Return MyBase.DW_CalcEntropy(Vx, T, P, st)
        End Function

        Public Overrides Function DW_CalcFugCoeff(Vx As Array, T As Double, P As Double, st As State) As Double()
            TInternal = T
            PInternal = P
            Return MyBase.DW_CalcFugCoeff(Vx, T, P, st)
        End Function

        Public Overrides Function DW_CalcKvalue(Vx As Array, T As Double, P As Double) As Double()
            TInternal = T
            PInternal = P
            Return MyBase.DW_CalcKvalue(Vx, T, P)
        End Function

        Public Overrides Function DW_CalcKvalue(Vx() As Double, Vy() As Double, T As Double, P As Double, Optional type As String = "LV") As Double()
            TInternal = T
            PInternal = P
            Return MyBase.DW_CalcKvalue(Vx, Vy, T, P, type)
        End Function

        Public Overrides Function DW_CalcMassaEspecifica_ISOL(Phase1 As Phase, T As Double, P As Double, Optional pvp As Double = 0) As Double
            TInternal = T
            PInternal = P
            Return MyBase.DW_CalcMassaEspecifica_ISOL(Phase1, T, P, pvp)
        End Function

        Public Overrides Function SaveData() As List(Of XElement)

            Dim elements = MyBase.SaveData()

            elements.Add(New XElement("NewInteractionParameters"))
            For Each kvp In KijExpressions
                elements((elements.Count - 1)).Add(New XElement("NewInteractionParameter", New XAttribute("Pair", kvp.Key), New XAttribute("Value", kvp.Value)))
            Next

            Return elements

        End Function

        Public Overrides Function LoadData(data As List(Of XElement)) As Boolean

            KijExpressions = New Dictionary(Of String, String)

            For Each xel As XElement In (From xel2 In data Where xel2.Name = "NewInteractionParameters" Select xel2).SingleOrDefault().Elements().ToList()
                KijExpressions.Add(xel.Attribute("Pair").Value, xel.Attribute("Value").Value)
            Next

            Return MyBase.LoadData(data)

        End Function

    End Class

End Namespace
