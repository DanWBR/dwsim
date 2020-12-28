Imports System.Linq
Imports WeifenLuo.WinFormsUI.Docking

Public Class FormAddUtility

    Public Property Flowsheet As Interfaces.IFlowsheet

    Private Sub ListBox1_SelectedIndexChanged(sender As Object, e As EventArgs) Handles ListBox1.SelectedIndexChanged

        ListBox2.Items.Clear()
        ListBox3.Items.Clear()

        Select Case ListBox1.SelectedIndex

            Case 0 'MS

                Dim items As String()

                If My.Settings.CultureInfo = "pt-BR" Then

                    items = New String() {"Hidratos de Gás Natural", "Diagrama de Fases", "Diagrama de Fases (Binário)", "Diagrama de Fases (Ternário)",
                        "Ponto Crítico Verdadeiro", "Propriedades de Petróleos"}

                Else

                    items = New String() {"Natural Gas Hydrates", "Phase Envelope", "Binary Phase Envelope", "Ternary Phase Envelope",
                        "True Critical Point", "Petroleum Cold Flow Properties"}

                End If

                ListBox2.Items.AddRange(items)

                Dim mslist As String() = Flowsheet.GraphicObjects.Values.Where(Function(x) x.ObjectType = ObjectType.MaterialStream).Select(Function(m) m.Tag).ToArray

                ListBox3.Items.AddRange(mslist)

            Case 1 'VSEP

                If My.Settings.CultureInfo = "pt-BR" Then

                    ListBox2.Items.Add("Dimensionamento (Vaso Vertical/Horizontal)")

                Else

                    ListBox2.Items.Add("Horizontal/Vertical Vessel Sizing")

                End If

                Dim objlist As String() = Flowsheet.GraphicObjects.Values.Where(Function(x) x.ObjectType = ObjectType.Vessel).Select(Function(m) m.Tag).ToArray

                ListBox3.Items.AddRange(objlist)

            Case 2 'VALVE

                If My.Settings.CultureInfo = "pt-BR" Then

                    ListBox2.Items.Add("Dimensionamento de PSV")

                Else

                    ListBox2.Items.Add("Pressure Safety Valve Sizing")

                End If

                Dim objlist As String() = Flowsheet.GraphicObjects.Values.Where(Function(x) x.ObjectType = ObjectType.Valve).Select(Function(m) m.Tag).ToArray

                ListBox3.Items.AddRange(objlist)

        End Select

    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        Me.Close()
    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click

        Me.Hide()

        Dim obj = Flowsheet.GetFlowsheetSimulationObject(ListBox3.SelectedItem.ToString)

        Dim utility As Interfaces.IAttachedUtility = Nothing

        Select Case ListBox1.SelectedIndex

            Case 0 'MS

                Select Case ListBox2.SelectedIndex
                    Case 0
                        'Natural Gas Hydrates
                        utility = Flowsheet.GetUtility(Interfaces.Enums.FlowsheetUtility.NaturalGasHydrates)
                        utility.Name = "NaturalGasHydrates" & (obj.AttachedUtilities.Where(Function(x) x.GetUtilityType = Interfaces.Enums.FlowsheetUtility.NaturalGasHydrates).Count + 1).ToString
                    Case 1
                        'Phase Envelope
                        utility = Flowsheet.GetUtility(Interfaces.Enums.FlowsheetUtility.PhaseEnvelope)
                        utility.Name = "PhaseEnvelope" & (obj.AttachedUtilities.Where(Function(x) x.GetUtilityType = Interfaces.Enums.FlowsheetUtility.PhaseEnvelope).Count + 1).ToString
                    Case 2
                        'Binary Phase Envelope
                        utility = Flowsheet.GetUtility(Interfaces.Enums.FlowsheetUtility.PhaseEnvelopeBinary)
                        utility.Name = "BinaryEnvelope" & (obj.AttachedUtilities.Where(Function(x) x.GetUtilityType = Interfaces.Enums.FlowsheetUtility.PhaseEnvelopeBinary).Count + 1).ToString
                    Case 3
                        'Ternary Phase Envelope
                        utility = Flowsheet.GetUtility(Interfaces.Enums.FlowsheetUtility.PhaseEnvelopeTernary)
                        utility.Name = "TernaryEnvelope" & (obj.AttachedUtilities.Where(Function(x) x.GetUtilityType = Interfaces.Enums.FlowsheetUtility.PhaseEnvelopeTernary).Count + 1).ToString
                    Case 4
                        'True Critical Point
                        utility = Flowsheet.GetUtility(Interfaces.Enums.FlowsheetUtility.TrueCriticalPoint)
                        utility.Name = "TrueCriticalPoint" & (obj.AttachedUtilities.Where(Function(x) x.GetUtilityType = Interfaces.Enums.FlowsheetUtility.TrueCriticalPoint).Count + 1).ToString
                    Case 5
                        'Petroleum Cold Flow Properties
                        utility = Flowsheet.GetUtility(Interfaces.Enums.FlowsheetUtility.PetroleumProperties)
                        utility.Name = "PetroleumProperties" & (obj.AttachedUtilities.Where(Function(x) x.GetUtilityType = Interfaces.Enums.FlowsheetUtility.PetroleumProperties).Count + 1).ToString
                End Select

            Case 1 'VS

                utility = Flowsheet.GetUtility(Interfaces.Enums.FlowsheetUtility.SeparatorSizing)
                utility.Name = "VesselSizing" & (obj.AttachedUtilities.Where(Function(x) x.GetUtilityType = Interfaces.Enums.FlowsheetUtility.SeparatorSizing).Count + 1).ToString

            Case 2 'VALVE

                utility = Flowsheet.GetUtility(Interfaces.Enums.FlowsheetUtility.PSVSizing)
                utility.Name = "PressureSafetyValveSizing" & (obj.AttachedUtilities.Where(Function(x) x.GetUtilityType = Interfaces.Enums.FlowsheetUtility.PSVSizing).Count + 1).ToString

        End Select

        utility.AttachedTo = obj

        DirectCast(utility, DockContent).ShowHint = WeifenLuo.WinFormsUI.Docking.DockState.Document

        obj.AttachedUtilities.Add(utility)
        Flowsheet.DisplayForm(utility)

        AddHandler DirectCast(utility, DockContent).FormClosed, Sub()
                                                                    obj.AttachedUtilities.Remove(utility)
                                                                    utility.AttachedTo = Nothing
                                                                End Sub

        Me.Close()

    End Sub

    Private Sub ListBox3_SelectedIndexChanged(sender As Object, e As EventArgs) Handles ListBox3.SelectedIndexChanged
        If ListBox1.SelectedIndex >= 0 And ListBox2.SelectedIndex >= 0 And ListBox3.SelectedIndex >= 0 Then Button1.Enabled = True Else Button1.Enabled = False
    End Sub

End Class