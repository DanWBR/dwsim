Imports DWSIM.DWSIM.Thermodynamics.BaseClasses
Imports DWSIM.DWSIM.SimulationObjects.Streams

Public Class CompositionEditorForm
    Inherits System.Windows.Forms.Form
    Public Componentes As Dictionary(Of String, Compound)
    Public InitialComposition As Dictionary(Of String, Double)
    Public Stream As MaterialStream
    Public Solvent As String = ""
    Public Q, W, T As Double
    Public SU As DWSIM.SystemsOfUnits.Units
    Public NF As String = ""
    Private loaded As Boolean = False

    Private Sub CompositionEditorForm_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        Dim comp As DWSIM.Thermodynamics.BaseClasses.Compound
        GridComp.Rows.Clear()
        GridComp.Columns(0).CellTemplate.Style.Format = Stream.FlowSheet.Options.FractionNumberFormat
        GridComp.Columns(1).CellTemplate.Style.Format = Stream.FlowSheet.Options.FractionNumberFormat
        ComboBox1.Items.Clear()
        If Solvent Is Nothing Then Solvent = ""
        If InitialComposition Is Nothing Then InitialComposition = New Dictionary(Of String, Double)()
        For Each comp In Me.Componentes.Values
            If Not InitialComposition.ContainsKey(comp.Nome) Then InitialComposition.Add(comp.Nome, comp.FracaoMolar)
            GridComp.Rows.Add(New Object() {comp.FracaoMolar, InitialComposition(comp.Nome)})
            GridComp.Rows(GridComp.Rows.Count - 1).HeaderCell.Value = DWSIM.App.GetComponentName(comp.Nome) & " (" & comp.ConstantProperties.OriginalDB & ")"
            GridComp.Rows(GridComp.Rows.Count - 1).HeaderCell.Tag = comp.Nome
            ComboBox1.Items.Add(DWSIM.App.GetComponentName(comp.Nome))
        Next
        If Solvent <> "" Then ComboBox1.SelectedItem = Solvent Else ComboBox1.SelectedIndex = 0
        Try
            Dim v As Double = 0
            For Each r As DataGridViewRow In Me.GridComp.Rows
                v += CDbl(r.Cells(0).Value)
            Next
            Me.Label3.Text = Format(v, "#0.0000")
            Me.Label3.ForeColor = Color.SlateBlue
            If Math.Abs(1 - v) < 0.0001 Then
                Me.Label2.Text = "OK"
                Me.Label2.ForeColor = Color.Green
            End If
        Catch ex As Exception
            Me.Label3.Text = DWSIM.App.GetLocalString("indefinido")
            Me.Label3.ForeColor = Color.Red
        End Try

        loaded = True

    End Sub

    Private Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles KButton1.Click
        Dim row As DataGridViewRow
        For Each row In GridComp.Rows
            row.Cells(0).Value = 0
        Next
    End Sub

    Private Sub Button23_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles KButton23.Click
        Dim total As Double = 0
        Dim row As DataGridViewRow
        For Each row In GridComp.Rows
            total += row.Cells(0).Value
        Next
        For Each row In GridComp.Rows
            row.Cells(0).Value = row.Cells(0).Value / total
        Next
    End Sub

    Private Sub Button3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles KButton3.Click
        Me.Close()
    End Sub

    Private Sub Button2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles KButton2.Click

        Call Me.ValidateData()

        If Not Me.Label2.Text = DWSIM.App.GetLocalString("Erro") Then

            Dim comp As DWSIM.Thermodynamics.BaseClasses.Compound
            Dim row As DataGridViewRow
            Dim mmtotal As Double = 0
            Dim mtotal As Double = 0
            If Me.RadioButton1.Checked Then

                Call Me.Button23_Click(sender, e)
                For Each row In Me.GridComp.Rows
                    Me.Componentes(row.HeaderCell.Tag).FracaoMolar = row.Cells(0).Value
                Next
                For Each comp In Me.Componentes.Values
                    mtotal += comp.FracaoMolar.GetValueOrDefault * comp.ConstantProperties.Molar_Weight
                Next
                For Each comp In Me.Componentes.Values
                    comp.FracaoMassica = comp.FracaoMolar.GetValueOrDefault * comp.ConstantProperties.Molar_Weight / mtotal
                Next

            ElseIf Me.RadioButton2.Checked Then

                Call Me.Button23_Click(sender, e)
                For Each row In Me.GridComp.Rows
                    Me.Componentes(row.HeaderCell.Tag).FracaoMassica = row.Cells(0).Value
                Next
                For Each comp In Me.Componentes.Values
                    mmtotal += comp.FracaoMassica.GetValueOrDefault / comp.ConstantProperties.Molar_Weight
                Next
                For Each comp In Me.Componentes.Values
                    comp.FracaoMolar = comp.FracaoMassica.GetValueOrDefault / comp.ConstantProperties.Molar_Weight / mmtotal
                Next

            ElseIf Me.RadioButton3.Checked Then

                Dim total As Double = 0
                For Each row In GridComp.Rows
                    total += row.Cells(0).Value
                Next
                Dim cv As New DWSIM.SystemsOfUnits.Converter
                Q = Converter.ConvertToSI(SU.molarflow, total)
                For Each row In Me.GridComp.Rows
                    Me.Componentes(row.HeaderCell.Tag).FracaoMolar = row.Cells(0).Value / total
                Next
                For Each comp In Me.Componentes.Values
                    mtotal += comp.FracaoMolar.GetValueOrDefault * comp.ConstantProperties.Molar_Weight
                Next
                W = 0
                For Each comp In Me.Componentes.Values
                    comp.FracaoMassica = comp.FracaoMolar.GetValueOrDefault * comp.ConstantProperties.Molar_Weight / mtotal
                    W += comp.FracaoMolar.GetValueOrDefault * comp.ConstantProperties.Molar_Weight / 1000 * Q
                Next

            ElseIf Me.RadioButton4.Checked Then

                Dim total As Double = 0
                For Each row In GridComp.Rows
                    total += row.Cells(0).Value
                Next
                Dim cv As New DWSIM.SystemsOfUnits.Converter
                W = Converter.ConvertToSI(SU.massflow, total)
                For Each row In Me.GridComp.Rows
                    Me.Componentes(row.HeaderCell.Tag).FracaoMassica = row.Cells(0).Value / total
                Next
                For Each comp In Me.Componentes.Values
                    mmtotal += comp.FracaoMassica.GetValueOrDefault / comp.ConstantProperties.Molar_Weight
                Next
                Q = 0
                For Each comp In Me.Componentes.Values
                    comp.FracaoMolar = comp.FracaoMassica.GetValueOrDefault / comp.ConstantProperties.Molar_Weight / mmtotal
                    Q += comp.FracaoMassica.GetValueOrDefault * W / comp.ConstantProperties.Molar_Weight * 1000
                Next

            ElseIf Me.RadioButton5.Checked Then

                'molarity = mol solute per liter solution
                Dim n As Integer = Me.Componentes.Count
                Dim liqdens(n - 1), nbp(n - 1) As Double
                Dim ipp As New DWSIM.SimulationObjects.PropertyPackages.RaoultPropertyPackage()
                ipp.CurrentMaterialStream = Stream
                Dim i As Integer = 0
                For Each s As Compound In Me.Componentes.Values
                    nbp(i) = s.ConstantProperties.Normal_Boiling_Point
                    If T > nbp(i) Then
                        liqdens(i) = ipp.AUX_LIQDENSi(s, nbp(i))
                    Else
                        liqdens(i) = ipp.AUX_LIQDENSi(s, T)
                    End If
                    i += 1
                Next

                Dim total As Double = 0
                Dim val As Double = 0
                i = 0
                For Each row In Me.GridComp.Rows
                    If DWSIM.App.GetLocalString(row.HeaderCell.Tag) = Me.ComboBox1.SelectedItem.ToString Then
                        total += row.Cells(0).Value / 1000 * liqdens(i) / Me.Componentes(row.HeaderCell.Tag).ConstantProperties.Molar_Weight * 1000
                    Else
                        total += row.Cells(0).Value
                    End If
                    i += 1
                Next

                Q = total

                i = 0
                For Each row In Me.GridComp.Rows
                    If DWSIM.App.GetLocalString(row.HeaderCell.Tag) = Me.ComboBox1.SelectedItem.ToString Then
                        Me.Componentes(row.HeaderCell.Tag).FracaoMolar = row.Cells(0).Value / 1000 * liqdens(i) / Me.Componentes(row.HeaderCell.Tag).ConstantProperties.Molar_Weight * 1000 / total
                    Else
                        Me.Componentes(row.HeaderCell.Tag).FracaoMolar = row.Cells(0).Value / total
                    End If
                    i += 1
                Next

                For Each comp In Me.Componentes.Values
                    mtotal += comp.FracaoMolar.GetValueOrDefault * comp.ConstantProperties.Molar_Weight
                Next

                W = 0
                For Each comp In Me.Componentes.Values
                    comp.FracaoMassica = comp.FracaoMolar.GetValueOrDefault * comp.ConstantProperties.Molar_Weight / mtotal
                    W += comp.FracaoMolar.GetValueOrDefault * comp.ConstantProperties.Molar_Weight / 1000 * Q
                Next

                ipp = Nothing

            ElseIf Me.RadioButton6.Checked Then

                'molarity = mol solute per kg solvent

                Dim total As Double = 0
                Dim val As Double = 0
                For Each row In Me.GridComp.Rows
                    If DWSIM.App.GetLocalString(row.HeaderCell.Tag) = Me.ComboBox1.SelectedItem.ToString Then
                        total += row.Cells(0).Value / Me.Componentes(row.HeaderCell.Tag).ConstantProperties.Molar_Weight * 1000
                    Else
                        total += row.Cells(0).Value
                    End If
                Next

                Q = total

                For Each row In Me.GridComp.Rows
                    If DWSIM.App.GetLocalString(row.HeaderCell.Tag) = Me.ComboBox1.SelectedItem.ToString Then
                        Me.Componentes(row.HeaderCell.Tag).FracaoMolar = row.Cells(0).Value / Me.Componentes(row.HeaderCell.Tag).ConstantProperties.Molar_Weight * 1000 / total
                    Else
                        Me.Componentes(row.HeaderCell.Tag).FracaoMolar = row.Cells(0).Value / total
                    End If
                Next

                For Each comp In Me.Componentes.Values
                    mtotal += comp.FracaoMolar.GetValueOrDefault * comp.ConstantProperties.Molar_Weight
                Next

                W = 0
                For Each comp In Me.Componentes.Values
                    comp.FracaoMassica = comp.FracaoMolar.GetValueOrDefault * comp.ConstantProperties.Molar_Weight / mtotal
                    W += comp.FracaoMolar.GetValueOrDefault * comp.ConstantProperties.Molar_Weight / 1000 * Q
                Next

            ElseIf Me.RadioButton7.Checked Then

                'liquid vol. frac
                Dim n As Integer = Me.Componentes.Count
                Dim liqdens(n - 1), nbp(n - 1), volfrac(n - 1), totalvol As Double
                Dim ipp As New DWSIM.SimulationObjects.PropertyPackages.RaoultPropertyPackage()
                ipp.CurrentMaterialStream = Stream
                Dim T As Double = 273.15 + 15.56 'standard temperature
                Dim i As Integer = 0
                totalvol = 0.0#
                For Each s As Compound In Me.Componentes.Values
                    nbp(i) = s.ConstantProperties.Normal_Boiling_Point
                    If T > nbp(i) Then
                        liqdens(i) = ipp.AUX_LIQDENSi(s, nbp(i))
                    Else
                        liqdens(i) = ipp.AUX_LIQDENSi(s, T)
                    End If
                    i += 1
                Next
                mtotal = 0.0#
                i = 0
                For Each row In Me.GridComp.Rows
                    mtotal += row.Cells(0).Value * liqdens(i)
                    i += 1
                Next
                i = 0
                For Each row In Me.GridComp.Rows
                    Me.Componentes(row.HeaderCell.Tag).FracaoMassica = row.Cells(0).Value * liqdens(i) / mtotal
                    i += 1
                Next
                mmtotal = 0.0#
                For Each comp In Me.Componentes.Values
                    mmtotal += comp.FracaoMassica.GetValueOrDefault / comp.ConstantProperties.Molar_Weight
                Next
                For Each comp In Me.Componentes.Values
                    comp.FracaoMolar = comp.FracaoMassica.GetValueOrDefault / comp.ConstantProperties.Molar_Weight / mmtotal
                Next
                ipp = Nothing

            End If

            If Not Me.ComboBox1.SelectedItem Is Nothing Then Me.Solvent = Me.ComboBox1.SelectedItem.ToString

            Me.InitialComposition.Clear()
            For Each comp In Me.Componentes.Values
                Me.InitialComposition.Add(comp.Nome, comp.FracaoMolar.GetValueOrDefault)
            Next

        End If

    End Sub

    Sub ValidateData()

        Me.Label2.Text = "OK"
        Me.Label2.ForeColor = Color.Green
        Dim row As DataGridViewRow
        For Each row In Me.GridComp.Rows
            If Not Double.TryParse(row.Cells(0).Value, New Double) Then
                Me.Label2.Text = DWSIM.App.GetLocalString("Erro")
                Me.Label2.ForeColor = Color.Red
                Exit Sub
            End If
        Next

    End Sub

    Private Sub GridComp_CellValueChanged(ByVal sender As Object, ByVal e As System.Windows.Forms.DataGridViewCellEventArgs) Handles GridComp.CellValueChanged

        If loaded Then
            Try
                Dim v As Double = 0
                If RadioButton5.Checked Then
                    Dim n As Integer = Me.Componentes.Count
                    Dim liqdens(n - 1), nbp(n - 1), volfrac(n - 1), totalvol As Double
                    Dim ipp As New DWSIM.SimulationObjects.PropertyPackages.RaoultPropertyPackage()
                    ipp.CurrentMaterialStream = Stream
                    Dim i As Integer = 0
                    totalvol = 0.0#
                    For Each s As Compound In Me.Componentes.Values
                        nbp(i) = s.ConstantProperties.Normal_Boiling_Point
                        If T > nbp(i) Then
                            liqdens(i) = ipp.AUX_LIQDENSi(s, nbp(i))
                        Else
                            liqdens(i) = ipp.AUX_LIQDENSi(s, T)
                        End If
                        i += 1
                    Next
                    ipp = Nothing
                    i = 0
                    For Each r As DataGridViewRow In Me.GridComp.Rows
                        If DWSIM.App.GetLocalString(r.HeaderCell.Tag) = Me.ComboBox1.SelectedItem.ToString Then
                            v += r.Cells(0).Value / 1000 * liqdens(i) / Me.Componentes(r.HeaderCell.Tag).ConstantProperties.Molar_Weight * 1000
                        Else
                            v += CDbl(r.Cells(0).Value)
                        End If
                        i += 1
                    Next
                ElseIf RadioButton6.Checked Then
                    Dim i As Integer = 0
                    For Each r As DataGridViewRow In Me.GridComp.Rows
                        If DWSIM.App.GetLocalString(r.HeaderCell.Tag) = Me.ComboBox1.SelectedItem.ToString Then
                            v += r.Cells(0).Value / Me.Componentes(r.HeaderCell.Tag).ConstantProperties.Molar_Weight * 1000
                        Else
                            v += CDbl(r.Cells(0).Value)
                        End If
                        i += 1
                    Next
                Else
                    For Each r As DataGridViewRow In Me.GridComp.Rows
                        v += CDbl(r.Cells(0).Value)
                    Next
                End If
                If Me.RadioButton1.Checked Or Me.RadioButton2.Checked Then
                    Me.Label3.Text = Format(v, "#0.0000")
                ElseIf Me.RadioButton3.Checked Then
                    Me.Label3.Text = Format(v, NF) & " " & SU.molarflow
                ElseIf Me.RadioButton4.Checked Then
                    Me.Label3.Text = Format(v, NF) & " " & SU.massflow
                ElseIf Me.RadioButton5.Checked Then
                    Me.Label3.Text = Format(v, NF) & " " & SU.molarflow
                ElseIf Me.RadioButton6.Checked Then
                    Me.Label3.Text = Format(v, NF) & " " & SU.molarflow
                ElseIf Me.RadioButton7.Checked Then
                    Me.Label3.Text = Format(v, "#0.0000")
                End If
                Me.Label3.ForeColor = Color.SlateBlue
            Catch ex As Exception
                Me.Label3.Text = DWSIM.App.GetLocalString("indefinido")
                Me.Label3.ForeColor = Color.Red
            End Try
        End If


    End Sub

    Private Sub RadioButton_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RadioButton1.CheckedChanged,
                                                                                                                RadioButton2.CheckedChanged,
                                                                                                                RadioButton3.CheckedChanged,
                                                                                                                RadioButton4.CheckedChanged,
                                                                                                                RadioButton5.CheckedChanged,
                                                                                                                RadioButton6.CheckedChanged,
                                                                                                                RadioButton7.CheckedChanged,
                                                                                                                ComboBox1.SelectedIndexChanged

        If RadioButton5.Checked Or RadioButton6.Checked Then
            ComboBox1.Enabled = True
            KButton23.Enabled = False
            ButtonEqualize.Enabled = False
            If RadioButton5.Checked Then
                Label4.Text = DWSIM.App.GetLocalString("MolarityUnits")
            Else
                Label4.Text = DWSIM.App.GetLocalString("MolalityUnits")
            End If
        Else
            ComboBox1.Enabled = False
            KButton23.Enabled = True
            ButtonEqualize.Enabled = True
            Label4.Text = ""
        End If

        If Me.loaded Then

            Call Me.ValidateData()

            If Not Me.Label2.Text = DWSIM.App.GetLocalString("Erro") Then

                Call Me.Button23_Click(sender, e)

                Dim row As DataGridViewRow
                If Me.RadioButton1.Checked Then
                    For Each row In Me.GridComp.Rows
                        row.Cells(0).Value = Me.Componentes(row.HeaderCell.Tag).FracaoMolar
                    Next
                ElseIf Me.RadioButton2.Checked Then
                    For Each row In Me.GridComp.Rows
                        row.Cells(0).Value = Me.Componentes(row.HeaderCell.Tag).FracaoMassica
                    Next
                ElseIf Me.RadioButton3.Checked Then
                    Dim cv As New DWSIM.SystemsOfUnits.Converter
                    For Each row In Me.GridComp.Rows
                        row.Cells(0).Value = Converter.ConvertFromSI(SU.molarflow, Me.Componentes(row.HeaderCell.Tag).FracaoMolar.GetValueOrDefault * Q)
                    Next
                ElseIf Me.RadioButton4.Checked Then
                    Dim cv As New DWSIM.SystemsOfUnits.Converter
                    For Each row In Me.GridComp.Rows
                        row.Cells(0).Value = Converter.ConvertFromSI(SU.massflow, Me.Componentes(row.HeaderCell.Tag).FracaoMassica.GetValueOrDefault * W)
                    Next
                ElseIf Me.RadioButton5.Checked Then
                    'molarity = mol solute per liter solution
                    Dim n As Integer = Me.Componentes.Count
                    Dim liqdens(n - 1), nbp(n - 1) As Double
                    Dim ipp As New DWSIM.SimulationObjects.PropertyPackages.RaoultPropertyPackage()
                    ipp.CurrentMaterialStream = Stream
                    Dim i As Integer = 0
                    For Each s As Compound In Me.Componentes.Values
                        nbp(i) = s.ConstantProperties.Normal_Boiling_Point
                        If T > nbp(i) Then
                            liqdens(i) = ipp.AUX_LIQDENSi(s, nbp(i))
                        Else
                            liqdens(i) = ipp.AUX_LIQDENSi(s, T)
                        End If
                        i += 1
                    Next
                    Dim cv As New DWSIM.SystemsOfUnits.Converter
                    i = 0
                    For Each row In Me.GridComp.Rows
                        If DWSIM.App.GetLocalString(row.HeaderCell.Tag) = Me.ComboBox1.SelectedItem.ToString Then
                            row.Cells(0).Value = Me.Componentes(row.HeaderCell.Tag).FracaoMolar.GetValueOrDefault * Q * Me.Componentes(row.HeaderCell.Tag).ConstantProperties.Molar_Weight / 1000 / liqdens(i) * 1000
                        Else
                            row.Cells(0).Value = Me.Componentes(row.HeaderCell.Tag).FracaoMolar.GetValueOrDefault * Q
                        End If
                        i += 1
                    Next
                ElseIf Me.RadioButton6.Checked Then
                    'molarity = mol solute per kg solvent
                    Dim cv As New DWSIM.SystemsOfUnits.Converter
                    For Each row In Me.GridComp.Rows
                        If DWSIM.App.GetLocalString(row.HeaderCell.Tag) = Me.ComboBox1.SelectedItem.ToString Then
                            row.Cells(0).Value = Me.Componentes(row.HeaderCell.Tag).FracaoMassica.GetValueOrDefault * W
                        Else
                            row.Cells(0).Value = Me.Componentes(row.HeaderCell.Tag).FracaoMolar.GetValueOrDefault * Q
                        End If
                    Next
                ElseIf Me.RadioButton7.Checked Then
                    'liquid vol. frac
                    Dim n As Integer = Me.Componentes.Count
                    Dim liqdens(n - 1), nbp(n - 1), volfrac(n - 1), totalvol As Double
                    Dim ipp As New DWSIM.SimulationObjects.PropertyPackages.RaoultPropertyPackage()
                    ipp.CurrentMaterialStream = Stream
                    Dim i As Integer = 0
                    totalvol = 0.0#
                    For Each s As Compound In Me.Componentes.Values
                        nbp(i) = s.ConstantProperties.Normal_Boiling_Point
                        If T > nbp(i) Then
                            liqdens(i) = ipp.AUX_LIQDENSi(s, nbp(i))
                        Else
                            liqdens(i) = ipp.AUX_LIQDENSi(s, T)
                        End If
                        totalvol += s.FracaoMolar * s.ConstantProperties.Molar_Weight / liqdens(i)
                        i += 1
                    Next
                    i = 0
                    For Each row In Me.GridComp.Rows
                        row.Cells(0).Value = Me.Componentes(row.HeaderCell.Tag).FracaoMolar * Me.Componentes(row.HeaderCell.Tag).ConstantProperties.Molar_Weight / liqdens(i) / totalvol
                        i += 1
                    Next
                    ipp = Nothing
                End If

            End If

        End If

    End Sub

    Private Sub GridComp_KeyDown(ByVal sender As System.Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles GridComp.KeyDown
        If e.KeyCode = Keys.V And e.Modifiers = Keys.Control Then PasteData(GridComp)
    End Sub

    Public Sub PasteData(ByRef dgv As DataGridView)
        Dim tArr() As String
        Dim arT() As String
        Dim i, ii As Integer
        Dim c, cc, r As Integer

        tArr = Clipboard.GetText().Split(Environment.NewLine)

        If dgv.SelectedCells.Count > 0 Then
            r = dgv.SelectedCells(0).RowIndex
            c = dgv.SelectedCells(0).ColumnIndex
        Else
            r = 0
            c = 0
        End If
        For i = 0 To tArr.Length - 2
            If tArr(i) <> "" Then
                arT = tArr(i).Split(vbTab)
                For ii = 0 To arT.Length - 1
                    If r > dgv.Rows.Count - 1 Then
                        dgv.Rows(0).Cells(0).Selected = True
                    End If
                Next
                r = r + 1
            End If
        Next
        If dgv.SelectedCells.Count > 0 Then
            r = dgv.SelectedCells(0).RowIndex
            c = dgv.SelectedCells(0).ColumnIndex
        Else
            r = 0
            c = 0
        End If
        For i = 0 To tArr.Length - 2
            If tArr(i) <> "" Then
                arT = tArr(i).Split(vbTab)
                cc = c
                For ii = 0 To arT.Length - 1
                    cc = 0
                    If cc > dgv.ColumnCount Then Exit For
                    dgv.Item(cc, r).Value = arT(ii).TrimStart
                    cc = cc + 1
                Next
                r = r + 1
            End If
        Next

    End Sub

    Public Sub New()

        ' This call is required by the designer.
        InitializeComponent()

        ' Add any initialization after the InitializeComponent() call.

    End Sub

    Private Sub ButtonEqualize_Click(sender As System.Object, e As System.EventArgs) Handles ButtonEqualize.Click
        Dim total As Double = 0
        Dim row As DataGridViewRow
        For Each row In GridComp.Rows
            row.Cells(0).Value = 1 / GridComp.Rows.Count
        Next
    End Sub

    Private Sub tbTag_TextChanged(sender As Object, e As EventArgs) Handles tbTag.TextChanged
        If loaded Then
            Stream.GraphicObject.Tag = Me.tbTag.Text
            Me.Text = Me.tbTag.Text & DWSIM.App.GetLocalString("EditComp")
        End If
    End Sub

 End Class