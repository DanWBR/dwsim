Imports DWSIM.Thermodynamics.BaseClasses
Imports Cudafy
Imports Cudafy.Host
Imports System.Drawing

Public Class FormConfigCAPEOPEN2

    Inherits FormConfigPropertyPackageBase

    Public _selcomps As Dictionary(Of String, ConstantProperties)
    Public _availcomps As Dictionary(Of String, ConstantProperties)

    Public loaded As Boolean = False

    Private prevsort As System.ComponentModel.ListSortDirection = System.ComponentModel.ListSortDirection.Ascending
    Private prevcol As Integer = 1

    Dim ACSC1 As AutoCompleteStringCollection

    Private Sub FormConfigCAPEOPEN2_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        Application.EnableVisualStyles()
        Application.DoEvents()

        Me.cbGPU.Items.Clear()

        CudafyModes.Target = eGPUType.Emulator
        For Each prop As GPGPUProperties In CudafyHost.GetDeviceProperties(CudafyModes.Target, False)
            Me.cbGPU.Items.Add("Emulator | " & prop.Name & " (" & prop.DeviceId & ")")
        Next
        Try
            CudafyModes.Target = eGPUType.Cuda
            For Each prop As GPGPUProperties In CudafyHost.GetDeviceProperties(CudafyModes.Target, False)
                Me.cbGPU.Items.Add("CUDA | " & prop.Name & " (" & prop.DeviceId & ")")
            Next
        Catch ex As Exception

        End Try
        Try
            CudafyModes.Target = eGPUType.OpenCL
            For Each prop As GPGPUProperties In CudafyHost.GetDeviceProperties(CudafyModes.Target, False)
                Me.cbGPU.Items.Add("OpenCL | " & prop.Name & " (" & prop.DeviceId & ")")
            Next
        Catch ex As Exception

        End Try

        CudafyModes.Target = GlobalSettings.Settings.CudafyTarget

        Dim i As Integer = 0
        Me.cbParallelism.Items.Clear()
        Me.cbParallelism.Items.Add("Default")
        For i = 1 To System.Environment.ProcessorCount
            Me.cbParallelism.Items.Add(i.ToString)
        Next
        If GlobalSettings.Settings.MaxDegreeOfParallelism = -1 Then
            Me.cbParallelism.SelectedIndex = 0
        ElseIf GlobalSettings.Settings.MaxDegreeOfParallelism <= System.Environment.ProcessorCount Then
            Me.cbParallelism.SelectedItem = GlobalSettings.Settings.MaxDegreeOfParallelism.ToString
        Else
            Me.cbParallelism.SelectedIndex = Me.cbParallelism.Items.Count - 1
        End If

        Me.chkEnableParallelCalcs.Checked = GlobalSettings.Settings.EnableParallelProcessing
        Me.chkEnableGPUProcessing.Checked = GlobalSettings.Settings.EnableGPUProcessing
        Me.cbGPU.Enabled = Me.chkEnableGPUProcessing.Checked
        Me.tbGPUCaps.Enabled = Me.chkEnableGPUProcessing.Checked
        Me.cbParallelism.Enabled = Me.chkEnableParallelCalcs.Checked

        Me.TextBox1.AutoCompleteSource = AutoCompleteSource.CustomSource

        Me.lblName.Text = _pp.ComponentName
        Me.lblDescription.Text = _pp.ComponentDescription

        Dim comp As ConstantProperties

        If Not loaded Then

            ACSC1 = New AutoCompleteStringCollection

            For Each comp In _selcomps.Values
                Me.ListViewA.Items.Add(comp.Name, comp.Name, 0).Tag = comp.Name
            Next
            For Each comp In _availcomps.Values
                Dim idx As Integer = Me.AddCompToGrid(comp)
                If Not idx = -1 Then
                    For Each c As DataGridViewCell In Me.ogc1.Rows(idx).Cells
                        If comp.Acentric_Factor = 0.0# Or comp.Critical_Compressibility = 0.0# Then
                            c.Style.ForeColor = Color.Red
                            c.ToolTipText = _form.GetTranslatedString("CompMissingData")
                        End If
                    Next
                    ACSC1.Add(comp.Name)
                End If
            Next

            Try
                Me.TextBox1.AutoCompleteCustomSource = ACSC1
            Catch ex As Exception

            End Try

        Else

            For Each r As DataGridViewRow In ogc1.Rows
                If _availcomps.ContainsKey(r.Cells(0).Value) Then
                    comp = _availcomps(r.Cells(0).Value)
                    For Each c As DataGridViewCell In r.Cells
                        If comp.Acentric_Factor = 0.0# Or comp.Critical_Compressibility = 0.0# Then
                            c.Style.ForeColor = Color.Red
                            c.ToolTipText = _form.GetTranslatedString("CompMissingData")
                        End If
                    Next
                End If
            Next

            Try
                Me.ogc1.Sort(ogc1.Columns(1), System.ComponentModel.ListSortDirection.Ascending)
            Catch ex As Exception
            End Try

        End If

        Select Case _pp.FlashAlgorithm
            Case PropertyPackages.FlashMethod.DWSIMDefault
                ComboBoxFlashAlg.SelectedIndex = 0
            Case PropertyPackages.FlashMethod.NestedLoops3P,
                    PropertyPackages.FlashMethod.NestedLoops3PV2,
                    PropertyPackages.FlashMethod.NestedLoops3PV3
                ComboBoxFlashAlg.SelectedIndex = 1
            Case PropertyPackages.FlashMethod.InsideOut
                ComboBoxFlashAlg.SelectedIndex = 2
            Case PropertyPackages.FlashMethod.InsideOut3P
                ComboBoxFlashAlg.SelectedIndex = 3
            Case PropertyPackages.FlashMethod.GibbsMin2P
                ComboBoxFlashAlg.SelectedIndex = 4
            Case PropertyPackages.FlashMethod.GibbsMin3P
                ComboBoxFlashAlg.SelectedIndex = 5
            Case PropertyPackages.FlashMethod.NestedLoopsSLE
                ComboBoxFlashAlg.SelectedIndex = 6
            Case PropertyPackages.FlashMethod.NestedLoopsSLE_SS
                ComboBoxFlashAlg.SelectedIndex = 7
            Case PropertyPackages.FlashMethod.NestedLoopsImmiscible
                ComboBoxFlashAlg.SelectedIndex = 8
            Case Else
                ComboBoxFlashAlg.SelectedIndex = 0
        End Select

        Dim comps, selected As New ArrayList
        If _pp._tpcompids Is Nothing Then _pp._tpcompids = New String() {}
        For Each c As ConstantProperties In _selcomps.Values
            comps.Add(c.Name)
            For Each s As String In _pp._tpcompids
                If s = c.Name Then
                    selected.Add(c.Name)
                    Exit For
                End If
            Next
        Next

        Me.ListView2.Items.Clear()

        Dim n As Integer
        n = comps.Count - 1
        For i = 0 To n
            With Me.ListView2.Items.Add(comps(i), comps(i))
                For Each s As String In selected
                    If s = comps(i) Then
                        .Checked = True
                        Exit For
                    End If
                Next
                .Tag = comps(i)
            End With
        Next

        Select Case _pp._tpseverity
            Case 0
                Me.RadioButton1.Checked = True
            Case 1
                Me.RadioButton2.Checked = True
            Case 2
                Me.RadioButton3.Checked = True
        End Select

        Me.loaded = True

    End Sub

    Private Sub ComboBoxFlashAlg_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ComboBoxFlashAlg.SelectedIndexChanged
        Select Case ComboBoxFlashAlg.SelectedIndex
            Case 0
                Me._pp.FlashAlgorithm = PropertyPackages.FlashMethod.DWSIMDefault
                Me.GroupBox11.Enabled = False
            Case 1
                Me._pp.FlashAlgorithm = PropertyPackages.FlashMethod.NestedLoops3PV3
                Me.GroupBox11.Enabled = True
            Case 2
                Me._pp.FlashAlgorithm = PropertyPackages.FlashMethod.InsideOut
                Me.GroupBox11.Enabled = False
            Case 3
                Me._pp.FlashAlgorithm = PropertyPackages.FlashMethod.InsideOut3P
                Me.GroupBox11.Enabled = True
            Case 4
                Me._pp.FlashAlgorithm = PropertyPackages.FlashMethod.GibbsMin2P
                Me.GroupBox11.Enabled = False
            Case 5
                Me._pp.FlashAlgorithm = PropertyPackages.FlashMethod.GibbsMin3P
                Me.GroupBox11.Enabled = True
            Case 6
                Me._pp.FlashAlgorithm = PropertyPackages.FlashMethod.NestedLoopsSLE
                Me.GroupBox11.Enabled = False
            Case 7
                Me._pp.FlashAlgorithm = PropertyPackages.FlashMethod.NestedLoopsSLE_SS
                Me.GroupBox11.Enabled = False
            Case 8
                Me._pp.FlashAlgorithm = PropertyPackages.FlashMethod.NestedLoopsImmiscible
                Me.GroupBox11.Enabled = True
        End Select
    End Sub

    Private Sub RadioButton1_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RadioButton1.CheckedChanged, RadioButton2.CheckedChanged, RadioButton3.CheckedChanged

        If Me.RadioButton1.Checked Then _pp._tpseverity = 0
        If Me.RadioButton2.Checked Then _pp._tpseverity = 1
        If Me.RadioButton3.Checked Then _pp._tpseverity = 2

    End Sub

    Public Function AddCompToGrid(ByRef comp As ConstantProperties) As Integer

        Dim contains As Boolean = False
        For Each r As DataGridViewRow In ogc1.Rows
            If r.Cells(0).Value = comp.Name Then contains = True
        Next

        If Not contains Then
            Dim r As New DataGridViewRow
            r.CreateCells(ogc1, New Object() {comp.Name, comp.Name, comp.OriginalDB, "", comp.Formula})
            ogc1.Rows.Add(r)
            Return ogc1.Rows.Count - 1
        Else
            Return -1
        End If

    End Function

    Private Sub Button7_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button7.Click
        If Me.ogc1.SelectedRows.Count > 0 Then
            Me.AddCompToSimulation(Me.ogc1.SelectedRows(0).Index)
        End If
    End Sub

    Private Sub Button10_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button10.Click
        If Me.ListViewA.SelectedItems.Count > 0 Then
            Me.RemoveCompFromSimulation(Me.ListViewA.SelectedItems(0).Tag)
        End If
    End Sub

    Private Sub Button11_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button11.Click
        For Each lvi As ListViewItem In Me.ListViewA.Items
            Me.RemoveCompFromSimulation(lvi.Tag)
        Next
    End Sub

    Private Sub Button6_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button6.Click
         Me.ogc1.Sort(ogc1.Columns(1), System.ComponentModel.ListSortDirection.Ascending)
    End Sub

    Sub AddComponent(ByVal compID As String)
        If Not _selcomps.ContainsKey(compID) Then
            Dim tmpcomp As New ConstantProperties
            tmpcomp = _availcomps(compID)
            _selcomps.Add(tmpcomp.Name, tmpcomp)
            _availcomps.Remove(tmpcomp.Name)
            Me.ListViewA.Items.Add(tmpcomp.Name, (tmpcomp.Name), 0).Tag = tmpcomp.Name
        End If
    End Sub

    Sub RemoveComponent(ByVal compID As String)
        Me.RemoveCompFromSimulation(compID)
    End Sub

    Sub AddCompToSimulation(ByVal index As Integer)

        If Me.loaded Then
            If Not _selcomps.ContainsKey(ogc1.Rows(index).Cells(0).Value) Then
                Dim tmpcomp As New ConstantProperties
                tmpcomp = _availcomps(ogc1.Rows(index).Cells(0).Value)
                _selcomps.Add(tmpcomp.Name, tmpcomp)
                _availcomps.Remove(tmpcomp.Name)
                Me.ListViewA.Items.Add(tmpcomp.Name, tmpcomp.Name).Tag = tmpcomp.Name
                Me.ogc1.Rows.RemoveAt(index)
            End If
        End If

    End Sub

    Sub RemoveCompFromSimulation(ByVal compid As String)

        Dim tmpcomp As New ConstantProperties
        Dim nm As String = compid
        tmpcomp = _selcomps(nm)
        _selcomps.Remove(tmpcomp.Name)
        Me.ListViewA.Items.RemoveByKey(tmpcomp.Name)
        _availcomps.Add(tmpcomp.Name, tmpcomp)
        Me.AddCompToGrid(tmpcomp)

    End Sub

    Private Sub Button3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button3.Click
        _pp.DisplayEditingForm()
    End Sub

    Private Sub TextBox1_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TextBox1.TextChanged
        For Each r As DataGridViewRow In ogc1.Rows
            If Not r.Cells(1).Value Is Nothing Then
                If r.Cells(1).Value.ToString = Me.TextBox1.Text Then
                    r.Selected = True
                    If r.Visible Then ogc1.FirstDisplayedScrollingRowIndex = r.Index
                Else
                    r.Selected = False
                End If
            End If
        Next
        If TextBox1.Text = "" Then
            ogc1.FirstDisplayedScrollingRowIndex = 0
            For Each r As DataGridViewRow In ogc1.Rows
                r.Selected = False
            Next
        End If
    End Sub

    Private Sub ListView2_ItemChecked(ByVal sender As System.Object, ByVal e As System.Windows.Forms.ItemCheckedEventArgs) Handles ListView2.ItemChecked

        If loaded Then

            Try
                Dim i As Integer = 0
                Dim sel As New ArrayList
                For Each lvi2 As ListViewItem In Me.ListView2.Items
                    If lvi2.Checked Then sel.Add(lvi2.Tag)
                Next
                _pp._tpcompids = sel.ToArray(Type.GetType("System.String"))
            Catch ex As Exception

            End Try

        End If

    End Sub

    Private Sub TextBox1_KeyDown(ByVal sender As System.Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles TextBox1.KeyDown
        If e.KeyCode = Keys.Enter Then
            Call Button7_Click(sender, e)
            Me.TextBox1.Text = ""
        End If
    End Sub

    Public Sub GetCUDACaps(prop As GPGPUProperties)

        Dim i As Integer = 0

        Me.tbGPUCaps.Text = ""

        Me.tbGPUCaps.AppendText(String.Format("   --- General Information for device {0} ---", i) & vbCrLf)
        Me.tbGPUCaps.AppendText(String.Format("Name:  {0}", prop.Name) & vbCrLf)
        Me.tbGPUCaps.AppendText(String.Format("Device Id:  {0}", prop.DeviceId) & vbCrLf)
        Me.tbGPUCaps.AppendText(String.Format("Compute capability:  {0}.{1}", prop.Capability.Major, prop.Capability.Minor) & vbCrLf)
        Me.tbGPUCaps.AppendText(String.Format("Clock rate: {0}", prop.ClockRate) & vbCrLf)
        Me.tbGPUCaps.AppendText(String.Format("Simulated: {0}", prop.IsSimulated) & vbCrLf)

        Me.tbGPUCaps.AppendText(String.Format("   --- Memory Information for device {0} ---", i) & vbCrLf)
        Me.tbGPUCaps.AppendText(String.Format("Total global mem:  {0}", prop.TotalMemory) & vbCrLf)
        Me.tbGPUCaps.AppendText(String.Format("Total constant Mem:  {0}", prop.TotalConstantMemory) & vbCrLf)
        Me.tbGPUCaps.AppendText(String.Format("Max mem pitch:  {0}", prop.MemoryPitch) & vbCrLf)
        Me.tbGPUCaps.AppendText(String.Format("Texture Alignment:  {0}", prop.TextureAlignment) & vbCrLf)

        Me.tbGPUCaps.AppendText(String.Format("   --- MP Information for device {0} ---", i) & vbCrLf)
        Me.tbGPUCaps.AppendText(String.Format("Shared mem per mp: {0}", prop.SharedMemoryPerBlock) & vbCrLf)
        Me.tbGPUCaps.AppendText(String.Format("Registers per mp:  {0}", prop.RegistersPerBlock) & vbCrLf)
        Me.tbGPUCaps.AppendText(String.Format("Threads in warp:  {0}", prop.WarpSize) & vbCrLf)
        Me.tbGPUCaps.AppendText(String.Format("Max threads per block:  {0}", prop.MaxThreadsPerBlock) & vbCrLf)
        Me.tbGPUCaps.AppendText(String.Format("Max thread dimensions:  ({0}, {1}, {2})", prop.MaxThreadsSize.x, prop.MaxThreadsSize.y, prop.MaxThreadsSize.z) & vbCrLf)
        Me.tbGPUCaps.AppendText(String.Format("Max grid dimensions:  ({0}, {1}, {2})", prop.MaxGridSize.x, prop.MaxGridSize.y, prop.MaxGridSize.z) & vbCrLf)

        Me.tbGPUCaps.SelectionStart = 0
        Me.tbGPUCaps.SelectionLength = 0
        Me.tbGPUCaps.ScrollToCaret()

    End Sub

    Private Sub chkEnableParallelCalcs_CheckedChanged(sender As System.Object, e As System.EventArgs) Handles chkEnableParallelCalcs.CheckedChanged
        GlobalSettings.Settings.EnableParallelProcessing = Me.chkEnableParallelCalcs.Checked
        Me.cbParallelism.Enabled = Me.chkEnableParallelCalcs.Checked
    End Sub

    Private Sub cbParallelism_SelectedIndexChanged(sender As System.Object, e As System.EventArgs) Handles cbParallelism.SelectedIndexChanged
        If Me.cbParallelism.SelectedIndex = 0 Then
            GlobalSettings.Settings.MaxDegreeOfParallelism = -1
        Else
            GlobalSettings.Settings.MaxDegreeOfParallelism = Me.cbParallelism.SelectedItem
        End If
    End Sub

    Private Sub cbGPU_SelectedIndexChanged(sender As System.Object, e As System.EventArgs) Handles cbGPU.SelectedIndexChanged
        If loaded Then
            If cbGPU.SelectedItem.ToString.Contains("Emulator") Then
                GlobalSettings.Settings.CudafyTarget = eGPUType.Emulator
            ElseIf cbGPU.SelectedItem.ToString.Contains("CUDA") Then
                GlobalSettings.Settings.CudafyTarget = eGPUType.Cuda
            Else
                GlobalSettings.Settings.CudafyTarget = eGPUType.OpenCL
            End If
            For Each prop As GPGPUProperties In CudafyHost.GetDeviceProperties(CudafyModes.Target, False)
                If Me.cbGPU.SelectedItem.ToString.Split("|")(1).Contains(prop.Name) Then
                    'GlobalSettings.Settings.SelectedGPU = Me.cbGPU.SelectedItem.ToString
                    GlobalSettings.Settings.CudafyDeviceID = prop.DeviceId
                    GetCUDACaps(prop)
                    Exit For
                End If
            Next
            MessageBox.Show(_form.GetTranslatedString("NextStartupOnly"))
        End If
    End Sub

    Private Sub chkEnableGPUProcessing_CheckedChanged(sender As System.Object, e As System.EventArgs) Handles chkEnableGPUProcessing.CheckedChanged
        Me.cbGPU.Enabled = chkEnableGPUProcessing.Checked
        Me.tbGPUCaps.Enabled = chkEnableGPUProcessing.Checked
        GlobalSettings.Settings.EnableGPUProcessing = chkEnableGPUProcessing.Checked
    End Sub

End Class