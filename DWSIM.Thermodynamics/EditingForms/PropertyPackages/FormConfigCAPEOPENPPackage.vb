Imports DWSIM.Thermodynamics.BaseClasses
Imports Cudafy
Imports Cudafy.Host
Imports System.Drawing
Imports System.Reflection
Imports DWSIM.Thermodynamics.PropertyPackages
Imports System.IO

Public Class FormConfigCAPEOPENPPackage

    Inherits FormConfigPropertyPackageBase

    Public loaded As Boolean = False

    Dim ACSC1 As AutoCompleteStringCollection

    Public FlashAlgorithms As New Dictionary(Of String, Thermodynamics.PropertyPackages.Auxiliary.FlashAlgorithms.FlashAlgorithm)

    Private Sub FormConfigCAPEOPENPPackage_FormClosing(sender As Object, e As FormClosingEventArgs) Handles Me.FormClosing
        Try
            Dim inifile As String = My.Computer.FileSystem.SpecialDirectories.MyDocuments & Path.DirectorySeparatorChar & "DWSIM Application Data" & Path.DirectorySeparatorChar & "config.ini"
            GlobalSettings.Settings.SaveExcelSettings(inifile)
        Catch ex As Exception
        End Try
    End Sub

    Private Sub FormConfigCAPEOPEN2_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        ExtensionMethods.ChangeDefaultFont(Me)

        Me.TopMost = True

        Application.DoEvents()

        Me.chkEnableParallelCalcs.Checked = GlobalSettings.Settings.EnableParallelProcessing
        Me.cbSIMD.Checked = GlobalSettings.Settings.UseSIMDExtensions

        Me.TextBox1.AutoCompleteSource = AutoCompleteSource.CustomSource

        Me.lblName.Text = _pp.ComponentName
        Me.lblDescription.Text = _pp.ComponentDescription
        Me.tbErrorLog.Text = _pp.ExceptionLog

        Dim comp As ConstantProperties

        If Not loaded Then

            ACSC1 = New AutoCompleteStringCollection

            For Each comp In _pp._selectedcomps.Values
                Me.ListViewA.Items.Add(comp.Name, comp.Name, 0).Tag = comp.Name
            Next
            For Each comp In _pp._availablecomps.Values
                Dim idx As Integer = Me.AddCompToGrid(comp)
                If Not idx = -1 Then
                    ACSC1.Add(comp.Name)
                End If
            Next

            Try
                Me.TextBox1.AutoCompleteCustomSource = ACSC1
            Catch ex As Exception

            End Try

        Else

            For Each r As DataGridViewRow In ogc1.Rows
                If _pp._availablecomps.ContainsKey(r.Cells(0).Value) Then
                    comp = _pp._availablecomps(r.Cells(0).Value)
                End If
            Next

            Try
                Me.ogc1.Sort(ogc1.Columns(1), System.ComponentModel.ListSortDirection.Ascending)
            Catch ex As Exception
            End Try

        End If

        Me.loaded = True

    End Sub

    Public Function AddCompToGrid(ByRef comp As ConstantProperties) As Integer

        Dim contains As Boolean = False
        For Each r As DataGridViewRow In ogc1.Rows
            If r.Cells(0).Value = comp.Name Then contains = True
        Next

        If Not contains Then
            Dim r As New DataGridViewRow
            r.CreateCells(ogc1, New Object() {comp.Name, comp.Name, comp.OriginalDB, comp.Formula})
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

    Private Sub Button6_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        Me.ogc1.Sort(ogc1.Columns(1), System.ComponentModel.ListSortDirection.Ascending)
    End Sub

    Sub AddComponent(ByVal compID As String)
        If Not _pp._selectedcomps.ContainsKey(compID) Then
            Dim tmpcomp As New ConstantProperties
            tmpcomp = _pp._availablecomps(compID)
            _pp._selectedcomps.Add(tmpcomp.Name, tmpcomp)
            _pp._availablecomps.Remove(tmpcomp.Name)
            Me.ListViewA.Items.Add(tmpcomp.Name, (tmpcomp.Name), 0).Tag = tmpcomp.Name
        End If
    End Sub

    Sub RemoveComponent(ByVal compID As String)
        Me.RemoveCompFromSimulation(compID)
    End Sub

    Sub AddCompToSimulation(ByVal index As Integer)

        If Me.loaded Then
            If Not _pp._selectedcomps.ContainsKey(ogc1.Rows(index).Cells(0).Value) Then
                Dim tmpcomp As New ConstantProperties
                tmpcomp = _pp._availablecomps(ogc1.Rows(index).Cells(0).Value)
                _pp._selectedcomps.Add(tmpcomp.Name, tmpcomp)
                _pp._availablecomps.Remove(tmpcomp.Name)
                Me.ListViewA.Items.Add(tmpcomp.Name & " (" & tmpcomp.OriginalDB & ")", tmpcomp.Name).Tag = tmpcomp.Name
                Me.ogc1.Rows.RemoveAt(index)
            End If
        End If

    End Sub

    Sub RemoveCompFromSimulation(ByVal compid As String)

        Dim tmpcomp As New ConstantProperties
        Dim nm As String = compid
        tmpcomp = _pp._selectedcomps(nm)
        _pp._selectedcomps.Remove(tmpcomp.Name)
        Me.ListViewA.Items.RemoveByKey(tmpcomp.Name)
        _pp._availablecomps.Add(tmpcomp.Name, tmpcomp)
        Me.AddCompToGrid(tmpcomp)

    End Sub

    Private Sub Button3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button3.Click

        _pp.DisplayGroupedEditingForm()

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

    Private Sub TextBox1_KeyDown(ByVal sender As System.Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles TextBox1.KeyDown
        If e.KeyCode = Keys.Enter Then
            Call Button7_Click(sender, e)
            Me.TextBox1.Text = ""
        End If
    End Sub

    Private Sub chkEnableParallelCalcs_CheckedChanged(sender As System.Object, e As System.EventArgs) Handles chkEnableParallelCalcs.CheckedChanged
        GlobalSettings.Settings.EnableParallelProcessing = Me.chkEnableParallelCalcs.Checked
    End Sub

    Private Sub Button5_Click(sender As Object, e As EventArgs)

        Dim fa As Auxiliary.FlashAlgorithms.FlashAlgorithm = _pp.FlashBase
        Dim f As New Thermodynamics.FlashAlgorithmConfig() With {.Settings = fa.FlashSettings}

        f.ShowDialog(Me)
        fa.FlashSettings = f.Settings
        f.Dispose()
        f = Nothing

    End Sub

    Private Sub cbSIMD_CheckedChanged(sender As Object, e As EventArgs) Handles cbSIMD.CheckedChanged
        GlobalSettings.Settings.UseSIMDExtensions = Me.cbSIMD.Checked
    End Sub

    Private Sub ogc1_CellDoubleClick(sender As Object, e As DataGridViewCellEventArgs) Handles ogc1.CellDoubleClick
        If e.RowIndex > -1 Then AddCompToSimulation(e.RowIndex)
    End Sub

    Sub AddFlashAlgorithms()

        Dim calculatorassembly = Assembly.GetExecutingAssembly
        Dim availableTypes As New List(Of Type)()

        availableTypes.AddRange(calculatorassembly.GetTypes().Where(Function(x) If(x.GetInterface("DWSIM.Interfaces.IFlashAlgorithm") IsNot Nothing, True, False)))

        For Each item In availableTypes.OrderBy(Function(x) x.Name)
            If Not item.IsAbstract Then
                Dim obj = DirectCast(Activator.CreateInstance(item), Interfaces.IFlashAlgorithm)
                obj.Tag = obj.Name
                If Not obj.InternalUseOnly Then FlashAlgorithms.Add(obj.Name, obj)
                If obj.Name.Contains("Gibbs") Then
                    Dim obj2 = DirectCast(Activator.CreateInstance(item), Interfaces.IFlashAlgorithm)
                    obj2.Tag = obj2.Name
                    DirectCast(obj2, Auxiliary.FlashAlgorithms.GibbsMinimization3P).ForceTwoPhaseOnly = True
                    FlashAlgorithms.Add(obj2.Name, obj2)
                End If
                If obj.Name.Contains("SLE") Then
                    Dim obj2 = DirectCast(Activator.CreateInstance(item), Interfaces.IFlashAlgorithm)
                    obj2.Tag = obj2.Name
                    DirectCast(obj2, Auxiliary.FlashAlgorithms.NestedLoopsSLE).SolidSolution = True
                    FlashAlgorithms.Add(obj2.Name, obj2)
                End If
            End If
        Next

    End Sub

End Class