'    Pipe Profile Editor
'    Copyright 2008 Daniel Wagner O. de Medeiros
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

Imports System.Windows.Forms
Imports DWSIM.UnitOperations.UnitOperations.Auxiliary.Pipe
Imports cv = DWSIM.SharedClasses.SystemsOfUnits.Converter
Imports System.Drawing

<System.Serializable()> Public Class PipeHydraulicProfileEditor

    Inherits System.Windows.Forms.UserControl

#Region "    Variable declarations"

    Public Event StatusChanged(ByVal e As EventArgs, ByVal statuscode As PipeEditorStatus)

    Public WithEvents GridMalha As System.Windows.Forms.DataGridView
    Public WithEvents CMenu1 As System.Windows.Forms.ContextMenuStrip
    Private components As System.ComponentModel.IContainer
    Public WithEvents ToolStripMenuItem2 As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents ToolStripMenuItem3 As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents ToolStripMenuItem4 As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents ToolStripMenuItem5 As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents ToolStripMenuItem6 As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents ToolStripMenuItem7 As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents ToolStripMenuItem8 As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents ToolStripMenuItem9 As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents ToolStripMenuItem10 As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents ToolStripMenuItem11 As System.Windows.Forms.ToolStripMenuItem

    Dim l As Integer
    Dim linha_atual As String() = New String() {}

    Public Shared ACD(27, 2) As String
    Dim DNom(218, 6) As String

    Dim ThisExe As Reflection.Assembly = Reflection.Assembly.GetExecutingAssembly
    Dim ThisExeName As String = ThisExe.GetName.Name
    Public WithEvents CBTemplate As New DataGridViewComboBoxCell()
    Public WithEvents CBMat As New DataGridViewComboBoxCell()

    Protected m_profile As PipeProfile

#End Region

    Public PipeOp As UnitOperations.Pipe

    Private Units As SharedClasses.SystemsOfUnits.Units
    Public WithEvents ToolStrip1 As System.Windows.Forms.ToolStrip
    Public WithEvents ToolStripButton1 As System.Windows.Forms.ToolStripButton
    Public WithEvents ToolStripButton2 As System.Windows.Forms.ToolStripButton
    Public WithEvents ToolStripButton3 As System.Windows.Forms.ToolStripButton
    Public WithEvents ToolStripButton4 As System.Windows.Forms.ToolStripButton
    Public WithEvents ToolStripSeparator1 As System.Windows.Forms.ToolStripSeparator
    Public WithEvents ToolStripButton5 As System.Windows.Forms.ToolStripButton
    Public WithEvents ToolStripSeparator2 As System.Windows.Forms.ToolStripSeparator
    Public WithEvents ToolStripLabel1 As System.Windows.Forms.ToolStripLabel
    Public WithEvents ToolStripLabel2 As System.Windows.Forms.ToolStripLabel
    Public WithEvents ColBase As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents TabControl1 As System.Windows.Forms.TabControl
    Public WithEvents TabPage1 As System.Windows.Forms.TabPage
    Public WithEvents TabPage2 As System.Windows.Forms.TabPage
    Public WithEvents KryptonRadioButton2 As System.Windows.Forms.RadioButton
    Public WithEvents KryptonRadioButton1 As System.Windows.Forms.RadioButton
    Public WithEvents GraphControl As ZedGraph.ZedGraphControl
    Private NumberFormat As String = ""

    Dim px, py As New ArrayList
    Friend WithEvents ToolTip1 As ToolTip
    Friend WithEvents tsbImportFromTable As ToolStripButton
    Dim loaded As Boolean = False

    Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container()
        Dim DataGridViewCellStyle2 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle3 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle4 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle1 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(PipeHydraulicProfileEditor))
        Me.GridMalha = New System.Windows.Forms.DataGridView()
        Me.ColBase = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.CMenu1 = New System.Windows.Forms.ContextMenuStrip(Me.components)
        Me.ToolStripMenuItem2 = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItem3 = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItem4 = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItem5 = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItem6 = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItem7 = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItem8 = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItem9 = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItem10 = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItem11 = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStrip1 = New System.Windows.Forms.ToolStrip()
        Me.ToolStripButton1 = New System.Windows.Forms.ToolStripButton()
        Me.ToolStripButton2 = New System.Windows.Forms.ToolStripButton()
        Me.ToolStripButton3 = New System.Windows.Forms.ToolStripButton()
        Me.ToolStripButton4 = New System.Windows.Forms.ToolStripButton()
        Me.ToolStripSeparator1 = New System.Windows.Forms.ToolStripSeparator()
        Me.ToolStripButton5 = New System.Windows.Forms.ToolStripButton()
        Me.ToolStripSeparator2 = New System.Windows.Forms.ToolStripSeparator()
        Me.ToolStripLabel1 = New System.Windows.Forms.ToolStripLabel()
        Me.ToolStripLabel2 = New System.Windows.Forms.ToolStripLabel()
        Me.tsbImportFromTable = New System.Windows.Forms.ToolStripButton()
        Me.TabControl1 = New System.Windows.Forms.TabControl()
        Me.TabPage1 = New System.Windows.Forms.TabPage()
        Me.TabPage2 = New System.Windows.Forms.TabPage()
        Me.KryptonRadioButton2 = New System.Windows.Forms.RadioButton()
        Me.KryptonRadioButton1 = New System.Windows.Forms.RadioButton()
        Me.GraphControl = New ZedGraph.ZedGraphControl()
        Me.ToolTip1 = New System.Windows.Forms.ToolTip(Me.components)
        CType(Me.GridMalha, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.CMenu1.SuspendLayout()
        Me.ToolStrip1.SuspendLayout()
        Me.TabControl1.SuspendLayout()
        Me.TabPage1.SuspendLayout()
        Me.TabPage2.SuspendLayout()
        Me.SuspendLayout()
        '
        'GridMalha
        '
        Me.GridMalha.AllowUserToAddRows = False
        Me.GridMalha.AllowUserToOrderColumns = True
        Me.GridMalha.AllowUserToResizeRows = False
        Me.GridMalha.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.AllCells
        Me.GridMalha.BackgroundColor = System.Drawing.SystemColors.Control
        Me.GridMalha.ColumnHeadersVisible = False
        Me.GridMalha.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.ColBase})
        DataGridViewCellStyle2.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleLeft
        DataGridViewCellStyle2.BackColor = System.Drawing.SystemColors.Window
        DataGridViewCellStyle2.Font = New System.Drawing.Font("Segoe UI", 9.0!)
        DataGridViewCellStyle2.ForeColor = System.Drawing.SystemColors.ControlText
        DataGridViewCellStyle2.FormatProvider = New System.Globalization.CultureInfo("pt-BR")
        DataGridViewCellStyle2.NullValue = "<empty>"
        DataGridViewCellStyle2.SelectionBackColor = System.Drawing.SystemColors.Highlight
        DataGridViewCellStyle2.SelectionForeColor = System.Drawing.SystemColors.HighlightText
        DataGridViewCellStyle2.WrapMode = System.Windows.Forms.DataGridViewTriState.[False]
        Me.GridMalha.DefaultCellStyle = DataGridViewCellStyle2
        resources.ApplyResources(Me.GridMalha, "GridMalha")
        Me.GridMalha.EditMode = System.Windows.Forms.DataGridViewEditMode.EditOnEnter
        Me.GridMalha.GridColor = System.Drawing.SystemColors.Control
        Me.GridMalha.Name = "GridMalha"
        Me.GridMalha.RowHeadersBorderStyle = System.Windows.Forms.DataGridViewHeaderBorderStyle.[Single]
        DataGridViewCellStyle3.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleLeft
        DataGridViewCellStyle3.BackColor = System.Drawing.SystemColors.Control
        DataGridViewCellStyle3.ForeColor = System.Drawing.SystemColors.MenuText
        DataGridViewCellStyle3.FormatProvider = New System.Globalization.CultureInfo("pt-BR")
        DataGridViewCellStyle3.NullValue = "0"
        DataGridViewCellStyle3.SelectionBackColor = System.Drawing.SystemColors.Highlight
        DataGridViewCellStyle3.SelectionForeColor = System.Drawing.SystemColors.HighlightText
        DataGridViewCellStyle3.WrapMode = System.Windows.Forms.DataGridViewTriState.[False]
        Me.GridMalha.RowHeadersDefaultCellStyle = DataGridViewCellStyle3
        DataGridViewCellStyle4.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleLeft
        DataGridViewCellStyle4.FormatProvider = New System.Globalization.CultureInfo("pt-BR")
        Me.GridMalha.RowsDefaultCellStyle = DataGridViewCellStyle4
        Me.GridMalha.RowTemplate.DefaultCellStyle.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleRight
        Me.GridMalha.RowTemplate.DefaultCellStyle.Font = New System.Drawing.Font("Segoe UI", 9.0!)
        Me.GridMalha.RowTemplate.DefaultCellStyle.FormatProvider = New System.Globalization.CultureInfo("pt-BR")
        Me.GridMalha.RowTemplate.Resizable = System.Windows.Forms.DataGridViewTriState.[True]
        '
        'ColBase
        '
        DataGridViewCellStyle1.NullValue = "<empty>"
        Me.ColBase.DefaultCellStyle = DataGridViewCellStyle1
        resources.ApplyResources(Me.ColBase, "ColBase")
        Me.ColBase.Name = "ColBase"
        '
        'CMenu1
        '
        Me.CMenu1.ImageScalingSize = New System.Drawing.Size(32, 32)
        Me.CMenu1.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.ToolStripMenuItem2, Me.ToolStripMenuItem3, Me.ToolStripMenuItem4, Me.ToolStripMenuItem5, Me.ToolStripMenuItem6, Me.ToolStripMenuItem7, Me.ToolStripMenuItem8, Me.ToolStripMenuItem9, Me.ToolStripMenuItem10, Me.ToolStripMenuItem11})
        Me.CMenu1.LayoutStyle = System.Windows.Forms.ToolStripLayoutStyle.HorizontalStackWithOverflow
        Me.CMenu1.Name = "ContextMenuStrip1"
        Me.CMenu1.RenderMode = System.Windows.Forms.ToolStripRenderMode.Professional
        Me.CMenu1.ShowImageMargin = False
        resources.ApplyResources(Me.CMenu1, "CMenu1")
        '
        'ToolStripMenuItem2
        '
        Me.ToolStripMenuItem2.Name = "ToolStripMenuItem2"
        resources.ApplyResources(Me.ToolStripMenuItem2, "ToolStripMenuItem2")
        '
        'ToolStripMenuItem3
        '
        Me.ToolStripMenuItem3.Name = "ToolStripMenuItem3"
        resources.ApplyResources(Me.ToolStripMenuItem3, "ToolStripMenuItem3")
        '
        'ToolStripMenuItem4
        '
        Me.ToolStripMenuItem4.Name = "ToolStripMenuItem4"
        resources.ApplyResources(Me.ToolStripMenuItem4, "ToolStripMenuItem4")
        '
        'ToolStripMenuItem5
        '
        Me.ToolStripMenuItem5.Name = "ToolStripMenuItem5"
        resources.ApplyResources(Me.ToolStripMenuItem5, "ToolStripMenuItem5")
        '
        'ToolStripMenuItem6
        '
        Me.ToolStripMenuItem6.Name = "ToolStripMenuItem6"
        resources.ApplyResources(Me.ToolStripMenuItem6, "ToolStripMenuItem6")
        '
        'ToolStripMenuItem7
        '
        Me.ToolStripMenuItem7.Name = "ToolStripMenuItem7"
        resources.ApplyResources(Me.ToolStripMenuItem7, "ToolStripMenuItem7")
        '
        'ToolStripMenuItem8
        '
        Me.ToolStripMenuItem8.Name = "ToolStripMenuItem8"
        resources.ApplyResources(Me.ToolStripMenuItem8, "ToolStripMenuItem8")
        '
        'ToolStripMenuItem9
        '
        Me.ToolStripMenuItem9.Name = "ToolStripMenuItem9"
        resources.ApplyResources(Me.ToolStripMenuItem9, "ToolStripMenuItem9")
        '
        'ToolStripMenuItem10
        '
        Me.ToolStripMenuItem10.Name = "ToolStripMenuItem10"
        resources.ApplyResources(Me.ToolStripMenuItem10, "ToolStripMenuItem10")
        '
        'ToolStripMenuItem11
        '
        Me.ToolStripMenuItem11.Name = "ToolStripMenuItem11"
        resources.ApplyResources(Me.ToolStripMenuItem11, "ToolStripMenuItem11")
        '
        'ToolStrip1
        '
        resources.ApplyResources(Me.ToolStrip1, "ToolStrip1")
        Me.ToolStrip1.ImageScalingSize = New System.Drawing.Size(32, 32)
        Me.ToolStrip1.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.ToolStripButton1, Me.ToolStripButton2, Me.ToolStripButton3, Me.ToolStripButton4, Me.ToolStripSeparator1, Me.ToolStripButton5, Me.ToolStripSeparator2, Me.ToolStripLabel1, Me.ToolStripLabel2, Me.tsbImportFromTable})
        Me.ToolStrip1.Name = "ToolStrip1"
        '
        'ToolStripButton1
        '
        Me.ToolStripButton1.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.ToolStripButton1.Image = Global.DWSIM.UnitOperations.My.Resources.Resources.add
        resources.ApplyResources(Me.ToolStripButton1, "ToolStripButton1")
        Me.ToolStripButton1.Name = "ToolStripButton1"
        '
        'ToolStripButton2
        '
        Me.ToolStripButton2.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.ToolStripButton2.Image = Global.DWSIM.UnitOperations.My.Resources.Resources.arrow_up
        resources.ApplyResources(Me.ToolStripButton2, "ToolStripButton2")
        Me.ToolStripButton2.Name = "ToolStripButton2"
        '
        'ToolStripButton3
        '
        Me.ToolStripButton3.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.ToolStripButton3.Image = Global.DWSIM.UnitOperations.My.Resources.Resources.delete
        resources.ApplyResources(Me.ToolStripButton3, "ToolStripButton3")
        Me.ToolStripButton3.Name = "ToolStripButton3"
        '
        'ToolStripButton4
        '
        Me.ToolStripButton4.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.ToolStripButton4.Image = Global.DWSIM.UnitOperations.My.Resources.Resources.cross
        resources.ApplyResources(Me.ToolStripButton4, "ToolStripButton4")
        Me.ToolStripButton4.Name = "ToolStripButton4"
        '
        'ToolStripSeparator1
        '
        Me.ToolStripSeparator1.Name = "ToolStripSeparator1"
        resources.ApplyResources(Me.ToolStripSeparator1, "ToolStripSeparator1")
        '
        'ToolStripButton5
        '
        Me.ToolStripButton5.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.ToolStripButton5.Image = Global.DWSIM.UnitOperations.My.Resources.Resources.bullet_tick
        resources.ApplyResources(Me.ToolStripButton5, "ToolStripButton5")
        Me.ToolStripButton5.Name = "ToolStripButton5"
        '
        'ToolStripSeparator2
        '
        Me.ToolStripSeparator2.Name = "ToolStripSeparator2"
        resources.ApplyResources(Me.ToolStripSeparator2, "ToolStripSeparator2")
        '
        'ToolStripLabel1
        '
        Me.ToolStripLabel1.Name = "ToolStripLabel1"
        resources.ApplyResources(Me.ToolStripLabel1, "ToolStripLabel1")
        '
        'ToolStripLabel2
        '
        Me.ToolStripLabel2.Name = "ToolStripLabel2"
        Me.ToolStripLabel2.Overflow = System.Windows.Forms.ToolStripItemOverflow.Never
        resources.ApplyResources(Me.ToolStripLabel2, "ToolStripLabel2")
        '
        'tsbImportFromTable
        '
        Me.tsbImportFromTable.Alignment = System.Windows.Forms.ToolStripItemAlignment.Right
        Me.tsbImportFromTable.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.tsbImportFromTable.Image = Global.DWSIM.UnitOperations.My.Resources.Resources.table_80px
        resources.ApplyResources(Me.tsbImportFromTable, "tsbImportFromTable")
        Me.tsbImportFromTable.Name = "tsbImportFromTable"
        '
        'TabControl1
        '
        Me.TabControl1.Controls.Add(Me.TabPage1)
        Me.TabControl1.Controls.Add(Me.TabPage2)
        resources.ApplyResources(Me.TabControl1, "TabControl1")
        Me.TabControl1.Name = "TabControl1"
        Me.TabControl1.SelectedIndex = 0
        '
        'TabPage1
        '
        Me.TabPage1.Controls.Add(Me.GridMalha)
        Me.TabPage1.Controls.Add(Me.ToolStrip1)
        resources.ApplyResources(Me.TabPage1, "TabPage1")
        Me.TabPage1.Name = "TabPage1"
        Me.TabPage1.UseVisualStyleBackColor = True
        '
        'TabPage2
        '
        Me.TabPage2.Controls.Add(Me.KryptonRadioButton2)
        Me.TabPage2.Controls.Add(Me.KryptonRadioButton1)
        Me.TabPage2.Controls.Add(Me.GraphControl)
        resources.ApplyResources(Me.TabPage2, "TabPage2")
        Me.TabPage2.Name = "TabPage2"
        Me.TabPage2.UseVisualStyleBackColor = True
        '
        'KryptonRadioButton2
        '
        resources.ApplyResources(Me.KryptonRadioButton2, "KryptonRadioButton2")
        Me.KryptonRadioButton2.Checked = True
        Me.KryptonRadioButton2.Name = "KryptonRadioButton2"
        Me.KryptonRadioButton2.TabStop = True
        '
        'KryptonRadioButton1
        '
        resources.ApplyResources(Me.KryptonRadioButton1, "KryptonRadioButton1")
        Me.KryptonRadioButton1.Name = "KryptonRadioButton1"
        '
        'GraphControl
        '
        resources.ApplyResources(Me.GraphControl, "GraphControl")
        Me.GraphControl.BackColor = System.Drawing.Color.WhiteSmoke
        Me.GraphControl.IsAntiAlias = True
        Me.GraphControl.IsAutoScrollRange = True
        Me.GraphControl.IsShowCopyMessage = False
        Me.GraphControl.Name = "GraphControl"
        Me.GraphControl.ScrollGrace = 0R
        Me.GraphControl.ScrollMaxX = 0R
        Me.GraphControl.ScrollMaxY = 0R
        Me.GraphControl.ScrollMaxY2 = 0R
        Me.GraphControl.ScrollMinX = 0R
        Me.GraphControl.ScrollMinY = 0R
        Me.GraphControl.ScrollMinY2 = 0R
        '
        'PipeHydraulicProfileEditor
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi
        Me.Controls.Add(Me.TabControl1)
        Me.Name = "PipeHydraulicProfileEditor"
        CType(Me.GridMalha, System.ComponentModel.ISupportInitialize).EndInit()
        Me.CMenu1.ResumeLayout(False)
        Me.ToolStrip1.ResumeLayout(False)
        Me.ToolStrip1.PerformLayout()
        Me.TabControl1.ResumeLayout(False)
        Me.TabPage1.ResumeLayout(False)
        Me.TabPage1.PerformLayout()
        Me.TabPage2.ResumeLayout(False)
        Me.TabPage2.PerformLayout()
        Me.ResumeLayout(False)

    End Sub

    Public Sub New()

        Me.InitializeComponent()

    End Sub


    Private Sub PipeEditor_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        ExtensionMethods.ChangeDefaultFont(Me)

        Using g1 = Me.CreateGraphics()

            Settings.DpiScale = g1.DpiX / 96.0

            Me.ToolStrip1.AutoSize = False
            Me.ToolStrip1.Size = New Size(ToolStrip1.Width, 28 * Settings.DpiScale)
            Me.ToolStrip1.ImageScalingSize = New Size(20 * Settings.DpiScale, 20 * Settings.DpiScale)
            For Each item In Me.ToolStrip1.Items
                If TryCast(item, ToolStripButton) IsNot Nothing Then
                    DirectCast(item, ToolStripButton).Size = New Size(ToolStrip1.ImageScalingSize.Width, ToolStrip1.ImageScalingSize.Height)
                End If
            Next
            Me.ToolStrip1.Invalidate()

        End Using

        Dim l, j As Integer
        Dim linha_atual As String() = New String() {}

#Region "Pipe diameters"

        Using MyReader2 As New Microsoft.VisualBasic.FileIO.TextFieldParser(ThisExe.GetManifestResourceStream(ThisExeName & "." & "pipes.dat"))
            MyReader2.TextFieldType = FileIO.FieldType.Delimited
            MyReader2.SetDelimiters(";")
            l = 0
            While Not MyReader2.EndOfData
                linha_atual = MyReader2.ReadFields()
                j = 0
                Do
                    DNom(l, j) = linha_atual(j)
                    j = j + 1
                Loop Until j = 7
                l = l + 1
            End While
        End Using

        Dim r, aux, linha_inicial, linha_final As Integer

        linha_inicial = 25
        linha_final = 30
        r = linha_inicial - 4
        With ToolStripMenuItem2.DropDownItems
            aux = .Count
            If aux <> 0 Then .Clear()
            Do
                .Add(DN(r, 2) & " / " & DN(r, 3) _
                    & " / " & DN(r, 4) & " (" & DN(r, 1) & " OD / " & DN(r, 6) & " ID)")
                r = r + 1
            Loop Until r = linha_final - 3
        End With

        linha_inicial = 43
        linha_final = 48
        r = linha_inicial - 4
        With ToolStripMenuItem3.DropDownItems
            aux = .Count
            If aux <> 0 Then .Clear()
            Do
                .Add(DN(r, 2) & " / " & DN(r, 3) _
                 & " / " & DN(r, 4) & " (" & DN(r, 1) & " OD / " & DN(r, 6) & " ID)")
                r = r + 1
            Loop Until r = linha_final - 3
        End With

        linha_inicial = 55
        linha_final = 60
        r = linha_inicial - 4
        With ToolStripMenuItem4.DropDownItems
            aux = .Count
            If aux <> 0 Then .Clear()
            Do
                .Add(DN(r, 2) & " / " & DN(r, 3) _
                 & " / " & DN(r, 4) & " (" & DN(r, 1) & " OD / " & DN(r, 6) & " ID)")
                r = r + 1
            Loop Until r = linha_final - 3
        End With

        linha_inicial = 65
        linha_final = 71
        r = linha_inicial - 4
        With ToolStripMenuItem5.DropDownItems
            aux = .Count
            If aux <> 0 Then .Clear()
            Do
                .Add(DN(r, 2) & " / " & DN(r, 3) _
                 & " / " & DN(r, 4) & " (" & DN(r, 1) & " OD / " & DN(r, 6) & " ID)")
                r = r + 1
            Loop Until r = linha_final - 3
        End With

        linha_inicial = 79
        linha_final = 85
        r = linha_inicial - 4
        With ToolStripMenuItem6.DropDownItems
            aux = .Count
            If aux <> 0 Then .Clear()
            Do
                .Add(DN(r, 2) & " / " & DN(r, 3) _
                 & " / " & DN(r, 4) & " (" & DN(r, 1) & " OD / " & DN(r, 6) & " ID)")
                r = r + 1
            Loop Until r = linha_final - 3
        End With

        linha_inicial = 86
        linha_final = 97
        r = linha_inicial - 4
        With ToolStripMenuItem7.DropDownItems
            aux = .Count
            If aux <> 0 Then .Clear()
            Do
                .Add(DN(r, 2) & " / " & DN(r, 3) _
                 & " / " & DN(r, 4) & " (" & DN(r, 1) & " OD / " & DN(r, 6) & " ID)")
                r = r + 1
            Loop Until r = linha_final - 3
        End With

        linha_inicial = 98
        linha_final = 108
        r = linha_inicial - 4
        With ToolStripMenuItem8.DropDownItems
            aux = .Count
            If aux <> 0 Then .Clear()
            Do
                .Add(DN(r, 2) & " / " & DN(r, 3) _
                 & " / " & DN(r, 4) & " (" & DN(r, 1) & " OD / " & DN(r, 6) & " ID)")
                r = r + 1
            Loop Until r = linha_final - 3
        End With

        linha_inicial = 109
        linha_final = 121
        r = linha_inicial - 4
        With ToolStripMenuItem9.DropDownItems
            aux = .Count
            If aux <> 0 Then .Clear()
            Do
                .Add(DN(r, 2) & " / " & DN(r, 3) _
                 & " / " & DN(r, 4) & " (" & DN(r, 1) & " OD / " & DN(r, 6) & " ID)")
                r = r + 1
            Loop Until r = linha_final - 3
        End With

        linha_inicial = 122
        linha_final = 134
        r = linha_inicial - 4

        With ToolStripMenuItem10.DropDownItems
            aux = .Count
            If aux <> 0 Then .Clear()
            Do
                .Add(DN(r, 2) & " / " & DN(r, 3) _
                 & " / " & DN(r, 4) & " (" & DN(r, 1) & " OD / " & DN(r, 6) & " ID)")
                r = r + 1
            Loop Until r = linha_final - 3
        End With

        linha_inicial = 135
        linha_final = 146
        r = linha_inicial - 4
        With ToolStripMenuItem11.DropDownItems
            aux = .Count
            If aux <> 0 Then .Clear()
            Do
                .Add(DN(r, 2) & " / " & DN(r, 3) _
                 & " / " & DN(r, 4) & " (" & DN(r, 1) & " OD / " & DN(r, 6) & " ID)")
                r = r + 1
            Loop Until r = linha_final - 3
        End With

        '======================================

#End Region

        GridMalha.AllowUserToResizeRows = True
        GridMalha.RowHeadersWidthSizeMode = System.Windows.Forms.DataGridViewRowHeadersWidthSizeMode.AutoSizeToAllHeaders
        GridMalha.Rows.Add()
        GridMalha.Rows.Add()
        GridMalha.Rows.Add()
        GridMalha.Rows.Add()
        GridMalha.Rows.Add()
        GridMalha.Rows.Add()
        GridMalha.Rows.Add()
        GridMalha.Rows.Add()
        GridMalha.Rows.Add()
        GridMalha.Rows.Add()
        GridMalha.Rows.Add()
        GridMalha.Rows(0).HeaderCell.Value = PipeOp.FlowSheet.GetTranslatedString("Segment")
        GridMalha.Rows(1).HeaderCell.Value = PipeOp.FlowSheet.GetTranslatedString("Fitting")
        GridMalha.Rows(2).HeaderCell.Value = PipeOp.FlowSheet.GetTranslatedString("Amount")
        GridMalha.Rows(3).HeaderCell.Value = PipeOp.FlowSheet.GetTranslatedString("Sections")
        GridMalha.Rows(4).HeaderCell.Value = PipeOp.FlowSheet.GetTranslatedString("Material")
        GridMalha.Rows(5).HeaderCell.Value = PipeOp.FlowSheet.GetTranslatedString("Rugosity")
        GridMalha.Rows(6).HeaderCell.Value = PipeOp.FlowSheet.GetTranslatedString("ThermCond")
        GridMalha.Rows(7).HeaderCell.Value = PipeOp.FlowSheet.GetTranslatedString("Length")
        GridMalha.Rows(8).HeaderCell.Value = PipeOp.FlowSheet.GetTranslatedString("Elevation")
        GridMalha.Rows(9).HeaderCell.Value = PipeOp.FlowSheet.GetTranslatedString("External Diameter")
        GridMalha.Rows(10).HeaderCell.Value = PipeOp.FlowSheet.GetTranslatedString("Internal Diameter")

        CBTemplate = New DataGridViewComboBoxCell()
        CBMat = New DataGridViewComboBoxCell()

        linha_atual = New String() {}

        Using MyReader As New Microsoft.VisualBasic.FileIO.TextFieldParser(ThisExe.GetManifestResourceStream(ThisExeName & "." & "fittings.dat"), System.Text.Encoding.Default, True)
            MyReader.TextFieldType = FileIO.FieldType.Delimited
            MyReader.SetDelimiters(";")
            l = 0
            While Not MyReader.EndOfData
                linha_atual = MyReader.ReadFields()
                ACD(l, 0) = linha_atual(0)
                ACD(l, 1) = linha_atual(1)
                ACD(l, 2) = linha_atual(2)
                l = l + 1
            End While
        End Using

        With CBTemplate
            .FlatStyle = FlatStyle.Popup
            .DropDownWidth = 180
            .Items.Add(PipeOp.FlowSheet.GetTranslatedString("Tubulaosimples"))
            .Value = PipeOp.FlowSheet.GetTranslatedString("Tubulaosimples")
            l = 0
            While Not l = ACD.GetUpperBound(0) + 1
                .Items.Add(ACD(l, 0))
                l = l + 1
            End While
            .Style.Alignment = DataGridViewContentAlignment.MiddleLeft
        End With

        With CBMat
            .FlatStyle = FlatStyle.Popup
            .DropDownWidth = 100
            .Value = PipeOp.FlowSheet.GetTranslatedString("AoComum")
            .Items.Add(PipeOp.FlowSheet.GetTranslatedString("AoComum"))
            .Items.Add(PipeOp.FlowSheet.GetTranslatedString("AoCarbono"))
            .Items.Add(PipeOp.FlowSheet.GetTranslatedString("FerroBottomido"))
            .Items.Add(PipeOp.FlowSheet.GetTranslatedString("AoInoxidvel"))
            .Items.Add("PVC")
            .Items.Add("PVC+PFRV")
            .Items.Add(PipeOp.FlowSheet.GetTranslatedString("CommercialCopper"))
            .Items.Add(PipeOp.FlowSheet.GetTranslatedString("UserDefined"))
            .Style.Alignment = DataGridViewContentAlignment.MiddleLeft
        End With

        GridMalha.Rows(1).Cells(0) = CBTemplate
        GridMalha.Rows(4).Cells(0) = CBMat
        GridMalha.Rows(2).Cells(0).Value = "1"
        GridMalha.Rows(3).Cells(0).Value = "5"
        With GridMalha.Rows(0).Cells(0)
            .Value = GridMalha.Columns(0).Index + 1
            .ReadOnly = True
            .Style.BackColor = System.Drawing.Color.LightGray
            .Style.Alignment = DataGridViewContentAlignment.MiddleCenter
        End With

        GridMalha.Rows(4 + 1).Cells(0).ReadOnly = True
        GridMalha.Rows(4 + 2).Cells(0).ReadOnly = True
        GridMalha.Rows(4 + 1).Cells(0).Value = PipeOp.GetRugosity("Steel", Nothing)
        GridMalha.Rows(4 + 2).Cells(0).Value = "T-Dep"
        GridMalha.Rows(4 + 1).Cells(0).Style.BackColor = System.Drawing.Color.LightGray
        GridMalha.Rows(4 + 2).Cells(0).Style.BackColor = System.Drawing.Color.LightGray

        Me.GridMalha.Rows(9).Cells(0).ToolTipText = PipeOp.FlowSheet.GetTranslatedString("StandardPipeSizes")
        Me.GridMalha.Rows(10).Cells(0).ToolTipText = PipeOp.FlowSheet.GetTranslatedString("StandardPipeSizes")

        Units = PipeOp.FlowSheet.FlowsheetOptions.SelectedUnitSystem
        NumberFormat = PipeOp.FlowSheet.FlowsheetOptions.NumberFormat

        If Not PipeOp.Profile Is Nothing Then
            GridMalha.Rows(5).HeaderCell.Value += " (" & Units.distance & ")"
            GridMalha.Rows(6).HeaderCell.Value += " (" & Units.thermalConductivity & ")"
            GridMalha.Rows(7).HeaderCell.Value += " (" & Units.distance & ")"
            GridMalha.Rows(8).HeaderCell.Value += " (" & Units.distance & ")"
            GridMalha.Rows(9).HeaderCell.Value += " (" & Units.diameter & ")"
            GridMalha.Rows(10).HeaderCell.Value += " (" & Units.diameter & ")"
            If PipeOp.Profile.Sections.Count > 0 Then
                Me.ConvertProfileToGrid(PipeOp.Profile)
                Me.GridMalha.Columns.RemoveAt(Me.GridMalha.Columns.Count - 1)
                ToolStripLabel2.Text = "OK"
                ToolStripLabel2.ForeColor = System.Drawing.Color.Green
                PipeOp.Profile.Status = PipeEditorStatus.OK
            End If
            Me.PipeEditor1_StatusChanged(e, PipeEditorStatus.OK)
        End If

        AddHandler GridMalha.EditingControlShowing, AddressOf Me.myDataGridView_EditingControlShowing

        loaded = True

    End Sub

    Public Function DN(ByVal i As Integer, ByVal k As Integer) As Object

        If Double.TryParse(DNom(i, k), System.Globalization.NumberStyles.Float, System.Globalization.CultureInfo.InvariantCulture.NumberFormat, New Double) Then
            Return Double.Parse(DNom(i, k), System.Globalization.CultureInfo.InvariantCulture.NumberFormat)
        Else
            Return DNom(i, k)
        End If

    End Function

    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        If disposing Then

            If (components IsNot Nothing) Then
                components.Dispose()
            End If

        End If
        MyBase.Dispose(disposing)
    End Sub

    Private Sub GridMalha_CurrentCellChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles GridMalha.CurrentCellChanged

        If GridMalha.Created = True And Not GridMalha.CurrentRow Is Nothing Then

            Dim x As Integer
            x = GridMalha.CurrentRow.Index
            If x = 1 Then
                If GridMalha.CurrentCell.Value <> PipeOp.FlowSheet.GetTranslatedString("Tubulaosimples") Then
                    GridMalha.Rows(3).Cells(GridMalha.CurrentCell.ColumnIndex).ReadOnly = True
                    GridMalha.Rows(7).Cells(GridMalha.CurrentCell.ColumnIndex).ReadOnly = True
                    GridMalha.Rows(8).Cells(GridMalha.CurrentCell.ColumnIndex).ReadOnly = True
                    GridMalha.Rows(9).Cells(GridMalha.CurrentCell.ColumnIndex).ReadOnly = True
                    GridMalha.Rows(3).Cells(GridMalha.CurrentCell.ColumnIndex).Style.BackColor = System.Drawing.Color.LightGray
                    GridMalha.Rows(7).Cells(GridMalha.CurrentCell.ColumnIndex).Style.BackColor = System.Drawing.Color.LightGray
                    GridMalha.Rows(8).Cells(GridMalha.CurrentCell.ColumnIndex).Style.BackColor = System.Drawing.Color.LightGray
                    GridMalha.Rows(9).Cells(GridMalha.CurrentCell.ColumnIndex).Style.BackColor = System.Drawing.Color.LightGray
                Else
                    GridMalha.Rows(3).Cells(GridMalha.CurrentCell.ColumnIndex).ReadOnly = False
                    GridMalha.Rows(7).Cells(GridMalha.CurrentCell.ColumnIndex).ReadOnly = False
                    GridMalha.Rows(8).Cells(GridMalha.CurrentCell.ColumnIndex).ReadOnly = False
                    GridMalha.Rows(9).Cells(GridMalha.CurrentCell.ColumnIndex).ReadOnly = False
                    GridMalha.Rows(3).Cells(GridMalha.CurrentCell.ColumnIndex).Style.BackColor = Nothing
                    GridMalha.Rows(7).Cells(GridMalha.CurrentCell.ColumnIndex).Style.BackColor = Nothing
                    GridMalha.Rows(8).Cells(GridMalha.CurrentCell.ColumnIndex).Style.BackColor = Nothing
                    GridMalha.Rows(9).Cells(GridMalha.CurrentCell.ColumnIndex).Style.BackColor = Nothing
                End If
            End If
        End If

    End Sub

    Private Sub GridMalha_DataError(ByVal sender As Object, ByVal e As System.Windows.Forms.DataGridViewDataErrorEventArgs) Handles GridMalha.DataError

    End Sub

    Private Sub GridMalha_MouseDoubleClick(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles GridMalha.MouseDoubleClick

        If GridMalha.CurrentCell.RowIndex >= 9 And GridMalha.CurrentCell.ReadOnly = False Then

            CMenu1.Show(MousePosition.X, MousePosition.Y)
            CMenu1.SetBounds(MousePosition.X, MousePosition.Y, 256, 20)

        End If

    End Sub

    Private Sub ToolStripMenuItem2_DropDownItemClicked(ByVal sender As Object, ByVal e As System.Windows.Forms.ToolStripItemClickedEventArgs) Handles ToolStripMenuItem2.DropDownItemClicked

        Dim idx = ToolStripMenuItem2.DropDownItems.IndexOf(e.ClickedItem)

        Dim r = idx + 25 - 4
        GridMalha.Rows(9).Cells(GridMalha.CurrentCell.ColumnIndex).Value = Format(cv.Convert("in", Units.diameter, DN(r, 1)), NumberFormat)
        GridMalha.Rows(10).Cells(GridMalha.CurrentCell.ColumnIndex).Value = Format(cv.Convert("in", Units.diameter, DN(r, 6)), NumberFormat)
        GridMalha.EndEdit()

    End Sub

    Private Sub ToolStripMenuItem3_DropDownItemClicked(ByVal sender As Object, ByVal e As System.Windows.Forms.ToolStripItemClickedEventArgs) Handles ToolStripMenuItem3.DropDownItemClicked

        Dim idx = ToolStripMenuItem3.DropDownItems.IndexOf(e.ClickedItem)

        Dim r = idx + 43 - 4
        GridMalha.Rows(9).Cells(GridMalha.CurrentCell.ColumnIndex).Value = Format(cv.Convert("in", Units.diameter, DN(r, 1)), NumberFormat)
        GridMalha.Rows(10).Cells(GridMalha.CurrentCell.ColumnIndex).Value = Format(cv.Convert("in", Units.diameter, DN(r, 6)), NumberFormat)
        GridMalha.EndEdit()

    End Sub

    Private Sub ToolStripMenuItem4_DropDownItemClicked(ByVal sender As Object, ByVal e As System.Windows.Forms.ToolStripItemClickedEventArgs) Handles ToolStripMenuItem4.DropDownItemClicked

        Dim idx = ToolStripMenuItem4.DropDownItems.IndexOf(e.ClickedItem)

        Dim r = idx + 55 - 4
        GridMalha.Rows(9).Cells(GridMalha.CurrentCell.ColumnIndex).Value = Format(cv.Convert("in", Units.diameter, DN(r, 1)), NumberFormat)
        GridMalha.Rows(10).Cells(GridMalha.CurrentCell.ColumnIndex).Value = Format(cv.Convert("in", Units.diameter, DN(r, 6)), NumberFormat)
        GridMalha.EndEdit()

    End Sub

    Private Sub ToolStripMenuItem5_DropDownItemClicked(ByVal sender As Object, ByVal e As System.Windows.Forms.ToolStripItemClickedEventArgs) Handles ToolStripMenuItem5.DropDownItemClicked

        Dim idx = ToolStripMenuItem5.DropDownItems.IndexOf(e.ClickedItem)

        Dim r = idx + 65 - 4
        GridMalha.Rows(9).Cells(GridMalha.CurrentCell.ColumnIndex).Value = Format(cv.Convert("in", Units.diameter, DN(r, 1)), NumberFormat)
        GridMalha.Rows(10).Cells(GridMalha.CurrentCell.ColumnIndex).Value = Format(cv.Convert("in", Units.diameter, DN(r, 6)), NumberFormat)
        GridMalha.EndEdit()

    End Sub

    Private Sub ToolStripMenuItem6_DropDownItemClicked(ByVal sender As Object, ByVal e As System.Windows.Forms.ToolStripItemClickedEventArgs) Handles ToolStripMenuItem6.DropDownItemClicked

        Dim idx = ToolStripMenuItem6.DropDownItems.IndexOf(e.ClickedItem)

        Dim r = idx + 79 - 4
        GridMalha.CurrentCell.Value = Format(cv.Convert("in", Units.diameter, DN(r, 1)), NumberFormat)
        GridMalha.Rows(GridMalha.CurrentRow.Index + 1).Cells(GridMalha.CurrentCell.ColumnIndex).Value = Format(cv.Convert("in", Units.diameter, DN(r, 6)), NumberFormat)

    End Sub

    Private Sub ToolStripMenuItem7_DropDownItemClicked(ByVal sender As Object, ByVal e As System.Windows.Forms.ToolStripItemClickedEventArgs) Handles ToolStripMenuItem7.DropDownItemClicked

        Dim idx = ToolStripMenuItem7.DropDownItems.IndexOf(e.ClickedItem)

        Dim r = idx + 86 - 4
        GridMalha.Rows(9).Cells(GridMalha.CurrentCell.ColumnIndex).Value = Format(cv.Convert("in", Units.diameter, DN(r, 1)), NumberFormat)
        GridMalha.Rows(10).Cells(GridMalha.CurrentCell.ColumnIndex).Value = Format(cv.Convert("in", Units.diameter, DN(r, 6)), NumberFormat)
        GridMalha.EndEdit()

    End Sub

    Private Sub ToolStripMenuItem8_DropDownItemClicked(ByVal sender As Object, ByVal e As System.Windows.Forms.ToolStripItemClickedEventArgs) Handles ToolStripMenuItem8.DropDownItemClicked

        Dim idx = ToolStripMenuItem8.DropDownItems.IndexOf(e.ClickedItem)

        Dim r = idx + 98 - 4
        GridMalha.Rows(9).Cells(GridMalha.CurrentCell.ColumnIndex).Value = Format(cv.Convert("in", Units.diameter, DN(r, 1)), NumberFormat)
        GridMalha.Rows(10).Cells(GridMalha.CurrentCell.ColumnIndex).Value = Format(cv.Convert("in", Units.diameter, DN(r, 6)), NumberFormat)
        GridMalha.EndEdit()

    End Sub

    Private Sub ToolStripMenuItem9_DropDownItemClicked(ByVal sender As Object, ByVal e As System.Windows.Forms.ToolStripItemClickedEventArgs) Handles ToolStripMenuItem9.DropDownItemClicked

        Dim idx = ToolStripMenuItem9.DropDownItems.IndexOf(e.ClickedItem)

        Dim r = idx + 109 - 4
        GridMalha.Rows(9).Cells(GridMalha.CurrentCell.ColumnIndex).Value = Format(cv.Convert("in", Units.diameter, DN(r, 1)), NumberFormat)
        GridMalha.Rows(10).Cells(GridMalha.CurrentCell.ColumnIndex).Value = Format(cv.Convert("in", Units.diameter, DN(r, 6)), NumberFormat)
        GridMalha.EndEdit()

    End Sub

    Private Sub ToolStripMenuItem10_DropDownItemClicked(ByVal sender As Object, ByVal e As System.Windows.Forms.ToolStripItemClickedEventArgs) Handles ToolStripMenuItem10.DropDownItemClicked

        Dim idx = ToolStripMenuItem10.DropDownItems.IndexOf(e.ClickedItem)

        Dim r = idx + 122 - 4
        GridMalha.CurrentCell.Value = Format(cv.Convert("in", Units.diameter, DN(r, 1)), NumberFormat)
        GridMalha.Rows(GridMalha.CurrentRow.Index + 1).Cells(GridMalha.CurrentCell.ColumnIndex).Value = Format(cv.Convert("in", Units.diameter, DN(r, 6)), NumberFormat)

    End Sub

    Private Sub ToolStripMenuItem11_DropDownItemClicked(ByVal sender As Object, ByVal e As System.Windows.Forms.ToolStripItemClickedEventArgs) Handles ToolStripMenuItem11.DropDownItemClicked

        Dim idx = ToolStripMenuItem11.DropDownItems.IndexOf(e.ClickedItem)

        Dim r = idx + 135 - 4
        GridMalha.Rows(9).Cells(GridMalha.CurrentCell.ColumnIndex).Value = Format(cv.Convert("in", Units.diameter, DN(r, 1)), NumberFormat)
        GridMalha.Rows(10).Cells(GridMalha.CurrentCell.ColumnIndex).Value = Format(cv.Convert("in", Units.diameter, DN(r, 6)), NumberFormat)
        GridMalha.EndEdit()

    End Sub

    Private Sub myDataGridView_EditingControlShowing(ByVal sender As Object, ByVal e As DataGridViewEditingControlShowingEventArgs)
        If (e.Control.GetType = GetType(DataGridViewComboBoxEditingControl)) Then
            Dim cmb As ComboBox = CType(e.Control, ComboBox)
            RemoveHandler GridMalha.EditingControlShowing, AddressOf Me.cmb_SelectionChangeCommitted
            AddHandler cmb.SelectionChangeCommitted, AddressOf Me.cmb_SelectionChangeCommitted
            SendKeys.Send("{F4}")
        End If
    End Sub

    Private Sub Button8_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripButton1.Click

        Me.GridMalha.Columns.Add("C" & Me.GridMalha.Columns.Count + 1, "Null")

        CBTemplate = New DataGridViewComboBoxCell()
        CBMat = New DataGridViewComboBoxCell()

        With CBTemplate
            .FlatStyle = FlatStyle.Popup
            .DropDownWidth = 180
            .Items.Add(PipeOp.FlowSheet.GetTranslatedString("Tubulaosimples"))
            .Value = PipeOp.FlowSheet.GetTranslatedString("Tubulaosimples")
            l = 0
            While Not l = ACD.GetUpperBound(0) + 1
                .Items.Add(ACD(l, 0))
                l = l + 1
            End While
            .Style.Alignment = DataGridViewContentAlignment.MiddleLeft
        End With

        With CBMat
            .FlatStyle = FlatStyle.Popup
            .DropDownWidth = 100
            .Value = PipeOp.FlowSheet.GetTranslatedString("AoComum")
            .Items.Add(PipeOp.FlowSheet.GetTranslatedString("AoComum"))
            .Items.Add(PipeOp.FlowSheet.GetTranslatedString("AoCarbono"))
            .Items.Add(PipeOp.FlowSheet.GetTranslatedString("FerroBottomido"))
            .Items.Add(PipeOp.FlowSheet.GetTranslatedString("AoInoxidvel"))
            .Items.Add("PVC")
            .Items.Add("PVC+PFRV")
            .Items.Add(PipeOp.FlowSheet.GetTranslatedString("CommercialCopper"))
            .Items.Add(PipeOp.FlowSheet.GetTranslatedString("UserDefined"))
            .Style.Alignment = DataGridViewContentAlignment.MiddleLeft
        End With

        Me.GridMalha.Rows(1).Cells(GridMalha.Columns.Count - 1) = CBTemplate
        Me.GridMalha.Rows(4).Cells(GridMalha.Columns.Count - 1) = CBMat
        Me.GridMalha.Rows(2).Cells(GridMalha.Columns.Count - 1).Value = "1"
        Me.GridMalha.Rows(3).Cells(GridMalha.Columns.Count - 1).Value = "5"
        With Me.GridMalha.Rows(0).Cells(GridMalha.Columns.Count - 1)
            .Value = GridMalha.Columns(GridMalha.ColumnCount - 1).Index + 1
            .ReadOnly = True
            .Style.BackColor = System.Drawing.Color.LightGray
            .Style.Alignment = DataGridViewContentAlignment.MiddleCenter
        End With

        GridMalha.Rows(4 + 1).Cells(GridMalha.Columns.Count - 1).ReadOnly = True
        GridMalha.Rows(4 + 2).Cells(GridMalha.Columns.Count - 1).ReadOnly = True
        GridMalha.Rows(4 + 1).Cells(GridMalha.Columns.Count - 1).Value = PipeOp.GetRugosity("Steel", Nothing)
        GridMalha.Rows(4 + 2).Cells(GridMalha.Columns.Count - 1).Value = "T-Dep"
        GridMalha.Rows(4 + 1).Cells(GridMalha.Columns.Count - 1).Style.BackColor = System.Drawing.Color.LightGray
        GridMalha.Rows(4 + 2).Cells(GridMalha.Columns.Count - 1).Style.BackColor = System.Drawing.Color.LightGray

        Me.GridMalha.Rows(9).Cells(GridMalha.Columns.Count - 1).ToolTipText = PipeOp.FlowSheet.GetTranslatedString("StandardPipeSizes")
        Me.GridMalha.Rows(10).Cells(GridMalha.Columns.Count - 1).ToolTipText = PipeOp.FlowSheet.GetTranslatedString("StandardPipeSizes")

    End Sub

    Private Sub Button7_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripButton2.Click

        Dim col1 As New DataGridViewColumn
        col1.CellTemplate = GridMalha.Columns(0).CellTemplate
        GridMalha.Columns.Insert(GridMalha.CurrentCell.ColumnIndex + 1, col1)
        Dim col2 As New DataGridViewColumn
        For Each col2 In Me.GridMalha.Columns
            Me.GridMalha.Rows(0).Cells(col2.Index).Value = col2.Index + 1
        Next
        col1.Dispose()
        col2.Dispose()

        CBTemplate = New DataGridViewComboBoxCell()
        CBMat = New DataGridViewComboBoxCell()

        With CBTemplate
            .FlatStyle = FlatStyle.Popup
            .DropDownWidth = 180
            .Items.Add(PipeOp.FlowSheet.GetTranslatedString("Tubulaosimples"))
            .Value = PipeOp.FlowSheet.GetTranslatedString("Tubulaosimples")
            l = 0
            While Not l = ACD.GetUpperBound(0) + 1
                .Items.Add(ACD(l, 0))
                l = l + 1
            End While
            .Style.Alignment = DataGridViewContentAlignment.MiddleLeft
        End With

        With CBMat
            .FlatStyle = FlatStyle.Popup
            .DropDownWidth = 100
            .Value = PipeOp.FlowSheet.GetTranslatedString("AoComum")
            .Items.Add(PipeOp.FlowSheet.GetTranslatedString("AoComum"))
            .Items.Add(PipeOp.FlowSheet.GetTranslatedString("AoCarbono"))
            .Items.Add(PipeOp.FlowSheet.GetTranslatedString("FerroBottomido"))
            .Items.Add(PipeOp.FlowSheet.GetTranslatedString("AoInoxidvel"))
            .Items.Add("PVC")
            .Items.Add("PVC+PFRV")
            .Items.Add(PipeOp.FlowSheet.GetTranslatedString("CommercialCopper"))
            .Items.Add(PipeOp.FlowSheet.GetTranslatedString("UserDefined"))
            .Style.Alignment = DataGridViewContentAlignment.MiddleLeft
        End With

        GridMalha.Rows(1).Cells(GridMalha.CurrentCell.ColumnIndex + 1) = CBTemplate
        GridMalha.Rows(4).Cells(GridMalha.CurrentCell.ColumnIndex + 1) = CBMat
        GridMalha.Rows(2).Cells(GridMalha.CurrentCell.ColumnIndex + 1).Value = "1"
        GridMalha.Rows(3).Cells(GridMalha.CurrentCell.ColumnIndex + 1).Value = "5"
        With GridMalha.Rows(0).Cells(GridMalha.CurrentCell.ColumnIndex + 1)
            '.Value = GridMalha.Columns(GridMalha.CurrentCell.ColumnIndex + 1).Index + 1
            .ReadOnly = True
            .Style.BackColor = System.Drawing.Color.LightGray
            .Style.Alignment = DataGridViewContentAlignment.MiddleCenter
        End With
        CBTemplate.Dispose()
        CBMat.Dispose()

    End Sub

    Private Sub Button5_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripButton3.Click

        If GridMalha.Columns.Count <> 1 Then GridMalha.Columns.RemoveAt(GridMalha.CurrentCell.ColumnIndex)
        Dim col2 As New DataGridViewColumn
        For Each col2 In Me.GridMalha.Columns
            Me.GridMalha.Rows(0).Cells(col2.Index).Value = col2.Index + 1
        Next
        col2.Dispose()

    End Sub

    Private Sub Button6_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripButton4.Click

        Dim inf As DialogResult

        inf = MessageBox.Show(PipeOp.FlowSheet.GetTranslatedString("CliqueemOKparalimparamalha"), PipeOp.FlowSheet.GetTranslatedString("Limparmalha"), MessageBoxButtons.OKCancel, MessageBoxIcon.Warning, MessageBoxDefaultButton.Button1, MessageBoxOptions.DefaultDesktopOnly, False)

        If inf = Windows.Forms.DialogResult.OK Then

            GridMalha.Columns.Clear()
            GridMalha.Columns.Add("CCW", "1")
            GridMalha.AllowUserToResizeRows = True
            GridMalha.RowHeadersWidthSizeMode = DataGridViewRowHeadersWidthSizeMode.AutoSizeToAllHeaders
            GridMalha.Rows.Add()
            GridMalha.Rows.Add()
            GridMalha.Rows.Add()
            GridMalha.Rows.Add()
            GridMalha.Rows.Add()
            GridMalha.Rows.Add()
            GridMalha.Rows.Add()
            GridMalha.Rows.Add()
            GridMalha.Rows.Add()
            GridMalha.Rows.Add()
            GridMalha.Rows.Add()
            GridMalha.Rows(0).HeaderCell.Value = PipeOp.FlowSheet.GetTranslatedString("Segmento")
            GridMalha.Rows(1).HeaderCell.Value = PipeOp.FlowSheet.GetTranslatedString("Tipo")
            GridMalha.Rows(2).HeaderCell.Value = PipeOp.FlowSheet.GetTranslatedString("Quantidade")
            GridMalha.Rows(3).HeaderCell.Value = PipeOp.FlowSheet.GetTranslatedString("Incrementos")
            GridMalha.Rows(4).HeaderCell.Value = PipeOp.FlowSheet.GetTranslatedString("Material")
            GridMalha.Rows(5).HeaderCell.Value = PipeOp.FlowSheet.GetTranslatedString("Rugosity").Replace("(m)", "(" & Units.distance & ")")
            GridMalha.Rows(6).HeaderCell.Value = PipeOp.FlowSheet.GetTranslatedString("ThermCond").Replace("(W.[m.K])", "(" & Units.thermalConductivity & ")")
            GridMalha.Rows(7).HeaderCell.Value = PipeOp.FlowSheet.GetTranslatedString("Comprimentom").Replace("(m)", "(" & Units.distance & ")")
            GridMalha.Rows(8).HeaderCell.Value = PipeOp.FlowSheet.GetTranslatedString("Elevaom").Replace("(m)", "(" & Units.distance & ")")
            GridMalha.Rows(9).HeaderCell.Value = PipeOp.FlowSheet.GetTranslatedString("Dexternoin").Replace("(in.)", "(" & Units.diameter & ")")
            GridMalha.Rows(10).HeaderCell.Value = PipeOp.FlowSheet.GetTranslatedString("Dinternoin").Replace("(in.)", "(" & Units.diameter & ")")

            Dim CBTemplate As New DataGridViewComboBoxCell()
            Dim CBMat As New DataGridViewComboBoxCell()

            Dim l As Integer
            Dim linha_atual As String() = New String() {}

            Using MyReader As New Microsoft.VisualBasic.FileIO.TextFieldParser(ThisExe.GetManifestResourceStream(ThisExeName & "." & "fittings.dat"), System.Text.Encoding.UTF8, True)
                MyReader.TextFieldType = FileIO.FieldType.Delimited
                MyReader.SetDelimiters(";")
                l = 0
                While Not MyReader.EndOfData
                    linha_atual = MyReader.ReadFields()
                    ACD(l, 0) = linha_atual(0)
                    ACD(l, 1) = linha_atual(1)
                    ACD(l, 2) = linha_atual(2)
                    l = l + 1
                End While
            End Using

            With CBTemplate
                .FlatStyle = FlatStyle.Popup
                .DropDownWidth = 180
                .Items.Add(PipeOp.FlowSheet.GetTranslatedString("Tubulaosimples"))
                .Value = PipeOp.FlowSheet.GetTranslatedString("Tubulaosimples")
                l = 0
                While Not l = ACD.GetUpperBound(0) + 1
                    .Items.Add(ACD(l, 0))
                    l = l + 1
                End While
                .Style.Alignment = DataGridViewContentAlignment.MiddleLeft
            End With

            With CBMat
                .FlatStyle = FlatStyle.Popup
                .DropDownWidth = 100
                .Value = PipeOp.FlowSheet.GetTranslatedString("AoComum")
                .Items.Add(PipeOp.FlowSheet.GetTranslatedString("AoComum"))
                .Items.Add(PipeOp.FlowSheet.GetTranslatedString("AoCarbono"))
                .Items.Add(PipeOp.FlowSheet.GetTranslatedString("FerroBottomido"))
                .Items.Add(PipeOp.FlowSheet.GetTranslatedString("AoInoxidvel"))
                .Items.Add("PVC")
                .Items.Add("PVC+PFRV")
                .Items.Add(PipeOp.FlowSheet.GetTranslatedString("CommercialCopper"))
                .Items.Add(PipeOp.FlowSheet.GetTranslatedString("UserDefined"))
                .Style.Alignment = DataGridViewContentAlignment.MiddleLeft
            End With

            GridMalha.Rows(1).Cells(0) = CBTemplate
            GridMalha.Rows(4).Cells(0) = CBMat
            GridMalha.Rows(2).Cells(0).Value = "1"
            GridMalha.Rows(3).Cells(0).Value = "5"
            With GridMalha.Rows(0).Cells(0)
                .Value = GridMalha.Columns(0).Index + 1
                .ReadOnly = True
                .Style.BackColor = System.Drawing.Color.LightGray
                .Style.Alignment = DataGridViewContentAlignment.MiddleCenter
            End With


        End If

    End Sub

    Private Sub Button4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripButton5.Click

        PipeOp.FlowSheet.RegisterSnapshot(Enums.SnapshotType.ObjectData, PipeOp)

        Dim column As New DataGridViewColumn
        Dim parsingresult As String
        Dim v1, v2, v3, v4, v5, v6, v7, v8, v9 As Object

        If Not PipeOp.Profile Is Nothing Then PipeOp.Profile.Sections.Clear()
        For Each column In Me.GridMalha.Columns
            parsingresult = ParseColumn(column)
            If parsingresult = "OK" Then
                v1 = column.Index + 1
                v2 = Me.GridMalha.Rows(1).Cells(column.Name).Value
                v3 = Me.GridMalha.Rows(2).Cells(column.Name).Value
                v4 = Me.GridMalha.Rows(3).Cells(column.Name).Value
                v5 = Me.GridMalha.Rows(4).Cells(column.Name).Value
                If v5 = PipeOp.FlowSheet.GetTranslatedString("UserDefined") Then v5 = "UserDefined"
                If v2 = PipeOp.FlowSheet.GetTranslatedString("Tubulaosimples") Then
                    v2 = "Tubulaosimples"
                End If
                If v2.ToString.ToLower = "tubulaosimples" Or v2.ToString.ToLower = "straight tube" Then
                    v6 = Me.GridMalha.Rows(7).Cells(column.Name).Value.ToString.ParseExpressionToDouble
                    v7 = Me.GridMalha.Rows(8).Cells(column.Name).Value.ToString.ParseExpressionToDouble
                    v8 = Me.GridMalha.Rows(9).Cells(column.Name).Value.ToString.ParseExpressionToDouble
                Else
                    v6 = 0.0
                    v7 = 0.0
                    v8 = 0.0
                End If
                v9 = Me.GridMalha.Rows(10).Cells(column.Name).Value.ToString.ParseExpressionToDouble
                Dim ps As New PipeSection(v1, v2, v3, v4, v5, cv.Convert(Me.Units.distance, "m", v6), cv.Convert(Me.Units.distance, "m", v7), cv.Convert(Me.Units.diameter, "in", v8), cv.Convert(Me.Units.diameter, "in", v9))
                If ps.Material = "UserDefined" Then
                    ps.PipeWallRugosity = cv.ConvertToSI(Units.distance, Me.GridMalha.Rows(5).Cells(column.Name).Value.ToString.ParseExpressionToDouble())
                    ps.PipeWallThermalConductivityExpression = Me.GridMalha.Rows(6).Cells(column.Name).Value
                End If
                PipeOp.Profile.Sections.Add(column.Index + 1, ps)
            Else
                ToolStripLabel2.Text = PipeOp.FlowSheet.GetTranslatedString("Erronasecao") & " " & column.Index + 1
                RaiseEvent StatusChanged(e, PipeEditorStatus.Erro)
                MessageBox.Show(parsingresult, PipeOp.FlowSheet.GetTranslatedString("Erronasecao") & " " & column.Index + 1, MessageBoxButtons.OK, MessageBoxIcon.Error)
                Exit Sub
            End If
        Next
        RaiseEvent StatusChanged(e, PipeEditorStatus.OK)

        column.Dispose()

    End Sub

    Public Sub handleStatus(ByVal e As EventArgs, ByVal statuscode As PipeEditorStatus) Handles Me.StatusChanged

        If statuscode = PipeEditorStatus.Erro Then
            'Label1.Text = "Erro"
            ToolStripLabel2.ForeColor = System.Drawing.Color.Red
            PipeOp.Profile.Status = PipeEditorStatus.Definir
        ElseIf statuscode = PipeEditorStatus.Definir Then
            ToolStripLabel2.Text = PipeOp.FlowSheet.GetTranslatedString("Indefinido")
            ToolStripLabel2.ForeColor = System.Drawing.Color.Red
            PipeOp.Profile.Status = PipeEditorStatus.Definir
        ElseIf statuscode = PipeEditorStatus.OK Then
            ToolStripLabel2.Text = "OK"
            ToolStripLabel2.ForeColor = System.Drawing.Color.Green
            PipeOp.Profile.Status = PipeEditorStatus.OK
        End If

    End Sub

    Private Function ParseColumn(ByVal column As DataGridViewColumn)
        Try
            With (Me.GridMalha)
                If Not Convert.ToDouble(.Rows(2).Cells(column.Name).Value) > 0.0# Then
                    Return PipeOp.FlowSheet.GetTranslatedString("Erro")
                    Exit Function
                End If
                If .Rows(1).Cells(column.Name).Value = PipeOp.FlowSheet.GetTranslatedString("Tubulaosimples") Then
                    If .Rows(3).Cells(column.Name).Value Is Nothing OrElse (Not .Rows(3).Cells(column.Name).Value.ToString.ParseExpressionToDouble > 0.0#) Then
                        Return "Invalid number of sections"
                    End If
                    If .Rows(7).Cells(column.Name).Value Is Nothing OrElse (Not .Rows(7).Cells(column.Name).Value.ToString.ParseExpressionToDouble > 0.0# Or Not .Rows(7).Cells(column.Name).Value.ToString.IsValidDoubleExpression) Then
                        Return "Invalid length"
                    End If
                    If .Rows(8).Cells(column.Name).Value Is Nothing OrElse (Not .Rows(8).Cells(column.Name).Value.ToString.IsValidDoubleExpression) Then
                        Return "Invalid elevation"
                        Exit Function
                    End If
                    If Math.Abs(.Rows(8).Cells(column.Name).Value.ToString.ParseExpressionToDouble) > Math.Abs(.Rows(7).Cells(column.Name).Value.ToString.ParseExpressionToDouble) Then
                        Return "Invalid elevation (H > L!)"
                    End If
                    If .Rows(9).Cells(column.Name).Value Is Nothing OrElse (Not .Rows(9).Cells(column.Name).Value.ToString.ParseExpressionToDouble > 0.0#) Then
                        Return "Invalid external diameter"
                        Exit Function
                    End If
                    If .Rows(10).Cells(column.Name).Value Is Nothing OrElse (Not .Rows(10).Cells(column.Name).Value.ToString.ParseExpressionToDouble > 0.0# Or .Rows(10).Cells(column.Name).Value.ToString.ParseExpressionToDouble > .Rows(9).Cells(column.Name).Value.ToString.ParseExpressionToDouble) Then
                        Return "Invalid internal diameter"
                        Exit Function
                    End If
                Else
                    If .Rows(10).Cells(column.Name).Value Is Nothing OrElse Not .Rows(10).Cells(column.Name).Value.ToString.ParseExpressionToDouble > 0.0# Then
                        Return "Invalid internal diameter"
                        Exit Function
                    End If
                End If
            End With
            Return "OK"
        Catch ex As Exception
            Return ex.Message.ToString
            Exit Function
        End Try

    End Function

    Private Sub ConvertProfileToGrid(ByVal Profile As PipeProfile)

        Dim psec As New PipeSection

        'If Not Me.GridMalha.Columns Is Nothing Then Me.GridMalha.Columns.Clear()
        For Each psec In Profile.Sections.Values
            Me.Button8_Click(Nothing, Nothing)
            Me.GridMalha.Rows(0).Cells(psec.Indice - 1).Value = psec.Indice
            If Not CBTemplate.Items.Contains(psec.TipoSegmento) Then
                Me.GridMalha.Rows(1).Cells(psec.Indice - 1).Value = CBTemplate.Items(0)
            Else
                Me.GridMalha.Rows(1).Cells(psec.Indice - 1).Value = psec.TipoSegmento
            End If
            Me.GridMalha.Rows(2).Cells(psec.Indice - 1).Value = psec.Quantidade
            Me.GridMalha.Rows(3).Cells(psec.Indice - 1).Value = psec.Incrementos
            If Not CBMat.Items.Contains(PipeOp.FlowSheet.GetTranslatedString(psec.Material)) Then
                Me.GridMalha.Rows(4).Cells(psec.Indice - 1).Value = CBMat.Items(0)
            Else
                Me.GridMalha.Rows(4).Cells(psec.Indice - 1).Value = PipeOp.FlowSheet.GetTranslatedString(psec.Material)
            End If
            If psec.Material = "UserDefined" Then
                Me.GridMalha.Rows(5).Cells(psec.Indice - 1).Value = cv.Convert("m", Me.Units.distance, psec.PipeWallRugosity)
                Me.GridMalha.Rows(6).Cells(psec.Indice - 1).Value = psec.PipeWallThermalConductivityExpression
                Me.GridMalha.Rows(5).Cells(psec.Indice - 1).Style.BackColor = Nothing
                Me.GridMalha.Rows(6).Cells(psec.Indice - 1).Style.BackColor = Nothing
                Me.GridMalha.Rows(5).Cells(psec.Indice - 1).ReadOnly = False
                Me.GridMalha.Rows(6).Cells(psec.Indice - 1).ReadOnly = False
            Else
                Me.GridMalha.Rows(5).Cells(psec.Indice - 1).Value = cv.ConvertFromSI(Me.Units.distance, PipeOp.GetRugosity(psec.Material, psec))
                Me.GridMalha.Rows(6).Cells(psec.Indice - 1).Value = "T-Dep"
                Me.GridMalha.Rows(5).Cells(psec.Indice - 1).Style.BackColor = Color.LightGray
                Me.GridMalha.Rows(6).Cells(psec.Indice - 1).Style.BackColor = Color.LightGray
                Me.GridMalha.Rows(5).Cells(psec.Indice - 1).ReadOnly = True
                Me.GridMalha.Rows(6).Cells(psec.Indice - 1).ReadOnly = True
            End If
            Me.GridMalha.Rows(7).Cells(psec.Indice - 1).Value = Format(cv.Convert("m", Me.Units.distance, psec.Comprimento), NumberFormat)
            Me.GridMalha.Rows(8).Cells(psec.Indice - 1).Value = Format(cv.Convert("m", Me.Units.distance, psec.Elevacao), NumberFormat)
            Me.GridMalha.Rows(9).Cells(psec.Indice - 1).Value = Format(cv.Convert("in", Me.Units.diameter, psec.DE), NumberFormat)
            Me.GridMalha.Rows(10).Cells(psec.Indice - 1).Value = Format(cv.Convert("in", Me.Units.diameter, psec.DI), NumberFormat)
            Me.GridMalha.Rows(9).Cells(psec.Indice - 1).ToolTipText = PipeOp.FlowSheet.GetTranslatedString("StandardPipeSizes")
            Me.GridMalha.Rows(10).Cells(psec.Indice - 1).ToolTipText = PipeOp.FlowSheet.GetTranslatedString("StandardPipeSizes")
            If psec.TipoSegmento <> "Tubulaosimples" Then
                GridMalha.Rows(3).Cells(psec.Indice - 1).ReadOnly = True
                GridMalha.Rows(7).Cells(psec.Indice - 1).ReadOnly = True
                GridMalha.Rows(8).Cells(psec.Indice - 1).ReadOnly = True
                GridMalha.Rows(9).Cells(psec.Indice - 1).ReadOnly = True
                GridMalha.Rows(3).Cells(psec.Indice - 1).Style.BackColor = System.Drawing.Color.LightGray
                GridMalha.Rows(7).Cells(psec.Indice - 1).Style.BackColor = System.Drawing.Color.LightGray
                GridMalha.Rows(8).Cells(psec.Indice - 1).Style.BackColor = System.Drawing.Color.LightGray
                GridMalha.Rows(9).Cells(psec.Indice - 1).Style.BackColor = System.Drawing.Color.LightGray
            Else
                GridMalha.Rows(3).Cells(psec.Indice - 1).ReadOnly = False
                GridMalha.Rows(7).Cells(psec.Indice - 1).ReadOnly = False
                GridMalha.Rows(8).Cells(psec.Indice - 1).ReadOnly = False
                GridMalha.Rows(9).Cells(psec.Indice - 1).ReadOnly = False
                GridMalha.Rows(3).Cells(psec.Indice - 1).Style.BackColor = Nothing
                GridMalha.Rows(7).Cells(psec.Indice - 1).Style.BackColor = Nothing
                GridMalha.Rows(8).Cells(psec.Indice - 1).Style.BackColor = Nothing
                GridMalha.Rows(9).Cells(psec.Indice - 1).Style.BackColor = Nothing
            End If
            Dim material = GridMalha.Rows(4).Cells(psec.Indice - 1).Value.ToString()
            If material IsNot Nothing Then
                If material.Contains("User") Or material.Contains("Usu") Then
                    material = "UserDefined"
                End If
                If material.ToString <> "UserDefined" Then
                    GridMalha.Rows(4 + 1).Cells(psec.Indice - 1).ReadOnly = True
                    GridMalha.Rows(4 + 2).Cells(psec.Indice - 1).ReadOnly = True
                    Try
                        GridMalha.Rows(4 + 1).Cells(psec.Indice - 1).Value = PipeOp.GetRugosity(material, PipeOp.Profile.Sections(psec.Indice - 1 + 1))
                    Catch ex As Exception
                        GridMalha.Rows(4 + 1).Cells(psec.Indice - 1).Value = PipeOp.GetRugosity(material, Nothing)
                    End Try
                    GridMalha.Rows(4 + 2).Cells(psec.Indice - 1).Value = "T-Dep"
                    GridMalha.Rows(4 + 1).Cells(psec.Indice - 1).Style.BackColor = System.Drawing.Color.LightGray
                    GridMalha.Rows(4 + 2).Cells(psec.Indice - 1).Style.BackColor = System.Drawing.Color.LightGray
                Else
                    GridMalha.Rows(4 + 1).Cells(psec.Indice - 1).ReadOnly = False
                    GridMalha.Rows(4 + 2).Cells(psec.Indice - 1).ReadOnly = False
                    If PipeOp.Profile.Sections.ContainsKey(psec.Indice - 1 + 1) Then
                        GridMalha.Rows(4 + 1).Cells(psec.Indice - 1).Value = PipeOp.Profile.Sections(psec.Indice - 1 + 1).PipeWallRugosity
                        GridMalha.Rows(4 + 2).Cells(psec.Indice - 1).Value = PipeOp.Profile.Sections(psec.Indice - 1 + 1).PipeWallThermalConductivityExpression
                    Else
                        GridMalha.Rows(4 + 1).Cells(psec.Indice - 1).Value = 0.00001
                        GridMalha.Rows(4 + 2).Cells(psec.Indice - 1).Value = ""
                    End If
                    GridMalha.Rows(4 + 1).Cells(psec.Indice - 1).Style.BackColor = Nothing
                    GridMalha.Rows(4 + 2).Cells(psec.Indice - 1).Style.BackColor = Nothing
                End If
            End If
        Next
        psec = Nothing

        RaiseEvent StatusChanged(New EventArgs, PipeEditorStatus.OK)

    End Sub

    Private Sub cmb_SelectionChangeCommitted(ByVal sender As Object, ByVal e As EventArgs)
        GridMalha.CurrentCell.Value = CType(sender, DataGridViewComboBoxEditingControl).EditingControlFormattedValue
    End Sub

    Private Sub KryptonRadioButton1_CheckedChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles KryptonRadioButton1.CheckedChanged, KryptonRadioButton2.CheckedChanged

        If loaded Then Call Me.PipeEditor1_StatusChanged(e, PipeEditorStatus.OK)

    End Sub

    Private Sub PipeEditor1_StatusChanged(ByVal e As System.EventArgs, ByVal statuscode As PipeEditorStatus) Handles Me.StatusChanged

        If PipeOp.Profile.Sections.Count = 0 Then Exit Sub

        If statuscode = PipeEditorStatus.OK Then
            px.Clear()
            py.Clear()
            px.Add(0.0#)
            py.Add(0.0#)
            If Me.KryptonRadioButton1.Checked Then
                With PipeOp.Profile
                    Dim i As Integer = 1
                    Do
                        If .Sections(i).TipoSegmento = "Tubulaosimples" Then
                            If i >= 2 Then
                                px.Add(px(px.Count - 1) + CDbl(.Sections(i).Comprimento))
                                py.Add(py(py.Count - 1) + CDbl(.Sections(i).Elevacao))
                            Else
                                px.Add(CDbl(.Sections(i).Comprimento))
                                py.Add(CDbl(.Sections(i).Elevacao))
                            End If
                        Else
                            If i >= 2 Then
                                px.Add(px(px.Count - 1))
                                py.Add(py(py.Count - 1))
                            Else
                                px.Add(0.0#)
                                py.Add(0.0#)
                            End If
                        End If
                        i = i + 1
                    Loop Until .Sections.ContainsKey(i) = False
                End With

                With Me.GraphControl.GraphPane
                    .XAxis.Title.Text = PipeOp.FlowSheet.GetTranslatedString("Comprimentom")
                    .YAxis.Title.Text = PipeOp.FlowSheet.GetTranslatedString("Elevaom")
                End With

            Else
                With PipeOp.Profile
                    Dim i As Integer = 1
                    Do
                        If .Sections(i).TipoSegmento = "Tubulaosimples" Then
                            If i >= 2 Then
                                px.Add(px(px.Count - 1) + (CDbl(.Sections(i).Comprimento ^ 2 - .Sections(i).Elevacao ^ 2) ^ 0.5))
                                py.Add(py(py.Count - 1) + CDbl(.Sections(i).Elevacao))
                            Else
                                px.Add((CDbl(.Sections(i).Comprimento ^ 2 - .Sections(i).Elevacao ^ 2) ^ 0.5))
                                py.Add(CDbl(.Sections(i).Elevacao))
                            End If
                        Else
                            If i >= 2 Then
                                px.Add(px(px.Count - 1))
                                py.Add(py(py.Count - 1))
                            Else
                                px.Add(0.0#)
                                py.Add(0.0#)
                            End If
                        End If
                        i = i + 1
                    Loop Until .Sections.ContainsKey(i) = False
                End With

                With Me.GraphControl.GraphPane
                    .XAxis.Title.Text = PipeOp.FlowSheet.GetTranslatedString("DistnciaHorizontalm")
                    .YAxis.Title.Text = PipeOp.FlowSheet.GetTranslatedString("Elevaom")
                End With

            End If

        End If

        With Me.GraphControl.GraphPane
            .Title.IsVisible = False
            .CurveList.Clear()
            With .AddCurve(PipeOp.FlowSheet.GetTranslatedString("Perfil"), px.ToArray(GetType(Double)), py.ToArray(GetType(Double)), Color.DeepSkyBlue, ZedGraph.SymbolType.Circle)
                .Color = Color.SteelBlue
                .Line.IsSmooth = False
                .Symbol.Fill.Type = ZedGraph.FillType.Solid
            End With
            Me.GraphControl.IsAutoScrollRange = True
            .XAxis.Title.FontSpec.Size = 22
            .YAxis.Title.FontSpec.Size = 22
            .XAxis.Scale.FontSpec.Size = 18
            .YAxis.Scale.FontSpec.Size = 18
            .AxisChange(Me.CreateGraphics)
            Me.GraphControl.Invalidate()
        End With

        Me.GraphControl.GraphPane.Legend.IsVisible = False

    End Sub

    Private Sub GridMalha_KeyDown(sender As Object, e As KeyEventArgs) Handles GridMalha.KeyDown
        If e.KeyCode = Keys.V And e.Modifiers = Keys.Control Then
            PasteData(sender)
        ElseIf e.KeyCode = Keys.Delete Then
            For Each c As DataGridViewCell In CType(sender, DataGridView).SelectedCells
                c.Value = Nothing
            Next
        End If
    End Sub

    Private Sub tsbImportFromTable_Click(sender As Object, e As EventArgs) Handles tsbImportFromTable.Click

        Dim ft As New EditingForm_Pipe_HydraulicProfileImportFromTabularData With {.PipeObject = PipeOp}
        ft.ShowDialog()

    End Sub

    Private Sub GridMalha_CellValueChanged(sender As Object, e As DataGridViewCellEventArgs) Handles GridMalha.CellValueChanged

        If loaded Then
            Try
                ToolStripLabel2.Text = PipeOp.FlowSheet.GetTranslatedString("Modified")
                ToolStripLabel2.ForeColor = Color.DarkOrange

                If e.RowIndex = 4 Or e.RowIndex = 1 Then
                    If e.RowIndex = 1 Then GridMalha_CurrentCellChanged(sender, e)
                    Dim material = GridMalha.Rows(4).Cells(e.ColumnIndex).Value.ToString()
                    If material IsNot Nothing Then
                        If material.Contains("User") Or material.Contains("Usu") Then
                            material = "UserDefined"
                        End If
                        If material.ToString <> "UserDefined" Then
                            GridMalha.Rows(4 + 1).Cells(e.ColumnIndex).ReadOnly = True
                            GridMalha.Rows(4 + 2).Cells(e.ColumnIndex).ReadOnly = True
                            Try
                                GridMalha.Rows(4 + 1).Cells(e.ColumnIndex).Value = PipeOp.GetRugosity(material, PipeOp.Profile.Sections(e.ColumnIndex + 1))
                            Catch ex As Exception
                                GridMalha.Rows(4 + 1).Cells(e.ColumnIndex).Value = PipeOp.GetRugosity(material, Nothing)
                            End Try
                            GridMalha.Rows(4 + 2).Cells(e.ColumnIndex).Value = "T-Dep"
                            GridMalha.Rows(4 + 1).Cells(e.ColumnIndex).Style.BackColor = System.Drawing.Color.LightGray
                            GridMalha.Rows(4 + 2).Cells(e.ColumnIndex).Style.BackColor = System.Drawing.Color.LightGray
                        Else
                            GridMalha.Rows(4 + 1).Cells(e.ColumnIndex).ReadOnly = False
                            GridMalha.Rows(4 + 2).Cells(e.ColumnIndex).ReadOnly = False
                            If PipeOp.Profile.Sections.ContainsKey(e.ColumnIndex + 1) Then
                                GridMalha.Rows(4 + 1).Cells(e.ColumnIndex).Value = PipeOp.Profile.Sections(e.ColumnIndex + 1).PipeWallRugosity
                                GridMalha.Rows(4 + 2).Cells(e.ColumnIndex).Value = PipeOp.Profile.Sections(e.ColumnIndex + 1).PipeWallThermalConductivityExpression
                            Else
                                GridMalha.Rows(4 + 1).Cells(e.ColumnIndex).Value = 0.00001
                                GridMalha.Rows(4 + 2).Cells(e.ColumnIndex).Value = ""
                            End If
                            GridMalha.Rows(4 + 1).Cells(e.ColumnIndex).Style.BackColor = Nothing
                            GridMalha.Rows(4 + 2).Cells(e.ColumnIndex).Style.BackColor = Nothing
                        End If
                    End If
                End If
            Catch ex As Exception

            End Try

        End If

    End Sub

End Class
