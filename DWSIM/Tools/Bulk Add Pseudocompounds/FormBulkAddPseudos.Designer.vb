<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()>
Partial Class FormBulkAddPseudos
    Inherits System.Windows.Forms.Form

    'Form overrides dispose to clean up the component list.
    <System.Diagnostics.DebuggerNonUserCode()>
    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        Try
            If disposing AndAlso components IsNot Nothing Then
                components.Dispose()
            End If
        Finally
            MyBase.Dispose(disposing)
        End Try
    End Sub

    'Required by the Windows Form Designer
    Private components As System.ComponentModel.IContainer

    'NOTE: The following procedure is required by the Windows Form Designer
    'It can be modified using the Windows Form Designer.  
    'Do not modify it using the code editor.
    <System.Diagnostics.DebuggerStepThrough()>
    Private Sub InitializeComponent()
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(FormBulkAddPseudos))
        Me.GroupBox2 = New System.Windows.Forms.GroupBox()
        Me.grid1 = New unvell.ReoGrid.ReoGridControl()
        Me.GroupBox3 = New System.Windows.Forms.GroupBox()
        Me.TabControl1 = New System.Windows.Forms.TabControl()
        Me.TabPage1 = New System.Windows.Forms.TabPage()
        Me.cbNBPUnits = New System.Windows.Forms.ComboBox()
        Me.Label4 = New System.Windows.Forms.Label()
        Me.cbPCUnits = New System.Windows.Forms.ComboBox()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.cbTCUnits = New System.Windows.Forms.ComboBox()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.TabPage2 = New System.Windows.Forms.TabPage()
        Me.ComboBoxMW = New System.Windows.Forms.ComboBox()
        Me.ComboBoxSG = New System.Windows.Forms.ComboBox()
        Me.ComboBoxAF = New System.Windows.Forms.ComboBox()
        Me.ComboBoxPC = New System.Windows.Forms.ComboBox()
        Me.Label33 = New System.Windows.Forms.Label()
        Me.Label31 = New System.Windows.Forms.Label()
        Me.Label30 = New System.Windows.Forms.Label()
        Me.Label29 = New System.Windows.Forms.Label()
        Me.ComboBoxTC = New System.Windows.Forms.ComboBox()
        Me.Label28 = New System.Windows.Forms.Label()
        Me.btnEstimate = New System.Windows.Forms.Button()
        Me.btnExport = New System.Windows.Forms.Button()
        Me.btnClose = New System.Windows.Forms.Button()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.btnAdd = New System.Windows.Forms.Button()
        Me.btnExporttoXML = New System.Windows.Forms.Button()
        Me.GroupBox1 = New System.Windows.Forms.GroupBox()
        Me.btnViewComp = New System.Windows.Forms.Button()
        Me.cbViewComp = New System.Windows.Forms.ComboBox()
        Me.Label5 = New System.Windows.Forms.Label()
        Me.GroupBox2.SuspendLayout()
        Me.GroupBox3.SuspendLayout()
        Me.TabControl1.SuspendLayout()
        Me.TabPage1.SuspendLayout()
        Me.TabPage2.SuspendLayout()
        Me.GroupBox1.SuspendLayout()
        Me.SuspendLayout()
        '
        'GroupBox2
        '
        Me.GroupBox2.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.GroupBox2.Controls.Add(Me.grid1)
        Me.GroupBox2.Location = New System.Drawing.Point(6, 41)
        Me.GroupBox2.Name = "GroupBox2"
        Me.GroupBox2.Size = New System.Drawing.Size(610, 495)
        Me.GroupBox2.TabIndex = 1
        Me.GroupBox2.TabStop = False
        Me.GroupBox2.Text = "Data"
        '
        'grid1
        '
        Me.grid1.BackColor = System.Drawing.Color.White
        Me.grid1.ColumnHeaderContextMenuStrip = Nothing
        Me.grid1.Dock = System.Windows.Forms.DockStyle.Fill
        Me.grid1.LeadHeaderContextMenuStrip = Nothing
        Me.grid1.Location = New System.Drawing.Point(3, 16)
        Me.grid1.Name = "grid1"
        Me.grid1.RowHeaderContextMenuStrip = Nothing
        Me.grid1.Script = Nothing
        Me.grid1.SheetTabContextMenuStrip = Nothing
        Me.grid1.SheetTabNewButtonVisible = False
        Me.grid1.SheetTabVisible = False
        Me.grid1.SheetTabWidth = 60
        Me.grid1.ShowScrollEndSpacing = True
        Me.grid1.Size = New System.Drawing.Size(604, 476)
        Me.grid1.TabIndex = 0
        Me.grid1.Text = "ReoGridControl1"
        '
        'GroupBox3
        '
        Me.GroupBox3.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.GroupBox3.Controls.Add(Me.TabControl1)
        Me.GroupBox3.Location = New System.Drawing.Point(622, 136)
        Me.GroupBox3.Name = "GroupBox3"
        Me.GroupBox3.Size = New System.Drawing.Size(288, 400)
        Me.GroupBox3.TabIndex = 2
        Me.GroupBox3.TabStop = False
        Me.GroupBox3.Text = "Settings"
        '
        'TabControl1
        '
        Me.TabControl1.Controls.Add(Me.TabPage1)
        Me.TabControl1.Controls.Add(Me.TabPage2)
        Me.TabControl1.Dock = System.Windows.Forms.DockStyle.Fill
        Me.TabControl1.Location = New System.Drawing.Point(3, 16)
        Me.TabControl1.Name = "TabControl1"
        Me.TabControl1.SelectedIndex = 0
        Me.TabControl1.Size = New System.Drawing.Size(282, 381)
        Me.TabControl1.TabIndex = 0
        '
        'TabPage1
        '
        Me.TabPage1.Controls.Add(Me.cbNBPUnits)
        Me.TabPage1.Controls.Add(Me.Label4)
        Me.TabPage1.Controls.Add(Me.cbPCUnits)
        Me.TabPage1.Controls.Add(Me.Label2)
        Me.TabPage1.Controls.Add(Me.cbTCUnits)
        Me.TabPage1.Controls.Add(Me.Label3)
        Me.TabPage1.Location = New System.Drawing.Point(4, 22)
        Me.TabPage1.Name = "TabPage1"
        Me.TabPage1.Padding = New System.Windows.Forms.Padding(3)
        Me.TabPage1.Size = New System.Drawing.Size(274, 355)
        Me.TabPage1.TabIndex = 0
        Me.TabPage1.Text = "Units"
        Me.TabPage1.UseVisualStyleBackColor = True
        '
        'cbNBPUnits
        '
        Me.cbNBPUnits.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbNBPUnits.FormattingEnabled = True
        Me.cbNBPUnits.Items.AddRange(New Object() {"K", "C", "F", "R"})
        Me.cbNBPUnits.Location = New System.Drawing.Point(166, 16)
        Me.cbNBPUnits.Name = "cbNBPUnits"
        Me.cbNBPUnits.Size = New System.Drawing.Size(92, 21)
        Me.cbNBPUnits.TabIndex = 37
        '
        'Label4
        '
        Me.Label4.AutoSize = True
        Me.Label4.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label4.Location = New System.Drawing.Point(11, 19)
        Me.Label4.Name = "Label4"
        Me.Label4.Size = New System.Drawing.Size(101, 13)
        Me.Label4.TabIndex = 36
        Me.Label4.Text = "Normal Boiling Point"
        '
        'cbPCUnits
        '
        Me.cbPCUnits.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbPCUnits.FormattingEnabled = True
        Me.cbPCUnits.Items.AddRange(New Object() {"Pa", "atm", "kgf/cm2", "kgf/cm2g", "lbf/ft2", "kPa", "kPag", "bar", "barg", "ftH2O", "inH2O", "inHg", "mbar", "mH2O", "mmH2O", "mmHg", "MPa", "psi", "psig"})
        Me.cbPCUnits.Location = New System.Drawing.Point(166, 76)
        Me.cbPCUnits.Name = "cbPCUnits"
        Me.cbPCUnits.Size = New System.Drawing.Size(92, 21)
        Me.cbPCUnits.TabIndex = 35
        '
        'Label2
        '
        Me.Label2.AutoSize = True
        Me.Label2.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label2.Location = New System.Drawing.Point(11, 80)
        Me.Label2.Name = "Label2"
        Me.Label2.Size = New System.Drawing.Size(82, 13)
        Me.Label2.TabIndex = 34
        Me.Label2.Text = "Critical Pressure"
        '
        'cbTCUnits
        '
        Me.cbTCUnits.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbTCUnits.FormattingEnabled = True
        Me.cbTCUnits.Items.AddRange(New Object() {"K", "C", "F", "R"})
        Me.cbTCUnits.Location = New System.Drawing.Point(166, 46)
        Me.cbTCUnits.Name = "cbTCUnits"
        Me.cbTCUnits.Size = New System.Drawing.Size(92, 21)
        Me.cbTCUnits.TabIndex = 33
        '
        'Label3
        '
        Me.Label3.AutoSize = True
        Me.Label3.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label3.Location = New System.Drawing.Point(11, 50)
        Me.Label3.Name = "Label3"
        Me.Label3.Size = New System.Drawing.Size(101, 13)
        Me.Label3.TabIndex = 32
        Me.Label3.Text = "Critical Temperature"
        '
        'TabPage2
        '
        Me.TabPage2.Controls.Add(Me.ComboBoxMW)
        Me.TabPage2.Controls.Add(Me.ComboBoxSG)
        Me.TabPage2.Controls.Add(Me.ComboBoxAF)
        Me.TabPage2.Controls.Add(Me.ComboBoxPC)
        Me.TabPage2.Controls.Add(Me.Label33)
        Me.TabPage2.Controls.Add(Me.Label31)
        Me.TabPage2.Controls.Add(Me.Label30)
        Me.TabPage2.Controls.Add(Me.Label29)
        Me.TabPage2.Controls.Add(Me.ComboBoxTC)
        Me.TabPage2.Controls.Add(Me.Label28)
        Me.TabPage2.Location = New System.Drawing.Point(4, 22)
        Me.TabPage2.Name = "TabPage2"
        Me.TabPage2.Padding = New System.Windows.Forms.Padding(3)
        Me.TabPage2.Size = New System.Drawing.Size(274, 355)
        Me.TabPage2.TabIndex = 1
        Me.TabPage2.Text = "Estimation Methods"
        Me.TabPage2.UseVisualStyleBackColor = True
        '
        'ComboBoxMW
        '
        Me.ComboBoxMW.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.ComboBoxMW.FormattingEnabled = True
        Me.ComboBoxMW.Items.AddRange(New Object() {"Winn (1956)", "Riazi (1986)", "Lee-Kesler (1974)"})
        Me.ComboBoxMW.Location = New System.Drawing.Point(125, 145)
        Me.ComboBoxMW.Name = "ComboBoxMW"
        Me.ComboBoxMW.Size = New System.Drawing.Size(138, 21)
        Me.ComboBoxMW.TabIndex = 34
        '
        'ComboBoxSG
        '
        Me.ComboBoxSG.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.ComboBoxSG.FormattingEnabled = True
        Me.ComboBoxSG.Items.AddRange(New Object() {"Riazi-Al-Sahhaf (1996)"})
        Me.ComboBoxSG.Location = New System.Drawing.Point(125, 113)
        Me.ComboBoxSG.Name = "ComboBoxSG"
        Me.ComboBoxSG.Size = New System.Drawing.Size(138, 21)
        Me.ComboBoxSG.TabIndex = 33
        '
        'ComboBoxAF
        '
        Me.ComboBoxAF.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.ComboBoxAF.FormattingEnabled = True
        Me.ComboBoxAF.Items.AddRange(New Object() {"Lee-Kesler (1976)", "Korsten (2000)"})
        Me.ComboBoxAF.Location = New System.Drawing.Point(125, 82)
        Me.ComboBoxAF.Name = "ComboBoxAF"
        Me.ComboBoxAF.Size = New System.Drawing.Size(138, 21)
        Me.ComboBoxAF.TabIndex = 32
        '
        'ComboBoxPC
        '
        Me.ComboBoxPC.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.ComboBoxPC.FormattingEnabled = True
        Me.ComboBoxPC.Items.AddRange(New Object() {"Riazi-Daubert (1985)", "Riazi (2005)", "Lee-Kesler (1976)", "Farah (2006)"})
        Me.ComboBoxPC.Location = New System.Drawing.Point(125, 51)
        Me.ComboBoxPC.Name = "ComboBoxPC"
        Me.ComboBoxPC.Size = New System.Drawing.Size(138, 21)
        Me.ComboBoxPC.TabIndex = 31
        '
        'Label33
        '
        Me.Label33.AutoSize = True
        Me.Label33.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label33.Location = New System.Drawing.Point(9, 117)
        Me.Label33.Name = "Label33"
        Me.Label33.Size = New System.Drawing.Size(81, 13)
        Me.Label33.TabIndex = 30
        Me.Label33.Text = "Specific Gravity"
        '
        'Label31
        '
        Me.Label31.AutoSize = True
        Me.Label31.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label31.Location = New System.Drawing.Point(9, 149)
        Me.Label31.Name = "Label31"
        Me.Label31.Size = New System.Drawing.Size(90, 13)
        Me.Label31.TabIndex = 29
        Me.Label31.Text = "Molecular Weight"
        '
        'Label30
        '
        Me.Label30.AutoSize = True
        Me.Label30.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label30.Location = New System.Drawing.Point(9, 86)
        Me.Label30.Name = "Label30"
        Me.Label30.Size = New System.Drawing.Size(79, 13)
        Me.Label30.TabIndex = 28
        Me.Label30.Text = "Acentric Factor"
        '
        'Label29
        '
        Me.Label29.AutoSize = True
        Me.Label29.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label29.Location = New System.Drawing.Point(9, 55)
        Me.Label29.Name = "Label29"
        Me.Label29.Size = New System.Drawing.Size(82, 13)
        Me.Label29.TabIndex = 27
        Me.Label29.Text = "Critical Pressure"
        '
        'ComboBoxTC
        '
        Me.ComboBoxTC.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.ComboBoxTC.FormattingEnabled = True
        Me.ComboBoxTC.Items.AddRange(New Object() {"Riazi-Daubert (1985)", "Riazi (2005)", "Lee-Kesler (1976)", "Farah (2006)"})
        Me.ComboBoxTC.Location = New System.Drawing.Point(125, 21)
        Me.ComboBoxTC.Name = "ComboBoxTC"
        Me.ComboBoxTC.Size = New System.Drawing.Size(138, 21)
        Me.ComboBoxTC.TabIndex = 26
        '
        'Label28
        '
        Me.Label28.AutoSize = True
        Me.Label28.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label28.Location = New System.Drawing.Point(9, 25)
        Me.Label28.Name = "Label28"
        Me.Label28.Size = New System.Drawing.Size(101, 13)
        Me.Label28.TabIndex = 25
        Me.Label28.Text = "Critical Temperature"
        '
        'btnEstimate
        '
        Me.btnEstimate.Anchor = CType((System.Windows.Forms.AnchorStyles.Bottom Or System.Windows.Forms.AnchorStyles.Left), System.Windows.Forms.AnchorStyles)
        Me.btnEstimate.Location = New System.Drawing.Point(6, 544)
        Me.btnEstimate.Name = "btnEstimate"
        Me.btnEstimate.Size = New System.Drawing.Size(231, 23)
        Me.btnEstimate.TabIndex = 3
        Me.btnEstimate.Text = "Commit Data / Estimate Missing Properties"
        Me.btnEstimate.UseVisualStyleBackColor = True
        '
        'btnExport
        '
        Me.btnExport.Anchor = CType((System.Windows.Forms.AnchorStyles.Bottom Or System.Windows.Forms.AnchorStyles.Left), System.Windows.Forms.AnchorStyles)
        Me.btnExport.Enabled = False
        Me.btnExport.Location = New System.Drawing.Point(245, 544)
        Me.btnExport.Name = "btnExport"
        Me.btnExport.Size = New System.Drawing.Size(160, 23)
        Me.btnExport.TabIndex = 4
        Me.btnExport.Text = "Export All to JSON Files"
        Me.btnExport.UseVisualStyleBackColor = True
        '
        'btnClose
        '
        Me.btnClose.Anchor = CType((System.Windows.Forms.AnchorStyles.Bottom Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.btnClose.Location = New System.Drawing.Point(810, 544)
        Me.btnClose.Name = "btnClose"
        Me.btnClose.Size = New System.Drawing.Size(100, 23)
        Me.btnClose.TabIndex = 5
        Me.btnClose.Text = "Close"
        Me.btnClose.UseVisualStyleBackColor = True
        '
        'Label1
        '
        Me.Label1.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Label1.Location = New System.Drawing.Point(11, 9)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(899, 29)
        Me.Label1.TabIndex = 6
        Me.Label1.Text = "Enter one compound per line. Missing data will be estimated depending on the avai" &
    "lable data for the corresponding compound. Please click on 'Commit Data' after y" &
    "ou make any changes to the table."
        '
        'btnAdd
        '
        Me.btnAdd.Anchor = CType((System.Windows.Forms.AnchorStyles.Bottom Or System.Windows.Forms.AnchorStyles.Left), System.Windows.Forms.AnchorStyles)
        Me.btnAdd.Enabled = False
        Me.btnAdd.Location = New System.Drawing.Point(577, 544)
        Me.btnAdd.Name = "btnAdd"
        Me.btnAdd.Size = New System.Drawing.Size(160, 23)
        Me.btnAdd.TabIndex = 7
        Me.btnAdd.Text = "Add All to Flowsheet"
        Me.btnAdd.UseVisualStyleBackColor = True
        '
        'btnExporttoXML
        '
        Me.btnExporttoXML.Anchor = CType((System.Windows.Forms.AnchorStyles.Bottom Or System.Windows.Forms.AnchorStyles.Left), System.Windows.Forms.AnchorStyles)
        Me.btnExporttoXML.Enabled = False
        Me.btnExporttoXML.Location = New System.Drawing.Point(411, 544)
        Me.btnExporttoXML.Name = "btnExporttoXML"
        Me.btnExporttoXML.Size = New System.Drawing.Size(160, 23)
        Me.btnExporttoXML.TabIndex = 8
        Me.btnExporttoXML.Text = "Export All to XML Database"
        Me.btnExporttoXML.UseVisualStyleBackColor = True
        '
        'GroupBox1
        '
        Me.GroupBox1.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.GroupBox1.Controls.Add(Me.btnViewComp)
        Me.GroupBox1.Controls.Add(Me.cbViewComp)
        Me.GroupBox1.Controls.Add(Me.Label5)
        Me.GroupBox1.Location = New System.Drawing.Point(622, 41)
        Me.GroupBox1.Name = "GroupBox1"
        Me.GroupBox1.Size = New System.Drawing.Size(288, 89)
        Me.GroupBox1.TabIndex = 9
        Me.GroupBox1.TabStop = False
        Me.GroupBox1.Text = "Tools"
        '
        'btnViewComp
        '
        Me.btnViewComp.Anchor = CType((System.Windows.Forms.AnchorStyles.Bottom Or System.Windows.Forms.AnchorStyles.Left), System.Windows.Forms.AnchorStyles)
        Me.btnViewComp.Enabled = False
        Me.btnViewComp.Location = New System.Drawing.Point(114, 51)
        Me.btnViewComp.Name = "btnViewComp"
        Me.btnViewComp.Size = New System.Drawing.Size(151, 23)
        Me.btnViewComp.TabIndex = 39
        Me.btnViewComp.Text = "View"
        Me.btnViewComp.UseVisualStyleBackColor = True
        '
        'cbViewComp
        '
        Me.cbViewComp.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbViewComp.FormattingEnabled = True
        Me.cbViewComp.Items.AddRange(New Object() {"K", "C", "F", "R"})
        Me.cbViewComp.Location = New System.Drawing.Point(114, 24)
        Me.cbViewComp.Name = "cbViewComp"
        Me.cbViewComp.Size = New System.Drawing.Size(151, 21)
        Me.cbViewComp.TabIndex = 38
        '
        'Label5
        '
        Me.Label5.AutoSize = True
        Me.Label5.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label5.Location = New System.Drawing.Point(18, 29)
        Me.Label5.Name = "Label5"
        Me.Label5.Size = New System.Drawing.Size(84, 13)
        Me.Label5.TabIndex = 37
        Me.Label5.Text = "View Compound"
        '
        'FormBulkAddPseudos
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(920, 577)
        Me.Controls.Add(Me.GroupBox1)
        Me.Controls.Add(Me.btnExporttoXML)
        Me.Controls.Add(Me.btnAdd)
        Me.Controls.Add(Me.Label1)
        Me.Controls.Add(Me.btnClose)
        Me.Controls.Add(Me.btnExport)
        Me.Controls.Add(Me.btnEstimate)
        Me.Controls.Add(Me.GroupBox3)
        Me.Controls.Add(Me.GroupBox2)
        Me.Icon = CType(resources.GetObject("$this.Icon"), System.Drawing.Icon)
        Me.Name = "FormBulkAddPseudos"
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen
        Me.Text = "Bulk Create Pseudocompounds"
        Me.GroupBox2.ResumeLayout(False)
        Me.GroupBox3.ResumeLayout(False)
        Me.TabControl1.ResumeLayout(False)
        Me.TabPage1.ResumeLayout(False)
        Me.TabPage1.PerformLayout()
        Me.TabPage2.ResumeLayout(False)
        Me.TabPage2.PerformLayout()
        Me.GroupBox1.ResumeLayout(False)
        Me.GroupBox1.PerformLayout()
        Me.ResumeLayout(False)

    End Sub

    Friend WithEvents GroupBox2 As GroupBox
    Friend WithEvents grid1 As unvell.ReoGrid.ReoGridControl
    Friend WithEvents GroupBox3 As GroupBox
    Friend WithEvents TabControl1 As TabControl
    Friend WithEvents TabPage1 As TabPage
    Friend WithEvents TabPage2 As TabPage
    Public WithEvents ComboBoxMW As ComboBox
    Public WithEvents ComboBoxSG As ComboBox
    Public WithEvents ComboBoxAF As ComboBox
    Public WithEvents ComboBoxPC As ComboBox
    Public WithEvents Label33 As Label
    Public WithEvents Label31 As Label
    Public WithEvents Label30 As Label
    Public WithEvents Label29 As Label
    Public WithEvents ComboBoxTC As ComboBox
    Public WithEvents Label28 As Label
    Friend WithEvents btnEstimate As Button
    Friend WithEvents btnExport As Button
    Friend WithEvents btnClose As Button
    Friend WithEvents Label1 As Label
    Public WithEvents cbNBPUnits As ComboBox
    Public WithEvents Label4 As Label
    Public WithEvents cbPCUnits As ComboBox
    Public WithEvents Label2 As Label
    Public WithEvents cbTCUnits As ComboBox
    Public WithEvents Label3 As Label
    Friend WithEvents btnAdd As Button
    Friend WithEvents btnExporttoXML As Button
    Friend WithEvents GroupBox1 As GroupBox
    Friend WithEvents btnViewComp As Button
    Public WithEvents cbViewComp As ComboBox
    Public WithEvents Label5 As Label
End Class
