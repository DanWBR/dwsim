<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class FormImportCompoundFromThermo
    Inherits System.Windows.Forms.Form

    'Form overrides dispose to clean up the component list.
    <System.Diagnostics.DebuggerNonUserCode()> _
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
    <System.Diagnostics.DebuggerStepThrough()> _
    Private Sub InitializeComponent()
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(FormImportCompoundFromThermo))
        Me.WizardControl1 = New AeroWizard.WizardControl()
        Me.WizardPage2 = New AeroWizard.WizardPage()
        Me.Label10 = New System.Windows.Forms.Label()
        Me.Label7 = New System.Windows.Forms.Label()
        Me.PictureBox1 = New System.Windows.Forms.PictureBox()
        Me.LinkLabel2 = New System.Windows.Forms.LinkLabel()
        Me.LinkLabel1 = New System.Windows.Forms.LinkLabel()
        Me.Label8 = New System.Windows.Forms.Label()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.WizardPage1 = New AeroWizard.WizardPage()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.tbSearchString = New System.Windows.Forms.TextBox()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.WizardPage3 = New AeroWizard.WizardPage()
        Me.Label5 = New System.Windows.Forms.Label()
        Me.tbQueryMatch = New System.Windows.Forms.TextBox()
        Me.Label4 = New System.Windows.Forms.Label()
        Me.WizardPage4 = New AeroWizard.WizardPage()
        Me.Button1 = New System.Windows.Forms.Button()
        Me.tbImportAs = New System.Windows.Forms.TextBox()
        Me.Label9 = New System.Windows.Forms.Label()
        Me.dgResults = New System.Windows.Forms.DataGridView()
        Me.Column2 = New System.Windows.Forms.DataGridViewImageColumn()
        Me.Column1 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Label6 = New System.Windows.Forms.Label()
        CType(Me.WizardControl1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.WizardPage2.SuspendLayout()
        CType(Me.PictureBox1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.WizardPage1.SuspendLayout()
        Me.WizardPage3.SuspendLayout()
        Me.WizardPage4.SuspendLayout()
        CType(Me.dgResults, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'WizardControl1
        '
        Me.WizardControl1.Location = New System.Drawing.Point(0, 0)
        Me.WizardControl1.Name = "WizardControl1"
        Me.WizardControl1.Pages.Add(Me.WizardPage2)
        Me.WizardControl1.Pages.Add(Me.WizardPage1)
        Me.WizardControl1.Pages.Add(Me.WizardPage3)
        Me.WizardControl1.Pages.Add(Me.WizardPage4)
        Me.WizardControl1.Size = New System.Drawing.Size(748, 509)
        Me.WizardControl1.TabIndex = 0
        Me.WizardControl1.Title = "Import Compound from Thermo/Chemicals"
        '
        'WizardPage2
        '
        Me.WizardPage2.Controls.Add(Me.Label10)
        Me.WizardPage2.Controls.Add(Me.Label7)
        Me.WizardPage2.Controls.Add(Me.PictureBox1)
        Me.WizardPage2.Controls.Add(Me.LinkLabel2)
        Me.WizardPage2.Controls.Add(Me.LinkLabel1)
        Me.WizardPage2.Controls.Add(Me.Label8)
        Me.WizardPage2.Controls.Add(Me.Label3)
        Me.WizardPage2.Name = "WizardPage2"
        Me.WizardPage2.Size = New System.Drawing.Size(701, 354)
        Me.WizardPage2.TabIndex = 1
        Me.WizardPage2.Text = "Welcome"
        '
        'Label10
        '
        Me.Label10.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label10.Location = New System.Drawing.Point(21, 282)
        Me.Label10.Name = "Label10"
        Me.Label10.Size = New System.Drawing.Size(663, 53)
        Me.Label10.TabIndex = 11
        Me.Label10.Text = "'thermo' and 'chemicals' Python libraries are Copyright (c) Caleb Bell and Contri" &
    "butors (2016-2021). Thermo: Chemical properties component of Chemical Engineerin" &
    "g Design Library (ChEDL)."
        '
        'Label7
        '
        Me.Label7.AutoSize = True
        Me.Label7.Font = New System.Drawing.Font("Arial", 15.75!)
        Me.Label7.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label7.Location = New System.Drawing.Point(157, 49)
        Me.Label7.Name = "Label7"
        Me.Label7.Size = New System.Drawing.Size(399, 24)
        Me.Label7.TabIndex = 10
        Me.Label7.Text = "Import Compound from Thermo/Chemicals"
        '
        'PictureBox1
        '
        Me.PictureBox1.Dock = System.Windows.Forms.DockStyle.Top
        Me.PictureBox1.Image = Global.DWSIM.My.Resources.Resources.Lab_icon
        Me.PictureBox1.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.PictureBox1.Location = New System.Drawing.Point(0, 0)
        Me.PictureBox1.Name = "PictureBox1"
        Me.PictureBox1.Size = New System.Drawing.Size(701, 128)
        Me.PictureBox1.SizeMode = System.Windows.Forms.PictureBoxSizeMode.AutoSize
        Me.PictureBox1.TabIndex = 9
        Me.PictureBox1.TabStop = False
        '
        'LinkLabel2
        '
        Me.LinkLabel2.AutoSize = True
        Me.LinkLabel2.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.LinkLabel2.Location = New System.Drawing.Point(126, 240)
        Me.LinkLabel2.Name = "LinkLabel2"
        Me.LinkLabel2.Size = New System.Drawing.Size(221, 15)
        Me.LinkLabel2.TabIndex = 8
        Me.LinkLabel2.TabStop = True
        Me.LinkLabel2.Text = "https://github.com/CalebBell/chemicals"
        '
        'LinkLabel1
        '
        Me.LinkLabel1.AutoSize = True
        Me.LinkLabel1.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.LinkLabel1.Location = New System.Drawing.Point(126, 205)
        Me.LinkLabel1.Name = "LinkLabel1"
        Me.LinkLabel1.Size = New System.Drawing.Size(207, 15)
        Me.LinkLabel1.TabIndex = 7
        Me.LinkLabel1.TabStop = True
        Me.LinkLabel1.Text = "https://github.com/CalebBell/thermo"
        '
        'Label8
        '
        Me.Label8.AutoSize = True
        Me.Label8.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label8.Location = New System.Drawing.Point(21, 205)
        Me.Label8.Name = "Label8"
        Me.Label8.Size = New System.Drawing.Size(67, 15)
        Me.Label8.TabIndex = 6
        Me.Label8.Text = "References:"
        '
        'Label3
        '
        Me.Label3.AutoSize = True
        Me.Label3.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label3.Location = New System.Drawing.Point(21, 142)
        Me.Label3.Name = "Label3"
        Me.Label3.Size = New System.Drawing.Size(489, 45)
        Me.Label3.TabIndex = 5
        Me.Label3.Text = "This tool imports compound data from Caleb Bell's Thermo and Chemicals Python lib" &
    "raries." & Global.Microsoft.VisualBasic.ChrW(13) & Global.Microsoft.VisualBasic.ChrW(10) & Global.Microsoft.VisualBasic.ChrW(13) & Global.Microsoft.VisualBasic.ChrW(10) & "Please install the 'thermo' and 'chemicals' packages with pip before " &
    "using this tool."
        '
        'WizardPage1
        '
        Me.WizardPage1.Controls.Add(Me.Label2)
        Me.WizardPage1.Controls.Add(Me.tbSearchString)
        Me.WizardPage1.Controls.Add(Me.Label1)
        Me.WizardPage1.Name = "WizardPage1"
        Me.WizardPage1.Size = New System.Drawing.Size(701, 354)
        Me.WizardPage1.TabIndex = 0
        Me.WizardPage1.Text = "Search Thermo/Chemicals Database"
        '
        'Label2
        '
        Me.Label2.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Label2.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label2.Location = New System.Drawing.Point(15, 63)
        Me.Label2.Name = "Label2"
        Me.Label2.Size = New System.Drawing.Size(662, 278)
        Me.Label2.TabIndex = 4
        Me.Label2.Text = resources.GetString("Label2.Text")
        '
        'tbSearchString
        '
        Me.tbSearchString.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.tbSearchString.Location = New System.Drawing.Point(112, 27)
        Me.tbSearchString.Name = "tbSearchString"
        Me.tbSearchString.Size = New System.Drawing.Size(565, 23)
        Me.tbSearchString.TabIndex = 3
        '
        'Label1
        '
        Me.Label1.AutoSize = True
        Me.Label1.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label1.Location = New System.Drawing.Point(15, 31)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(76, 15)
        Me.Label1.TabIndex = 2
        Me.Label1.Text = "Search String"
        '
        'WizardPage3
        '
        Me.WizardPage3.Controls.Add(Me.Label5)
        Me.WizardPage3.Controls.Add(Me.tbQueryMatch)
        Me.WizardPage3.Controls.Add(Me.Label4)
        Me.WizardPage3.Name = "WizardPage3"
        Me.WizardPage3.Size = New System.Drawing.Size(701, 354)
        Me.WizardPage3.TabIndex = 2
        Me.WizardPage3.Text = "Query Match"
        '
        'Label5
        '
        Me.Label5.AutoSize = True
        Me.Label5.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label5.Location = New System.Drawing.Point(18, 95)
        Me.Label5.Name = "Label5"
        Me.Label5.Size = New System.Drawing.Size(446, 15)
        Me.Label5.TabIndex = 6
        Me.Label5.Text = "If the compound found matches your search criteria, click 'Next' to retrieve its " &
    "data."
        '
        'tbQueryMatch
        '
        Me.tbQueryMatch.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.tbQueryMatch.Location = New System.Drawing.Point(115, 36)
        Me.tbQueryMatch.Name = "tbQueryMatch"
        Me.tbQueryMatch.ReadOnly = True
        Me.tbQueryMatch.Size = New System.Drawing.Size(565, 23)
        Me.tbQueryMatch.TabIndex = 5
        '
        'Label4
        '
        Me.Label4.AutoSize = True
        Me.Label4.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label4.Location = New System.Drawing.Point(18, 40)
        Me.Label4.Name = "Label4"
        Me.Label4.Size = New System.Drawing.Size(76, 15)
        Me.Label4.TabIndex = 4
        Me.Label4.Text = "Query Match"
        '
        'WizardPage4
        '
        Me.WizardPage4.Controls.Add(Me.Button1)
        Me.WizardPage4.Controls.Add(Me.tbImportAs)
        Me.WizardPage4.Controls.Add(Me.Label9)
        Me.WizardPage4.Controls.Add(Me.dgResults)
        Me.WizardPage4.Controls.Add(Me.Label6)
        Me.WizardPage4.HelpText = ""
        Me.WizardPage4.IsFinishPage = True
        Me.WizardPage4.Name = "WizardPage4"
        Me.WizardPage4.Size = New System.Drawing.Size(701, 354)
        Me.WizardPage4.TabIndex = 3
        Me.WizardPage4.Text = "Review Collected Data and Import Compound"
        '
        'Button1
        '
        Me.Button1.Anchor = CType((System.Windows.Forms.AnchorStyles.Bottom Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Button1.Location = New System.Drawing.Point(578, 317)
        Me.Button1.Name = "Button1"
        Me.Button1.Size = New System.Drawing.Size(109, 23)
        Me.Button1.TabIndex = 10
        Me.Button1.Text = "Export to JSON"
        Me.Button1.UseVisualStyleBackColor = True
        '
        'tbImportAs
        '
        Me.tbImportAs.Anchor = CType(((System.Windows.Forms.AnchorStyles.Bottom Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.tbImportAs.Location = New System.Drawing.Point(120, 317)
        Me.tbImportAs.Name = "tbImportAs"
        Me.tbImportAs.Size = New System.Drawing.Size(452, 23)
        Me.tbImportAs.TabIndex = 8
        '
        'Label9
        '
        Me.Label9.Anchor = CType((System.Windows.Forms.AnchorStyles.Bottom Or System.Windows.Forms.AnchorStyles.Left), System.Windows.Forms.AnchorStyles)
        Me.Label9.AutoSize = True
        Me.Label9.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label9.Location = New System.Drawing.Point(11, 321)
        Me.Label9.Name = "Label9"
        Me.Label9.Size = New System.Drawing.Size(103, 15)
        Me.Label9.TabIndex = 7
        Me.Label9.Text = "Compound Name"
        '
        'dgResults
        '
        Me.dgResults.AllowUserToAddRows = False
        Me.dgResults.AllowUserToDeleteRows = False
        Me.dgResults.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.dgResults.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.dgResults.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.dgResults.ColumnHeadersVisible = False
        Me.dgResults.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.Column2, Me.Column1})
        Me.dgResults.Location = New System.Drawing.Point(13, 84)
        Me.dgResults.Name = "dgResults"
        Me.dgResults.RowHeadersVisible = False
        Me.dgResults.Size = New System.Drawing.Size(674, 217)
        Me.dgResults.TabIndex = 5
        '
        'Column2
        '
        Me.Column2.FillWeight = 10.0!
        Me.Column2.HeaderText = ""
        Me.Column2.Name = "Column2"
        Me.Column2.ReadOnly = True
        '
        'Column1
        '
        Me.Column1.FillWeight = 80.0!
        Me.Column1.HeaderText = "Propriedade"
        Me.Column1.Name = "Column1"
        Me.Column1.ReadOnly = True
        '
        'Label6
        '
        Me.Label6.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Label6.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label6.Location = New System.Drawing.Point(10, 7)
        Me.Label6.Name = "Label6"
        Me.Label6.Size = New System.Drawing.Size(677, 74)
        Me.Label6.TabIndex = 4
        Me.Label6.Text = resources.GetString("Label6.Text")
        '
        'FormImportCompoundFromThermo
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(96.0!, 96.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi
        Me.ClientSize = New System.Drawing.Size(748, 509)
        Me.Controls.Add(Me.WizardControl1)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedToolWindow
        Me.Name = "FormImportCompoundFromThermo"
        Me.ShowIcon = False
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen
        Me.Text = "Import Compound From Thermo"
        CType(Me.WizardControl1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.WizardPage2.ResumeLayout(False)
        Me.WizardPage2.PerformLayout()
        CType(Me.PictureBox1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.WizardPage1.ResumeLayout(False)
        Me.WizardPage1.PerformLayout()
        Me.WizardPage3.ResumeLayout(False)
        Me.WizardPage3.PerformLayout()
        Me.WizardPage4.ResumeLayout(False)
        Me.WizardPage4.PerformLayout()
        CType(Me.dgResults, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)

    End Sub

    Friend WithEvents WizardControl1 As AeroWizard.WizardControl
    Friend WithEvents WizardPage1 As AeroWizard.WizardPage
    Friend WithEvents WizardPage2 As AeroWizard.WizardPage
    Friend WithEvents Label2 As Label
    Friend WithEvents tbSearchString As TextBox
    Friend WithEvents Label1 As Label
    Friend WithEvents Label3 As Label
    Friend WithEvents LinkLabel2 As LinkLabel
    Friend WithEvents LinkLabel1 As LinkLabel
    Friend WithEvents Label8 As Label
    Friend WithEvents PictureBox1 As PictureBox
    Friend WithEvents Label7 As Label
    Friend WithEvents WizardPage3 As AeroWizard.WizardPage
    Friend WithEvents Label5 As Label
    Friend WithEvents tbQueryMatch As TextBox
    Friend WithEvents Label4 As Label
    Friend WithEvents WizardPage4 As AeroWizard.WizardPage
    Friend WithEvents dgResults As DataGridView
    Friend WithEvents Column2 As DataGridViewImageColumn
    Friend WithEvents Column1 As DataGridViewTextBoxColumn
    Friend WithEvents Label6 As Label
    Friend WithEvents tbImportAs As TextBox
    Friend WithEvents Label9 As Label
    Friend WithEvents Label10 As Label
    Friend WithEvents Button1 As Button
End Class
