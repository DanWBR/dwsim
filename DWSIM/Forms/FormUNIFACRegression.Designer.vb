<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class FormUNIFACRegression
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
        Me.components = New System.ComponentModel.Container()
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(FormUNIFACRegression))
        Dim DataGridViewCellStyle1 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle2 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle3 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle4 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle5 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Me.FaTabStrip1 = New FarsiLibrary.Win.FATabStrip()
        Me.TSInformation = New FarsiLibrary.Win.FATabStripItem()
        Me.Panel3 = New System.Windows.Forms.Panel()
        Me.BtnSelectIPDB = New System.Windows.Forms.Button()
        Me.Label5 = New System.Windows.Forms.Label()
        Me.tbIPDBName = New System.Windows.Forms.TextBox()
        Me.BtnSaveIPDB = New System.Windows.Forms.Button()
        Me.BtnNewIPDB = New System.Windows.Forms.Button()
        Me.tbDescription = New System.Windows.Forms.TextBox()
        Me.Label11 = New System.Windows.Forms.Label()
        Me.tbTitle = New System.Windows.Forms.TextBox()
        Me.Label10 = New System.Windows.Forms.Label()
        Me.TSModel = New FarsiLibrary.Win.FATabStripItem()
        Me.Panel2 = New System.Windows.Forms.Panel()
        Me.TBcji = New System.Windows.Forms.TextBox()
        Me.TBbji = New System.Windows.Forms.TextBox()
        Me.TBaji = New System.Windows.Forms.TextBox()
        Me.Lblcji = New System.Windows.Forms.Label()
        Me.Lblbji = New System.Windows.Forms.Label()
        Me.Lblaji = New System.Windows.Forms.Label()
        Me.TBcij = New System.Windows.Forms.TextBox()
        Me.TBbij = New System.Windows.Forms.TextBox()
        Me.TBaij = New System.Windows.Forms.TextBox()
        Me.Lblcij = New System.Windows.Forms.Label()
        Me.Lblbij = New System.Windows.Forms.Label()
        Me.Lblaij = New System.Windows.Forms.Label()
        Me.LblMGj = New System.Windows.Forms.Label()
        Me.LblMGi = New System.Windows.Forms.Label()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.BtnShowIPGrid = New System.Windows.Forms.Button()
        Me.Panel1 = New System.Windows.Forms.Panel()
        Me.IPGrid = New System.Windows.Forms.DataGridView()
        Me.Column6 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Label4 = New System.Windows.Forms.Label()
        Me.cbModel = New System.Windows.Forms.ComboBox()
        Me.LblC1 = New System.Windows.Forms.Label()
        Me.LblC2 = New System.Windows.Forms.Label()
        Me.cbCompound1 = New System.Windows.Forms.ComboBox()
        Me.cbCompound2 = New System.Windows.Forms.ComboBox()
        Me.TSData = New FarsiLibrary.Win.FATabStripItem()
        Me.LblTUnit = New System.Windows.Forms.Label()
        Me.TbUnifacTemp = New System.Windows.Forms.TextBox()
        Me.BtnNewTemp = New System.Windows.Forms.Button()
        Me.TBStatus = New System.Windows.Forms.TextBox()
        Me.BtnDrawIPs = New System.Windows.Forms.Button()
        Me.BtnDrawChart = New System.Windows.Forms.Button()
        Me.BtnRegressIP = New System.Windows.Forms.Button()
        Me.GridExpData = New System.Windows.Forms.DataGridView()
        Me.Column1 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Column2 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Column3 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Column4 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Column5 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Column7 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Column8 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Column9 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.TSChart = New FarsiLibrary.Win.FATabStripItem()
        Me.graph = New ZedGraph.ZedGraphControl()
        CType(Me.FaTabStrip1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.FaTabStrip1.SuspendLayout()
        Me.TSInformation.SuspendLayout()
        Me.Panel3.SuspendLayout()
        Me.TSModel.SuspendLayout()
        Me.Panel2.SuspendLayout()
        Me.Panel1.SuspendLayout()
        CType(Me.IPGrid, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.TSData.SuspendLayout()
        CType(Me.GridExpData, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.TSChart.SuspendLayout()
        Me.SuspendLayout()
        '
        'FaTabStrip1
        '
        Me.FaTabStrip1.AlwaysShowClose = False
        Me.FaTabStrip1.AlwaysShowMenuGlyph = False
        resources.ApplyResources(Me.FaTabStrip1, "FaTabStrip1")
        Me.FaTabStrip1.Items.AddRange(New FarsiLibrary.Win.FATabStripItem() {Me.TSInformation, Me.TSModel, Me.TSData, Me.TSChart})
        Me.FaTabStrip1.Name = "FaTabStrip1"
        Me.FaTabStrip1.SelectedItem = Me.TSInformation
        '
        'TSInformation
        '
        Me.TSInformation.CanClose = False
        Me.TSInformation.Controls.Add(Me.Panel3)
        Me.TSInformation.Controls.Add(Me.tbDescription)
        Me.TSInformation.Controls.Add(Me.Label11)
        Me.TSInformation.Controls.Add(Me.tbTitle)
        Me.TSInformation.Controls.Add(Me.Label10)
        Me.TSInformation.IsDrawn = True
        Me.TSInformation.Name = "TSInformation"
        Me.TSInformation.Selected = True
        resources.ApplyResources(Me.TSInformation, "TSInformation")
        '
        'Panel3
        '
        Me.Panel3.BackColor = System.Drawing.SystemColors.ControlLight
        Me.Panel3.Controls.Add(Me.BtnSelectIPDB)
        Me.Panel3.Controls.Add(Me.Label5)
        Me.Panel3.Controls.Add(Me.tbIPDBName)
        Me.Panel3.Controls.Add(Me.BtnSaveIPDB)
        Me.Panel3.Controls.Add(Me.BtnNewIPDB)
        resources.ApplyResources(Me.Panel3, "Panel3")
        Me.Panel3.Name = "Panel3"
        '
        'BtnSelectIPDB
        '
        Me.BtnSelectIPDB.Image = Global.DWSIM.My.Resources.Resources.folder_go
        resources.ApplyResources(Me.BtnSelectIPDB, "BtnSelectIPDB")
        Me.BtnSelectIPDB.Name = "BtnSelectIPDB"
        Me.BtnSelectIPDB.UseVisualStyleBackColor = True
        '
        'Label5
        '
        resources.ApplyResources(Me.Label5, "Label5")
        Me.Label5.Name = "Label5"
        '
        'tbIPDBName
        '
        resources.ApplyResources(Me.tbIPDBName, "tbIPDBName")
        Me.tbIPDBName.Name = "tbIPDBName"
        '
        'BtnSaveIPDB
        '
        Me.BtnSaveIPDB.Image = Global.DWSIM.My.Resources.Resources.disk
        resources.ApplyResources(Me.BtnSaveIPDB, "BtnSaveIPDB")
        Me.BtnSaveIPDB.Name = "BtnSaveIPDB"
        Me.BtnSaveIPDB.UseVisualStyleBackColor = True
        '
        'BtnNewIPDB
        '
        Me.BtnNewIPDB.Image = Global.DWSIM.My.Resources.Resources.page_white
        resources.ApplyResources(Me.BtnNewIPDB, "BtnNewIPDB")
        Me.BtnNewIPDB.Name = "BtnNewIPDB"
        Me.BtnNewIPDB.UseVisualStyleBackColor = True
        '
        'tbDescription
        '
        resources.ApplyResources(Me.tbDescription, "tbDescription")
        Me.tbDescription.Name = "tbDescription"
        '
        'Label11
        '
        resources.ApplyResources(Me.Label11, "Label11")
        Me.Label11.Name = "Label11"
        '
        'tbTitle
        '
        resources.ApplyResources(Me.tbTitle, "tbTitle")
        Me.tbTitle.Name = "tbTitle"
        '
        'Label10
        '
        resources.ApplyResources(Me.Label10, "Label10")
        Me.Label10.Name = "Label10"
        '
        'TSModel
        '
        Me.TSModel.CanClose = False
        Me.TSModel.Controls.Add(Me.Panel2)
        Me.TSModel.Controls.Add(Me.BtnShowIPGrid)
        Me.TSModel.Controls.Add(Me.Panel1)
        Me.TSModel.Controls.Add(Me.Label4)
        Me.TSModel.Controls.Add(Me.cbModel)
        Me.TSModel.Controls.Add(Me.LblC1)
        Me.TSModel.Controls.Add(Me.LblC2)
        Me.TSModel.Controls.Add(Me.cbCompound1)
        Me.TSModel.Controls.Add(Me.cbCompound2)
        Me.TSModel.IsDrawn = True
        Me.TSModel.Name = "TSModel"
        resources.ApplyResources(Me.TSModel, "TSModel")
        '
        'Panel2
        '
        Me.Panel2.BackColor = System.Drawing.SystemColors.ControlLight
        Me.Panel2.Controls.Add(Me.TBcji)
        Me.Panel2.Controls.Add(Me.TBbji)
        Me.Panel2.Controls.Add(Me.TBaji)
        Me.Panel2.Controls.Add(Me.Lblcji)
        Me.Panel2.Controls.Add(Me.Lblbji)
        Me.Panel2.Controls.Add(Me.Lblaji)
        Me.Panel2.Controls.Add(Me.TBcij)
        Me.Panel2.Controls.Add(Me.TBbij)
        Me.Panel2.Controls.Add(Me.TBaij)
        Me.Panel2.Controls.Add(Me.Lblcij)
        Me.Panel2.Controls.Add(Me.Lblbij)
        Me.Panel2.Controls.Add(Me.Lblaij)
        Me.Panel2.Controls.Add(Me.LblMGj)
        Me.Panel2.Controls.Add(Me.LblMGi)
        Me.Panel2.Controls.Add(Me.Label2)
        Me.Panel2.Controls.Add(Me.Label1)
        resources.ApplyResources(Me.Panel2, "Panel2")
        Me.Panel2.Name = "Panel2"
        '
        'TBcji
        '
        resources.ApplyResources(Me.TBcji, "TBcji")
        Me.TBcji.Name = "TBcji"
        '
        'TBbji
        '
        resources.ApplyResources(Me.TBbji, "TBbji")
        Me.TBbji.Name = "TBbji"
        '
        'TBaji
        '
        resources.ApplyResources(Me.TBaji, "TBaji")
        Me.TBaji.Name = "TBaji"
        '
        'Lblcji
        '
        resources.ApplyResources(Me.Lblcji, "Lblcji")
        Me.Lblcji.Name = "Lblcji"
        '
        'Lblbji
        '
        resources.ApplyResources(Me.Lblbji, "Lblbji")
        Me.Lblbji.Name = "Lblbji"
        '
        'Lblaji
        '
        resources.ApplyResources(Me.Lblaji, "Lblaji")
        Me.Lblaji.Name = "Lblaji"
        '
        'TBcij
        '
        resources.ApplyResources(Me.TBcij, "TBcij")
        Me.TBcij.Name = "TBcij"
        '
        'TBbij
        '
        resources.ApplyResources(Me.TBbij, "TBbij")
        Me.TBbij.Name = "TBbij"
        '
        'TBaij
        '
        resources.ApplyResources(Me.TBaij, "TBaij")
        Me.TBaij.Name = "TBaij"
        '
        'Lblcij
        '
        resources.ApplyResources(Me.Lblcij, "Lblcij")
        Me.Lblcij.Name = "Lblcij"
        '
        'Lblbij
        '
        resources.ApplyResources(Me.Lblbij, "Lblbij")
        Me.Lblbij.Name = "Lblbij"
        '
        'Lblaij
        '
        resources.ApplyResources(Me.Lblaij, "Lblaij")
        Me.Lblaij.Name = "Lblaij"
        '
        'LblMGj
        '
        resources.ApplyResources(Me.LblMGj, "LblMGj")
        Me.LblMGj.Name = "LblMGj"
        '
        'LblMGi
        '
        resources.ApplyResources(Me.LblMGi, "LblMGi")
        Me.LblMGi.Name = "LblMGi"
        '
        'Label2
        '
        resources.ApplyResources(Me.Label2, "Label2")
        Me.Label2.Name = "Label2"
        '
        'Label1
        '
        resources.ApplyResources(Me.Label1, "Label1")
        Me.Label1.Name = "Label1"
        '
        'BtnShowIPGrid
        '
        Me.BtnShowIPGrid.Image = Global.DWSIM.My.Resources.Resources.table
        resources.ApplyResources(Me.BtnShowIPGrid, "BtnShowIPGrid")
        Me.BtnShowIPGrid.Name = "BtnShowIPGrid"
        Me.BtnShowIPGrid.UseVisualStyleBackColor = True
        '
        'Panel1
        '
        resources.ApplyResources(Me.Panel1, "Panel1")
        Me.Panel1.BackColor = System.Drawing.SystemColors.ControlLightLight
        Me.Panel1.Controls.Add(Me.IPGrid)
        Me.Panel1.Name = "Panel1"
        '
        'IPGrid
        '
        Me.IPGrid.AllowUserToAddRows = False
        Me.IPGrid.AllowUserToDeleteRows = False
        Me.IPGrid.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.IPGrid.AutoSizeRowsMode = System.Windows.Forms.DataGridViewAutoSizeRowsMode.AllCells
        Me.IPGrid.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.IPGrid.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.Column6})
        resources.ApplyResources(Me.IPGrid, "IPGrid")
        Me.IPGrid.MultiSelect = False
        Me.IPGrid.Name = "IPGrid"
        Me.IPGrid.ReadOnly = True
        '
        'Column6
        '
        resources.ApplyResources(Me.Column6, "Column6")
        Me.Column6.Name = "Column6"
        Me.Column6.ReadOnly = True
        '
        'Label4
        '
        resources.ApplyResources(Me.Label4, "Label4")
        Me.Label4.Name = "Label4"
        '
        'cbModel
        '
        Me.cbModel.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbModel.FormattingEnabled = True
        Me.cbModel.Items.AddRange(New Object() {resources.GetString("cbModel.Items"), resources.GetString("cbModel.Items1"), resources.GetString("cbModel.Items2")})
        resources.ApplyResources(Me.cbModel, "cbModel")
        Me.cbModel.Name = "cbModel"
        '
        'LblC1
        '
        resources.ApplyResources(Me.LblC1, "LblC1")
        Me.LblC1.Name = "LblC1"
        '
        'LblC2
        '
        resources.ApplyResources(Me.LblC2, "LblC2")
        Me.LblC2.Name = "LblC2"
        '
        'cbCompound1
        '
        Me.cbCompound1.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbCompound1.FormattingEnabled = True
        resources.ApplyResources(Me.cbCompound1, "cbCompound1")
        Me.cbCompound1.Name = "cbCompound1"
        '
        'cbCompound2
        '
        Me.cbCompound2.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbCompound2.FormattingEnabled = True
        resources.ApplyResources(Me.cbCompound2, "cbCompound2")
        Me.cbCompound2.Name = "cbCompound2"
        '
        'TSData
        '
        Me.TSData.CanClose = False
        Me.TSData.Controls.Add(Me.LblTUnit)
        Me.TSData.Controls.Add(Me.TbUnifacTemp)
        Me.TSData.Controls.Add(Me.BtnNewTemp)
        Me.TSData.Controls.Add(Me.TBStatus)
        Me.TSData.Controls.Add(Me.BtnDrawIPs)
        Me.TSData.Controls.Add(Me.BtnDrawChart)
        Me.TSData.Controls.Add(Me.BtnRegressIP)
        Me.TSData.Controls.Add(Me.GridExpData)
        Me.TSData.IsDrawn = True
        Me.TSData.Name = "TSData"
        resources.ApplyResources(Me.TSData, "TSData")
        '
        'LblTUnit
        '
        resources.ApplyResources(Me.LblTUnit, "LblTUnit")
        Me.LblTUnit.Name = "LblTUnit"
        '
        'TbUnifacTemp
        '
        resources.ApplyResources(Me.TbUnifacTemp, "TbUnifacTemp")
        Me.TbUnifacTemp.Name = "TbUnifacTemp"
        '
        'BtnNewTemp
        '
        resources.ApplyResources(Me.BtnNewTemp, "BtnNewTemp")
        Me.BtnNewTemp.Name = "BtnNewTemp"
        Me.BtnNewTemp.UseVisualStyleBackColor = True
        '
        'TBStatus
        '
        resources.ApplyResources(Me.TBStatus, "TBStatus")
        Me.TBStatus.Name = "TBStatus"
        '
        'BtnDrawIPs
        '
        resources.ApplyResources(Me.BtnDrawIPs, "BtnDrawIPs")
        Me.BtnDrawIPs.Name = "BtnDrawIPs"
        Me.BtnDrawIPs.UseVisualStyleBackColor = True
        '
        'BtnDrawChart
        '
        resources.ApplyResources(Me.BtnDrawChart, "BtnDrawChart")
        Me.BtnDrawChart.Name = "BtnDrawChart"
        Me.BtnDrawChart.UseVisualStyleBackColor = True
        '
        'BtnRegressIP
        '
        resources.ApplyResources(Me.BtnRegressIP, "BtnRegressIP")
        Me.BtnRegressIP.Name = "BtnRegressIP"
        Me.BtnRegressIP.UseVisualStyleBackColor = True
        '
        'GridExpData
        '
        resources.ApplyResources(Me.GridExpData, "GridExpData")
        Me.GridExpData.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.GridExpData.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.GridExpData.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.Column1, Me.Column2, Me.Column3, Me.Column4, Me.Column5, Me.Column7, Me.Column8, Me.Column9})
        Me.GridExpData.Name = "GridExpData"
        '
        'Column1
        '
        resources.ApplyResources(Me.Column1, "Column1")
        Me.Column1.Name = "Column1"
        '
        'Column2
        '
        resources.ApplyResources(Me.Column2, "Column2")
        Me.Column2.Name = "Column2"
        '
        'Column3
        '
        resources.ApplyResources(Me.Column3, "Column3")
        Me.Column3.Name = "Column3"
        '
        'Column4
        '
        DataGridViewCellStyle1.BackColor = System.Drawing.Color.WhiteSmoke
        Me.Column4.DefaultCellStyle = DataGridViewCellStyle1
        resources.ApplyResources(Me.Column4, "Column4")
        Me.Column4.Name = "Column4"
        Me.Column4.ReadOnly = True
        '
        'Column5
        '
        DataGridViewCellStyle2.BackColor = System.Drawing.Color.WhiteSmoke
        Me.Column5.DefaultCellStyle = DataGridViewCellStyle2
        resources.ApplyResources(Me.Column5, "Column5")
        Me.Column5.Name = "Column5"
        Me.Column5.ReadOnly = True
        '
        'Column7
        '
        DataGridViewCellStyle3.BackColor = System.Drawing.Color.WhiteSmoke
        Me.Column7.DefaultCellStyle = DataGridViewCellStyle3
        resources.ApplyResources(Me.Column7, "Column7")
        Me.Column7.Name = "Column7"
        Me.Column7.ReadOnly = True
        '
        'Column8
        '
        DataGridViewCellStyle4.BackColor = System.Drawing.Color.FromArgb(CType(CType(192, Byte), Integer), CType(CType(255, Byte), Integer), CType(CType(192, Byte), Integer))
        Me.Column8.DefaultCellStyle = DataGridViewCellStyle4
        resources.ApplyResources(Me.Column8, "Column8")
        Me.Column8.Name = "Column8"
        Me.Column8.ReadOnly = True
        '
        'Column9
        '
        DataGridViewCellStyle5.BackColor = System.Drawing.Color.FromArgb(CType(CType(192, Byte), Integer), CType(CType(255, Byte), Integer), CType(CType(192, Byte), Integer))
        Me.Column9.DefaultCellStyle = DataGridViewCellStyle5
        resources.ApplyResources(Me.Column9, "Column9")
        Me.Column9.Name = "Column9"
        Me.Column9.ReadOnly = True
        '
        'TSChart
        '
        Me.TSChart.CanClose = False
        Me.TSChart.Controls.Add(Me.graph)
        Me.TSChart.IsDrawn = True
        Me.TSChart.Name = "TSChart"
        resources.ApplyResources(Me.TSChart, "TSChart")
        '
        'graph
        '
        resources.ApplyResources(Me.graph, "graph")
        Me.graph.IsShowCopyMessage = False
        Me.graph.Name = "graph"
        Me.graph.ScrollGrace = 0R
        Me.graph.ScrollMaxX = 0R
        Me.graph.ScrollMaxY = 0R
        Me.graph.ScrollMaxY2 = 0R
        Me.graph.ScrollMinX = 0R
        Me.graph.ScrollMinY = 0R
        Me.graph.ScrollMinY2 = 0R
        '
        'FormUNIFACRegression
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi
        Me.Controls.Add(Me.FaTabStrip1)
        Me.Name = "FormUNIFACRegression"
        CType(Me.FaTabStrip1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.FaTabStrip1.ResumeLayout(False)
        Me.TSInformation.ResumeLayout(False)
        Me.TSInformation.PerformLayout()
        Me.Panel3.ResumeLayout(False)
        Me.Panel3.PerformLayout()
        Me.TSModel.ResumeLayout(False)
        Me.TSModel.PerformLayout()
        Me.Panel2.ResumeLayout(False)
        Me.Panel2.PerformLayout()
        Me.Panel1.ResumeLayout(False)
        CType(Me.IPGrid, System.ComponentModel.ISupportInitialize).EndInit()
        Me.TSData.ResumeLayout(False)
        Me.TSData.PerformLayout()
        CType(Me.GridExpData, System.ComponentModel.ISupportInitialize).EndInit()
        Me.TSChart.ResumeLayout(False)
        Me.ResumeLayout(False)

    End Sub
    Friend WithEvents FaTabStrip1 As FarsiLibrary.Win.FATabStrip
    Friend WithEvents TSInformation As FarsiLibrary.Win.FATabStripItem
    Friend WithEvents TSData As FarsiLibrary.Win.FATabStripItem
    Friend WithEvents TSChart As FarsiLibrary.Win.FATabStripItem
    Friend WithEvents graph As ZedGraph.ZedGraphControl
    Friend WithEvents tbTitle As System.Windows.Forms.TextBox
    Friend WithEvents Label10 As System.Windows.Forms.Label
    Friend WithEvents tbDescription As System.Windows.Forms.TextBox
    Friend WithEvents Label11 As System.Windows.Forms.Label
    Friend WithEvents TSModel As FarsiLibrary.Win.FATabStripItem
    Friend WithEvents LblC1 As System.Windows.Forms.Label
    Friend WithEvents LblC2 As System.Windows.Forms.Label
    Friend WithEvents cbCompound1 As System.Windows.Forms.ComboBox
    Friend WithEvents cbCompound2 As System.Windows.Forms.ComboBox
    Friend WithEvents Label4 As System.Windows.Forms.Label
    Friend WithEvents cbModel As System.Windows.Forms.ComboBox
    Friend WithEvents Panel1 As System.Windows.Forms.Panel
    Friend WithEvents IPGrid As System.Windows.Forms.DataGridView
    Friend WithEvents Column6 As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents BtnShowIPGrid As System.Windows.Forms.Button
    Friend WithEvents Panel2 As System.Windows.Forms.Panel
    Friend WithEvents TBcji As System.Windows.Forms.TextBox
    Friend WithEvents TBbji As System.Windows.Forms.TextBox
    Friend WithEvents TBaji As System.Windows.Forms.TextBox
    Friend WithEvents Lblcji As System.Windows.Forms.Label
    Friend WithEvents Lblbji As System.Windows.Forms.Label
    Friend WithEvents Lblaji As System.Windows.Forms.Label
    Friend WithEvents TBcij As System.Windows.Forms.TextBox
    Friend WithEvents TBbij As System.Windows.Forms.TextBox
    Friend WithEvents TBaij As System.Windows.Forms.TextBox
    Friend WithEvents Lblcij As System.Windows.Forms.Label
    Friend WithEvents Lblbij As System.Windows.Forms.Label
    Friend WithEvents Lblaij As System.Windows.Forms.Label
    Friend WithEvents LblMGj As System.Windows.Forms.Label
    Friend WithEvents LblMGi As System.Windows.Forms.Label
    Friend WithEvents Label2 As System.Windows.Forms.Label
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents GridExpData As System.Windows.Forms.DataGridView
    Friend WithEvents BtnRegressIP As System.Windows.Forms.Button
    Friend WithEvents BtnDrawChart As System.Windows.Forms.Button
    Friend WithEvents BtnDrawIPs As System.Windows.Forms.Button
    Friend WithEvents TBStatus As System.Windows.Forms.TextBox
    Friend WithEvents Column1 As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents Column2 As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents Column3 As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents Column4 As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents Column5 As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents Column7 As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents Column8 As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents Column9 As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents tbIPDBName As System.Windows.Forms.TextBox
    Friend WithEvents Label5 As System.Windows.Forms.Label
    Friend WithEvents Panel3 As System.Windows.Forms.Panel
    Friend WithEvents BtnSelectIPDB As System.Windows.Forms.Button
    Friend WithEvents BtnSaveIPDB As System.Windows.Forms.Button
    Friend WithEvents BtnNewIPDB As System.Windows.Forms.Button
    Friend WithEvents LblTUnit As System.Windows.Forms.Label
    Friend WithEvents TbUnifacTemp As System.Windows.Forms.TextBox
    Friend WithEvents BtnNewTemp As System.Windows.Forms.Button
End Class
