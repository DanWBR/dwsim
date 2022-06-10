<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()>
Partial Class FormWelcome

    Inherits UserControl

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
        Me.components = New System.ComponentModel.Container()
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(FormWelcome))
        Me.lvlatest = New System.Windows.Forms.ListView()
        Me.ColumnHeader1 = CType(New System.Windows.Forms.ColumnHeader(), System.Windows.Forms.ColumnHeader)
        Me.ImageList1 = New System.Windows.Forms.ImageList(Me.components)
        Me.lvlatestfolders = New System.Windows.Forms.ListView()
        Me.ColumnHeader3 = CType(New System.Windows.Forms.ColumnHeader(), System.Windows.Forms.ColumnHeader)
        Me.TabControl1 = New System.Windows.Forms.TabControl()
        Me.TabPage1 = New System.Windows.Forms.TabPage()
        Me.TabPage2 = New System.Windows.Forms.TabPage()
        Me.lvsamples = New System.Windows.Forms.ListView()
        Me.ColumnHeader2 = CType(New System.Windows.Forms.ColumnHeader(), System.Windows.Forms.ColumnHeader)
        Me.TabPage5 = New System.Windows.Forms.TabPage()
        Me.Label8 = New System.Windows.Forms.Label()
        Me.Label5 = New System.Windows.Forms.Label()
        Me.Label13 = New System.Windows.Forms.Label()
        Me.Button6 = New System.Windows.Forms.Button()
        Me.Button4 = New System.Windows.Forms.Button()
        Me.FOSSEEList = New System.Windows.Forms.ListView()
        Me.ColumnHeader4 = CType(New System.Windows.Forms.ColumnHeader(), System.Windows.Forms.ColumnHeader)
        Me.TabPage3 = New System.Windows.Forms.TabPage()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.PictureBox1 = New System.Windows.Forms.PictureBox()
        Me.Panel1 = New System.Windows.Forms.Panel()
        Me.LinkLabel3 = New System.Windows.Forms.LinkLabel()
        Me.LinkLabel2 = New System.Windows.Forms.LinkLabel()
        Me.LinkLabel1 = New System.Windows.Forms.LinkLabel()
        Me.Label4 = New System.Windows.Forms.Label()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.Panel2 = New System.Windows.Forms.Panel()
        Me.LinkLabel9 = New System.Windows.Forms.LinkLabel()
        Me.LinkLabel5 = New System.Windows.Forms.LinkLabel()
        Me.PictureBox7 = New System.Windows.Forms.PictureBox()
        Me.LinkLabel6 = New System.Windows.Forms.LinkLabel()
        Me.LinkLabel11 = New System.Windows.Forms.LinkLabel()
        Me.LinkLabel4 = New System.Windows.Forms.LinkLabel()
        Me.Label7 = New System.Windows.Forms.Label()
        Me.Panel3 = New System.Windows.Forms.Panel()
        Me.LinkLabel7 = New System.Windows.Forms.LinkLabel()
        Me.Label6 = New System.Windows.Forms.Label()
        Me.Label10 = New System.Windows.Forms.Label()
        Me.PictureBox3 = New System.Windows.Forms.PictureBox()
        Me.Panel4 = New System.Windows.Forms.Panel()
        Me.Button3 = New System.Windows.Forms.Button()
        Me.Button2 = New System.Windows.Forms.Button()
        Me.Button1 = New System.Windows.Forms.Button()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.Label11 = New System.Windows.Forms.Label()
        Me.PictureBox6 = New System.Windows.Forms.PictureBox()
        Me.Label12 = New System.Windows.Forms.Label()
        Me.Panel5 = New System.Windows.Forms.Panel()
        Me.LinkLabel13 = New System.Windows.Forms.LinkLabel()
        Me.LinkLabel12 = New System.Windows.Forms.LinkLabel()
        Me.LinkLabel10 = New System.Windows.Forms.LinkLabel()
        Me.LinkLabel8 = New System.Windows.Forms.LinkLabel()
        Me.Label15 = New System.Windows.Forms.Label()
        Me.PictureBox5 = New System.Windows.Forms.PictureBox()
        Me.Panel6 = New System.Windows.Forms.Panel()
        Me.LinkLabel14 = New System.Windows.Forms.LinkLabel()
        Me.Label9 = New System.Windows.Forms.Label()
        Me.Label14 = New System.Windows.Forms.Label()
        Me.PictureBox4 = New System.Windows.Forms.PictureBox()
        Me.Panel7 = New System.Windows.Forms.Panel()
        Me.Label16 = New System.Windows.Forms.Label()
        Me.Label17 = New System.Windows.Forms.Label()
        Me.LinkLabel15 = New System.Windows.Forms.LinkLabel()
        Me.PictureBox8 = New System.Windows.Forms.PictureBox()
        Me.TabControl1.SuspendLayout()
        Me.TabPage1.SuspendLayout()
        Me.TabPage2.SuspendLayout()
        Me.TabPage5.SuspendLayout()
        Me.TabPage3.SuspendLayout()
        CType(Me.PictureBox1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.Panel1.SuspendLayout()
        Me.Panel2.SuspendLayout()
        CType(Me.PictureBox7, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.Panel3.SuspendLayout()
        CType(Me.PictureBox3, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.Panel4.SuspendLayout()
        CType(Me.PictureBox6, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.Panel5.SuspendLayout()
        CType(Me.PictureBox5, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.Panel6.SuspendLayout()
        CType(Me.PictureBox4, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.Panel7.SuspendLayout()
        CType(Me.PictureBox8, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'lvlatest
        '
        resources.ApplyResources(Me.lvlatest, "lvlatest")
        Me.lvlatest.Activation = System.Windows.Forms.ItemActivation.OneClick
        Me.lvlatest.AutoArrange = False
        Me.lvlatest.BorderStyle = System.Windows.Forms.BorderStyle.None
        Me.lvlatest.Columns.AddRange(New System.Windows.Forms.ColumnHeader() {Me.ColumnHeader1})
        Me.lvlatest.FullRowSelect = True
        Me.lvlatest.HeaderStyle = System.Windows.Forms.ColumnHeaderStyle.None
        Me.lvlatest.HideSelection = False
        Me.lvlatest.HoverSelection = True
        Me.lvlatest.LargeImageList = Me.ImageList1
        Me.lvlatest.MultiSelect = False
        Me.lvlatest.Name = "lvlatest"
        Me.lvlatest.ShowGroups = False
        Me.lvlatest.SmallImageList = Me.ImageList1
        Me.lvlatest.TileSize = New System.Drawing.Size(320, 20)
        Me.lvlatest.UseCompatibleStateImageBehavior = False
        Me.lvlatest.View = System.Windows.Forms.View.Details
        '
        'ColumnHeader1
        '
        resources.ApplyResources(Me.ColumnHeader1, "ColumnHeader1")
        '
        'ImageList1
        '
        Me.ImageList1.ImageStream = CType(resources.GetObject("ImageList1.ImageStream"), System.Windows.Forms.ImageListStreamer)
        Me.ImageList1.TransparentColor = System.Drawing.Color.Transparent
        Me.ImageList1.Images.SetKeyName(0, "DWSIM_BIN.png")
        Me.ImageList1.Images.SetKeyName(1, "DWSIM_CSD.png")
        Me.ImageList1.Images.SetKeyName(2, "DWSIM_RSD.png")
        Me.ImageList1.Images.SetKeyName(3, "DWSIM_XML.png")
        Me.ImageList1.Images.SetKeyName(4, "folder.png")
        '
        'lvlatestfolders
        '
        resources.ApplyResources(Me.lvlatestfolders, "lvlatestfolders")
        Me.lvlatestfolders.Activation = System.Windows.Forms.ItemActivation.OneClick
        Me.lvlatestfolders.AutoArrange = False
        Me.lvlatestfolders.BorderStyle = System.Windows.Forms.BorderStyle.None
        Me.lvlatestfolders.Columns.AddRange(New System.Windows.Forms.ColumnHeader() {Me.ColumnHeader3})
        Me.lvlatestfolders.FullRowSelect = True
        Me.lvlatestfolders.HeaderStyle = System.Windows.Forms.ColumnHeaderStyle.None
        Me.lvlatestfolders.HideSelection = False
        Me.lvlatestfolders.HoverSelection = True
        Me.lvlatestfolders.LargeImageList = Me.ImageList1
        Me.lvlatestfolders.MultiSelect = False
        Me.lvlatestfolders.Name = "lvlatestfolders"
        Me.lvlatestfolders.ShowGroups = False
        Me.lvlatestfolders.SmallImageList = Me.ImageList1
        Me.lvlatestfolders.TileSize = New System.Drawing.Size(320, 20)
        Me.lvlatestfolders.UseCompatibleStateImageBehavior = False
        Me.lvlatestfolders.View = System.Windows.Forms.View.Details
        '
        'ColumnHeader3
        '
        resources.ApplyResources(Me.ColumnHeader3, "ColumnHeader3")
        '
        'TabControl1
        '
        resources.ApplyResources(Me.TabControl1, "TabControl1")
        Me.TabControl1.Controls.Add(Me.TabPage1)
        Me.TabControl1.Controls.Add(Me.TabPage2)
        Me.TabControl1.Controls.Add(Me.TabPage5)
        Me.TabControl1.Controls.Add(Me.TabPage3)
        Me.TabControl1.Name = "TabControl1"
        Me.TabControl1.SelectedIndex = 0
        '
        'TabPage1
        '
        resources.ApplyResources(Me.TabPage1, "TabPage1")
        Me.TabPage1.Controls.Add(Me.lvlatest)
        Me.TabPage1.Name = "TabPage1"
        Me.TabPage1.UseVisualStyleBackColor = True
        '
        'TabPage2
        '
        resources.ApplyResources(Me.TabPage2, "TabPage2")
        Me.TabPage2.Controls.Add(Me.lvsamples)
        Me.TabPage2.Name = "TabPage2"
        Me.TabPage2.UseVisualStyleBackColor = True
        '
        'lvsamples
        '
        resources.ApplyResources(Me.lvsamples, "lvsamples")
        Me.lvsamples.Activation = System.Windows.Forms.ItemActivation.OneClick
        Me.lvsamples.AutoArrange = False
        Me.lvsamples.BorderStyle = System.Windows.Forms.BorderStyle.None
        Me.lvsamples.Columns.AddRange(New System.Windows.Forms.ColumnHeader() {Me.ColumnHeader2})
        Me.lvsamples.FullRowSelect = True
        Me.lvsamples.HeaderStyle = System.Windows.Forms.ColumnHeaderStyle.None
        Me.lvsamples.HideSelection = False
        Me.lvsamples.HoverSelection = True
        Me.lvsamples.LargeImageList = Me.ImageList1
        Me.lvsamples.MultiSelect = False
        Me.lvsamples.Name = "lvsamples"
        Me.lvsamples.ShowGroups = False
        Me.lvsamples.SmallImageList = Me.ImageList1
        Me.lvsamples.TileSize = New System.Drawing.Size(320, 20)
        Me.lvsamples.UseCompatibleStateImageBehavior = False
        Me.lvsamples.View = System.Windows.Forms.View.Details
        '
        'ColumnHeader2
        '
        resources.ApplyResources(Me.ColumnHeader2, "ColumnHeader2")
        '
        'TabPage5
        '
        resources.ApplyResources(Me.TabPage5, "TabPage5")
        Me.TabPage5.BackColor = System.Drawing.Color.White
        Me.TabPage5.Controls.Add(Me.Label8)
        Me.TabPage5.Controls.Add(Me.Label5)
        Me.TabPage5.Controls.Add(Me.Label13)
        Me.TabPage5.Controls.Add(Me.Button6)
        Me.TabPage5.Controls.Add(Me.Button4)
        Me.TabPage5.Controls.Add(Me.FOSSEEList)
        Me.TabPage5.Name = "TabPage5"
        '
        'Label8
        '
        resources.ApplyResources(Me.Label8, "Label8")
        Me.Label8.BackColor = System.Drawing.Color.Transparent
        Me.Label8.ForeColor = System.Drawing.Color.Black
        Me.Label8.Name = "Label8"
        '
        'Label5
        '
        resources.ApplyResources(Me.Label5, "Label5")
        Me.Label5.BackColor = System.Drawing.Color.Transparent
        Me.Label5.ForeColor = System.Drawing.Color.Black
        Me.Label5.Name = "Label5"
        '
        'Label13
        '
        resources.ApplyResources(Me.Label13, "Label13")
        Me.Label13.BackColor = System.Drawing.Color.Transparent
        Me.Label13.ForeColor = System.Drawing.Color.Black
        Me.Label13.Name = "Label13"
        '
        'Button6
        '
        resources.ApplyResources(Me.Button6, "Button6")
        Me.Button6.Image = Global.DWSIM.My.Resources.Resources.information
        Me.Button6.Name = "Button6"
        Me.Button6.UseVisualStyleBackColor = True
        '
        'Button4
        '
        resources.ApplyResources(Me.Button4, "Button4")
        Me.Button4.Image = Global.DWSIM.My.Resources.Resources.arrow_up1
        Me.Button4.Name = "Button4"
        Me.Button4.UseVisualStyleBackColor = True
        '
        'FOSSEEList
        '
        resources.ApplyResources(Me.FOSSEEList, "FOSSEEList")
        Me.FOSSEEList.Activation = System.Windows.Forms.ItemActivation.OneClick
        Me.FOSSEEList.AutoArrange = False
        Me.FOSSEEList.Columns.AddRange(New System.Windows.Forms.ColumnHeader() {Me.ColumnHeader4})
        Me.FOSSEEList.FullRowSelect = True
        Me.FOSSEEList.HeaderStyle = System.Windows.Forms.ColumnHeaderStyle.None
        Me.FOSSEEList.HideSelection = False
        Me.FOSSEEList.HoverSelection = True
        Me.FOSSEEList.LargeImageList = Me.ImageList1
        Me.FOSSEEList.MultiSelect = False
        Me.FOSSEEList.Name = "FOSSEEList"
        Me.FOSSEEList.ShowGroups = False
        Me.FOSSEEList.SmallImageList = Me.ImageList1
        Me.FOSSEEList.TileSize = New System.Drawing.Size(320, 20)
        Me.FOSSEEList.UseCompatibleStateImageBehavior = False
        Me.FOSSEEList.View = System.Windows.Forms.View.Details
        '
        'ColumnHeader4
        '
        resources.ApplyResources(Me.ColumnHeader4, "ColumnHeader4")
        '
        'TabPage3
        '
        resources.ApplyResources(Me.TabPage3, "TabPage3")
        Me.TabPage3.Controls.Add(Me.lvlatestfolders)
        Me.TabPage3.Name = "TabPage3"
        Me.TabPage3.UseVisualStyleBackColor = True
        '
        'Label1
        '
        resources.ApplyResources(Me.Label1, "Label1")
        Me.Label1.BackColor = System.Drawing.Color.Transparent
        Me.Label1.ForeColor = System.Drawing.Color.Black
        Me.Label1.Name = "Label1"
        '
        'PictureBox1
        '
        resources.ApplyResources(Me.PictureBox1, "PictureBox1")
        Me.PictureBox1.BackgroundImage = Global.DWSIM.My.Resources.Resources.icons8_chemical_plant
        Me.PictureBox1.Name = "PictureBox1"
        Me.PictureBox1.TabStop = False
        '
        'Panel1
        '
        resources.ApplyResources(Me.Panel1, "Panel1")
        Me.Panel1.Controls.Add(Me.LinkLabel3)
        Me.Panel1.Controls.Add(Me.LinkLabel2)
        Me.Panel1.Controls.Add(Me.LinkLabel1)
        Me.Panel1.Controls.Add(Me.Label4)
        Me.Panel1.Controls.Add(Me.Label3)
        Me.Panel1.Controls.Add(Me.PictureBox1)
        Me.Panel1.Name = "Panel1"
        '
        'LinkLabel3
        '
        resources.ApplyResources(Me.LinkLabel3, "LinkLabel3")
        Me.LinkLabel3.LinkBehavior = System.Windows.Forms.LinkBehavior.HoverUnderline
        Me.LinkLabel3.LinkColor = System.Drawing.Color.DodgerBlue
        Me.LinkLabel3.Name = "LinkLabel3"
        Me.LinkLabel3.TabStop = True
        '
        'LinkLabel2
        '
        resources.ApplyResources(Me.LinkLabel2, "LinkLabel2")
        Me.LinkLabel2.LinkBehavior = System.Windows.Forms.LinkBehavior.HoverUnderline
        Me.LinkLabel2.LinkColor = System.Drawing.Color.DodgerBlue
        Me.LinkLabel2.Name = "LinkLabel2"
        Me.LinkLabel2.TabStop = True
        '
        'LinkLabel1
        '
        resources.ApplyResources(Me.LinkLabel1, "LinkLabel1")
        Me.LinkLabel1.LinkBehavior = System.Windows.Forms.LinkBehavior.HoverUnderline
        Me.LinkLabel1.LinkColor = System.Drawing.Color.DodgerBlue
        Me.LinkLabel1.Name = "LinkLabel1"
        Me.LinkLabel1.TabStop = True
        '
        'Label4
        '
        resources.ApplyResources(Me.Label4, "Label4")
        Me.Label4.BackColor = System.Drawing.Color.Transparent
        Me.Label4.ForeColor = System.Drawing.Color.Black
        Me.Label4.Name = "Label4"
        '
        'Label3
        '
        resources.ApplyResources(Me.Label3, "Label3")
        Me.Label3.BackColor = System.Drawing.Color.Transparent
        Me.Label3.ForeColor = System.Drawing.Color.Black
        Me.Label3.Name = "Label3"
        '
        'Panel2
        '
        resources.ApplyResources(Me.Panel2, "Panel2")
        Me.Panel2.Controls.Add(Me.LinkLabel9)
        Me.Panel2.Controls.Add(Me.LinkLabel5)
        Me.Panel2.Controls.Add(Me.PictureBox7)
        Me.Panel2.Controls.Add(Me.LinkLabel6)
        Me.Panel2.Controls.Add(Me.LinkLabel11)
        Me.Panel2.Controls.Add(Me.LinkLabel4)
        Me.Panel2.Controls.Add(Me.Label7)
        Me.Panel2.Name = "Panel2"
        '
        'LinkLabel9
        '
        resources.ApplyResources(Me.LinkLabel9, "LinkLabel9")
        Me.LinkLabel9.LinkBehavior = System.Windows.Forms.LinkBehavior.HoverUnderline
        Me.LinkLabel9.LinkColor = System.Drawing.Color.DodgerBlue
        Me.LinkLabel9.Name = "LinkLabel9"
        Me.LinkLabel9.TabStop = True
        '
        'LinkLabel5
        '
        resources.ApplyResources(Me.LinkLabel5, "LinkLabel5")
        Me.LinkLabel5.LinkBehavior = System.Windows.Forms.LinkBehavior.HoverUnderline
        Me.LinkLabel5.LinkColor = System.Drawing.Color.DodgerBlue
        Me.LinkLabel5.Name = "LinkLabel5"
        Me.LinkLabel5.TabStop = True
        '
        'PictureBox7
        '
        resources.ApplyResources(Me.PictureBox7, "PictureBox7")
        Me.PictureBox7.BackgroundImage = Global.DWSIM.My.Resources.Resources.icons8_line_chart
        Me.PictureBox7.Name = "PictureBox7"
        Me.PictureBox7.TabStop = False
        '
        'LinkLabel6
        '
        resources.ApplyResources(Me.LinkLabel6, "LinkLabel6")
        Me.LinkLabel6.LinkBehavior = System.Windows.Forms.LinkBehavior.HoverUnderline
        Me.LinkLabel6.LinkColor = System.Drawing.Color.DodgerBlue
        Me.LinkLabel6.Name = "LinkLabel6"
        Me.LinkLabel6.TabStop = True
        '
        'LinkLabel11
        '
        resources.ApplyResources(Me.LinkLabel11, "LinkLabel11")
        Me.LinkLabel11.LinkBehavior = System.Windows.Forms.LinkBehavior.HoverUnderline
        Me.LinkLabel11.LinkColor = System.Drawing.Color.DodgerBlue
        Me.LinkLabel11.Name = "LinkLabel11"
        Me.LinkLabel11.TabStop = True
        '
        'LinkLabel4
        '
        resources.ApplyResources(Me.LinkLabel4, "LinkLabel4")
        Me.LinkLabel4.LinkBehavior = System.Windows.Forms.LinkBehavior.HoverUnderline
        Me.LinkLabel4.LinkColor = System.Drawing.Color.DodgerBlue
        Me.LinkLabel4.Name = "LinkLabel4"
        Me.LinkLabel4.TabStop = True
        '
        'Label7
        '
        resources.ApplyResources(Me.Label7, "Label7")
        Me.Label7.BackColor = System.Drawing.Color.Transparent
        Me.Label7.ForeColor = System.Drawing.Color.Black
        Me.Label7.Name = "Label7"
        '
        'Panel3
        '
        resources.ApplyResources(Me.Panel3, "Panel3")
        Me.Panel3.Controls.Add(Me.LinkLabel7)
        Me.Panel3.Controls.Add(Me.Label6)
        Me.Panel3.Controls.Add(Me.Label10)
        Me.Panel3.Controls.Add(Me.PictureBox3)
        Me.Panel3.Name = "Panel3"
        '
        'LinkLabel7
        '
        resources.ApplyResources(Me.LinkLabel7, "LinkLabel7")
        Me.LinkLabel7.ActiveLinkColor = System.Drawing.Color.White
        Me.LinkLabel7.BackColor = System.Drawing.Color.FromArgb(CType(CType(4, Byte), Integer), CType(CType(148, Byte), Integer), CType(CType(143, Byte), Integer))
        Me.LinkLabel7.LinkBehavior = System.Windows.Forms.LinkBehavior.HoverUnderline
        Me.LinkLabel7.LinkColor = System.Drawing.Color.White
        Me.LinkLabel7.Name = "LinkLabel7"
        Me.LinkLabel7.TabStop = True
        '
        'Label6
        '
        resources.ApplyResources(Me.Label6, "Label6")
        Me.Label6.BackColor = System.Drawing.Color.Transparent
        Me.Label6.ForeColor = System.Drawing.Color.Black
        Me.Label6.Name = "Label6"
        '
        'Label10
        '
        resources.ApplyResources(Me.Label10, "Label10")
        Me.Label10.BackColor = System.Drawing.Color.Transparent
        Me.Label10.ForeColor = System.Drawing.Color.Black
        Me.Label10.Name = "Label10"
        '
        'PictureBox3
        '
        resources.ApplyResources(Me.PictureBox3, "PictureBox3")
        Me.PictureBox3.BackgroundImage = Global.DWSIM.My.Resources.Resources.Icon512
        Me.PictureBox3.Name = "PictureBox3"
        Me.PictureBox3.TabStop = False
        '
        'Panel4
        '
        resources.ApplyResources(Me.Panel4, "Panel4")
        Me.Panel4.Controls.Add(Me.Button3)
        Me.Panel4.Controls.Add(Me.Button2)
        Me.Panel4.Controls.Add(Me.Button1)
        Me.Panel4.Controls.Add(Me.Label2)
        Me.Panel4.Controls.Add(Me.Label11)
        Me.Panel4.Controls.Add(Me.PictureBox6)
        Me.Panel4.Name = "Panel4"
        '
        'Button3
        '
        resources.ApplyResources(Me.Button3, "Button3")
        Me.Button3.BackgroundImage = Global.DWSIM.My.Resources.Resources.bmc_button
        Me.Button3.FlatAppearance.BorderSize = 0
        Me.Button3.Name = "Button3"
        Me.Button3.UseVisualStyleBackColor = True
        '
        'Button2
        '
        resources.ApplyResources(Me.Button2, "Button2")
        Me.Button2.BackgroundImage = Global.DWSIM.My.Resources.Resources.become_a_patron_button
        Me.Button2.FlatAppearance.BorderSize = 0
        Me.Button2.Name = "Button2"
        Me.Button2.UseVisualStyleBackColor = True
        '
        'Button1
        '
        resources.ApplyResources(Me.Button1, "Button1")
        Me.Button1.BackgroundImage = Global.DWSIM.My.Resources.Resources.githubsponsors1
        Me.Button1.FlatAppearance.BorderColor = System.Drawing.Color.White
        Me.Button1.FlatAppearance.BorderSize = 0
        Me.Button1.Name = "Button1"
        Me.Button1.UseVisualStyleBackColor = True
        '
        'Label2
        '
        resources.ApplyResources(Me.Label2, "Label2")
        Me.Label2.BackColor = System.Drawing.Color.Transparent
        Me.Label2.ForeColor = System.Drawing.Color.Black
        Me.Label2.Name = "Label2"
        '
        'Label11
        '
        resources.ApplyResources(Me.Label11, "Label11")
        Me.Label11.BackColor = System.Drawing.Color.Transparent
        Me.Label11.ForeColor = System.Drawing.Color.Black
        Me.Label11.Name = "Label11"
        '
        'PictureBox6
        '
        resources.ApplyResources(Me.PictureBox6, "PictureBox6")
        Me.PictureBox6.BackgroundImage = Global.DWSIM.My.Resources.Resources.icons8_heart
        Me.PictureBox6.Name = "PictureBox6"
        Me.PictureBox6.TabStop = False
        '
        'Label12
        '
        resources.ApplyResources(Me.Label12, "Label12")
        Me.Label12.BackColor = System.Drawing.Color.Transparent
        Me.Label12.ForeColor = System.Drawing.Color.Black
        Me.Label12.Name = "Label12"
        '
        'Panel5
        '
        resources.ApplyResources(Me.Panel5, "Panel5")
        Me.Panel5.Controls.Add(Me.LinkLabel13)
        Me.Panel5.Controls.Add(Me.LinkLabel12)
        Me.Panel5.Controls.Add(Me.LinkLabel10)
        Me.Panel5.Controls.Add(Me.LinkLabel8)
        Me.Panel5.Controls.Add(Me.Label15)
        Me.Panel5.Controls.Add(Me.PictureBox5)
        Me.Panel5.Name = "Panel5"
        '
        'LinkLabel13
        '
        resources.ApplyResources(Me.LinkLabel13, "LinkLabel13")
        Me.LinkLabel13.LinkBehavior = System.Windows.Forms.LinkBehavior.HoverUnderline
        Me.LinkLabel13.LinkColor = System.Drawing.Color.DodgerBlue
        Me.LinkLabel13.Name = "LinkLabel13"
        Me.LinkLabel13.TabStop = True
        '
        'LinkLabel12
        '
        resources.ApplyResources(Me.LinkLabel12, "LinkLabel12")
        Me.LinkLabel12.LinkBehavior = System.Windows.Forms.LinkBehavior.HoverUnderline
        Me.LinkLabel12.LinkColor = System.Drawing.Color.DodgerBlue
        Me.LinkLabel12.Name = "LinkLabel12"
        Me.LinkLabel12.TabStop = True
        '
        'LinkLabel10
        '
        resources.ApplyResources(Me.LinkLabel10, "LinkLabel10")
        Me.LinkLabel10.LinkBehavior = System.Windows.Forms.LinkBehavior.HoverUnderline
        Me.LinkLabel10.LinkColor = System.Drawing.Color.DodgerBlue
        Me.LinkLabel10.Name = "LinkLabel10"
        Me.LinkLabel10.TabStop = True
        '
        'LinkLabel8
        '
        resources.ApplyResources(Me.LinkLabel8, "LinkLabel8")
        Me.LinkLabel8.LinkBehavior = System.Windows.Forms.LinkBehavior.HoverUnderline
        Me.LinkLabel8.LinkColor = System.Drawing.Color.DodgerBlue
        Me.LinkLabel8.Name = "LinkLabel8"
        Me.LinkLabel8.TabStop = True
        '
        'Label15
        '
        resources.ApplyResources(Me.Label15, "Label15")
        Me.Label15.BackColor = System.Drawing.Color.Transparent
        Me.Label15.ForeColor = System.Drawing.Color.Black
        Me.Label15.Name = "Label15"
        '
        'PictureBox5
        '
        resources.ApplyResources(Me.PictureBox5, "PictureBox5")
        Me.PictureBox5.BackgroundImage = Global.DWSIM.My.Resources.Resources.icons8_books
        Me.PictureBox5.Name = "PictureBox5"
        Me.PictureBox5.TabStop = False
        '
        'Panel6
        '
        resources.ApplyResources(Me.Panel6, "Panel6")
        Me.Panel6.Controls.Add(Me.LinkLabel14)
        Me.Panel6.Controls.Add(Me.Label9)
        Me.Panel6.Controls.Add(Me.Label14)
        Me.Panel6.Controls.Add(Me.PictureBox4)
        Me.Panel6.Name = "Panel6"
        '
        'LinkLabel14
        '
        resources.ApplyResources(Me.LinkLabel14, "LinkLabel14")
        Me.LinkLabel14.ActiveLinkColor = System.Drawing.Color.White
        Me.LinkLabel14.BackColor = System.Drawing.Color.FromArgb(CType(CType(4, Byte), Integer), CType(CType(148, Byte), Integer), CType(CType(143, Byte), Integer))
        Me.LinkLabel14.LinkBehavior = System.Windows.Forms.LinkBehavior.HoverUnderline
        Me.LinkLabel14.LinkColor = System.Drawing.Color.White
        Me.LinkLabel14.Name = "LinkLabel14"
        Me.LinkLabel14.TabStop = True
        '
        'Label9
        '
        resources.ApplyResources(Me.Label9, "Label9")
        Me.Label9.BackColor = System.Drawing.Color.Transparent
        Me.Label9.ForeColor = System.Drawing.Color.Black
        Me.Label9.Name = "Label9"
        '
        'Label14
        '
        resources.ApplyResources(Me.Label14, "Label14")
        Me.Label14.BackColor = System.Drawing.Color.Transparent
        Me.Label14.ForeColor = System.Drawing.Color.Black
        Me.Label14.Name = "Label14"
        '
        'PictureBox4
        '
        resources.ApplyResources(Me.PictureBox4, "PictureBox4")
        Me.PictureBox4.BackgroundImage = Global.DWSIM.My.Resources.Resources.s365_logo
        Me.PictureBox4.Name = "PictureBox4"
        Me.PictureBox4.TabStop = False
        '
        'Panel7
        '
        resources.ApplyResources(Me.Panel7, "Panel7")
        Me.Panel7.Controls.Add(Me.Label16)
        Me.Panel7.Controls.Add(Me.Label17)
        Me.Panel7.Controls.Add(Me.LinkLabel15)
        Me.Panel7.Controls.Add(Me.PictureBox8)
        Me.Panel7.Name = "Panel7"
        '
        'Label16
        '
        resources.ApplyResources(Me.Label16, "Label16")
        Me.Label16.BackColor = System.Drawing.Color.Transparent
        Me.Label16.ForeColor = System.Drawing.Color.Black
        Me.Label16.Name = "Label16"
        '
        'Label17
        '
        resources.ApplyResources(Me.Label17, "Label17")
        Me.Label17.BackColor = System.Drawing.Color.Transparent
        Me.Label17.ForeColor = System.Drawing.Color.Black
        Me.Label17.Name = "Label17"
        '
        'LinkLabel15
        '
        resources.ApplyResources(Me.LinkLabel15, "LinkLabel15")
        Me.LinkLabel15.BackColor = System.Drawing.Color.DodgerBlue
        Me.LinkLabel15.LinkBehavior = System.Windows.Forms.LinkBehavior.HoverUnderline
        Me.LinkLabel15.LinkColor = System.Drawing.Color.White
        Me.LinkLabel15.Name = "LinkLabel15"
        Me.LinkLabel15.TabStop = True
        '
        'PictureBox8
        '
        resources.ApplyResources(Me.PictureBox8, "PictureBox8")
        Me.PictureBox8.BackgroundImage = Global.DWSIM.My.Resources.Resources.icons8_volunteering
        Me.PictureBox8.Name = "PictureBox8"
        Me.PictureBox8.TabStop = False
        '
        'FormWelcome
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi
        Me.BackColor = System.Drawing.Color.White
        Me.BackgroundImage = Global.DWSIM.My.Resources.Resources.background_welcome
        Me.Controls.Add(Me.Panel7)
        Me.Controls.Add(Me.Panel6)
        Me.Controls.Add(Me.Panel5)
        Me.Controls.Add(Me.Label12)
        Me.Controls.Add(Me.Panel4)
        Me.Controls.Add(Me.Panel3)
        Me.Controls.Add(Me.Panel2)
        Me.Controls.Add(Me.Panel1)
        Me.Controls.Add(Me.Label1)
        Me.Controls.Add(Me.TabControl1)
        Me.DoubleBuffered = True
        Me.Name = "FormWelcome"
        Me.TabControl1.ResumeLayout(False)
        Me.TabPage1.ResumeLayout(False)
        Me.TabPage2.ResumeLayout(False)
        Me.TabPage5.ResumeLayout(False)
        Me.TabPage5.PerformLayout()
        Me.TabPage3.ResumeLayout(False)
        CType(Me.PictureBox1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.Panel1.ResumeLayout(False)
        Me.Panel1.PerformLayout()
        Me.Panel2.ResumeLayout(False)
        Me.Panel2.PerformLayout()
        CType(Me.PictureBox7, System.ComponentModel.ISupportInitialize).EndInit()
        Me.Panel3.ResumeLayout(False)
        Me.Panel3.PerformLayout()
        CType(Me.PictureBox3, System.ComponentModel.ISupportInitialize).EndInit()
        Me.Panel4.ResumeLayout(False)
        Me.Panel4.PerformLayout()
        CType(Me.PictureBox6, System.ComponentModel.ISupportInitialize).EndInit()
        Me.Panel5.ResumeLayout(False)
        Me.Panel5.PerformLayout()
        CType(Me.PictureBox5, System.ComponentModel.ISupportInitialize).EndInit()
        Me.Panel6.ResumeLayout(False)
        Me.Panel6.PerformLayout()
        CType(Me.PictureBox4, System.ComponentModel.ISupportInitialize).EndInit()
        Me.Panel7.ResumeLayout(False)
        Me.Panel7.PerformLayout()
        CType(Me.PictureBox8, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Friend WithEvents lvlatestfolders As System.Windows.Forms.ListView
    Friend WithEvents ImageList1 As System.Windows.Forms.ImageList
    Friend WithEvents ColumnHeader1 As System.Windows.Forms.ColumnHeader
    Friend WithEvents ColumnHeader3 As System.Windows.Forms.ColumnHeader
    Friend WithEvents TabControl1 As System.Windows.Forms.TabControl
    Friend WithEvents TabPage1 As System.Windows.Forms.TabPage
    Friend WithEvents TabPage2 As System.Windows.Forms.TabPage
    Friend WithEvents ColumnHeader2 As System.Windows.Forms.ColumnHeader
    Friend WithEvents TabPage3 As System.Windows.Forms.TabPage
    Friend WithEvents TabPage5 As TabPage
    Friend WithEvents FOSSEEList As ListView
    Friend WithEvents ColumnHeader4 As ColumnHeader
    Public WithEvents Label8 As Label
    Friend WithEvents Button6 As Button
    Friend WithEvents Button4 As Button
    Public WithEvents Label5 As Label
    Public WithEvents Label13 As Label
    Friend WithEvents Label1 As Label
    Friend WithEvents PictureBox1 As PictureBox
    Friend WithEvents Panel1 As Panel
    Friend WithEvents LinkLabel2 As LinkLabel
    Friend WithEvents LinkLabel1 As LinkLabel
    Friend WithEvents Label4 As Label
    Friend WithEvents Label3 As Label
    Friend WithEvents Panel2 As Panel
    Friend WithEvents LinkLabel4 As LinkLabel
    Friend WithEvents Label7 As Label
    Friend WithEvents Panel3 As Panel
    Friend WithEvents LinkLabel5 As LinkLabel
    Friend WithEvents LinkLabel6 As LinkLabel
    Friend WithEvents Label10 As Label
    Friend WithEvents PictureBox3 As PictureBox
    Friend WithEvents Panel4 As Panel
    Friend WithEvents Label2 As Label
    Friend WithEvents Label11 As Label
    Friend WithEvents Label12 As Label
    Friend WithEvents Panel5 As Panel
    Friend WithEvents LinkLabel10 As LinkLabel
    Friend WithEvents LinkLabel8 As LinkLabel
    Friend WithEvents Label15 As Label
    Friend WithEvents PictureBox5 As PictureBox
    Friend WithEvents PictureBox6 As PictureBox
    Friend WithEvents LinkLabel11 As LinkLabel
    Friend WithEvents PictureBox7 As PictureBox
    Friend WithEvents LinkLabel7 As LinkLabel
    Friend WithEvents Label6 As Label
    Friend WithEvents LinkLabel12 As LinkLabel
    Friend WithEvents LinkLabel13 As LinkLabel
    Friend WithEvents Panel6 As Panel
    Friend WithEvents LinkLabel14 As LinkLabel
    Friend WithEvents Label9 As Label
    Friend WithEvents Label14 As Label
    Friend WithEvents PictureBox4 As PictureBox
    Public WithEvents lvlatest As ListView
    Public WithEvents lvsamples As ListView
    Friend WithEvents Panel7 As Panel
    Friend WithEvents Label16 As Label
    Friend WithEvents Label17 As Label
    Friend WithEvents LinkLabel15 As LinkLabel
    Friend WithEvents PictureBox8 As PictureBox
    Friend WithEvents LinkLabel3 As LinkLabel
    Friend WithEvents Button3 As Button
    Friend WithEvents Button2 As Button
    Friend WithEvents Button1 As Button
    Friend WithEvents LinkLabel9 As LinkLabel
End Class
