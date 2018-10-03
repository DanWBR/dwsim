<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class FormWelcome
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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(FormWelcome))
        Me.Label11 = New System.Windows.Forms.Label()
        Me.Label12 = New System.Windows.Forms.Label()
        Me.lvlatest = New System.Windows.Forms.ListView()
        Me.ColumnHeader1 = CType(New System.Windows.Forms.ColumnHeader(), System.Windows.Forms.ColumnHeader)
        Me.ImageList1 = New System.Windows.Forms.ImageList(Me.components)
        Me.Label6 = New System.Windows.Forms.Label()
        Me.Label7 = New System.Windows.Forms.Label()
        Me.Label10 = New System.Windows.Forms.Label()
        Me.Label9 = New System.Windows.Forms.Label()
        Me.Label4 = New System.Windows.Forms.Label()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.lvlatestfolders = New System.Windows.Forms.ListView()
        Me.ColumnHeader3 = CType(New System.Windows.Forms.ColumnHeader(), System.Windows.Forms.ColumnHeader)
        Me.Label14 = New System.Windows.Forms.Label()
        Me.TabControl1 = New System.Windows.Forms.TabControl()
        Me.TabPage1 = New System.Windows.Forms.TabPage()
        Me.TabPage2 = New System.Windows.Forms.TabPage()
        Me.lvsamples = New System.Windows.Forms.ListView()
        Me.ColumnHeader2 = CType(New System.Windows.Forms.ColumnHeader(), System.Windows.Forms.ColumnHeader)
        Me.TabPage5 = New System.Windows.Forms.TabPage()
        Me.FOSSEEList = New System.Windows.Forms.ListView()
        Me.ColumnHeader4 = CType(New System.Windows.Forms.ColumnHeader(), System.Windows.Forms.ColumnHeader)
        Me.TabPage3 = New System.Windows.Forms.TabPage()
        Me.TabPage4 = New System.Windows.Forms.TabPage()
        Me.LinkLabel5 = New System.Windows.Forms.LinkLabel()
        Me.LinkLabel4 = New System.Windows.Forms.LinkLabel()
        Me.LinkLabel3 = New System.Windows.Forms.LinkLabel()
        Me.LinkLabel2 = New System.Windows.Forms.LinkLabel()
        Me.LinkLabel1 = New System.Windows.Forms.LinkLabel()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.Panel1 = New System.Windows.Forms.Panel()
        Me.Button8 = New System.Windows.Forms.Button()
        Me.Button11 = New System.Windows.Forms.Button()
        Me.Button12 = New System.Windows.Forms.Button()
        Me.Button1 = New System.Windows.Forms.Button()
        Me.Button5 = New System.Windows.Forms.Button()
        Me.Button3 = New System.Windows.Forms.Button()
        Me.Button2 = New System.Windows.Forms.Button()
        Me.TabControl1.SuspendLayout()
        Me.TabPage1.SuspendLayout()
        Me.TabPage2.SuspendLayout()
        Me.TabPage5.SuspendLayout()
        Me.TabPage3.SuspendLayout()
        Me.TabPage4.SuspendLayout()
        Me.Panel1.SuspendLayout()
        Me.SuspendLayout()
        '
        'Label11
        '
        resources.ApplyResources(Me.Label11, "Label11")
        Me.Label11.BackColor = System.Drawing.Color.Transparent
        Me.Label11.ForeColor = System.Drawing.Color.SteelBlue
        Me.Label11.Name = "Label11"
        '
        'Label12
        '
        resources.ApplyResources(Me.Label12, "Label12")
        Me.Label12.BackColor = System.Drawing.Color.Transparent
        Me.Label12.ForeColor = System.Drawing.Color.SteelBlue
        Me.Label12.Name = "Label12"
        '
        'lvlatest
        '
        resources.ApplyResources(Me.lvlatest, "lvlatest")
        Me.lvlatest.Activation = System.Windows.Forms.ItemActivation.OneClick
        Me.lvlatest.AutoArrange = False
        Me.lvlatest.BackColor = System.Drawing.Color.White
        Me.lvlatest.BorderStyle = System.Windows.Forms.BorderStyle.None
        Me.lvlatest.Columns.AddRange(New System.Windows.Forms.ColumnHeader() {Me.ColumnHeader1})
        Me.lvlatest.ForeColor = System.Drawing.Color.SteelBlue
        Me.lvlatest.FullRowSelect = True
        Me.lvlatest.HeaderStyle = System.Windows.Forms.ColumnHeaderStyle.None
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
        'Label6
        '
        resources.ApplyResources(Me.Label6, "Label6")
        Me.Label6.BackColor = System.Drawing.Color.Transparent
        Me.Label6.ForeColor = System.Drawing.Color.SteelBlue
        Me.Label6.Name = "Label6"
        '
        'Label7
        '
        resources.ApplyResources(Me.Label7, "Label7")
        Me.Label7.BackColor = System.Drawing.Color.Transparent
        Me.Label7.ForeColor = System.Drawing.Color.SteelBlue
        Me.Label7.Name = "Label7"
        '
        'Label10
        '
        resources.ApplyResources(Me.Label10, "Label10")
        Me.Label10.BackColor = System.Drawing.Color.Transparent
        Me.Label10.ForeColor = System.Drawing.Color.SteelBlue
        Me.Label10.Name = "Label10"
        '
        'Label9
        '
        resources.ApplyResources(Me.Label9, "Label9")
        Me.Label9.BackColor = System.Drawing.Color.Transparent
        Me.Label9.ForeColor = System.Drawing.Color.SteelBlue
        Me.Label9.Name = "Label9"
        '
        'Label4
        '
        resources.ApplyResources(Me.Label4, "Label4")
        Me.Label4.BackColor = System.Drawing.Color.Transparent
        Me.Label4.ForeColor = System.Drawing.Color.SteelBlue
        Me.Label4.Name = "Label4"
        '
        'Label3
        '
        resources.ApplyResources(Me.Label3, "Label3")
        Me.Label3.BackColor = System.Drawing.Color.Transparent
        Me.Label3.ForeColor = System.Drawing.Color.SteelBlue
        Me.Label3.Name = "Label3"
        '
        'Label1
        '
        resources.ApplyResources(Me.Label1, "Label1")
        Me.Label1.BackColor = System.Drawing.Color.Transparent
        Me.Label1.ForeColor = System.Drawing.Color.SteelBlue
        Me.Label1.Name = "Label1"
        '
        'lvlatestfolders
        '
        resources.ApplyResources(Me.lvlatestfolders, "lvlatestfolders")
        Me.lvlatestfolders.Activation = System.Windows.Forms.ItemActivation.OneClick
        Me.lvlatestfolders.AutoArrange = False
        Me.lvlatestfolders.BackColor = System.Drawing.Color.White
        Me.lvlatestfolders.BorderStyle = System.Windows.Forms.BorderStyle.None
        Me.lvlatestfolders.Columns.AddRange(New System.Windows.Forms.ColumnHeader() {Me.ColumnHeader3})
        Me.lvlatestfolders.ForeColor = System.Drawing.Color.SteelBlue
        Me.lvlatestfolders.FullRowSelect = True
        Me.lvlatestfolders.HeaderStyle = System.Windows.Forms.ColumnHeaderStyle.None
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
        'Label14
        '
        resources.ApplyResources(Me.Label14, "Label14")
        Me.Label14.BackColor = System.Drawing.Color.Transparent
        Me.Label14.ForeColor = System.Drawing.Color.SteelBlue
        Me.Label14.Name = "Label14"
        '
        'TabControl1
        '
        resources.ApplyResources(Me.TabControl1, "TabControl1")
        Me.TabControl1.Controls.Add(Me.TabPage1)
        Me.TabControl1.Controls.Add(Me.TabPage2)
        Me.TabControl1.Controls.Add(Me.TabPage5)
        Me.TabControl1.Controls.Add(Me.TabPage3)
        Me.TabControl1.Controls.Add(Me.TabPage4)
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
        Me.lvsamples.BackColor = System.Drawing.Color.White
        Me.lvsamples.BorderStyle = System.Windows.Forms.BorderStyle.None
        Me.lvsamples.Columns.AddRange(New System.Windows.Forms.ColumnHeader() {Me.ColumnHeader2})
        Me.lvsamples.ForeColor = System.Drawing.Color.SteelBlue
        Me.lvsamples.FullRowSelect = True
        Me.lvsamples.HeaderStyle = System.Windows.Forms.ColumnHeaderStyle.None
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
        Me.TabPage5.Controls.Add(Me.FOSSEEList)
        Me.TabPage5.Name = "TabPage5"
        Me.TabPage5.UseVisualStyleBackColor = True
        '
        'FOSSEEList
        '
        resources.ApplyResources(Me.FOSSEEList, "FOSSEEList")
        Me.FOSSEEList.Activation = System.Windows.Forms.ItemActivation.OneClick
        Me.FOSSEEList.AutoArrange = False
        Me.FOSSEEList.BackColor = System.Drawing.Color.White
        Me.FOSSEEList.BorderStyle = System.Windows.Forms.BorderStyle.None
        Me.FOSSEEList.Columns.AddRange(New System.Windows.Forms.ColumnHeader() {Me.ColumnHeader4})
        Me.FOSSEEList.ForeColor = System.Drawing.Color.SteelBlue
        Me.FOSSEEList.FullRowSelect = True
        Me.FOSSEEList.HeaderStyle = System.Windows.Forms.ColumnHeaderStyle.None
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
        'TabPage4
        '
        resources.ApplyResources(Me.TabPage4, "TabPage4")
        Me.TabPage4.Controls.Add(Me.LinkLabel5)
        Me.TabPage4.Controls.Add(Me.LinkLabel4)
        Me.TabPage4.Controls.Add(Me.LinkLabel3)
        Me.TabPage4.Controls.Add(Me.LinkLabel2)
        Me.TabPage4.Controls.Add(Me.LinkLabel1)
        Me.TabPage4.Name = "TabPage4"
        Me.TabPage4.UseVisualStyleBackColor = True
        '
        'LinkLabel5
        '
        resources.ApplyResources(Me.LinkLabel5, "LinkLabel5")
        Me.LinkLabel5.ForeColor = System.Drawing.Color.SteelBlue
        Me.LinkLabel5.LinkBehavior = System.Windows.Forms.LinkBehavior.NeverUnderline
        Me.LinkLabel5.LinkColor = System.Drawing.Color.SteelBlue
        Me.LinkLabel5.Name = "LinkLabel5"
        Me.LinkLabel5.TabStop = True
        '
        'LinkLabel4
        '
        resources.ApplyResources(Me.LinkLabel4, "LinkLabel4")
        Me.LinkLabel4.ForeColor = System.Drawing.Color.SteelBlue
        Me.LinkLabel4.LinkBehavior = System.Windows.Forms.LinkBehavior.NeverUnderline
        Me.LinkLabel4.LinkColor = System.Drawing.Color.SteelBlue
        Me.LinkLabel4.Name = "LinkLabel4"
        Me.LinkLabel4.TabStop = True
        '
        'LinkLabel3
        '
        resources.ApplyResources(Me.LinkLabel3, "LinkLabel3")
        Me.LinkLabel3.ForeColor = System.Drawing.Color.SteelBlue
        Me.LinkLabel3.LinkBehavior = System.Windows.Forms.LinkBehavior.NeverUnderline
        Me.LinkLabel3.LinkColor = System.Drawing.Color.SteelBlue
        Me.LinkLabel3.Name = "LinkLabel3"
        Me.LinkLabel3.TabStop = True
        '
        'LinkLabel2
        '
        resources.ApplyResources(Me.LinkLabel2, "LinkLabel2")
        Me.LinkLabel2.ForeColor = System.Drawing.Color.SteelBlue
        Me.LinkLabel2.LinkBehavior = System.Windows.Forms.LinkBehavior.NeverUnderline
        Me.LinkLabel2.LinkColor = System.Drawing.Color.SteelBlue
        Me.LinkLabel2.Name = "LinkLabel2"
        Me.LinkLabel2.TabStop = True
        '
        'LinkLabel1
        '
        resources.ApplyResources(Me.LinkLabel1, "LinkLabel1")
        Me.LinkLabel1.ForeColor = System.Drawing.Color.SteelBlue
        Me.LinkLabel1.LinkBehavior = System.Windows.Forms.LinkBehavior.NeverUnderline
        Me.LinkLabel1.LinkColor = System.Drawing.Color.SteelBlue
        Me.LinkLabel1.Name = "LinkLabel1"
        Me.LinkLabel1.TabStop = True
        '
        'Label2
        '
        resources.ApplyResources(Me.Label2, "Label2")
        Me.Label2.BackColor = System.Drawing.Color.Transparent
        Me.Label2.ForeColor = System.Drawing.Color.SteelBlue
        Me.Label2.Name = "Label2"
        '
        'Panel1
        '
        resources.ApplyResources(Me.Panel1, "Panel1")
        Me.Panel1.BackColor = System.Drawing.Color.SteelBlue
        Me.Panel1.Controls.Add(Me.Button8)
        Me.Panel1.Name = "Panel1"
        '
        'Button8
        '
        resources.ApplyResources(Me.Button8, "Button8")
        Me.Button8.BackgroundImage = Global.DWSIM.My.Resources.Resources.PayPal_Donate_Button_PNG_HD
        Me.Button8.FlatAppearance.BorderSize = 0
        Me.Button8.ForeColor = System.Drawing.Color.Black
        Me.Button8.Name = "Button8"
        Me.Button8.UseVisualStyleBackColor = True
        '
        'Button11
        '
        resources.ApplyResources(Me.Button11, "Button11")
        Me.Button11.BackColor = System.Drawing.Color.Black
        Me.Button11.BackgroundImage = Global.DWSIM.My.Resources.Resources.En_play_badge
        Me.Button11.FlatAppearance.BorderSize = 0
        Me.Button11.ForeColor = System.Drawing.Color.Black
        Me.Button11.Name = "Button11"
        Me.Button11.UseVisualStyleBackColor = False
        '
        'Button12
        '
        resources.ApplyResources(Me.Button12, "Button12")
        Me.Button12.BackColor = System.Drawing.Color.Black
        Me.Button12.BackgroundImage = Global.DWSIM.My.Resources.Resources.DownloadFromAppStore
        Me.Button12.FlatAppearance.BorderSize = 0
        Me.Button12.ForeColor = System.Drawing.Color.Black
        Me.Button12.Name = "Button12"
        Me.Button12.UseVisualStyleBackColor = False
        '
        'Button1
        '
        resources.ApplyResources(Me.Button1, "Button1")
        Me.Button1.BackgroundImage = Global.DWSIM.My.Resources.Resources.document_open
        Me.Button1.FlatAppearance.BorderColor = System.Drawing.Color.FromArgb(CType(CType(224, Byte), Integer), CType(CType(224, Byte), Integer), CType(CType(224, Byte), Integer))
        Me.Button1.FlatAppearance.MouseDownBackColor = System.Drawing.Color.Silver
        Me.Button1.FlatAppearance.MouseOverBackColor = System.Drawing.Color.FromArgb(CType(CType(224, Byte), Integer), CType(CType(224, Byte), Integer), CType(CType(224, Byte), Integer))
        Me.Button1.Name = "Button1"
        Me.Button1.UseVisualStyleBackColor = True
        '
        'Button5
        '
        resources.ApplyResources(Me.Button5, "Button5")
        Me.Button5.BackgroundImage = Global.DWSIM.My.Resources.Resources.accessories_text_editor
        Me.Button5.FlatAppearance.BorderColor = System.Drawing.Color.FromArgb(CType(CType(224, Byte), Integer), CType(CType(224, Byte), Integer), CType(CType(224, Byte), Integer))
        Me.Button5.FlatAppearance.MouseDownBackColor = System.Drawing.Color.Silver
        Me.Button5.FlatAppearance.MouseOverBackColor = System.Drawing.Color.FromArgb(CType(CType(224, Byte), Integer), CType(CType(224, Byte), Integer), CType(CType(224, Byte), Integer))
        Me.Button5.Name = "Button5"
        Me.Button5.UseVisualStyleBackColor = True
        '
        'Button3
        '
        resources.ApplyResources(Me.Button3, "Button3")
        Me.Button3.BackgroundImage = Global.DWSIM.My.Resources.Resources.Lab_icon
        Me.Button3.FlatAppearance.BorderColor = System.Drawing.Color.FromArgb(CType(CType(224, Byte), Integer), CType(CType(224, Byte), Integer), CType(CType(224, Byte), Integer))
        Me.Button3.FlatAppearance.MouseDownBackColor = System.Drawing.Color.Silver
        Me.Button3.FlatAppearance.MouseOverBackColor = System.Drawing.Color.FromArgb(CType(CType(224, Byte), Integer), CType(CType(224, Byte), Integer), CType(CType(224, Byte), Integer))
        Me.Button3.Name = "Button3"
        Me.Button3.UseVisualStyleBackColor = True
        '
        'Button2
        '
        resources.ApplyResources(Me.Button2, "Button2")
        Me.Button2.BackgroundImage = Global.DWSIM.My.Resources.Resources.document_new
        Me.Button2.FlatAppearance.BorderColor = System.Drawing.Color.FromArgb(CType(CType(224, Byte), Integer), CType(CType(224, Byte), Integer), CType(CType(224, Byte), Integer))
        Me.Button2.FlatAppearance.MouseDownBackColor = System.Drawing.Color.Silver
        Me.Button2.FlatAppearance.MouseOverBackColor = System.Drawing.Color.FromArgb(CType(CType(224, Byte), Integer), CType(CType(224, Byte), Integer), CType(CType(224, Byte), Integer))
        Me.Button2.Name = "Button2"
        Me.Button2.UseVisualStyleBackColor = True
        '
        'FormWelcome
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.BackColor = System.Drawing.Color.White
        Me.Controls.Add(Me.Panel1)
        Me.Controls.Add(Me.Label2)
        Me.Controls.Add(Me.TabControl1)
        Me.Controls.Add(Me.Label14)
        Me.Controls.Add(Me.Button11)
        Me.Controls.Add(Me.Button12)
        Me.Controls.Add(Me.Label1)
        Me.Controls.Add(Me.Button2)
        Me.Controls.Add(Me.Button3)
        Me.Controls.Add(Me.Button5)
        Me.Controls.Add(Me.Label11)
        Me.Controls.Add(Me.Button1)
        Me.Controls.Add(Me.Label12)
        Me.Controls.Add(Me.Label3)
        Me.Controls.Add(Me.Label4)
        Me.Controls.Add(Me.Label9)
        Me.Controls.Add(Me.Label10)
        Me.Controls.Add(Me.Label6)
        Me.Controls.Add(Me.Label7)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.SizableToolWindow
        Me.MaximizeBox = False
        Me.MinimizeBox = False
        Me.Name = "FormWelcome"
        Me.ShowIcon = False
        Me.ShowInTaskbar = False
        Me.TabControl1.ResumeLayout(False)
        Me.TabPage1.ResumeLayout(False)
        Me.TabPage2.ResumeLayout(False)
        Me.TabPage5.ResumeLayout(False)
        Me.TabPage3.ResumeLayout(False)
        Me.TabPage4.ResumeLayout(False)
        Me.TabPage4.PerformLayout()
        Me.Panel1.ResumeLayout(False)
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Public WithEvents Label10 As System.Windows.Forms.Label
    Public WithEvents Label9 As System.Windows.Forms.Label
    Public WithEvents Label4 As System.Windows.Forms.Label
    Public WithEvents Label3 As System.Windows.Forms.Label
    Public WithEvents Button1 As System.Windows.Forms.Button
    Friend WithEvents lvlatest As System.Windows.Forms.ListView
    Public WithEvents Label6 As System.Windows.Forms.Label
    Public WithEvents Label7 As System.Windows.Forms.Label
    Public WithEvents Label11 As System.Windows.Forms.Label
    Public WithEvents Label12 As System.Windows.Forms.Label
    Public WithEvents Button5 As System.Windows.Forms.Button
    Public WithEvents Button3 As System.Windows.Forms.Button
    Public WithEvents Button2 As System.Windows.Forms.Button
    Public WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents lvlatestfolders As System.Windows.Forms.ListView
    Friend WithEvents ImageList1 As System.Windows.Forms.ImageList
    Public WithEvents Button11 As System.Windows.Forms.Button
    Public WithEvents Button12 As System.Windows.Forms.Button
    Friend WithEvents ColumnHeader1 As System.Windows.Forms.ColumnHeader
    Friend WithEvents ColumnHeader3 As System.Windows.Forms.ColumnHeader
    Public WithEvents Label14 As System.Windows.Forms.Label
    Friend WithEvents TabControl1 As System.Windows.Forms.TabControl
    Friend WithEvents TabPage1 As System.Windows.Forms.TabPage
    Friend WithEvents TabPage2 As System.Windows.Forms.TabPage
    Friend WithEvents lvsamples As System.Windows.Forms.ListView
    Friend WithEvents ColumnHeader2 As System.Windows.Forms.ColumnHeader
    Friend WithEvents TabPage3 As System.Windows.Forms.TabPage
    Public WithEvents Label2 As System.Windows.Forms.Label
    Friend WithEvents TabPage4 As System.Windows.Forms.TabPage
    Friend WithEvents LinkLabel5 As System.Windows.Forms.LinkLabel
    Friend WithEvents LinkLabel4 As System.Windows.Forms.LinkLabel
    Friend WithEvents LinkLabel3 As System.Windows.Forms.LinkLabel
    Friend WithEvents LinkLabel2 As System.Windows.Forms.LinkLabel
    Friend WithEvents LinkLabel1 As System.Windows.Forms.LinkLabel
    Friend WithEvents Panel1 As Panel
    Public WithEvents Button8 As Button
    Friend WithEvents TabPage5 As TabPage
    Friend WithEvents FOSSEEList As ListView
    Friend WithEvents ColumnHeader4 As ColumnHeader
End Class
