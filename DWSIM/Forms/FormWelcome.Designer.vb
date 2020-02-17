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
        Me.Label2 = New System.Windows.Forms.Label()
        Me.chkAutoClose = New System.Windows.Forms.CheckBox()
        Me.Button8 = New System.Windows.Forms.Button()
        Me.Button1 = New System.Windows.Forms.Button()
        Me.Button5 = New System.Windows.Forms.Button()
        Me.Button3 = New System.Windows.Forms.Button()
        Me.Button2 = New System.Windows.Forms.Button()
        Me.TabControl1.SuspendLayout()
        Me.TabPage1.SuspendLayout()
        Me.TabPage2.SuspendLayout()
        Me.TabPage5.SuspendLayout()
        Me.TabPage3.SuspendLayout()
        Me.SuspendLayout()
        '
        'lvlatest
        '
        resources.ApplyResources(Me.lvlatest, "lvlatest")
        Me.lvlatest.Activation = System.Windows.Forms.ItemActivation.OneClick
        Me.lvlatest.AutoArrange = False
        Me.lvlatest.BackColor = System.Drawing.Color.SteelBlue
        Me.lvlatest.BorderStyle = System.Windows.Forms.BorderStyle.None
        Me.lvlatest.Columns.AddRange(New System.Windows.Forms.ColumnHeader() {Me.ColumnHeader1})
        Me.lvlatest.ForeColor = System.Drawing.Color.White
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
        Me.lvlatestfolders.BackColor = System.Drawing.Color.SteelBlue
        Me.lvlatestfolders.BorderStyle = System.Windows.Forms.BorderStyle.None
        Me.lvlatestfolders.Columns.AddRange(New System.Windows.Forms.ColumnHeader() {Me.ColumnHeader3})
        Me.lvlatestfolders.ForeColor = System.Drawing.Color.White
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
        Me.lvsamples.BackColor = System.Drawing.Color.SteelBlue
        Me.lvsamples.BorderStyle = System.Windows.Forms.BorderStyle.None
        Me.lvsamples.Columns.AddRange(New System.Windows.Forms.ColumnHeader() {Me.ColumnHeader2})
        Me.lvsamples.ForeColor = System.Drawing.Color.White
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
        Me.TabPage5.BackColor = System.Drawing.Color.SteelBlue
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
        Me.Label8.ForeColor = System.Drawing.Color.White
        Me.Label8.Name = "Label8"
        '
        'Label5
        '
        resources.ApplyResources(Me.Label5, "Label5")
        Me.Label5.BackColor = System.Drawing.Color.Transparent
        Me.Label5.ForeColor = System.Drawing.Color.White
        Me.Label5.Name = "Label5"
        '
        'Label13
        '
        resources.ApplyResources(Me.Label13, "Label13")
        Me.Label13.BackColor = System.Drawing.Color.Transparent
        Me.Label13.ForeColor = System.Drawing.Color.White
        Me.Label13.Name = "Label13"
        '
        'Button6
        '
        resources.ApplyResources(Me.Button6, "Button6")
        Me.Button6.ForeColor = System.Drawing.Color.White
        Me.Button6.Image = Global.DWSIM.My.Resources.Resources.information
        Me.Button6.Name = "Button6"
        Me.Button6.UseVisualStyleBackColor = False
        '
        'Button4
        '
        resources.ApplyResources(Me.Button4, "Button4")
        Me.Button4.ForeColor = System.Drawing.Color.White
        Me.Button4.Image = Global.DWSIM.My.Resources.Resources.arrow_up1
        Me.Button4.Name = "Button4"
        Me.Button4.UseVisualStyleBackColor = False
        '
        'FOSSEEList
        '
        resources.ApplyResources(Me.FOSSEEList, "FOSSEEList")
        Me.FOSSEEList.Activation = System.Windows.Forms.ItemActivation.OneClick
        Me.FOSSEEList.AutoArrange = False
        Me.FOSSEEList.BackColor = System.Drawing.Color.SteelBlue
        Me.FOSSEEList.Columns.AddRange(New System.Windows.Forms.ColumnHeader() {Me.ColumnHeader4})
        Me.FOSSEEList.ForeColor = System.Drawing.Color.White
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
        Me.Label1.ForeColor = System.Drawing.Color.White
        Me.Label1.Name = "Label1"
        '
        'Label2
        '
        resources.ApplyResources(Me.Label2, "Label2")
        Me.Label2.BackColor = System.Drawing.Color.Transparent
        Me.Label2.ForeColor = System.Drawing.Color.White
        Me.Label2.Name = "Label2"
        '
        'chkAutoClose
        '
        resources.ApplyResources(Me.chkAutoClose, "chkAutoClose")
        Me.chkAutoClose.ForeColor = System.Drawing.Color.White
        Me.chkAutoClose.Name = "chkAutoClose"
        Me.chkAutoClose.UseVisualStyleBackColor = True
        '
        'Button8
        '
        resources.ApplyResources(Me.Button8, "Button8")
        Me.Button8.BackColor = System.Drawing.Color.White
        Me.Button8.ForeColor = System.Drawing.Color.SteelBlue
        Me.Button8.Image = Global.DWSIM.My.Resources.Resources.become_a_patron_button
        Me.Button8.Name = "Button8"
        Me.Button8.UseVisualStyleBackColor = False
        '
        'Button1
        '
        resources.ApplyResources(Me.Button1, "Button1")
        Me.Button1.FlatAppearance.BorderColor = System.Drawing.Color.FromArgb(CType(CType(224, Byte), Integer), CType(CType(224, Byte), Integer), CType(CType(224, Byte), Integer))
        Me.Button1.FlatAppearance.MouseDownBackColor = System.Drawing.Color.Silver
        Me.Button1.FlatAppearance.MouseOverBackColor = System.Drawing.Color.FromArgb(CType(CType(224, Byte), Integer), CType(CType(224, Byte), Integer), CType(CType(224, Byte), Integer))
        Me.Button1.ForeColor = System.Drawing.Color.White
        Me.Button1.Image = Global.DWSIM.My.Resources.Resources.icons8_opened_folder2
        Me.Button1.Name = "Button1"
        Me.Button1.UseVisualStyleBackColor = False
        '
        'Button5
        '
        resources.ApplyResources(Me.Button5, "Button5")
        Me.Button5.FlatAppearance.BorderColor = System.Drawing.Color.FromArgb(CType(CType(224, Byte), Integer), CType(CType(224, Byte), Integer), CType(CType(224, Byte), Integer))
        Me.Button5.FlatAppearance.MouseDownBackColor = System.Drawing.Color.Silver
        Me.Button5.FlatAppearance.MouseOverBackColor = System.Drawing.Color.FromArgb(CType(CType(224, Byte), Integer), CType(CType(224, Byte), Integer), CType(CType(224, Byte), Integer))
        Me.Button5.ForeColor = System.Drawing.Color.White
        Me.Button5.Image = Global.DWSIM.My.Resources.Resources.icons8_combo_chart3
        Me.Button5.Name = "Button5"
        Me.Button5.UseVisualStyleBackColor = False
        '
        'Button3
        '
        resources.ApplyResources(Me.Button3, "Button3")
        Me.Button3.FlatAppearance.BorderColor = System.Drawing.Color.FromArgb(CType(CType(224, Byte), Integer), CType(CType(224, Byte), Integer), CType(CType(224, Byte), Integer))
        Me.Button3.FlatAppearance.MouseDownBackColor = System.Drawing.Color.Silver
        Me.Button3.FlatAppearance.MouseOverBackColor = System.Drawing.Color.FromArgb(CType(CType(224, Byte), Integer), CType(CType(224, Byte), Integer), CType(CType(224, Byte), Integer))
        Me.Button3.ForeColor = System.Drawing.Color.White
        Me.Button3.Image = Global.DWSIM.My.Resources.Resources.icons8_petrol1
        Me.Button3.Name = "Button3"
        Me.Button3.UseVisualStyleBackColor = False
        '
        'Button2
        '
        resources.ApplyResources(Me.Button2, "Button2")
        Me.Button2.FlatAppearance.BorderColor = System.Drawing.Color.FromArgb(CType(CType(224, Byte), Integer), CType(CType(224, Byte), Integer), CType(CType(224, Byte), Integer))
        Me.Button2.FlatAppearance.MouseDownBackColor = System.Drawing.Color.Silver
        Me.Button2.FlatAppearance.MouseOverBackColor = System.Drawing.Color.FromArgb(CType(CType(224, Byte), Integer), CType(CType(224, Byte), Integer), CType(CType(224, Byte), Integer))
        Me.Button2.ForeColor = System.Drawing.Color.White
        Me.Button2.Image = Global.DWSIM.My.Resources.Resources.icons8_file1
        Me.Button2.Name = "Button2"
        Me.Button2.UseVisualStyleBackColor = False
        '
        'FormWelcome
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.BackColor = System.Drawing.Color.SteelBlue
        Me.Controls.Add(Me.chkAutoClose)
        Me.Controls.Add(Me.Label2)
        Me.Controls.Add(Me.Label1)
        Me.Controls.Add(Me.Button8)
        Me.Controls.Add(Me.TabControl1)
        Me.Controls.Add(Me.Button2)
        Me.Controls.Add(Me.Button3)
        Me.Controls.Add(Me.Button5)
        Me.Controls.Add(Me.Button1)
        Me.Name = "FormWelcome"
        Me.TabControl1.ResumeLayout(False)
        Me.TabPage1.ResumeLayout(False)
        Me.TabPage2.ResumeLayout(False)
        Me.TabPage5.ResumeLayout(False)
        Me.TabPage5.PerformLayout()
        Me.TabPage3.ResumeLayout(False)
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Public WithEvents Button1 As System.Windows.Forms.Button
    Friend WithEvents lvlatest As System.Windows.Forms.ListView
    Public WithEvents Button5 As System.Windows.Forms.Button
    Public WithEvents Button3 As System.Windows.Forms.Button
    Public WithEvents Button2 As System.Windows.Forms.Button
    Friend WithEvents lvlatestfolders As System.Windows.Forms.ListView
    Friend WithEvents ImageList1 As System.Windows.Forms.ImageList
    Friend WithEvents ColumnHeader1 As System.Windows.Forms.ColumnHeader
    Friend WithEvents ColumnHeader3 As System.Windows.Forms.ColumnHeader
    Friend WithEvents TabControl1 As System.Windows.Forms.TabControl
    Friend WithEvents TabPage1 As System.Windows.Forms.TabPage
    Friend WithEvents TabPage2 As System.Windows.Forms.TabPage
    Friend WithEvents lvsamples As System.Windows.Forms.ListView
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
    Friend WithEvents Button8 As Button
    Friend WithEvents Label1 As Label
    Friend WithEvents Label2 As Label
    Friend WithEvents chkAutoClose As CheckBox
End Class
