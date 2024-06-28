<Serializable> Public Class AttachedUtilityClass

    Inherits WeifenLuo.WinFormsUI.Docking.DockContent

    Implements Interfaces.IAttachedUtility

    Friend WithEvents PanelUtility As System.Windows.Forms.Panel
    Friend WithEvents chkAutoUpdate As System.Windows.Forms.CheckBox
    Friend WithEvents tbAttachedTo As System.Windows.Forms.TextBox
    Friend WithEvents Label2 As System.Windows.Forms.Label
    Friend WithEvents tbName As System.Windows.Forms.TextBox
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents ToolStrip1 As System.Windows.Forms.ToolStrip
    Friend WithEvents tsbDockingLeftAutoHide As System.Windows.Forms.ToolStripButton
    Friend WithEvents tsbDockingRightAutoHide As System.Windows.Forms.ToolStripButton
    Friend WithEvents tsbDockingLeft As System.Windows.Forms.ToolStripButton
    Friend WithEvents tsbDockingRight As System.Windows.Forms.ToolStripButton
    Friend WithEvents tsbDockingTop As System.Windows.Forms.ToolStripButton
    Friend WithEvents tsbDockingBottom As System.Windows.Forms.ToolStripButton
    Friend WithEvents ToolStripLabel1 As System.Windows.Forms.ToolStripLabel
    Friend WithEvents tsbDockingDocument As System.Windows.Forms.ToolStripButton
    Friend WithEvents tsbDockingFloat As System.Windows.Forms.ToolStripButton
    Friend WithEvents tsbClose As System.Windows.Forms.ToolStripButton
    Friend WithEvents PanelHeader As System.Windows.Forms.Panel

    Public Property InternalUtility As Interfaces.IAttachedUtility

    Public Property AttachedTo As Interfaces.ISimulationObject Implements Interfaces.IAttachedUtility.AttachedTo
        Get
            Return InternalUtility.AttachedTo
        End Get
        Set(value As Interfaces.ISimulationObject)
            InternalUtility.AttachedTo = value
        End Set
    End Property

    Public Property ID As Integer Implements Interfaces.IAttachedUtility.ID
        Get
            Return InternalUtility.ID
        End Get
        Set(value As Integer)
            InternalUtility.ID = value
        End Set
    End Property

    Public Property UtilityName As String Implements Interfaces.IAttachedUtility.Name
        Get
            Return InternalUtility.Name
        End Get
        Set(value As String)
            InternalUtility.Name = value
        End Set
    End Property

    Public Function GetPropertyUnits(pname As String) As String Implements Interfaces.IAttachedUtility.GetPropertyUnits
        Return InternalUtility.GetPropertyUnits(pname)
    End Function

    Public Function GetPropertyValue(pname As String) As Object Implements Interfaces.IAttachedUtility.GetPropertyValue
        Return InternalUtility.GetPropertyValue(pname)
    End Function

    Public Sub SetPropertyValue(pname As String, pvalue As Object) Implements Interfaces.IAttachedUtility.SetPropertyValue
        InternalUtility.SetPropertyValue(pname, pvalue)
    End Sub

    Public Function GetPropertyList() As List(Of String) Implements Interfaces.IAttachedUtility.GetPropertyList
        Return InternalUtility.GetPropertyList()
    End Function

    Public Sub UpdateResults() Implements Interfaces.IAttachedUtility.Update
        If AutoUpdate Then
            AttachedTo.GetFlowsheet.ShowMessage(String.Format(DWSIM.App.GetLocalString("SolvingUtility"), Me.UtilityName, Me.AttachedTo.GraphicObject.Tag), Interfaces.IFlowsheet.MessageType.Information)
            InternalUtility.Update()
        End If
    End Sub

    Private Sub AttachedUtilityClass_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        ExtensionMethods.ChangeDefaultFont(Me)

        tbName.Text = UtilityName
        tbAttachedTo.Text = AttachedTo.GraphicObject.Tag
        InternalUtility.AttachedTo = AttachedTo

        PanelUtility.Controls.Add(InternalUtility)
        DirectCast(InternalUtility, UserControl).Dock = DockStyle.Fill

    End Sub

    Sub New()

        InitializeComponent()

    End Sub

    Private Sub InitializeComponent()
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(AttachedUtilityClass))
        Me.PanelHeader = New System.Windows.Forms.Panel()
        Me.ToolStrip1 = New System.Windows.Forms.ToolStrip()
        Me.ToolStripLabel1 = New System.Windows.Forms.ToolStripLabel()
        Me.tsbDockingLeft = New System.Windows.Forms.ToolStripButton()
        Me.tsbDockingLeftAutoHide = New System.Windows.Forms.ToolStripButton()
        Me.tsbDockingRightAutoHide = New System.Windows.Forms.ToolStripButton()
        Me.tsbDockingRight = New System.Windows.Forms.ToolStripButton()
        Me.tsbDockingTop = New System.Windows.Forms.ToolStripButton()
        Me.tsbDockingBottom = New System.Windows.Forms.ToolStripButton()
        Me.tsbDockingDocument = New System.Windows.Forms.ToolStripButton()
        Me.tsbDockingFloat = New System.Windows.Forms.ToolStripButton()
        Me.tsbClose = New System.Windows.Forms.ToolStripButton()
        Me.chkAutoUpdate = New System.Windows.Forms.CheckBox()
        Me.tbAttachedTo = New System.Windows.Forms.TextBox()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.tbName = New System.Windows.Forms.TextBox()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.PanelUtility = New System.Windows.Forms.Panel()
        Me.PanelHeader.SuspendLayout()
        Me.ToolStrip1.SuspendLayout()
        Me.SuspendLayout()
        '
        'PanelHeader
        '
        resources.ApplyResources(Me.PanelHeader, "PanelHeader")
        Me.PanelHeader.BackColor = System.Drawing.Color.SteelBlue
        Me.PanelHeader.Controls.Add(Me.ToolStrip1)
        Me.PanelHeader.Controls.Add(Me.chkAutoUpdate)
        Me.PanelHeader.Controls.Add(Me.tbAttachedTo)
        Me.PanelHeader.Controls.Add(Me.Label2)
        Me.PanelHeader.Controls.Add(Me.tbName)
        Me.PanelHeader.Controls.Add(Me.Label1)
        Me.PanelHeader.Name = "PanelHeader"
        '
        'ToolStrip1
        '
        resources.ApplyResources(Me.ToolStrip1, "ToolStrip1")
        Me.ToolStrip1.BackColor = System.Drawing.Color.SteelBlue
        Me.ToolStrip1.GripStyle = System.Windows.Forms.ToolStripGripStyle.Hidden
        Me.ToolStrip1.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.ToolStripLabel1, Me.tsbDockingLeft, Me.tsbDockingLeftAutoHide, Me.tsbDockingRightAutoHide, Me.tsbDockingRight, Me.tsbDockingTop, Me.tsbDockingBottom, Me.tsbDockingDocument, Me.tsbDockingFloat, Me.tsbClose})
        Me.ToolStrip1.Name = "ToolStrip1"
        Me.ToolStrip1.RenderMode = System.Windows.Forms.ToolStripRenderMode.System
        '
        'ToolStripLabel1
        '
        resources.ApplyResources(Me.ToolStripLabel1, "ToolStripLabel1")
        Me.ToolStripLabel1.ForeColor = System.Drawing.Color.White
        Me.ToolStripLabel1.Name = "ToolStripLabel1"
        '
        'tsbDockingLeft
        '
        resources.ApplyResources(Me.tsbDockingLeft, "tsbDockingLeft")
        Me.tsbDockingLeft.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.tsbDockingLeft.Image = Global.DWSIM.My.Resources.Resources.arrow_left1
        Me.tsbDockingLeft.Name = "tsbDockingLeft"
        '
        'tsbDockingLeftAutoHide
        '
        resources.ApplyResources(Me.tsbDockingLeftAutoHide, "tsbDockingLeftAutoHide")
        Me.tsbDockingLeftAutoHide.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.tsbDockingLeftAutoHide.Image = Global.DWSIM.My.Resources.Resources.rewind
        Me.tsbDockingLeftAutoHide.Name = "tsbDockingLeftAutoHide"
        '
        'tsbDockingRightAutoHide
        '
        resources.ApplyResources(Me.tsbDockingRightAutoHide, "tsbDockingRightAutoHide")
        Me.tsbDockingRightAutoHide.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.tsbDockingRightAutoHide.Image = Global.DWSIM.My.Resources.Resources.fast_forward
        Me.tsbDockingRightAutoHide.Name = "tsbDockingRightAutoHide"
        '
        'tsbDockingRight
        '
        resources.ApplyResources(Me.tsbDockingRight, "tsbDockingRight")
        Me.tsbDockingRight.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.tsbDockingRight.Image = Global.DWSIM.My.Resources.Resources.arrow_right
        Me.tsbDockingRight.Name = "tsbDockingRight"
        '
        'tsbDockingTop
        '
        resources.ApplyResources(Me.tsbDockingTop, "tsbDockingTop")
        Me.tsbDockingTop.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.tsbDockingTop.Image = Global.DWSIM.My.Resources.Resources.arrow_up1
        Me.tsbDockingTop.Name = "tsbDockingTop"
        '
        'tsbDockingBottom
        '
        resources.ApplyResources(Me.tsbDockingBottom, "tsbDockingBottom")
        Me.tsbDockingBottom.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.tsbDockingBottom.Image = Global.DWSIM.My.Resources.Resources.arrow_down1
        Me.tsbDockingBottom.Name = "tsbDockingBottom"
        '
        'tsbDockingDocument
        '
        resources.ApplyResources(Me.tsbDockingDocument, "tsbDockingDocument")
        Me.tsbDockingDocument.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.tsbDockingDocument.Image = Global.DWSIM.My.Resources.Resources.arrows_4_way
        Me.tsbDockingDocument.Name = "tsbDockingDocument"
        '
        'tsbDockingFloat
        '
        resources.ApplyResources(Me.tsbDockingFloat, "tsbDockingFloat")
        Me.tsbDockingFloat.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.tsbDockingFloat.Image = Global.DWSIM.My.Resources.Resources._new
        Me.tsbDockingFloat.Name = "tsbDockingFloat"
        '
        'tsbClose
        '
        resources.ApplyResources(Me.tsbClose, "tsbClose")
        Me.tsbClose.Alignment = System.Windows.Forms.ToolStripItemAlignment.Right
        Me.tsbClose.ForeColor = System.Drawing.Color.White
        Me.tsbClose.Image = Global.DWSIM.My.Resources.Resources.no
        Me.tsbClose.Name = "tsbClose"
        '
        'chkAutoUpdate
        '
        resources.ApplyResources(Me.chkAutoUpdate, "chkAutoUpdate")
        Me.chkAutoUpdate.ForeColor = System.Drawing.Color.White
        Me.chkAutoUpdate.Name = "chkAutoUpdate"
        Me.chkAutoUpdate.UseVisualStyleBackColor = True
        '
        'tbAttachedTo
        '
        resources.ApplyResources(Me.tbAttachedTo, "tbAttachedTo")
        Me.tbAttachedTo.Name = "tbAttachedTo"
        Me.tbAttachedTo.ReadOnly = True
        '
        'Label2
        '
        resources.ApplyResources(Me.Label2, "Label2")
        Me.Label2.ForeColor = System.Drawing.Color.White
        Me.Label2.Name = "Label2"
        '
        'tbName
        '
        resources.ApplyResources(Me.tbName, "tbName")
        Me.tbName.Name = "tbName"
        '
        'Label1
        '
        resources.ApplyResources(Me.Label1, "Label1")
        Me.Label1.ForeColor = System.Drawing.Color.White
        Me.Label1.Name = "Label1"
        '
        'PanelUtility
        '
        resources.ApplyResources(Me.PanelUtility, "PanelUtility")
        Me.PanelUtility.Name = "PanelUtility"
        '
        'AttachedUtilityClass
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi
        Me.Controls.Add(Me.PanelUtility)
        Me.Controls.Add(Me.PanelHeader)
        Me.HideOnClose = True
        Me.Name = "AttachedUtilityClass"
        Me.ShowHint = WeifenLuo.WinFormsUI.Docking.DockState.Float
        Me.ShowIcon = False
        Me.PanelHeader.ResumeLayout(False)
        Me.PanelHeader.PerformLayout()
        Me.ToolStrip1.ResumeLayout(False)
        Me.ToolStrip1.PerformLayout()
        Me.ResumeLayout(False)

    End Sub

    Private Sub tbName_TextChanged(sender As Object, e As EventArgs) Handles tbName.TextChanged
        Me.UtilityName = tbName.Text
        Me.InternalUtility.Name = tbName.Text
        Me.Text = tbName.Text
        Me.TabText = tbName.Text
    End Sub

    Public Function GetUtilityType() As Interfaces.Enums.FlowsheetUtility Implements Interfaces.IAttachedUtility.GetUtilityType
        Return InternalUtility.GetUtilityType
    End Function

    Private Sub tsbClose_Click(sender As Object, e As EventArgs) Handles tsbClose.Click
        If MessageBox.Show(DWSIM.App.GetLocalString("ConfirmOperation"), "DWSIM", MessageBoxButtons.YesNo, MessageBoxIcon.Question) = Windows.Forms.DialogResult.Yes Then
            HideOnClose = False
            Close()
        End If
    End Sub

    Public Sub DockingHandler(sender As Object, e As EventArgs) Handles tsbDockingLeft.Click, tsbDockingBottom.Click, tsbDockingDocument.Click,
                                                                        tsbDockingFloat.Click, tsbDockingLeftAutoHide.Click, tsbDockingRight.Click,
                                                                        tsbDockingRightAutoHide.Click, tsbDockingTop.Click

        If sender Is tsbDockingLeft Then
            Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.DockLeft
        ElseIf sender Is tsbDockingLeftAutoHide Then
            Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.DockLeftAutoHide
        ElseIf sender Is tsbDockingRight Then
            Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.DockRight
        ElseIf sender Is tsbDockingRightAutoHide Then
            Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.DockRightAutoHide
        ElseIf sender Is tsbDockingTop Then
            Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.DockTop
        ElseIf sender Is tsbDockingBottom Then
            Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.DockBottom
        ElseIf sender Is tsbDockingDocument Then
            Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.Document
        ElseIf sender Is tsbDockingFloat Then
            Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.Float
        End If

    End Sub

    Public Property AutoUpdate As Boolean Implements Interfaces.IAttachedUtility.AutoUpdate
        Get
            Return InternalUtility.AutoUpdate
        End Get
        Set(value As Boolean)
            InternalUtility.AutoUpdate = value
        End Set
    End Property

    Private Sub chkAutoUpdate_CheckedChanged(sender As Object, e As EventArgs) Handles chkAutoUpdate.CheckedChanged
        AutoUpdate = chkAutoUpdate.Checked
    End Sub

    Public Sub LoadData(data As Dictionary(Of String, Object)) Implements Interfaces.IAttachedUtility.LoadData
        For Each item In data
            InternalUtility.SetPropertyValue(item.Key, item.Value)
        Next
    End Sub

    Public Function SaveData() As Dictionary(Of String, Object) Implements Interfaces.IAttachedUtility.SaveData
        Dim props As New Dictionary(Of String, Object)
        For Each prop In InternalUtility.GetPropertyList
            props.Add(prop, InternalUtility.GetPropertyValue(prop))
        Next
        Return props
    End Function

    Public Sub Initialize() Implements Interfaces.IAttachedUtility.Initialize
        InternalUtility.Initialize()
    End Sub

    Public Sub Populate() Implements Interfaces.IAttachedUtility.Populate
        InternalUtility.Populate()
    End Sub

    Private Sub AttachedUtilityClass_Shown(sender As Object, e As EventArgs) Handles Me.Shown

        FormMain.TranslateFormFunction?.Invoke(Me)

    End Sub
End Class
