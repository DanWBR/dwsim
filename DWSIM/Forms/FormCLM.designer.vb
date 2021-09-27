<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class FormCLM
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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(FormCLM))
        Me.Panel1 = New System.Windows.Forms.Panel()
        Me.SpinningProgress1 = New CircularProgress.SpinningProgress.SpinningProgress()
        Me.LblRunNumber = New System.Windows.Forms.Label()
        Me.LblSimulationName = New System.Windows.Forms.Label()
        Me.LblSimulationFile = New System.Windows.Forms.Label()
        Me.BtnAbort = New System.Windows.Forms.Button()
        Me.Label4 = New System.Windows.Forms.Label()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.GroupBox1 = New System.Windows.Forms.GroupBox()
        Me.LBLogMsg = New System.Windows.Forms.ListBox()
        Me.Panel1.SuspendLayout()
        Me.GroupBox1.SuspendLayout()
        Me.SuspendLayout()
        '
        'Panel1
        '
        resources.ApplyResources(Me.Panel1, "Panel1")
        Me.Panel1.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D
        Me.Panel1.Controls.Add(Me.SpinningProgress1)
        Me.Panel1.Controls.Add(Me.LblRunNumber)
        Me.Panel1.Controls.Add(Me.LblSimulationName)
        Me.Panel1.Controls.Add(Me.LblSimulationFile)
        Me.Panel1.Controls.Add(Me.BtnAbort)
        Me.Panel1.Controls.Add(Me.Label4)
        Me.Panel1.Controls.Add(Me.Label3)
        Me.Panel1.Controls.Add(Me.Label2)
        Me.Panel1.Controls.Add(Me.Label1)
        Me.Panel1.Controls.Add(Me.GroupBox1)
        Me.Panel1.Name = "Panel1"
        '
        'SpinningProgress1
        '
        resources.ApplyResources(Me.SpinningProgress1, "SpinningProgress1")
        Me.SpinningProgress1.ActiveSegmentColour = System.Drawing.Color.RoyalBlue
        Me.SpinningProgress1.AutoIncrementFrequency = 75.0R
        Me.SpinningProgress1.InactiveSegmentColour = System.Drawing.SystemColors.Control
        Me.SpinningProgress1.Name = "SpinningProgress1"
        Me.SpinningProgress1.TransistionSegment = 11
        Me.SpinningProgress1.TransistionSegmentColour = System.Drawing.Color.LightSteelBlue
        '
        'LblRunNumber
        '
        resources.ApplyResources(Me.LblRunNumber, "LblRunNumber")
        Me.LblRunNumber.Name = "LblRunNumber"
        '
        'LblSimulationName
        '
        resources.ApplyResources(Me.LblSimulationName, "LblSimulationName")
        Me.LblSimulationName.Name = "LblSimulationName"
        '
        'LblSimulationFile
        '
        resources.ApplyResources(Me.LblSimulationFile, "LblSimulationFile")
        Me.LblSimulationFile.Name = "LblSimulationFile"
        '
        'BtnAbort
        '
        resources.ApplyResources(Me.BtnAbort, "BtnAbort")
        Me.BtnAbort.Name = "BtnAbort"
        Me.BtnAbort.UseVisualStyleBackColor = True
        '
        'Label4
        '
        resources.ApplyResources(Me.Label4, "Label4")
        Me.Label4.Name = "Label4"
        '
        'Label3
        '
        resources.ApplyResources(Me.Label3, "Label3")
        Me.Label3.Name = "Label3"
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
        'GroupBox1
        '
        resources.ApplyResources(Me.GroupBox1, "GroupBox1")
        Me.GroupBox1.Controls.Add(Me.LBLogMsg)
        Me.GroupBox1.Name = "GroupBox1"
        Me.GroupBox1.TabStop = False
        '
        'LBLogMsg
        '
        resources.ApplyResources(Me.LBLogMsg, "LBLogMsg")
        Me.LBLogMsg.FormattingEnabled = True
        Me.LBLogMsg.Name = "LBLogMsg"
        '
        'FormCLM
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi
Me.AutoScaleDimensions = New System.Drawing.SizeF(96, 96)
        Me.Controls.Add(Me.Panel1)
        Me.DoubleBuffered = True
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog
        Me.MaximizeBox = False
        Me.Name = "FormCLM"
        Me.Panel1.ResumeLayout(False)
        Me.Panel1.PerformLayout()
        Me.GroupBox1.ResumeLayout(False)
        Me.ResumeLayout(False)

    End Sub
    Public WithEvents Panel1 As System.Windows.Forms.Panel
    Public WithEvents GroupBox1 As System.Windows.Forms.GroupBox
    Public WithEvents LBLogMsg As System.Windows.Forms.ListBox
    Public WithEvents Label3 As System.Windows.Forms.Label
    Public WithEvents Label2 As System.Windows.Forms.Label
    Public WithEvents Label1 As System.Windows.Forms.Label
    Public WithEvents Label4 As System.Windows.Forms.Label
    Public WithEvents LblRunNumber As System.Windows.Forms.Label
    Public WithEvents LblSimulationName As System.Windows.Forms.Label
    Public WithEvents LblSimulationFile As System.Windows.Forms.Label
    Public WithEvents BtnAbort As System.Windows.Forms.Button
    Public WithEvents SpinningProgress1 As CircularProgress.SpinningProgress.SpinningProgress
End Class
