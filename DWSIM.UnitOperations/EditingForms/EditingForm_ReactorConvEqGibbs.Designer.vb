﻿<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class EditingForm_ReactorConvEqGibbs

    Inherits SharedClasses.ObjectEditorForm

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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(EditingForm_ReactorConvEqGibbs))
        Dim DataGridViewCellStyle1 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle2 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle3 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle4 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle5 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle6 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle7 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle8 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle9 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Me.GroupBox5 = New System.Windows.Forms.GroupBox()
        Me.lblTag = New System.Windows.Forms.TextBox()
        Me.chkActive = New System.Windows.Forms.CheckBox()
        Me.lblConnectedTo = New System.Windows.Forms.Label()
        Me.lblStatus = New System.Windows.Forms.Label()
        Me.Label13 = New System.Windows.Forms.Label()
        Me.Label12 = New System.Windows.Forms.Label()
        Me.Label11 = New System.Windows.Forms.Label()
        Me.GroupBox4 = New System.Windows.Forms.GroupBox()
        Me.rtbAnnotations = New Extended.Windows.Forms.RichTextBoxExtended()
        Me.btnConfigurePP = New System.Windows.Forms.Button()
        Me.cbPropPack = New System.Windows.Forms.ComboBox()
        Me.Label9 = New System.Windows.Forms.Label()
        Me.GroupBox2 = New System.Windows.Forms.GroupBox()
        Me.TabControlParameters = New System.Windows.Forms.TabControl()
        Me.TabPageParams = New System.Windows.Forms.TabPage()
        Me.btnConfigExtSolver = New System.Windows.Forms.Button()
        Me.cbExternalSolver = New System.Windows.Forms.ComboBox()
        Me.LabelExternalSolver = New System.Windows.Forms.Label()
        Me.Label4 = New System.Windows.Forms.Label()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.cbPDrop = New System.Windows.Forms.ComboBox()
        Me.cbCalcMode = New System.Windows.Forms.ComboBox()
        Me.tbPDrop = New System.Windows.Forms.TextBox()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.Label5 = New System.Windows.Forms.Label()
        Me.tbOutletTemperature = New System.Windows.Forms.TextBox()
        Me.cbReacSet = New System.Windows.Forms.ComboBox()
        Me.cbTemp = New System.Windows.Forms.ComboBox()
        Me.TabPageCompounds = New System.Windows.Forms.TabPage()
        Me.ListViewCompounds = New System.Windows.Forms.ListView()
        Me.TabPageElements = New System.Windows.Forms.TabPage()
        Me.TabPageGibbsParams = New System.Windows.Forms.TabPage()
        Me.chkGibbsUsePreviousSolution = New System.Windows.Forms.CheckBox()
        Me.tbIntLoopTol = New System.Windows.Forms.TextBox()
        Me.Label18 = New System.Windows.Forms.Label()
        Me.tbIntLoopMaxIts = New System.Windows.Forms.TextBox()
        Me.Label16 = New System.Windows.Forms.Label()
        Me.TabPageEqParams = New System.Windows.Forms.TabPage()
        Me.chkUseIPOPT = New System.Windows.Forms.CheckBox()
        Me.tbExtLoopTolEq = New System.Windows.Forms.TextBox()
        Me.Label21 = New System.Windows.Forms.Label()
        Me.tbIntLoopTolEq = New System.Windows.Forms.TextBox()
        Me.Label22 = New System.Windows.Forms.Label()
        Me.tbIntLoopMaxItsEq = New System.Windows.Forms.TextBox()
        Me.Label23 = New System.Windows.Forms.Label()
        Me.tbExtLoopMaxItsEq = New System.Windows.Forms.TextBox()
        Me.Label24 = New System.Windows.Forms.Label()
        Me.chkInitializeExtents = New System.Windows.Forms.CheckBox()
        Me.GroupBox1 = New System.Windows.Forms.GroupBox()
        Me.btnCreateAndConnectEnergy2 = New System.Windows.Forms.Button()
        Me.btnDisconnectEnergy2 = New System.Windows.Forms.Button()
        Me.Label6 = New System.Windows.Forms.Label()
        Me.cbEnergy2 = New System.Windows.Forms.ComboBox()
        Me.btnCreateAndConnectEnergy1 = New System.Windows.Forms.Button()
        Me.btnCreateAndConnectOutlet2 = New System.Windows.Forms.Button()
        Me.btnCreateAndConnectOutlet1 = New System.Windows.Forms.Button()
        Me.btnCreateAndConnectInlet1 = New System.Windows.Forms.Button()
        Me.btnDisconnectOutlet2 = New System.Windows.Forms.Button()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.cbOutlet2 = New System.Windows.Forms.ComboBox()
        Me.btnDisconnectEnergy1 = New System.Windows.Forms.Button()
        Me.Label14 = New System.Windows.Forms.Label()
        Me.cbEnergy1 = New System.Windows.Forms.ComboBox()
        Me.btnDisconnectOutlet1 = New System.Windows.Forms.Button()
        Me.btnDisconnect1 = New System.Windows.Forms.Button()
        Me.Label7 = New System.Windows.Forms.Label()
        Me.cbOutlet1 = New System.Windows.Forms.ComboBox()
        Me.cbInlet1 = New System.Windows.Forms.ComboBox()
        Me.Label19 = New System.Windows.Forms.Label()
        Me.ToolTip1 = New System.Windows.Forms.ToolTip(Me.components)
        Me.GroupBox6 = New System.Windows.Forms.GroupBox()
        Me.tabstrip1 = New System.Windows.Forms.TabControl()
        Me.TabPage1 = New System.Windows.Forms.TabPage()
        Me.gridResults = New System.Windows.Forms.DataGridView()
        Me.DataGridViewTextBoxColumn1 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewTextBoxColumn2 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Column1 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.TabPage2 = New System.Windows.Forms.TabPage()
        Me.gridReactions = New System.Windows.Forms.DataGridView()
        Me.Column2 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewTextBoxColumn3 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewTextBoxColumn4 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewTextBoxColumn5 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.TabPage3 = New System.Windows.Forms.TabPage()
        Me.gridConversions = New System.Windows.Forms.DataGridView()
        Me.DataGridViewTextBoxColumn6 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewTextBoxColumn7 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.ToolTipChangeTag = New System.Windows.Forms.ToolTip(Me.components)
        Me.GroupBox5.SuspendLayout()
        Me.GroupBox4.SuspendLayout()
        Me.GroupBox2.SuspendLayout()
        Me.TabControlParameters.SuspendLayout()
        Me.TabPageParams.SuspendLayout()
        Me.TabPageCompounds.SuspendLayout()
        Me.TabPageGibbsParams.SuspendLayout()
        Me.TabPageEqParams.SuspendLayout()
        Me.GroupBox1.SuspendLayout()
        Me.GroupBox6.SuspendLayout()
        Me.tabstrip1.SuspendLayout()
        Me.TabPage1.SuspendLayout()
        CType(Me.gridResults, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.TabPage2.SuspendLayout()
        CType(Me.gridReactions, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.TabPage3.SuspendLayout()
        CType(Me.gridConversions, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'GroupBox5
        '
        resources.ApplyResources(Me.GroupBox5, "GroupBox5")
        Me.GroupBox5.Controls.Add(Me.lblTag)
        Me.GroupBox5.Controls.Add(Me.chkActive)
        Me.GroupBox5.Controls.Add(Me.lblConnectedTo)
        Me.GroupBox5.Controls.Add(Me.lblStatus)
        Me.GroupBox5.Controls.Add(Me.Label13)
        Me.GroupBox5.Controls.Add(Me.Label12)
        Me.GroupBox5.Controls.Add(Me.Label11)
        Me.GroupBox5.Name = "GroupBox5"
        Me.GroupBox5.TabStop = False
        '
        'lblTag
        '
        resources.ApplyResources(Me.lblTag, "lblTag")
        Me.lblTag.Name = "lblTag"
        '
        'chkActive
        '
        resources.ApplyResources(Me.chkActive, "chkActive")
        Me.chkActive.Image = Global.DWSIM.UnitOperations.My.Resources.Resources.bullet_tick
        Me.chkActive.Name = "chkActive"
        Me.ToolTip1.SetToolTip(Me.chkActive, resources.GetString("chkActive.ToolTip"))
        Me.chkActive.UseVisualStyleBackColor = True
        '
        'lblConnectedTo
        '
        resources.ApplyResources(Me.lblConnectedTo, "lblConnectedTo")
        Me.lblConnectedTo.Name = "lblConnectedTo"
        '
        'lblStatus
        '
        resources.ApplyResources(Me.lblStatus, "lblStatus")
        Me.lblStatus.Name = "lblStatus"
        '
        'Label13
        '
        resources.ApplyResources(Me.Label13, "Label13")
        Me.Label13.Name = "Label13"
        '
        'Label12
        '
        resources.ApplyResources(Me.Label12, "Label12")
        Me.Label12.Name = "Label12"
        '
        'Label11
        '
        resources.ApplyResources(Me.Label11, "Label11")
        Me.Label11.Name = "Label11"
        '
        'GroupBox4
        '
        resources.ApplyResources(Me.GroupBox4, "GroupBox4")
        Me.GroupBox4.Controls.Add(Me.rtbAnnotations)
        Me.GroupBox4.Name = "GroupBox4"
        Me.GroupBox4.TabStop = False
        '
        'rtbAnnotations
        '
        resources.ApplyResources(Me.rtbAnnotations, "rtbAnnotations")
        Me.rtbAnnotations.Name = "rtbAnnotations"
        Me.rtbAnnotations.Rtf = "{\rtf1\ansi\ansicpg1252\deff0\nouicompat\deflang1046{\fonttbl{\f0\fnil Microsoft " &
    "Sans Serif;}}" & Global.Microsoft.VisualBasic.ChrW(13) & Global.Microsoft.VisualBasic.ChrW(10) & "{\*\generator Riched20 10.0.18362}\viewkind4\uc1 " & Global.Microsoft.VisualBasic.ChrW(13) & Global.Microsoft.VisualBasic.ChrW(10) & "\pard\f0\fs17\" &
    "par" & Global.Microsoft.VisualBasic.ChrW(13) & Global.Microsoft.VisualBasic.ChrW(10) & "}" & Global.Microsoft.VisualBasic.ChrW(13) & Global.Microsoft.VisualBasic.ChrW(10)
        Me.rtbAnnotations.ShowRedo = False
        Me.rtbAnnotations.ShowUndo = False
        '
        'btnConfigurePP
        '
        resources.ApplyResources(Me.btnConfigurePP, "btnConfigurePP")
        Me.btnConfigurePP.BackgroundImage = Global.DWSIM.UnitOperations.My.Resources.Resources.cog
        Me.btnConfigurePP.Name = "btnConfigurePP"
        Me.ToolTip1.SetToolTip(Me.btnConfigurePP, resources.GetString("btnConfigurePP.ToolTip"))
        Me.btnConfigurePP.UseVisualStyleBackColor = True
        '
        'cbPropPack
        '
        resources.ApplyResources(Me.cbPropPack, "cbPropPack")
        Me.cbPropPack.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbPropPack.FormattingEnabled = True
        Me.cbPropPack.Name = "cbPropPack"
        '
        'Label9
        '
        resources.ApplyResources(Me.Label9, "Label9")
        Me.Label9.Name = "Label9"
        '
        'GroupBox2
        '
        resources.ApplyResources(Me.GroupBox2, "GroupBox2")
        Me.GroupBox2.Controls.Add(Me.TabControlParameters)
        Me.GroupBox2.Name = "GroupBox2"
        Me.GroupBox2.TabStop = False
        '
        'TabControlParameters
        '
        Me.TabControlParameters.Controls.Add(Me.TabPageParams)
        Me.TabControlParameters.Controls.Add(Me.TabPageCompounds)
        Me.TabControlParameters.Controls.Add(Me.TabPageElements)
        Me.TabControlParameters.Controls.Add(Me.TabPageGibbsParams)
        Me.TabControlParameters.Controls.Add(Me.TabPageEqParams)
        resources.ApplyResources(Me.TabControlParameters, "TabControlParameters")
        Me.TabControlParameters.Name = "TabControlParameters"
        Me.TabControlParameters.SelectedIndex = 0
        '
        'TabPageParams
        '
        Me.TabPageParams.Controls.Add(Me.btnConfigExtSolver)
        Me.TabPageParams.Controls.Add(Me.cbExternalSolver)
        Me.TabPageParams.Controls.Add(Me.LabelExternalSolver)
        Me.TabPageParams.Controls.Add(Me.btnConfigurePP)
        Me.TabPageParams.Controls.Add(Me.Label4)
        Me.TabPageParams.Controls.Add(Me.cbPropPack)
        Me.TabPageParams.Controls.Add(Me.Label9)
        Me.TabPageParams.Controls.Add(Me.Label2)
        Me.TabPageParams.Controls.Add(Me.cbPDrop)
        Me.TabPageParams.Controls.Add(Me.cbCalcMode)
        Me.TabPageParams.Controls.Add(Me.tbPDrop)
        Me.TabPageParams.Controls.Add(Me.Label3)
        Me.TabPageParams.Controls.Add(Me.Label5)
        Me.TabPageParams.Controls.Add(Me.tbOutletTemperature)
        Me.TabPageParams.Controls.Add(Me.cbReacSet)
        Me.TabPageParams.Controls.Add(Me.cbTemp)
        resources.ApplyResources(Me.TabPageParams, "TabPageParams")
        Me.TabPageParams.Name = "TabPageParams"
        Me.TabPageParams.UseVisualStyleBackColor = True
        '
        'btnConfigExtSolver
        '
        resources.ApplyResources(Me.btnConfigExtSolver, "btnConfigExtSolver")
        Me.btnConfigExtSolver.BackgroundImage = Global.DWSIM.UnitOperations.My.Resources.Resources.cog
        Me.btnConfigExtSolver.Name = "btnConfigExtSolver"
        Me.ToolTip1.SetToolTip(Me.btnConfigExtSolver, resources.GetString("btnConfigExtSolver.ToolTip"))
        Me.btnConfigExtSolver.UseVisualStyleBackColor = True
        '
        'cbExternalSolver
        '
        resources.ApplyResources(Me.cbExternalSolver, "cbExternalSolver")
        Me.cbExternalSolver.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbExternalSolver.FormattingEnabled = True
        Me.cbExternalSolver.Name = "cbExternalSolver"
        '
        'LabelExternalSolver
        '
        resources.ApplyResources(Me.LabelExternalSolver, "LabelExternalSolver")
        Me.LabelExternalSolver.Name = "LabelExternalSolver"
        '
        'Label4
        '
        resources.ApplyResources(Me.Label4, "Label4")
        Me.Label4.Name = "Label4"
        '
        'Label2
        '
        resources.ApplyResources(Me.Label2, "Label2")
        Me.Label2.Name = "Label2"
        '
        'cbPDrop
        '
        resources.ApplyResources(Me.cbPDrop, "cbPDrop")
        Me.cbPDrop.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbPDrop.FormattingEnabled = True
        Me.cbPDrop.Items.AddRange(New Object() {resources.GetString("cbPDrop.Items"), resources.GetString("cbPDrop.Items1"), resources.GetString("cbPDrop.Items2")})
        Me.cbPDrop.Name = "cbPDrop"
        '
        'cbCalcMode
        '
        resources.ApplyResources(Me.cbCalcMode, "cbCalcMode")
        Me.cbCalcMode.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbCalcMode.FormattingEnabled = True
        Me.cbCalcMode.Items.AddRange(New Object() {resources.GetString("cbCalcMode.Items"), resources.GetString("cbCalcMode.Items1"), resources.GetString("cbCalcMode.Items2")})
        Me.cbCalcMode.Name = "cbCalcMode"
        '
        'tbPDrop
        '
        resources.ApplyResources(Me.tbPDrop, "tbPDrop")
        Me.tbPDrop.Name = "tbPDrop"
        '
        'Label3
        '
        resources.ApplyResources(Me.Label3, "Label3")
        Me.Label3.Name = "Label3"
        '
        'Label5
        '
        resources.ApplyResources(Me.Label5, "Label5")
        Me.Label5.Name = "Label5"
        '
        'tbOutletTemperature
        '
        resources.ApplyResources(Me.tbOutletTemperature, "tbOutletTemperature")
        Me.tbOutletTemperature.Name = "tbOutletTemperature"
        '
        'cbReacSet
        '
        resources.ApplyResources(Me.cbReacSet, "cbReacSet")
        Me.cbReacSet.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbReacSet.FormattingEnabled = True
        Me.cbReacSet.Items.AddRange(New Object() {resources.GetString("cbReacSet.Items"), resources.GetString("cbReacSet.Items1"), resources.GetString("cbReacSet.Items2")})
        Me.cbReacSet.Name = "cbReacSet"
        '
        'cbTemp
        '
        resources.ApplyResources(Me.cbTemp, "cbTemp")
        Me.cbTemp.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbTemp.FormattingEnabled = True
        Me.cbTemp.Items.AddRange(New Object() {resources.GetString("cbTemp.Items"), resources.GetString("cbTemp.Items1"), resources.GetString("cbTemp.Items2")})
        Me.cbTemp.Name = "cbTemp"
        '
        'TabPageCompounds
        '
        Me.TabPageCompounds.Controls.Add(Me.ListViewCompounds)
        resources.ApplyResources(Me.TabPageCompounds, "TabPageCompounds")
        Me.TabPageCompounds.Name = "TabPageCompounds"
        Me.TabPageCompounds.UseVisualStyleBackColor = True
        '
        'ListViewCompounds
        '
        Me.ListViewCompounds.CheckBoxes = True
        resources.ApplyResources(Me.ListViewCompounds, "ListViewCompounds")
        Me.ListViewCompounds.HideSelection = False
        Me.ListViewCompounds.Name = "ListViewCompounds"
        Me.ListViewCompounds.UseCompatibleStateImageBehavior = False
        Me.ListViewCompounds.View = System.Windows.Forms.View.List
        '
        'TabPageElements
        '
        resources.ApplyResources(Me.TabPageElements, "TabPageElements")
        Me.TabPageElements.Name = "TabPageElements"
        Me.TabPageElements.UseVisualStyleBackColor = True
        '
        'TabPageGibbsParams
        '
        Me.TabPageGibbsParams.Controls.Add(Me.chkGibbsUsePreviousSolution)
        Me.TabPageGibbsParams.Controls.Add(Me.tbIntLoopTol)
        Me.TabPageGibbsParams.Controls.Add(Me.Label18)
        Me.TabPageGibbsParams.Controls.Add(Me.tbIntLoopMaxIts)
        Me.TabPageGibbsParams.Controls.Add(Me.Label16)
        resources.ApplyResources(Me.TabPageGibbsParams, "TabPageGibbsParams")
        Me.TabPageGibbsParams.Name = "TabPageGibbsParams"
        Me.TabPageGibbsParams.UseVisualStyleBackColor = True
        '
        'chkGibbsUsePreviousSolution
        '
        resources.ApplyResources(Me.chkGibbsUsePreviousSolution, "chkGibbsUsePreviousSolution")
        Me.chkGibbsUsePreviousSolution.Name = "chkGibbsUsePreviousSolution"
        Me.chkGibbsUsePreviousSolution.UseVisualStyleBackColor = True
        '
        'tbIntLoopTol
        '
        resources.ApplyResources(Me.tbIntLoopTol, "tbIntLoopTol")
        Me.tbIntLoopTol.Name = "tbIntLoopTol"
        '
        'Label18
        '
        resources.ApplyResources(Me.Label18, "Label18")
        Me.Label18.Name = "Label18"
        '
        'tbIntLoopMaxIts
        '
        resources.ApplyResources(Me.tbIntLoopMaxIts, "tbIntLoopMaxIts")
        Me.tbIntLoopMaxIts.Name = "tbIntLoopMaxIts"
        '
        'Label16
        '
        resources.ApplyResources(Me.Label16, "Label16")
        Me.Label16.Name = "Label16"
        '
        'TabPageEqParams
        '
        Me.TabPageEqParams.Controls.Add(Me.chkUseIPOPT)
        Me.TabPageEqParams.Controls.Add(Me.tbExtLoopTolEq)
        Me.TabPageEqParams.Controls.Add(Me.Label21)
        Me.TabPageEqParams.Controls.Add(Me.tbIntLoopTolEq)
        Me.TabPageEqParams.Controls.Add(Me.Label22)
        Me.TabPageEqParams.Controls.Add(Me.tbIntLoopMaxItsEq)
        Me.TabPageEqParams.Controls.Add(Me.Label23)
        Me.TabPageEqParams.Controls.Add(Me.tbExtLoopMaxItsEq)
        Me.TabPageEqParams.Controls.Add(Me.Label24)
        Me.TabPageEqParams.Controls.Add(Me.chkInitializeExtents)
        resources.ApplyResources(Me.TabPageEqParams, "TabPageEqParams")
        Me.TabPageEqParams.Name = "TabPageEqParams"
        Me.TabPageEqParams.UseVisualStyleBackColor = True
        '
        'chkUseIPOPT
        '
        resources.ApplyResources(Me.chkUseIPOPT, "chkUseIPOPT")
        Me.chkUseIPOPT.Name = "chkUseIPOPT"
        Me.chkUseIPOPT.UseVisualStyleBackColor = True
        '
        'tbExtLoopTolEq
        '
        resources.ApplyResources(Me.tbExtLoopTolEq, "tbExtLoopTolEq")
        Me.tbExtLoopTolEq.Name = "tbExtLoopTolEq"
        '
        'Label21
        '
        resources.ApplyResources(Me.Label21, "Label21")
        Me.Label21.Name = "Label21"
        '
        'tbIntLoopTolEq
        '
        resources.ApplyResources(Me.tbIntLoopTolEq, "tbIntLoopTolEq")
        Me.tbIntLoopTolEq.Name = "tbIntLoopTolEq"
        '
        'Label22
        '
        resources.ApplyResources(Me.Label22, "Label22")
        Me.Label22.Name = "Label22"
        '
        'tbIntLoopMaxItsEq
        '
        resources.ApplyResources(Me.tbIntLoopMaxItsEq, "tbIntLoopMaxItsEq")
        Me.tbIntLoopMaxItsEq.Name = "tbIntLoopMaxItsEq"
        '
        'Label23
        '
        resources.ApplyResources(Me.Label23, "Label23")
        Me.Label23.Name = "Label23"
        '
        'tbExtLoopMaxItsEq
        '
        resources.ApplyResources(Me.tbExtLoopMaxItsEq, "tbExtLoopMaxItsEq")
        Me.tbExtLoopMaxItsEq.Name = "tbExtLoopMaxItsEq"
        '
        'Label24
        '
        resources.ApplyResources(Me.Label24, "Label24")
        Me.Label24.Name = "Label24"
        '
        'chkInitializeExtents
        '
        resources.ApplyResources(Me.chkInitializeExtents, "chkInitializeExtents")
        Me.chkInitializeExtents.Name = "chkInitializeExtents"
        Me.chkInitializeExtents.UseVisualStyleBackColor = True
        '
        'GroupBox1
        '
        resources.ApplyResources(Me.GroupBox1, "GroupBox1")
        Me.GroupBox1.Controls.Add(Me.btnCreateAndConnectEnergy2)
        Me.GroupBox1.Controls.Add(Me.btnDisconnectEnergy2)
        Me.GroupBox1.Controls.Add(Me.Label6)
        Me.GroupBox1.Controls.Add(Me.cbEnergy2)
        Me.GroupBox1.Controls.Add(Me.btnCreateAndConnectEnergy1)
        Me.GroupBox1.Controls.Add(Me.btnCreateAndConnectOutlet2)
        Me.GroupBox1.Controls.Add(Me.btnCreateAndConnectOutlet1)
        Me.GroupBox1.Controls.Add(Me.btnCreateAndConnectInlet1)
        Me.GroupBox1.Controls.Add(Me.btnDisconnectOutlet2)
        Me.GroupBox1.Controls.Add(Me.Label1)
        Me.GroupBox1.Controls.Add(Me.cbOutlet2)
        Me.GroupBox1.Controls.Add(Me.btnDisconnectEnergy1)
        Me.GroupBox1.Controls.Add(Me.Label14)
        Me.GroupBox1.Controls.Add(Me.cbEnergy1)
        Me.GroupBox1.Controls.Add(Me.btnDisconnectOutlet1)
        Me.GroupBox1.Controls.Add(Me.btnDisconnect1)
        Me.GroupBox1.Controls.Add(Me.Label7)
        Me.GroupBox1.Controls.Add(Me.cbOutlet1)
        Me.GroupBox1.Controls.Add(Me.cbInlet1)
        Me.GroupBox1.Controls.Add(Me.Label19)
        Me.GroupBox1.Name = "GroupBox1"
        Me.GroupBox1.TabStop = False
        '
        'btnCreateAndConnectEnergy2
        '
        resources.ApplyResources(Me.btnCreateAndConnectEnergy2, "btnCreateAndConnectEnergy2")
        Me.btnCreateAndConnectEnergy2.BackgroundImage = Global.DWSIM.UnitOperations.My.Resources.Resources.bullet_lightning
        Me.btnCreateAndConnectEnergy2.Name = "btnCreateAndConnectEnergy2"
        Me.ToolTip1.SetToolTip(Me.btnCreateAndConnectEnergy2, resources.GetString("btnCreateAndConnectEnergy2.ToolTip"))
        Me.btnCreateAndConnectEnergy2.UseVisualStyleBackColor = True
        '
        'btnDisconnectEnergy2
        '
        resources.ApplyResources(Me.btnDisconnectEnergy2, "btnDisconnectEnergy2")
        Me.btnDisconnectEnergy2.Image = Global.DWSIM.UnitOperations.My.Resources.Resources.disconnect
        Me.btnDisconnectEnergy2.Name = "btnDisconnectEnergy2"
        Me.ToolTip1.SetToolTip(Me.btnDisconnectEnergy2, resources.GetString("btnDisconnectEnergy2.ToolTip"))
        Me.btnDisconnectEnergy2.UseVisualStyleBackColor = True
        '
        'Label6
        '
        resources.ApplyResources(Me.Label6, "Label6")
        Me.Label6.Name = "Label6"
        '
        'cbEnergy2
        '
        resources.ApplyResources(Me.cbEnergy2, "cbEnergy2")
        Me.cbEnergy2.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbEnergy2.FormattingEnabled = True
        Me.cbEnergy2.Name = "cbEnergy2"
        '
        'btnCreateAndConnectEnergy1
        '
        resources.ApplyResources(Me.btnCreateAndConnectEnergy1, "btnCreateAndConnectEnergy1")
        Me.btnCreateAndConnectEnergy1.BackgroundImage = Global.DWSIM.UnitOperations.My.Resources.Resources.bullet_lightning
        Me.btnCreateAndConnectEnergy1.Name = "btnCreateAndConnectEnergy1"
        Me.ToolTip1.SetToolTip(Me.btnCreateAndConnectEnergy1, resources.GetString("btnCreateAndConnectEnergy1.ToolTip"))
        Me.btnCreateAndConnectEnergy1.UseVisualStyleBackColor = True
        '
        'btnCreateAndConnectOutlet2
        '
        resources.ApplyResources(Me.btnCreateAndConnectOutlet2, "btnCreateAndConnectOutlet2")
        Me.btnCreateAndConnectOutlet2.BackgroundImage = Global.DWSIM.UnitOperations.My.Resources.Resources.bullet_lightning
        Me.btnCreateAndConnectOutlet2.Name = "btnCreateAndConnectOutlet2"
        Me.ToolTip1.SetToolTip(Me.btnCreateAndConnectOutlet2, resources.GetString("btnCreateAndConnectOutlet2.ToolTip"))
        Me.btnCreateAndConnectOutlet2.UseVisualStyleBackColor = True
        '
        'btnCreateAndConnectOutlet1
        '
        resources.ApplyResources(Me.btnCreateAndConnectOutlet1, "btnCreateAndConnectOutlet1")
        Me.btnCreateAndConnectOutlet1.BackgroundImage = Global.DWSIM.UnitOperations.My.Resources.Resources.bullet_lightning
        Me.btnCreateAndConnectOutlet1.Name = "btnCreateAndConnectOutlet1"
        Me.ToolTip1.SetToolTip(Me.btnCreateAndConnectOutlet1, resources.GetString("btnCreateAndConnectOutlet1.ToolTip"))
        Me.btnCreateAndConnectOutlet1.UseVisualStyleBackColor = True
        '
        'btnCreateAndConnectInlet1
        '
        resources.ApplyResources(Me.btnCreateAndConnectInlet1, "btnCreateAndConnectInlet1")
        Me.btnCreateAndConnectInlet1.BackgroundImage = Global.DWSIM.UnitOperations.My.Resources.Resources.bullet_lightning
        Me.btnCreateAndConnectInlet1.Name = "btnCreateAndConnectInlet1"
        Me.ToolTip1.SetToolTip(Me.btnCreateAndConnectInlet1, resources.GetString("btnCreateAndConnectInlet1.ToolTip"))
        Me.btnCreateAndConnectInlet1.UseVisualStyleBackColor = True
        '
        'btnDisconnectOutlet2
        '
        resources.ApplyResources(Me.btnDisconnectOutlet2, "btnDisconnectOutlet2")
        Me.btnDisconnectOutlet2.Image = Global.DWSIM.UnitOperations.My.Resources.Resources.disconnect
        Me.btnDisconnectOutlet2.Name = "btnDisconnectOutlet2"
        Me.ToolTip1.SetToolTip(Me.btnDisconnectOutlet2, resources.GetString("btnDisconnectOutlet2.ToolTip"))
        Me.btnDisconnectOutlet2.UseVisualStyleBackColor = True
        '
        'Label1
        '
        resources.ApplyResources(Me.Label1, "Label1")
        Me.Label1.Name = "Label1"
        '
        'cbOutlet2
        '
        resources.ApplyResources(Me.cbOutlet2, "cbOutlet2")
        Me.cbOutlet2.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbOutlet2.FormattingEnabled = True
        Me.cbOutlet2.Name = "cbOutlet2"
        '
        'btnDisconnectEnergy1
        '
        resources.ApplyResources(Me.btnDisconnectEnergy1, "btnDisconnectEnergy1")
        Me.btnDisconnectEnergy1.Image = Global.DWSIM.UnitOperations.My.Resources.Resources.disconnect
        Me.btnDisconnectEnergy1.Name = "btnDisconnectEnergy1"
        Me.ToolTip1.SetToolTip(Me.btnDisconnectEnergy1, resources.GetString("btnDisconnectEnergy1.ToolTip"))
        Me.btnDisconnectEnergy1.UseVisualStyleBackColor = True
        '
        'Label14
        '
        resources.ApplyResources(Me.Label14, "Label14")
        Me.Label14.Name = "Label14"
        '
        'cbEnergy1
        '
        resources.ApplyResources(Me.cbEnergy1, "cbEnergy1")
        Me.cbEnergy1.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbEnergy1.FormattingEnabled = True
        Me.cbEnergy1.Name = "cbEnergy1"
        '
        'btnDisconnectOutlet1
        '
        resources.ApplyResources(Me.btnDisconnectOutlet1, "btnDisconnectOutlet1")
        Me.btnDisconnectOutlet1.Image = Global.DWSIM.UnitOperations.My.Resources.Resources.disconnect
        Me.btnDisconnectOutlet1.Name = "btnDisconnectOutlet1"
        Me.ToolTip1.SetToolTip(Me.btnDisconnectOutlet1, resources.GetString("btnDisconnectOutlet1.ToolTip"))
        Me.btnDisconnectOutlet1.UseVisualStyleBackColor = True
        '
        'btnDisconnect1
        '
        resources.ApplyResources(Me.btnDisconnect1, "btnDisconnect1")
        Me.btnDisconnect1.Image = Global.DWSIM.UnitOperations.My.Resources.Resources.disconnect
        Me.btnDisconnect1.Name = "btnDisconnect1"
        Me.ToolTip1.SetToolTip(Me.btnDisconnect1, resources.GetString("btnDisconnect1.ToolTip"))
        Me.btnDisconnect1.UseVisualStyleBackColor = True
        '
        'Label7
        '
        resources.ApplyResources(Me.Label7, "Label7")
        Me.Label7.Name = "Label7"
        '
        'cbOutlet1
        '
        resources.ApplyResources(Me.cbOutlet1, "cbOutlet1")
        Me.cbOutlet1.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbOutlet1.FormattingEnabled = True
        Me.cbOutlet1.Name = "cbOutlet1"
        '
        'cbInlet1
        '
        resources.ApplyResources(Me.cbInlet1, "cbInlet1")
        Me.cbInlet1.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbInlet1.FormattingEnabled = True
        Me.cbInlet1.Name = "cbInlet1"
        '
        'Label19
        '
        resources.ApplyResources(Me.Label19, "Label19")
        Me.Label19.Name = "Label19"
        '
        'GroupBox6
        '
        resources.ApplyResources(Me.GroupBox6, "GroupBox6")
        Me.GroupBox6.Controls.Add(Me.tabstrip1)
        Me.GroupBox6.Name = "GroupBox6"
        Me.GroupBox6.TabStop = False
        '
        'tabstrip1
        '
        Me.tabstrip1.Controls.Add(Me.TabPage1)
        Me.tabstrip1.Controls.Add(Me.TabPage2)
        Me.tabstrip1.Controls.Add(Me.TabPage3)
        resources.ApplyResources(Me.tabstrip1, "tabstrip1")
        Me.tabstrip1.Name = "tabstrip1"
        Me.tabstrip1.SelectedIndex = 0
        '
        'TabPage1
        '
        Me.TabPage1.Controls.Add(Me.gridResults)
        resources.ApplyResources(Me.TabPage1, "TabPage1")
        Me.TabPage1.Name = "TabPage1"
        Me.TabPage1.UseVisualStyleBackColor = True
        '
        'gridResults
        '
        Me.gridResults.AllowUserToAddRows = False
        Me.gridResults.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.gridResults.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.gridResults.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.DataGridViewTextBoxColumn1, Me.DataGridViewTextBoxColumn2, Me.Column1})
        resources.ApplyResources(Me.gridResults, "gridResults")
        Me.gridResults.Name = "gridResults"
        Me.gridResults.ReadOnly = True
        Me.gridResults.RowHeadersVisible = False
        '
        'DataGridViewTextBoxColumn1
        '
        DataGridViewCellStyle1.BackColor = System.Drawing.SystemColors.Control
        Me.DataGridViewTextBoxColumn1.DefaultCellStyle = DataGridViewCellStyle1
        Me.DataGridViewTextBoxColumn1.FillWeight = 60.0!
        resources.ApplyResources(Me.DataGridViewTextBoxColumn1, "DataGridViewTextBoxColumn1")
        Me.DataGridViewTextBoxColumn1.Name = "DataGridViewTextBoxColumn1"
        Me.DataGridViewTextBoxColumn1.ReadOnly = True
        '
        'DataGridViewTextBoxColumn2
        '
        DataGridViewCellStyle2.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleRight
        Me.DataGridViewTextBoxColumn2.DefaultCellStyle = DataGridViewCellStyle2
        Me.DataGridViewTextBoxColumn2.FillWeight = 40.0!
        resources.ApplyResources(Me.DataGridViewTextBoxColumn2, "DataGridViewTextBoxColumn2")
        Me.DataGridViewTextBoxColumn2.Name = "DataGridViewTextBoxColumn2"
        Me.DataGridViewTextBoxColumn2.ReadOnly = True
        '
        'Column1
        '
        DataGridViewCellStyle3.BackColor = System.Drawing.SystemColors.Control
        Me.Column1.DefaultCellStyle = DataGridViewCellStyle3
        Me.Column1.FillWeight = 30.0!
        resources.ApplyResources(Me.Column1, "Column1")
        Me.Column1.Name = "Column1"
        Me.Column1.ReadOnly = True
        '
        'TabPage2
        '
        Me.TabPage2.Controls.Add(Me.gridReactions)
        resources.ApplyResources(Me.TabPage2, "TabPage2")
        Me.TabPage2.Name = "TabPage2"
        Me.TabPage2.UseVisualStyleBackColor = True
        '
        'gridReactions
        '
        Me.gridReactions.AllowUserToAddRows = False
        Me.gridReactions.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.gridReactions.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.gridReactions.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.Column2, Me.DataGridViewTextBoxColumn3, Me.DataGridViewTextBoxColumn4, Me.DataGridViewTextBoxColumn5})
        resources.ApplyResources(Me.gridReactions, "gridReactions")
        Me.gridReactions.Name = "gridReactions"
        Me.gridReactions.ReadOnly = True
        Me.gridReactions.RowHeadersVisible = False
        '
        'Column2
        '
        DataGridViewCellStyle4.BackColor = System.Drawing.SystemColors.Control
        Me.Column2.DefaultCellStyle = DataGridViewCellStyle4
        Me.Column2.FillWeight = 40.0!
        resources.ApplyResources(Me.Column2, "Column2")
        Me.Column2.Name = "Column2"
        Me.Column2.ReadOnly = True
        '
        'DataGridViewTextBoxColumn3
        '
        DataGridViewCellStyle5.BackColor = System.Drawing.SystemColors.Control
        Me.DataGridViewTextBoxColumn3.DefaultCellStyle = DataGridViewCellStyle5
        Me.DataGridViewTextBoxColumn3.FillWeight = 60.0!
        resources.ApplyResources(Me.DataGridViewTextBoxColumn3, "DataGridViewTextBoxColumn3")
        Me.DataGridViewTextBoxColumn3.Name = "DataGridViewTextBoxColumn3"
        Me.DataGridViewTextBoxColumn3.ReadOnly = True
        '
        'DataGridViewTextBoxColumn4
        '
        DataGridViewCellStyle6.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleRight
        Me.DataGridViewTextBoxColumn4.DefaultCellStyle = DataGridViewCellStyle6
        Me.DataGridViewTextBoxColumn4.FillWeight = 40.0!
        resources.ApplyResources(Me.DataGridViewTextBoxColumn4, "DataGridViewTextBoxColumn4")
        Me.DataGridViewTextBoxColumn4.Name = "DataGridViewTextBoxColumn4"
        Me.DataGridViewTextBoxColumn4.ReadOnly = True
        '
        'DataGridViewTextBoxColumn5
        '
        DataGridViewCellStyle7.BackColor = System.Drawing.SystemColors.Control
        Me.DataGridViewTextBoxColumn5.DefaultCellStyle = DataGridViewCellStyle7
        Me.DataGridViewTextBoxColumn5.FillWeight = 30.0!
        resources.ApplyResources(Me.DataGridViewTextBoxColumn5, "DataGridViewTextBoxColumn5")
        Me.DataGridViewTextBoxColumn5.Name = "DataGridViewTextBoxColumn5"
        Me.DataGridViewTextBoxColumn5.ReadOnly = True
        '
        'TabPage3
        '
        Me.TabPage3.Controls.Add(Me.gridConversions)
        resources.ApplyResources(Me.TabPage3, "TabPage3")
        Me.TabPage3.Name = "TabPage3"
        Me.TabPage3.UseVisualStyleBackColor = True
        '
        'gridConversions
        '
        Me.gridConversions.AllowUserToAddRows = False
        Me.gridConversions.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.gridConversions.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.gridConversions.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.DataGridViewTextBoxColumn6, Me.DataGridViewTextBoxColumn7})
        resources.ApplyResources(Me.gridConversions, "gridConversions")
        Me.gridConversions.Name = "gridConversions"
        Me.gridConversions.ReadOnly = True
        Me.gridConversions.RowHeadersVisible = False
        '
        'DataGridViewTextBoxColumn6
        '
        DataGridViewCellStyle8.BackColor = System.Drawing.SystemColors.Control
        Me.DataGridViewTextBoxColumn6.DefaultCellStyle = DataGridViewCellStyle8
        Me.DataGridViewTextBoxColumn6.FillWeight = 60.0!
        resources.ApplyResources(Me.DataGridViewTextBoxColumn6, "DataGridViewTextBoxColumn6")
        Me.DataGridViewTextBoxColumn6.Name = "DataGridViewTextBoxColumn6"
        Me.DataGridViewTextBoxColumn6.ReadOnly = True
        '
        'DataGridViewTextBoxColumn7
        '
        DataGridViewCellStyle9.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleRight
        Me.DataGridViewTextBoxColumn7.DefaultCellStyle = DataGridViewCellStyle9
        Me.DataGridViewTextBoxColumn7.FillWeight = 40.0!
        resources.ApplyResources(Me.DataGridViewTextBoxColumn7, "DataGridViewTextBoxColumn7")
        Me.DataGridViewTextBoxColumn7.Name = "DataGridViewTextBoxColumn7"
        Me.DataGridViewTextBoxColumn7.ReadOnly = True
        '
        'ToolTipChangeTag
        '
        Me.ToolTipChangeTag.ToolTipIcon = System.Windows.Forms.ToolTipIcon.Info
        Me.ToolTipChangeTag.ToolTipTitle = "Info"
        '
        'EditingForm_ReactorConvEqGibbs
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi
        Me.Controls.Add(Me.GroupBox4)
        Me.Controls.Add(Me.GroupBox6)
        Me.Controls.Add(Me.GroupBox1)
        Me.Controls.Add(Me.GroupBox5)
        Me.Controls.Add(Me.GroupBox2)
        Me.Name = "EditingForm_ReactorConvEqGibbs"
        Me.GroupBox5.ResumeLayout(False)
        Me.GroupBox5.PerformLayout()
        Me.GroupBox4.ResumeLayout(False)
        Me.GroupBox2.ResumeLayout(False)
        Me.TabControlParameters.ResumeLayout(False)
        Me.TabPageParams.ResumeLayout(False)
        Me.TabPageParams.PerformLayout()
        Me.TabPageCompounds.ResumeLayout(False)
        Me.TabPageGibbsParams.ResumeLayout(False)
        Me.TabPageGibbsParams.PerformLayout()
        Me.TabPageEqParams.ResumeLayout(False)
        Me.TabPageEqParams.PerformLayout()
        Me.GroupBox1.ResumeLayout(False)
        Me.GroupBox1.PerformLayout()
        Me.GroupBox6.ResumeLayout(False)
        Me.tabstrip1.ResumeLayout(False)
        Me.TabPage1.ResumeLayout(False)
        CType(Me.gridResults, System.ComponentModel.ISupportInitialize).EndInit()
        Me.TabPage2.ResumeLayout(False)
        CType(Me.gridReactions, System.ComponentModel.ISupportInitialize).EndInit()
        Me.TabPage3.ResumeLayout(False)
        CType(Me.gridConversions, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)

    End Sub
    Public WithEvents GroupBox5 As System.Windows.Forms.GroupBox
    Public WithEvents chkActive As System.Windows.Forms.CheckBox
    Public WithEvents lblConnectedTo As System.Windows.Forms.Label
    Public WithEvents lblStatus As System.Windows.Forms.Label
    Public WithEvents Label13 As System.Windows.Forms.Label
    Public WithEvents Label12 As System.Windows.Forms.Label
    Public WithEvents Label11 As System.Windows.Forms.Label
    Public WithEvents GroupBox4 As System.Windows.Forms.GroupBox
    Public WithEvents rtbAnnotations As Extended.Windows.Forms.RichTextBoxExtended
    Public WithEvents cbPropPack As System.Windows.Forms.ComboBox
    Public WithEvents Label9 As System.Windows.Forms.Label
    Public WithEvents GroupBox2 As System.Windows.Forms.GroupBox
    Public WithEvents btnConfigurePP As System.Windows.Forms.Button
    Public WithEvents GroupBox1 As System.Windows.Forms.GroupBox
    Public WithEvents btnDisconnectEnergy1 As System.Windows.Forms.Button
    Public WithEvents cbEnergy1 As System.Windows.Forms.ComboBox
    Public WithEvents btnDisconnectOutlet1 As System.Windows.Forms.Button
    Public WithEvents btnDisconnect1 As System.Windows.Forms.Button
    Public WithEvents Label7 As System.Windows.Forms.Label
    Public WithEvents cbOutlet1 As System.Windows.Forms.ComboBox
    Public WithEvents cbInlet1 As System.Windows.Forms.ComboBox
    Public WithEvents Label19 As System.Windows.Forms.Label
    Public WithEvents ToolTip1 As System.Windows.Forms.ToolTip
    Public WithEvents lblTag As System.Windows.Forms.TextBox
    Public WithEvents btnDisconnectOutlet2 As System.Windows.Forms.Button
    Public WithEvents Label1 As System.Windows.Forms.Label
    Public WithEvents cbOutlet2 As System.Windows.Forms.ComboBox
    Public WithEvents GroupBox6 As System.Windows.Forms.GroupBox
    Public WithEvents tabstrip1 As System.Windows.Forms.TabControl
    Public WithEvents TabPage1 As System.Windows.Forms.TabPage
    Public WithEvents TabPage2 As System.Windows.Forms.TabPage
    Public WithEvents TabPage3 As System.Windows.Forms.TabPage
    Public WithEvents gridResults As System.Windows.Forms.DataGridView
    Public WithEvents gridReactions As System.Windows.Forms.DataGridView
    Public WithEvents gridConversions As System.Windows.Forms.DataGridView
    Public WithEvents cbCalcMode As System.Windows.Forms.ComboBox
    Public WithEvents Label2 As System.Windows.Forms.Label
    Public WithEvents cbReacSet As System.Windows.Forms.ComboBox
    Public WithEvents Label4 As System.Windows.Forms.Label
    Public WithEvents cbTemp As System.Windows.Forms.ComboBox
    Public WithEvents tbOutletTemperature As System.Windows.Forms.TextBox
    Public WithEvents Label3 As System.Windows.Forms.Label
    Public WithEvents cbPDrop As System.Windows.Forms.ComboBox
    Public WithEvents tbPDrop As System.Windows.Forms.TextBox
    Public WithEvents Label5 As System.Windows.Forms.Label
    Public WithEvents btnCreateAndConnectEnergy1 As System.Windows.Forms.Button
    Public WithEvents btnCreateAndConnectOutlet2 As System.Windows.Forms.Button
    Public WithEvents btnCreateAndConnectOutlet1 As System.Windows.Forms.Button
    Public WithEvents btnCreateAndConnectInlet1 As System.Windows.Forms.Button
    Public WithEvents TabControlParameters As System.Windows.Forms.TabControl
    Public WithEvents TabPageParams As System.Windows.Forms.TabPage
    Public WithEvents TabPageCompounds As System.Windows.Forms.TabPage
    Public WithEvents TabPageElements As System.Windows.Forms.TabPage
    Public WithEvents ListViewCompounds As System.Windows.Forms.ListView
    Public WithEvents DataGridViewTextBoxColumn1 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents DataGridViewTextBoxColumn2 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents Column1 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents Column2 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents DataGridViewTextBoxColumn3 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents DataGridViewTextBoxColumn4 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents DataGridViewTextBoxColumn5 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents DataGridViewTextBoxColumn6 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents DataGridViewTextBoxColumn7 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents TabPageGibbsParams As System.Windows.Forms.TabPage
    Public WithEvents tbIntLoopTol As System.Windows.Forms.TextBox
    Public WithEvents Label18 As System.Windows.Forms.Label
    Public WithEvents tbIntLoopMaxIts As System.Windows.Forms.TextBox
    Public WithEvents Label16 As System.Windows.Forms.Label
    Public WithEvents TabPageEqParams As TabPage
    Public WithEvents tbExtLoopMaxItsEq As TextBox
    Public WithEvents Label21 As Label
    Public WithEvents tbIntLoopMaxItsEq As TextBox
    Public WithEvents Label22 As Label
    Public WithEvents tbExtLoopTolEq As TextBox
    Public WithEvents Label23 As Label
    Public WithEvents tbIntLoopTolEq As TextBox
    Public WithEvents Label24 As Label
    Public WithEvents chkInitializeExtents As CheckBox
    Friend WithEvents ToolTipChangeTag As ToolTip
    Public WithEvents chkGibbsUsePreviousSolution As CheckBox
    Public WithEvents chkUseIPOPT As CheckBox
    Public WithEvents cbExternalSolver As ComboBox
    Public WithEvents LabelExternalSolver As Label
    Public WithEvents btnConfigExtSolver As Button
    Public WithEvents btnCreateAndConnectEnergy2 As Button
    Public WithEvents btnDisconnectEnergy2 As Button
    Public WithEvents Label6 As Label
    Public WithEvents cbEnergy2 As ComboBox
    Public WithEvents Label14 As Label
End Class
