Imports System.Linq
Imports System.IO

Public Class SimulationObjectsPanel

    Inherits UserControl

    Public Flowsheet As Interfaces.IFlowsheet

    Dim arrow_down, arrow_right As Bitmap

    'Public ObjectList As New List(Of Interfaces.ISimulationObject)

    Private Sub Simulation_Objects_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        ExtensionMethods.ChangeDefaultFont(Me)

        'collapse all the panels (they're expanded in the designer)
        PanelCE.Height = 0
        PanelColumns.Height = 0
        PanelControllers.Height = 0
        PanelCustomModelsFOSSEE.Height = 0
        PanelExchangers.Height = 0
        PanelIndicators.Height = 0
        PanelLogical.Height = 0
        PanelMixers.Height = 0
        PanelOther.Height = 0
        PanelPressure.Height = 0
        PanelReactors.Height = 0
        PanelSeparators.Height = 0
        PanelSolids.Height = 0
        PanelStreams.Height = 75 * Settings.DpiScale
        PanelUser.Height = 0

        PanelCE.Tag = 150
        PanelColumns.Tag = 225
        PanelControllers.Tag = 75
        PanelCustomModelsFOSSEE.Tag = 225
        PanelExchangers.Tag = 150
        PanelIndicators.Tag = 75
        PanelLogical.Tag = 225
        PanelMixers.Tag = 150
        PanelOther.Tag = 75
        PanelPressure.Tag = 225
        PanelReactors.Tag = 150
        PanelSeparators.Tag = 75
        PanelSolids.Tag = 75
        PanelStreams.Tag = 75
        PanelUser.Tag = 150

        CheckBox1.Image = My.Resources.arrow_down

        'Associate a Panel with each CheckBox
        CheckBox1.Tag = PanelStreams
        CheckBox2.Tag = PanelPressure
        CheckBox3.Tag = PanelSeparators
        CheckBox4.Tag = PanelMixers
        CheckBox5.Tag = PanelExchangers
        CheckBox6.Tag = PanelReactors
        CheckBox7.Tag = PanelColumns
        CheckBox8.Tag = PanelSolids
        CheckBox9.Tag = PanelCE
        CheckBox10.Tag = PanelUser
        CheckBox11.Tag = PanelCustomModelsFOSSEE
        CheckBox12.Tag = PanelLogical
        CheckBox13.Tag = PanelIndicators
        CheckBox14.Tag = PanelControllers
        CheckBox15.Tag = PanelOther

        If Settings.DpiScale > 1.0 Then
            arrow_right = New Bitmap(My.Resources.arrow_32px, New Size(Settings.DpiScale * 10, Settings.DpiScale * 10))
            arrow_down = New Bitmap(My.Resources.thick_arrow_pointing_down_32px, New Size(Settings.DpiScale * 10, Settings.DpiScale * 10))
        Else
            arrow_right = My.Resources.arrow_right
            arrow_down = My.Resources.arrow_down
        End If

        CheckBox1.Image = arrow_down
        CheckBox2.Image = arrow_right
        CheckBox3.Image = arrow_right
        CheckBox4.Image = arrow_right
        CheckBox5.Image = arrow_right
        CheckBox6.Image = arrow_right
        CheckBox7.Image = arrow_right
        CheckBox8.Image = arrow_right
        CheckBox9.Image = arrow_right
        CheckBox10.Image = arrow_right
        CheckBox11.Image = arrow_right
        CheckBox12.Image = arrow_right
        CheckBox13.Image = arrow_right
        CheckBox14.Image = arrow_right
        CheckBox15.Image = arrow_right

        Dim add As Boolean = True

        Dim litems As New List(Of ListItem)

        'add chemsep model

        Dim csmodel = My.Application.MainWindowForm.aTypeList.Where(Function(x) x.FullName.ToLower.Contains("distillationcolumn")).SingleOrDefault
        Dim comodel = My.Application.MainWindowForm.aTypeList.Where(Function(x) x.FullName.ToLower.Contains("capeopenuo")).SingleOrDefault
        Dim csobj = DirectCast(Activator.CreateInstance(csmodel), Interfaces.ISimulationObject)
        Dim coobj = DirectCast(Activator.CreateInstance(comodel), Interfaces.ISimulationObject)
        If Not Flowsheet.MobileCompatibilityMode Then
            Dim li As New ListItem
            li.lblName.Text = "ChemSep Column"
            li.lblName.Font = New Font(SystemFonts.MessageBoxFont.FontFamily, 7.0, System.Drawing.FontStyle.Bold)
            li.ToolTip1.SetToolTip(li.lblName, "ChemSep Rigorous Separation Column (CAPE-OPEN)")
            li.Image.Image = csobj.GetIconBitmap
            li.ToolTip1.SetToolTip(li.Image, "ChemSep Rigorous Separation Column (CAPE-OPEN)")
            li.ObjectTypeInfo = coobj.GetType
            li.Tag = csobj.ObjectClass
            Me.PanelColumns.Controls.Add(li)
            csobj = Nothing
            coobj = Nothing
        End If

        'add other models

        For Each item In My.Application.MainWindowForm.aTypeList
            If Not item.IsAbstract Then
                Dim obj = DirectCast(Activator.CreateInstance(item), Interfaces.ISimulationObject)
                If Not Flowsheet.MobileCompatibilityMode Then
                    add = obj.GetType.GetProperty("Visible").GetValue(obj)
                Else
                    add = obj.MobileCompatible
                End If
                If add Then
                    obj.SetFlowsheet(Flowsheet)
                    Dim li As New ListItem
                    li.lblName.Text = obj.GetDisplayName
                    li.lblName.Font = New Font(SystemFonts.MessageBoxFont.FontFamily, 7.0, System.Drawing.FontStyle.Bold)
                    li.ToolTip1.SetToolTip(li.lblName, obj.GetDisplayDescription)
                    li.Image.Image = obj.GetIconBitmap
                    li.ToolTip1.SetToolTip(li.Image, obj.GetDisplayDescription)
                    li.ObjectTypeInfo = obj.GetType
                    li.Tag = obj.ObjectClass
                    litems.Add(li)
                    obj = Nothing
                End If
            End If
        Next

        For Each item In My.Application.MainWindowForm.ExternalUnitOperations
            Dim obj = item.Value
            If Not Flowsheet.MobileCompatibilityMode Then
                add = obj.GetType.GetProperty("Visible").GetValue(obj)
            Else
                add = obj.MobileCompatible
            End If
            If add Then
                obj.SetFlowsheet(Flowsheet)
                Dim li As New ListItem
                li.lblName.Text = obj.GetDisplayName
                li.lblName.Font = New Font(SystemFonts.MessageBoxFont.FontFamily, 7.0, System.Drawing.FontStyle.Bold)
                li.ToolTip1.SetToolTip(li.lblName, obj.GetDisplayDescription)
                li.Image.Image = obj.GetIconBitmap
                li.ToolTip1.SetToolTip(li.Image, obj.GetDisplayDescription)
                li.ObjectTypeInfo = obj.GetType
                li.Tag = obj.ObjectClass
                litems.Add(li)
                obj.SetFlowsheet(Nothing)
                obj = Nothing
            End If
        Next

        For Each item In litems
            Select Case DirectCast(item.Tag, Interfaces.Enums.SimulationObjectClass)
                Case SimulationObjectClass.CAPEOPEN
                    Me.PanelUser.Controls.Add(item)
                Case SimulationObjectClass.Columns
                    Me.PanelColumns.Controls.Add(item)
                Case SimulationObjectClass.Exchangers
                    Me.PanelExchangers.Controls.Add(item)
                Case SimulationObjectClass.Logical, SimulationObjectClass.Inputs, SimulationObjectClass.Switches
                    Me.PanelLogical.Controls.Add(item)
                Case SimulationObjectClass.MixersSplitters
                    Me.PanelMixers.Controls.Add(item)
                Case SimulationObjectClass.Other
                    Me.PanelOther.Controls.Add(item)
                Case SimulationObjectClass.PressureChangers
                    Me.PanelPressure.Controls.Add(item)
                Case SimulationObjectClass.Reactors
                    Me.PanelReactors.Controls.Add(item)
                Case SimulationObjectClass.Separators
                    Me.PanelSeparators.Controls.Add(item)
                Case SimulationObjectClass.Solids
                    Me.PanelSolids.Controls.Add(item)
                Case SimulationObjectClass.Streams
                    Me.PanelStreams.Controls.Add(item)
                Case SimulationObjectClass.UserModels
                    Me.PanelUser.Controls.Add(item)
                Case SimulationObjectClass.Indicators
                    Me.PanelIndicators.Controls.Add(item)
                Case SimulationObjectClass.Controllers
                    Me.PanelControllers.Controls.Add(item)
                Case SimulationObjectClass.CleanPowerSources,
                     SimulationObjectClass.Electrolyzers
                    Me.PanelCE.Controls.Add(item)
            End Select
        Next

        If Not FormMain.IsPro Then
            ProFeatures.Functions.AddProUnitOps(New FlowLayoutPanel() {PanelMixers, PanelColumns, PanelExchangers, PanelLogical, PanelPressure, PanelUser, PanelReactors})
        End If

        'fossee models
        Dim folders = Directory.GetDirectories(Path.Combine(My.Application.Info.DirectoryPath, "FOSSEE"))
        For Each folder In folders
            Dim name = Path.GetFileName(folder.Replace("_", " "))
            Dim obj = New CustomUO
            Dim li As New ListItem
            li.lblName.Text = name
            li.lblName.Font = New Font(SystemFonts.MessageBoxFont.FontFamily, 7.0, System.Drawing.FontStyle.Bold)
            li.ToolTip1.SetToolTip(li.lblName, name)
            li.Image.Image = My.Resources.icons8_python
            li.ToolTip1.SetToolTip(li.Image, name)
            li.ObjectTypeInfo = obj.GetType

            Dim txtfile = Directory.GetFiles(folder, "*.txt", SearchOption.TopDirectoryOnly)
            Dim pdffile = Directory.GetFiles(folder, "*.pdf", SearchOption.TopDirectoryOnly)

            li.Tag = New Object() {obj.ObjectClass, txtfile(0), pdffile(0)}

            PanelCustomModelsFOSSEE.Controls.Add(li)

            obj = Nothing

        Next

    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs)
        Process.Start("https://dwsim.fossee.in/custom-model")
    End Sub

    Private Sub CheckBox1_Click(sender As Object, e As EventArgs) Handles CheckBox9.Click, CheckBox8.Click, CheckBox7.Click, CheckBox6.Click,
        CheckBox5.Click, CheckBox4.Click, CheckBox3.Click, CheckBox2.Click, CheckBox15.Click, CheckBox14.Click, CheckBox13.Click, CheckBox12.Click,
        CheckBox11.Click, CheckBox10.Click, CheckBox1.Click

        'find out which checkbox was clicked
        Dim chkB As CheckBox = CType(sender, CheckBox)

        'get the panel tagged to the checkbox
        Dim pnl As Panel = CType(chkB.Tag, Panel)

        If chkB.Checked Then
            pnl.Height = CInt(pnl.Tag) * Settings.DpiScale
            chkB.Image = arrow_down
        Else
            pnl.Height = 0
            chkB.Image = arrow_right
            chkB.Checked = False
        End If

    End Sub

End Class