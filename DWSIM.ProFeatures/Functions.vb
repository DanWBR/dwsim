Imports System.Drawing
Imports System.Windows.Forms
Imports DWSIM.Thermodynamics.PropertyPackages

Public Class Functions

    Public Shared Sub AddProUnitOps(Panels() As FlowLayoutPanel)

        For Each panel In Panels
            Select Case panel.Name
                Case "PanelPressure"
                    AddUnitOp("Pipe Network", My.Resources.pipe_network_icon, panel)
                Case "PanelMixers"
                    AddUnitOp("Energy Stream Splitter", My.Resources.uo_split_32, panel)
                Case "PanelExchangers"
                    AddUnitOp("Falling Film Evaporator", My.Resources.fallingfilm_icon, panel)
                Case "PanelColumns"
                    AddUnitOp("Three-Phase/Reactive Column (Pro)", My.Resources.col_dc_32, panel)
                    AddUnitOp("PPBDesigner Column", My.Resources.ppbdesigner, panel)
                Case "PanelUser"
                    AddUnitOp("Neural Network (Pro)", My.Resources.icons8_artificial_intelligence, panel)
                Case "PanelLogical"
                    AddUnitOp("Material Stream Switch", My.Resources.switch_material, panel)
                    AddUnitOp("Energy Stream Switch", My.Resources.switch_energy, panel)
                Case "PanelReactors"
                    AddUnitOp("(Semi)Batch Reactor", My.Resources.jacketedreactor_icon, panel)
            End Select
        Next

    End Sub

    Private Shared Sub AddUnitOp(name As String, image As Image, panel As FlowLayoutPanel)

        Dim li As New ListItem
        li.lblName.Text = name
        li.lblName.Font = New Font(SystemFonts.MessageBoxFont.FontFamily, 7.0, System.Drawing.FontStyle.Bold)
        li.ToolTip1.SetToolTip(li.lblName, "Upgrade to DWSIM Pro")
        li.Image.Image = image
        li.ToolTip1.SetToolTip(li.Image, "Upgrade to DWSIM Pro")
        li.ObjectTypeInfo = li.GetType()
        li.Tag = DWSIM.Interfaces.Enums.SimulationObjectClass.None
        panel.Controls.Add(li)

    End Sub

    Public Shared Sub AddProPPs(grid As DataGridView)

        grid.Rows.Add(New Object() {PackageType.ActivityCoefficient, 0, Nothing, My.Resources.Icon1281, "Amines", "Upgrade to DWSIM Pro to use this Property Package."})
        grid.Rows(grid.Rows.Count - 1).DefaultCellStyle.BackColor = Color.Honeydew
        grid.Rows.Add(New Object() {PackageType.ActivityCoefficient, 0, Nothing, My.Resources.Icon1281, "Peng-Robinson 1978 (PR78) for Petroleum Industry", "Upgrade to DWSIM Pro to use this Property Package."})
        grid.Rows(grid.Rows.Count - 1).DefaultCellStyle.BackColor = Color.Honeydew
        'grid.Rows.Add(New Object() {PackageType.ActivityCoefficient, 0, Nothing, My.Resources.Icon1281, "NRTL (Pro)", "Upgrade to DWSIM Pro to use this Property Package."})
        'grid.Rows.Add(New Object() {PackageType.ActivityCoefficient, 0, Nothing, My.Resources.Icon1281, "UNIQUAC (Pro)", "Upgrade to DWSIM Pro to use this Property Package."})
        'grid.Rows.Add(New Object() {PackageType.ActivityCoefficient, 0, Nothing, My.Resources.Icon1281, "UNIFAC (Pro)", "Upgrade to DWSIM Pro to use this Property Package."})
        'grid.Rows.Add(New Object() {PackageType.ActivityCoefficient, 0, Nothing, My.Resources.Icon1281, "UNIFAC-LL (Pro)", "Upgrade to DWSIM Pro to use this Property Package."})
        'grid.Rows.Add(New Object() {PackageType.ActivityCoefficient, 0, Nothing, My.Resources.Icon1281, "Modified UNIFAC (Dortmund) (Pro)", "Upgrade to DWSIM Pro to use this Property Package."})
        'grid.Rows.Add(New Object() {PackageType.ActivityCoefficient, 0, Nothing, My.Resources.Icon1281, "Modified UNIFAC (NIST) (Pro)", "Upgrade to DWSIM Pro to use this Property Package."})
        grid.Rows.Add(New Object() {PackageType.Miscelaneous, 0, Nothing, My.Resources.Icon1281, "REFPROP", "Upgrade to DWSIM Pro to use this Property Package."})
        grid.Rows(grid.Rows.Count - 1).DefaultCellStyle.BackColor = Color.Honeydew
        grid.Rows.Add(New Object() {PackageType.Miscelaneous, 0, Nothing, My.Resources.Icon1281, "Extended CoolProp", "Upgrade to DWSIM Pro to use this Property Package."})
        grid.Rows(grid.Rows.Count - 1).DefaultCellStyle.BackColor = Color.Honeydew
        'grid.Rows.Add(New Object() {PackageType.EOS, 0, Nothing, My.Resources.Icon1281, "Peng-Robinson 1978 Pro (PR78 Pro)", "Upgrade to DWSIM Pro to use this Property Package."})
        'grid.Rows.Add(New Object() {PackageType.EOS, 0, Nothing, My.Resources.Icon1281, "Soave-Redlich-Kwong Pro (SRK Pro)", "Upgrade to DWSIM Pro to use this Property Package."})

    End Sub

    Public Shared Sub AddProPPs2(grid As DataGridView)

        grid.Rows.Add(New Object() {"", My.Resources.Icon1281, "Amines", "Upgrade to DWSIM Pro to use this Property Package."})
        grid.Rows(grid.Rows.Count - 1).DefaultCellStyle.BackColor = Color.Honeydew
        grid.Rows.Add(New Object() {"", My.Resources.Icon1281, "Peng-Robinson 1978 (PR78) for Petroleum Industry", "Upgrade to DWSIM Pro to use this Property Package."})
        grid.Rows(grid.Rows.Count - 1).DefaultCellStyle.BackColor = Color.Honeydew
        'grid.Rows.Add(New Object() {"", My.Resources.Icon1281, "NRTL (Pro)", "Upgrade to DWSIM Pro to use this Property Package."})
        'grid.Rows.Add(New Object() {"", My.Resources.Icon1281, "UNIQUAC (Pro)", "Upgrade to DWSIM Pro to use this Property Package."})
        'grid.Rows.Add(New Object() {"", My.Resources.Icon1281, "UNIFAC (Pro)", "Upgrade to DWSIM Pro to use this Property Package."})
        'grid.Rows.Add(New Object() {"", My.Resources.Icon1281, "UNIFAC-LL (Pro)", "Upgrade to DWSIM Pro to use this Property Package."})
        'grid.Rows.Add(New Object() {"", My.Resources.Icon1281, "Modified UNIFAC (Dortmund) (Pro)", "Upgrade to DWSIM Pro to use this Property Package."})
        'grid.Rows.Add(New Object() {"", My.Resources.Icon1281, "Modified UNIFAC (NIST) (Pro)", "Upgrade to DWSIM Pro to use this Property Package."})
        grid.Rows.Add(New Object() {"", My.Resources.Icon1281, "REFPROP", "Upgrade to DWSIM Pro to use this Property Package."})
        grid.Rows(grid.Rows.Count - 1).DefaultCellStyle.BackColor = Color.Honeydew
        grid.Rows.Add(New Object() {"", My.Resources.Icon1281, "Extended CoolProp", "Upgrade to DWSIM Pro to use this Property Package."})
        grid.Rows(grid.Rows.Count - 1).DefaultCellStyle.BackColor = Color.Honeydew
        'grid.Rows.Add(New Object() {"", My.Resources.Icon1281, "Peng-Robinson 1978 Pro (PR78 Pro)", "Upgrade to DWSIM Pro to use this Property Package."})
        'grid.Rows.Add(New Object() {"", My.Resources.Icon1281, "Soave-Redlich-Kwong Pro (SRK Pro)", "Upgrade to DWSIM Pro to use this Property Package."})

    End Sub

End Class
