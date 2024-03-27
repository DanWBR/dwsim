Imports System.Drawing
Imports System.Windows.Forms
Imports DWSIM.Interfaces
Imports DWSIM.SharedClasses.DWSIM.Flowsheet
Imports DWSIM.Thermodynamics.PropertyPackages

Public Class Functions

    Public Shared Async Sub ProcessTransition(flowsheet As IFlowsheet)



    End Sub

    Public Shared Sub DisplayTransitionForm(flowsheet As IFlowsheet, featurename As String)

        Dim fp As New FormBridgeToPro()
        fp.lblFeature.Text = featurename
        fp.CurrentFlowsheet = flowsheet
        fp.ShowDialog()

    End Sub

    Public Shared Sub AddProUnitOps(Panels() As FlowLayoutPanel)

        For Each panel In Panels
            AddUnitOp("(Semi)Batch Reactor", "ProExtensions.UnitOperations.SemiBatchReactor.SemiBatchReactor", My.Resources.jacketedreactor_icon, panel)
            AddUnitOp("Falling Film Evaporator", "DWSIM.AdditionalUnitOperations.FallingFilmEvaporator", My.Resources.fallingfilm_icon, panel)
            AddUnitOp("PPBDesigner Column", "ProExtensions.PPBDesigner.PPBDesignerUnitOperation", My.Resources.ppbdesigner, panel)
            AddUnitOp("Three-Phase/Reactive Column (Pro)", "ProExtensions.UnitOperations.ProRigorousColumns.DistillationColumnPro", My.Resources.col_dc_32, panel)
            AddUnitOp("Pipe Network", "DWSIM.UnitOperations.PipeNetworkUnitOperation", My.Resources.pipe_network_icon, panel)
            AddUnitOp("Neural Network (Pro)", "DWSIM.UnitOperations.NeuralNetworkUnitOperation", My.Resources.icons8_artificial_intelligence, panel)
            AddUnitOp("Energy Stream Splitter", "DWSIM.AdditionalUnitOperations.EnergySplitter", My.Resources.uo_split_32, panel)
            AddUnitOp("Material Stream Switch", "DWSIM.AdditionalUnitOperations.MaterialStreamSwitch", My.Resources.switch_material, panel)
            AddUnitOp("Energy Stream Switch", "DWSIM.AdditionalUnitOperations.EnergyStreamSwitch", My.Resources.switch_energy, panel)
        Next

    End Sub

    Private Shared Sub AddUnitOp(name As String, typename As String, image As Image, panel As FlowLayoutPanel)

        Dim li As New ListItem
        li.lblName.Text = name
        li.lblName.Tag = typename
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

    Public Shared Sub CreateTransitionObject(fs As IFlowsheet, feature As String, type As String, action As String, location As String, position As Double())

        Dim ts As New FlowsheetTransitionRestore
        With ts
            .FeatureName = feature
            .FeatureType = type
            .Action = action
            .Location = location
        End With
        If position IsNot Nothing Then ts.Position = New List(Of Double)(position)
        fs.FlowsheetOptions.FlowsheetTransitionObject = ts

    End Sub

End Class
