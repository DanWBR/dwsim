Public Delegate Function HFL(ByVal Tref As Double, ByVal H As Double, ByVal P As Double, ByVal Vz As Object, ByVal VTc As Object, ByVal VPc As Object, ByVal VTb As Object, ByVal Vw As Object, ByVal VMM As Object, ByVal V As Double, ByVal KI As Object) As Object
Public Delegate Function SFL(ByVal Tref As Double, ByVal S As Double, ByVal P As Double, ByVal Vz As Object, ByVal VTc As Object, ByVal VPc As Object, ByVal VTb As Object, ByVal Vw As Object, ByVal VMM As Object, ByVal V As Double, ByVal KI As Object) As Object

Public Class FrmPsvSize

    Inherits System.Windows.Forms.Form

    Dim valve As DWSIM.SimulationObjects.UnitOps.Valve
    Dim entmat As DWSIM.SimulationObjects.Streams.MaterialStream
    Dim saimat As DWSIM.SimulationObjects.Streams.MaterialStream
    Dim Frm As FormFlowsheet

    Dim sz As DWSIM.Utilities.PSV.Sizing
    Dim ev As DWSIM.Utilities.PSV.Evaluation

    Public su As New DWSIM.SistemasDeUnidades.Unidades
    Public cv As New DWSIM.SistemasDeUnidades.Conversor
    Public nf As String

    Private Sub Form4_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        Me.Frm = My.Application.ActiveSimulation

        Me.sz = New DWSIM.Utilities.PSV.Sizing
        Me.ev = New DWSIM.Utilities.PSV.Evaluation

        Me.su = Frm.Options.SelectedUnitSystem
        Me.nf = Frm.Options.NumberFormat

        Me.ComboBox3.Items.Clear()
        For Each valve2 In Me.Frm.Collections.CLCS_ValveCollection.Values
            Me.ComboBox3.Items.Add(valve2.GraphicObject.Tag.ToString)
        Next

        If Me.ComboBox3.Items.Count > 0 Then Me.ComboBox3.SelectedIndex = 0
        'Me.ComboBox1.SelectedIndex = 0
        Me.ComboBox2.SelectedIndex = 0

        Me.Text = DWSIM.App.GetLocalString("DWSIMUtilitriosDimen")

        Label7.Text = Frm.Options.SelectedUnitSystem.spmp_temperature
        Label8.Text = Frm.Options.SelectedUnitSystem.spmp_pressure
        Label10.Text = Frm.Options.SelectedUnitSystem.spmp_pressure

        Label13.Text = Frm.Options.SelectedUnitSystem.spmp_volumetricFlow
        Label14.Text = Frm.Options.SelectedUnitSystem.spmp_volumetricFlow
        Label11.Text = Frm.Options.SelectedUnitSystem.spmp_volumetricFlow

        TextBox10.Text = Format(0.85#, "0.00")
        TextBox9.Text = Format(1.0#, "0.00")
        TextBox6.Text = Format(1.0#, "0.00")

    End Sub

    Private Sub KryptonButton1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles KryptonButton1.Click

        'Preparar variáveis
        Dim T, P, SPP, BP As Double
        Dim QT, QL, QV, WT, WL, WV As Double
        Dim visc, visc_l, visc_v, me_l, me_v, xv_l, me_m As Double
        Dim zg, cpcv, mm_g, sg, xm_g As Double
        Dim Kd, Kb, Kc As Double

        Dim Fluido = "", Metodo = ""

        Try

            Kd = CDbl(TextBox10.Text)
            Kb = CDbl(TextBox9.Text)
            Kc = CDbl(TextBox6.Text)

            T = Me.entmat.Fases(0).SPMProperties.temperature
            P = Me.entmat.Fases(0).SPMProperties.pressure
            SPP = CDbl(TextBox3.Text)
            BP = Me.saimat.Fases(0).SPMProperties.pressure

            With Me.entmat
                visc = .Fases(0).SPMProperties.viscosity.GetValueOrDefault
                me_m = .Fases(0).SPMProperties.density.GetValueOrDefault
                QT = .Fases(0).SPMProperties.volumetric_flow.GetValueOrDefault
                WT = .Fases(0).SPMProperties.massflow.GetValueOrDefault
                visc_l = .Fases(3).SPMProperties.viscosity.GetValueOrDefault
                me_l = .Fases(3).SPMProperties.density.GetValueOrDefault
                QL = .Fases(3).SPMProperties.volumetric_flow.GetValueOrDefault
                WL = .Fases(3).SPMProperties.massflow.GetValueOrDefault
                xv_l = .Fases(3).SPMProperties.volumetric_flow.GetValueOrDefault / .Fases(0).SPMProperties.volumetric_flow.GetValueOrDefault
                QV = .Fases(2).SPMProperties.volumetric_flow.GetValueOrDefault
                WV = .Fases(2).SPMProperties.massflow.GetValueOrDefault
                visc_v = .Fases(2).SPMProperties.viscosity.GetValueOrDefault
                me_v = .Fases(2).SPMProperties.density.GetValueOrDefault
                zg = .Fases(2).SPMProperties.compressibilityFactor.GetValueOrDefault
                cpcv = .Fases(2).SPMProperties.heatCapacityCp.GetValueOrDefault / .Fases(2).SPMProperties.heatCapacityCv.GetValueOrDefault
                mm_g = .Fases(2).SPMProperties.molecularWeight.GetValueOrDefault
                sg = mm_g / 28.9644
                xm_g = .Fases(2).SPMProperties.massfraction.GetValueOrDefault
            End With

        Catch ex As Exception

            MessageBox.Show(DWSIM.App.GetLocalString("Porfavorverifiquesen"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)

        End Try

        Dim Ao As Double

        'Dimensionar para líquido
        If Me.ComboBox1.SelectedItem = DWSIM.App.GetLocalString("Lquido") And Me.ComboBox2.SelectedItem = "API RP 520" Then

            Ao = sz.PSV_LCC_D(QL * 24 * 3600, (P * 1.033 / 101325) * (1 + SPP / 100), (BP * 1.033 / 101325), me_l, visc_l, Kd, Kc)

        End If

        'Dimensionar para gás apenas
        If Me.ComboBox1.SelectedItem = DWSIM.App.GetLocalString("Vapor") And Me.ComboBox2.SelectedItem = "API RP 520" Then

            Ao = sz.PSV_G_D((P * 1.033 / 101325) * (1 + SPP / 100), (BP * 1.033 / 101325), T, WV * 3600, zg, mm_g, cpcv, Kd, Kb, Kc)

        End If

        'Dimensionar para bifásico
        If Me.ComboBox1.SelectedItem = DWSIM.App.GetLocalString("GsLquidoBifsico") And Me.ComboBox2.SelectedItem = "API RP 520" Then

            Dim mymat As DWSIM.SimulationObjects.Streams.MaterialStream = entmat.Clone
            mymat.Fases(0).SPMProperties.pressure = entmat.Fases(0).SPMProperties.pressure.GetValueOrDefault * 0.9

            With mymat.PropertyPackage
                .CurrentMaterialStream = mymat
                .DW_CalcEquilibrium(DWSIM.SimulationObjects.PropertyPackages.FlashSpec.T, DWSIM.SimulationObjects.PropertyPackages.FlashSpec.P)
                If mymat.Fases(3).SPMProperties.molarfraction.GetValueOrDefault > 0 Then
                    .DW_CalcPhaseProps(DWSIM.SimulationObjects.PropertyPackages.Fase.Liquid1)
                Else
                    .DW_ZerarPhaseProps(DWSIM.SimulationObjects.PropertyPackages.Fase.Liquid1)
                End If
                If mymat.Fases(2).SPMProperties.molarfraction.GetValueOrDefault > 0 Then
                    .DW_CalcPhaseProps(DWSIM.SimulationObjects.PropertyPackages.Fase.Vapor)
                Else
                    .DW_ZerarPhaseProps(DWSIM.SimulationObjects.PropertyPackages.Fase.Vapor)
                End If
                If mymat.Fases(2).SPMProperties.molarfraction.GetValueOrDefault >= 0 And mymat.Fases(2).SPMProperties.molarfraction.GetValueOrDefault <= 1 Then
                    .DW_CalcPhaseProps(DWSIM.SimulationObjects.PropertyPackages.Fase.Liquid)
                Else
                    .DW_ZerarPhaseProps(DWSIM.SimulationObjects.PropertyPackages.Fase.Liquid)
                End If
                mymat.PropertyPackage.DW_CalcPhaseProps(DWSIM.SimulationObjects.PropertyPackages.Fase.Mixture)
            End With

            Dim rho90 = mymat.Fases(0).SPMProperties.density.GetValueOrDefault

            mymat.Clear()
            mymat = Nothing

            Dim _tmp = sz.PSV_GL_D23_D(xm_g, me_v, me_m, rho90, (P * 1.033 / 101325) * (1 + SPP / 100) - 1.033, (BP * 1.033 / 101325) - 1.033, QT * 24 * 3600, Kd, Kb, Kc)

            Ao = _tmp(0)

        End If

        If Double.IsNaN(Ao) = False Or Ao > 0 Then
            TextBox11.Text = Format(Ao, "####0.##")

            Dim tmp = sz.ORIF_API(Ao)

            Dim L_API = tmp(1)
            Dim A_API = tmp(2)

            TextBox12.Text = L_API & " / " & Format(CDbl(A_API), "####0.##")

        Else
            TextBox11.Text = DWSIM.App.GetLocalString("Erro")
            TextBox12.Text = DWSIM.App.GetLocalString("Erro")
            MessageBox.Show(DWSIM.App.GetLocalString("Erronoclculodareadoo"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)

        End If

    End Sub

    Private Sub Button1_Click_1(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button1.Click

        If Not Me.ComboBox3.SelectedItem Is Nothing Then

            Dim gobj As Microsoft.MSDN.Samples.GraphicObjects.GraphicObject = FormFlowsheet.SearchSurfaceObjectsByTag(Me.ComboBox3.SelectedItem, Frm.FormSurface.FlowsheetDesignSurface)
            Me.valve = Frm.Collections.CLCS_ValveCollection(gobj.Name)
            Me.LblSelected.Text = Me.valve.GraphicObject.Tag
            Me.entmat = Frm.Collections.CLCS_MaterialStreamCollection(Me.valve.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name)
            Me.saimat = Frm.Collections.CLCS_MaterialStreamCollection(Me.valve.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Name)


            Me.TextBox1.Text = Format(Conversor.ConverterDoSI(su.spmp_temperature, entmat.Fases(0).SPMProperties.temperature), nf)
            Me.TextBox2.Text = Format(Conversor.ConverterDoSI(su.spmp_pressure, entmat.Fases(0).SPMProperties.pressure), nf)
            Me.TextBox4.Text = Format(Conversor.ConverterDoSI(su.spmp_pressure, saimat.Fases(0).SPMProperties.pressure), nf)

            Me.TextBox8.Text = Format(Conversor.ConverterDoSI(su.spmp_volumetricFlow, entmat.Fases(0).SPMProperties.volumetric_flow.GetValueOrDefault), nf)
            Me.TextBox7.Text = Format(Conversor.ConverterDoSI(su.spmp_volumetricFlow, entmat.Fases(3).SPMProperties.volumetric_flow.GetValueOrDefault), nf)
            Me.TextBox5.Text = Format(Conversor.ConverterDoSI(su.spmp_volumetricFlow, entmat.Fases(2).SPMProperties.volumetric_flow.GetValueOrDefault), nf)

            'Gás
            'Líquido
            'Gás + Líquido (Bifásico)

            If entmat.Fases(0).SPMProperties.volumetric_flow.GetValueOrDefault = 0 Then
                ComboBox1.Items.Clear()
            Else
                If entmat.Fases(2).SPMProperties.volumetric_flow.GetValueOrDefault = 0 And _
                        entmat.Fases(3).SPMProperties.volumetric_flow.GetValueOrDefault <> 0 Then
                    ComboBox1.Items.Clear()
                    ComboBox1.Items.Add(DWSIM.App.GetLocalString("Lquido"))
                    ComboBox1.SelectedIndex = 0
                ElseIf entmat.Fases(2).SPMProperties.volumetric_flow.GetValueOrDefault <> 0 And _
                        entmat.Fases(3).SPMProperties.volumetric_flow.GetValueOrDefault = 0 Then
                    ComboBox1.Items.Clear()
                    ComboBox1.Items.Add(DWSIM.App.GetLocalString("Vapor"))
                    ComboBox1.SelectedIndex = 0
                Else
                    ComboBox1.Items.Clear()
                    ComboBox1.Items.Add(DWSIM.App.GetLocalString("Vapor"))
                    ComboBox1.Items.Add(DWSIM.App.GetLocalString("Lquido"))
                    ComboBox1.Items.Add(DWSIM.App.GetLocalString("GsLquidoBifsico"))
                    ComboBox1.SelectedIndex = 0
                End If

            End If

        Else

            Me.valve = Nothing
            Me.LblSelected.Text = ""

        End If

    End Sub

    Private Sub FrmPsvSize_HelpRequested(sender As System.Object, hlpevent As System.Windows.Forms.HelpEventArgs) Handles MyBase.HelpRequested
        DWSIM.App.HelpRequested("UT_PSVSizing.htm") 'no topic yet
    End Sub
End Class