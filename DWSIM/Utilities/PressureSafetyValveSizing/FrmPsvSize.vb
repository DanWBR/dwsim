Public Delegate Function HFL(ByVal Tref As Double, ByVal H As Double, ByVal P As Double, ByVal Vz As Object, ByVal VTc As Object, ByVal VPc As Object, ByVal VTb As Object, ByVal Vw As Object, ByVal VMM As Object, ByVal V As Double, ByVal KI As Object) As Object
Public Delegate Function SFL(ByVal Tref As Double, ByVal S As Double, ByVal P As Double, ByVal Vz As Object, ByVal VTc As Object, ByVal VPc As Object, ByVal VTb As Object, ByVal Vw As Object, ByVal VMM As Object, ByVal V As Double, ByVal KI As Object) As Object

Public Class FrmPsvSize

    Inherits UserControl

    Implements Interfaces.IAttachedUtility

    Dim valve As Valve
    Dim entmat As Streams.MaterialStream
    Dim saimat As Streams.MaterialStream
    Dim Frm As FormFlowsheet

    Dim sz As DWSIM.Utilities.PSV.Sizing
    Dim ev As DWSIM.Utilities.PSV.Evaluation

    Public su As New SystemsOfUnits.Units
    Public cv As New SystemsOfUnits.Converter
    Public nf As String

    Private Sub Form4_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        ExtensionMethods.ChangeDefaultFont(Me)

        Me.Frm = AttachedTo.GetFlowsheet

        Me.sz = New DWSIM.Utilities.PSV.Sizing
        Me.ev = New DWSIM.Utilities.PSV.Evaluation

        Me.su = Frm.Options.SelectedUnitSystem
        Me.nf = Frm.Options.NumberFormat

        Me.ComboBox2.SelectedIndex = 0

        Me.Text = DWSIM.App.GetLocalString("DWSIMUtilitriosDimen")

        Label7.Text = Frm.Options.SelectedUnitSystem.temperature
        Label8.Text = Frm.Options.SelectedUnitSystem.pressure
        Label10.Text = Frm.Options.SelectedUnitSystem.pressure

        Label13.Text = Frm.Options.SelectedUnitSystem.volumetricFlow
        Label14.Text = Frm.Options.SelectedUnitSystem.volumetricFlow
        Label11.Text = Frm.Options.SelectedUnitSystem.volumetricFlow

        TextBox10.Text = Format(0.85#, "0.00")
        TextBox9.Text = Format(1.0#, "0.00")
        TextBox6.Text = Format(1.0#, "0.00")

        Calculate()

    End Sub

    Private Sub KryptonButton1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles KryptonButton1.Click

        'Preparar variaveis
        Dim T, P, SPP, BP As Double
        Dim QT, QL, QV, WT, WL, WV As Double
        Dim visc, visc_l, visc_v, me_l, me_v, xv_l, me_m As Double
        Dim zg, cpcv, mm_g, sg, xm_g As Double
        Dim Kd, Kb, Kc As Double

        Dim Fluido = "", Metodo = ""

        Try

            Kd = Convert.ToDouble(TextBox10.Text)
            Kb = Convert.ToDouble(TextBox9.Text)
            Kc = Convert.ToDouble(TextBox6.Text)

            T = Me.entmat.Phases(0).Properties.temperature
            P = Me.entmat.Phases(0).Properties.pressure
            SPP = Convert.ToDouble(TextBox3.Text)
            BP = Me.saimat.Phases(0).Properties.pressure

            With Me.entmat
                visc = .Phases(0).Properties.viscosity.GetValueOrDefault
                me_m = .Phases(0).Properties.density.GetValueOrDefault
                QT = .Phases(0).Properties.volumetric_flow.GetValueOrDefault
                WT = .Phases(0).Properties.massflow.GetValueOrDefault
                visc_l = .Phases(3).Properties.viscosity.GetValueOrDefault
                me_l = .Phases(3).Properties.density.GetValueOrDefault
                QL = .Phases(3).Properties.volumetric_flow.GetValueOrDefault
                WL = .Phases(3).Properties.massflow.GetValueOrDefault
                xv_l = .Phases(3).Properties.volumetric_flow.GetValueOrDefault / .Phases(0).Properties.volumetric_flow.GetValueOrDefault
                QV = .Phases(2).Properties.volumetric_flow.GetValueOrDefault
                WV = .Phases(2).Properties.massflow.GetValueOrDefault
                visc_v = .Phases(2).Properties.viscosity.GetValueOrDefault
                me_v = .Phases(2).Properties.density.GetValueOrDefault
                zg = .Phases(2).Properties.compressibilityFactor.GetValueOrDefault
                cpcv = .Phases(2).Properties.heatCapacityCp.GetValueOrDefault / .Phases(2).Properties.heatCapacityCv.GetValueOrDefault
                mm_g = .Phases(2).Properties.molecularWeight.GetValueOrDefault
                sg = mm_g / 28.9644
                xm_g = .Phases(2).Properties.massfraction.GetValueOrDefault
            End With

        Catch ex As Exception

            MessageBox.Show(DWSIM.App.GetLocalString("Porfavorverifiquesen"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)

        End Try

        Dim Ao As Double

        'Dimensionar para liquido
        If Me.ComboBox1.SelectedItem = DWSIM.App.GetLocalString("Lquido") And Me.ComboBox2.SelectedItem = "API RP 520" Then

            Ao = sz.PSV_LCC_D(QL * 24 * 3600, (P * 1.033 / 101325) * (1 + SPP / 100), (BP * 1.033 / 101325), me_l, visc_l, Kd, Kc)

        End If

        'Dimensionar para gas apenas
        If Me.ComboBox1.SelectedItem = DWSIM.App.GetLocalString("Vapor") And Me.ComboBox2.SelectedItem = "API RP 520" Then

            Ao = sz.PSV_G_D((P * 1.033 / 101325) * (1 + SPP / 100), (BP * 1.033 / 101325), T, WV * 3600, zg, mm_g, cpcv, Kd, Kb, Kc)

        End If

        'Dimensionar para bifasico
        If Me.ComboBox1.SelectedItem = DWSIM.App.GetLocalString("GsLquidoBifsico") And Me.ComboBox2.SelectedItem = "API RP 520" Then

            Dim mymat As Streams.MaterialStream = entmat.Clone

            mymat.PropertyPackage = entmat.PropertyPackage

            mymat.Phases(0).Properties.pressure = entmat.Phases(0).Properties.pressure.GetValueOrDefault * 0.9

            mymat.PropertyPackage.CurrentMaterialStream = mymat
            mymat.Calculate()

            Dim rho90 = mymat.Phases(0).Properties.density.GetValueOrDefault

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

            TextBox12.Text = L_API & " / " & Format(Convert.ToDouble(A_API), "####0.##")

        Else
            TextBox11.Text = DWSIM.App.GetLocalString("Erro")
            TextBox12.Text = DWSIM.App.GetLocalString("Erro")
            MessageBox.Show(DWSIM.App.GetLocalString("Erronoclculodareadoo"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)

        End If

    End Sub

    Private Sub Calculate()


        Dim gobj As Drawing.SkiaSharp.GraphicObjects.GraphicObject = AttachedTo.GraphicObject
        Me.valve = Frm.Collections.FlowsheetObjectCollection(gobj.Name)
        Me.entmat = Frm.Collections.FlowsheetObjectCollection(Me.valve.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name)
        Me.saimat = Frm.Collections.FlowsheetObjectCollection(Me.valve.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Name)


        Me.TextBox1.Text = Format(SystemsOfUnits.Converter.ConvertFromSI(su.temperature, entmat.Phases(0).Properties.temperature), nf)
        Me.TextBox2.Text = Format(SystemsOfUnits.Converter.ConvertFromSI(su.pressure, entmat.Phases(0).Properties.pressure), nf)
        Me.TextBox4.Text = Format(SystemsOfUnits.Converter.ConvertFromSI(su.pressure, saimat.Phases(0).Properties.pressure), nf)

        Me.TextBox8.Text = Format(SystemsOfUnits.Converter.ConvertFromSI(su.volumetricFlow, entmat.Phases(0).Properties.volumetric_flow.GetValueOrDefault), nf)
        Me.TextBox7.Text = Format(SystemsOfUnits.Converter.ConvertFromSI(su.volumetricFlow, entmat.Phases(3).Properties.volumetric_flow.GetValueOrDefault), nf)
        Me.TextBox5.Text = Format(SystemsOfUnits.Converter.ConvertFromSI(su.volumetricFlow, entmat.Phases(2).Properties.volumetric_flow.GetValueOrDefault), nf)

        'Gas
        'Liquido
        'Gas + Liquido (Bifasico)

        If entmat.Phases(0).Properties.volumetric_flow.GetValueOrDefault = 0 Then
            ComboBox1.Items.Clear()
        Else
            If entmat.Phases(2).Properties.volumetric_flow.GetValueOrDefault = 0 And _
                    entmat.Phases(3).Properties.volumetric_flow.GetValueOrDefault <> 0 Then
                ComboBox1.Items.Clear()
                ComboBox1.Items.Add(DWSIM.App.GetLocalString("Lquido"))
                ComboBox1.SelectedIndex = 0
            ElseIf entmat.Phases(2).Properties.volumetric_flow.GetValueOrDefault <> 0 And _
                    entmat.Phases(3).Properties.volumetric_flow.GetValueOrDefault = 0 Then
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

    End Sub

    Private Sub FrmPsvSize_HelpRequested(sender As System.Object, hlpevent As System.Windows.Forms.HelpEventArgs) Handles MyBase.HelpRequested
        DWSIM.App.HelpRequested("UT_PSVSizing.htm") 'no topic yet
    End Sub

    Public Property AttachedTo As Interfaces.ISimulationObject Implements Interfaces.IAttachedUtility.AttachedTo

    Public Function GetPropertyList() As List(Of String) Implements Interfaces.IAttachedUtility.GetPropertyList
        Return New List(Of String)(New String() {"Name", "AutoUpdate", "OverPressure", "Kd", "Kb", "Kc", "Method", "RelievedFluid"})

    End Function

    Public Function GetPropertyUnits(pname As String) As String Implements Interfaces.IAttachedUtility.GetPropertyUnits
        Return ""

    End Function

    Public Function GetPropertyValue(pname As String) As Object Implements Interfaces.IAttachedUtility.GetPropertyValue
        Select Case pname
            Case "Name"
                Return Name
            Case "AutoUpdate"
                Return AutoUpdate
            Case "OverPressure"
                Return Double.Parse(TextBox3.Text)
            Case "Kd"
                Return Double.Parse(TextBox10.Text)
            Case "Kb"
                Return Double.Parse(TextBox9.Text)
            Case "Kc"
                Return Double.Parse(TextBox6.Text)
            Case "Method"
                Return ComboBox2.SelectedIndex
            Case "RelievedFluid"
                Return ComboBox1.SelectedIndex
        End Select
        Return ""
    End Function

    Public Property ID As Integer Implements Interfaces.IAttachedUtility.ID

    Public Property Name1 As String Implements Interfaces.IAttachedUtility.Name

    Public Sub SetPropertyValue(pname As String, pvalue As Object) Implements Interfaces.IAttachedUtility.SetPropertyValue
        Select Case pname
            Case "Name"
                Name = pvalue
            Case "AutoUpdate"
                AutoUpdate = pvalue
            Case "OverPressure"
                TextBox3.Text = pvalue
            Case "Kd"
                TextBox10.Text = pvalue
            Case "Kb"
                TextBox9.Text = pvalue
            Case "Kc"
                TextBox6.Text = pvalue
            Case "Method"
                ComboBox2.SelectedIndex = pvalue
            Case "RelievedFluid"
                ComboBox1.SelectedIndex = pvalue
        End Select
    End Sub

    Public Sub Update1() Implements Interfaces.IAttachedUtility.Update
        KryptonButton1_Click(Me, New EventArgs)
    End Sub

    Public Function GetUtilityType() As FlowsheetUtility Implements Interfaces.IAttachedUtility.GetUtilityType
        Return FlowsheetUtility.PSVSizing
    End Function

    Public Property AutoUpdate As Boolean Implements Interfaces.IAttachedUtility.AutoUpdate

    Public Sub LoadData(data As Dictionary(Of String, Object)) Implements Interfaces.IAttachedUtility.LoadData
        For Each item In data
            SetPropertyValue(item.Key, item.Value)
        Next
    End Sub

    Public Function SaveData() As Dictionary(Of String, Object) Implements Interfaces.IAttachedUtility.SaveData
        Dim props As New Dictionary(Of String, Object)
        For Each prop In GetPropertyList()
            props.Add(prop, GetPropertyValue(prop))
        Next
        Return props
    End Function

    Public Sub Initialize() Implements Interfaces.IAttachedUtility.Initialize

    End Sub

    Public Sub Populate() Implements Interfaces.IAttachedUtility.Populate

    End Sub
End Class