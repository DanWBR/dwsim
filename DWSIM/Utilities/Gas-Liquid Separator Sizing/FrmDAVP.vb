Public Class FrmDAVP

    Inherits UserControl

    Implements Interfaces.IAttachedUtility

    Dim vessel As Vessel
    Dim entmat As Streams.MaterialStream
    Dim Frm As FormFlowsheet
    Dim rhol, rhov, ql, qv, qe, rhoe, wl, wv As Double
    Dim RLD, C, VGI, SURGE, TR, VMAX, K As Double

    Private Sub Label4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles LblLiq.Click

    End Sub

    Public Sub Initialize() Implements Interfaces.IAttachedUtility.Initialize

        Me.Frm = AttachedTo.GetFlowsheet

        Me.ComboBox1.Items.Clear()
        Me.ComboBox1.Items.Add(AttachedTo.GraphicObject.Tag.ToString)
        Me.ComboBox1.SelectedIndex = 0
        Me.ComboBox1.Enabled = False

    End Sub

    Public Sub Populate() Implements Interfaces.IAttachedUtility.Populate

    End Sub

    Private Sub FrmDAVP_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        Initialize()

        Me.ComboBoxTipoVaso.SelectedIndex = 0

        tbK.Text = 0.0692.ToString
        TextBox114.Text = 1.2.ToString


    End Sub

    Private Sub VScrollBar1_Scroll(ByVal sender As System.Object, ByVal e As System.Windows.Forms.ScrollEventArgs) Handles VScrollBar1.Scroll

        If e.Type = ScrollEventType.SmallDecrement Then

            TextBox114.Text = Format(Convert.ToDouble(TextBox114.Text) + 0.1, "#0.0#")

        ElseIf e.Type = ScrollEventType.SmallIncrement Then

            TextBox114.Text = Format(Convert.ToDouble(TextBox114.Text) - 0.1, "#0.0#")

        End If

    End Sub

    Private Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button1.Click

        If Not Me.ComboBox1.SelectedItem Is Nothing Then

            Dim gobj As Drawing.SkiaSharp.GraphicObjects.GraphicObject = FormFlowsheet.SearchSurfaceObjectsByTag(Me.ComboBox1.SelectedItem, Frm.FormSurface.FlowsheetSurface)
            Me.vessel = Frm.Collections.FlowsheetObjectCollection(gobj.Name)

            Dim msv, msl As Streams.MaterialStream

            msv = Frm.Collections.FlowsheetObjectCollection(Me.vessel.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Name)
            msl = Frm.Collections.FlowsheetObjectCollection(Me.vessel.GraphicObject.OutputConnectors(1).AttachedConnector.AttachedTo.Name)

            Me.entmat = Frm.Collections.FlowsheetObjectCollection(Me.vessel.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name)

            Me.rhol = msl.Phases(0).Properties.density.GetValueOrDefault
            Me.rhov = msv.Phases(0).Properties.density.GetValueOrDefault
            Me.ql = msl.Phases(0).Properties.volumetric_flow.GetValueOrDefault
            Me.qv = msv.Phases(0).Properties.volumetric_flow.GetValueOrDefault
            Me.wl = msl.Phases(0).Properties.massflow.GetValueOrDefault
            Me.wv = msv.Phases(0).Properties.massflow.GetValueOrDefault
            Me.rhoe = Me.entmat.Phases(0).Properties.density.GetValueOrDefault
            Me.qe = Me.entmat.Phases(0).Properties.volumetric_flow.GetValueOrDefault

            Dim su As SystemsOfUnits.Units = Frm.Options.SelectedUnitSystem
            Dim conv As New SystemsOfUnits.Converter

            Me.LblLiq.Text = Format(SystemsOfUnits.Converter.ConvertFromSI(su.volumetricFlow, ql), Frm.Options.NumberFormat) & " " & su.volumetricFlow & _
                            ", " & Format(SystemsOfUnits.Converter.ConvertFromSI(su.massflow, wl), Frm.Options.NumberFormat) & " " & su.massflow & _
                            ", " & Format(SystemsOfUnits.Converter.ConvertFromSI(su.density, rhol), Frm.Options.NumberFormat) & " " & su.density

            Me.LblGas.Text = Format(SystemsOfUnits.Converter.ConvertFromSI(su.molarflow, msv.Phases(0).Properties.molarflow.GetValueOrDefault), Frm.Options.NumberFormat) & " " & su.molarflow & _
                            ", " & Format(SystemsOfUnits.Converter.ConvertFromSI(su.massflow, wv), Frm.Options.NumberFormat) & " " & su.massflow & _
                            ", " & Format(SystemsOfUnits.Converter.ConvertFromSI(su.density, rhov), Frm.Options.NumberFormat) & " " & su.density

            Me.RLD = Me.tbRLD.Text
            Me.C = Me.tbC.Text
            Me.VGI = Me.tbVGI.Text
            Me.VMAX = Me.tbVMAX.Text
            Me.TR = Me.tbTR.Text
            Me.K = Me.tbK.Text
            Me.SURGE = Me.TextBox114.Text

            If Me.ComboBoxTipoVaso.SelectedItem = DWSIM.App.GetLocalString("Vertical") Then

                Me.SizeVertical()

            ElseIf Me.ComboBoxTipoVaso.SelectedItem = DWSIM.App.GetLocalString("Horizontal") Then

                Me.SizeHorizontal()

            End If

        Else

            Me.vessel = Nothing

        End If

    End Sub

    Private Sub SizeVertical()

        Try

            Dim qv = Me.qv * Me.SURGE
            Dim ql = Me.ql * Me.SURGE

            Dim tres = Me.TR

            Dim rho_ml = Me.rhol
            Dim rho_ns = Me.rhoe

            Dim vk = Me.K * ((rho_ml - Me.rhov) / Me.rhov) ^ 0.5
            Dim vp = Me.VGI / 100 * vk
            Dim At = qv / vp

            Dim dmin = (4 * At / Math.PI) ^ 0.5 * 1000
            Dim lmin = Convert.ToDouble(tbRLD.Text) * dmin

            'bocal de entrada
            Dim vmaxbe = Convert.ToDouble(tbC.Text) / (rho_ns) ^ 0.5
            Dim aminbe = (qv + ql) / (vmaxbe)
            Dim dminbe = (4 * aminbe / Math.PI) ^ 0.5 * 39.37

            'bocal de gas
            Dim vmaxbg = Me.C / (Me.rhov) ^ 0.5
            Dim aminbg = (qv) / (vmaxbg)
            Dim dminbg = (4 * aminbg / Math.PI) ^ 0.5 * 39.37

            'bocal de liquido
            Dim vmaxbl = Me.VMAX
            Dim aminbl2 = (ql) / (vmaxbl)
            Dim dminbl = (4 * aminbl2 / Math.PI) ^ 0.5 * 39.37

            TbBE.Text = Format(dminbe, "####0.##")
            TbBSG.Text = Format(dminbg, "####0.##")
            TbBSL.Text = Format(dminbl, "####0.##")
            TbD.Text = Format(dmin, "####0.##")
            TbA.Text = Format(lmin, "####0.##")

        Catch ex As Exception

        End Try

    End Sub

    Private Sub SizeHorizontal()

        Try

            Dim qv = Me.qv * Me.SURGE
            Dim ql = Me.ql * Me.SURGE

            Dim rho_ml = Me.rhol
            Dim rho_ns = Me.rhoe

            Dim x, y, l_d, dv, dl, vl1, vl2, cv As Double

            Dim vk = Me.K * ((rho_ml - Me.rhov) / Me.rhov) ^ 0.5
            Dim vp = Me.VGI / 100 * vk

            'bocal de entrada
            Dim vmaxbe = Convert.ToDouble(tbC.Text) / (rho_ns) ^ 0.5
            Dim aminbe = (qv + ql) / (vmaxbe)
            Dim dminbe = (4 * aminbe / Math.PI) ^ 0.5 * 39.37

            'bocal de gas
            Dim vmaxbg = Me.C / (Me.rhov) ^ 0.5
            Dim aminbg = (qv) / (vmaxbg)
            Dim dminbg = (4 * aminbg / Math.PI) ^ 0.5 * 39.37

            'bocal de liquido
            Dim vmaxbl = Me.VMAX
            Dim aminbl2 = (ql) / (vmaxbl)
            Dim dminbl = (4 * aminbl2 / Math.PI) ^ 0.5 * 39.37

            'vaso
            Dim tr = Me.TR

            l_d = Me.RLD

            x = 0.01
            Do
                y = (1 / Math.PI) * Math.Acos(1 - 2 * x) - (2 / Math.PI) * (1 - 2 * x) * (x * (1 - x)) ^ 0.5
                dv = (4 / Math.PI * qv / (vp)) ^ 0.5 * ((x / y) / l_d) ^ 0.5
                dl = ((4 / (Math.PI * l_d)) * (ql) * Convert.ToDouble(tr * 60) / (1 - y)) ^ (1 / 3)
                x += 0.0001
            Loop Until Math.Abs(dv - dl) < 0.0001 Or x >= 0.5
            vl1 = (ql) * tr / (1 / 60)
            vl2 = (1 - y) * Math.PI * dl ^ 3 / 4 * l_d
            If vl2 < vl1 Then
                Do
                    vl2 = (1 - y) * Math.PI * dl ^ 3 / 4 * l_d
                    dl = dl * 1.001
                Loop Until Math.Abs(vl2 - vl1) < 0.001
            End If

            Dim diam As Double
            If dl > dv Then diam = dl
            If dv > dl Then diam = dv

            cv = l_d * diam

            TbBE.Text = Format(dminbe, "####0.##")
            TbBSG.Text = Format(dminbg, "####0.##")
            TbBSL.Text = Format(dminbl, "####0.##")
            TbD.Text = Format(diam * 1000, "####0.##")
            TbA.Text = Format(cv * 1000, "####0.##")

        Catch ex As Exception


        End Try

    End Sub

    Private Sub FrmDAVP_HelpRequested(sender As System.Object, hlpevent As System.Windows.Forms.HelpEventArgs) Handles MyBase.HelpRequested
        DWSIM.App.HelpRequested("UT_VesselSizing.htm") 'no topic yet
    End Sub

    Public Property AttachedTo As Interfaces.ISimulationObject Implements Interfaces.IAttachedUtility.AttachedTo

    Public Function GetPropertyList() As List(Of String) Implements Interfaces.IAttachedUtility.GetPropertyList
        Return New List(Of String)(New String() {"Name", "AutoUpdate", "L_D", "C", "Tres", "Fsurge", "Vmaxliq", "Vgi", "K", "Type"})
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
            Case "L_D"
                Return Double.Parse(tbRLD.Text)
            Case "C"
                Return Double.Parse(tbC.Text)
            Case "Tres"
                Return Double.Parse(tbTR.Text)
            Case "Fsurge"
                Return Double.Parse(TextBox114.Text)
            Case "Vmaxliq"
                Return Double.Parse(tbVMAX.Text)
            Case "Vgi"
                Return Double.Parse(tbVGI.Text)
            Case "K"
                Return Double.Parse(tbK.Text)
            Case "Type"
                Return ComboBoxTipoVaso.SelectedIndex
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
            Case "L_D"
                tbRLD.Text = pvalue
            Case "C"
                tbC.Text = pvalue
            Case "Tres"
                tbTR.Text = pvalue
            Case "Fsurge"
                TextBox114.Text = pvalue
            Case "Vmaxliq"
                tbVMAX.Text = pvalue
            Case "Vgi"
                tbVGI.Text = pvalue
            Case "K"
                tbK.Text = pvalue
            Case "Type"
                ComboBoxTipoVaso.SelectedIndex = pvalue
        End Select
    End Sub

    Public Sub Update1() Implements Interfaces.IAttachedUtility.Update
        Button1_Click(Me, New EventArgs)
    End Sub

    Public Function GetUtilityType() As Interfaces.Enums.FlowsheetUtility Implements Interfaces.IAttachedUtility.GetUtilityType
        Return FlowsheetUtility.SeparatorSizing
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

End Class