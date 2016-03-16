Imports Microsoft.Msdn.Samples.GraphicObjects
Imports System.Collections.Generic
Imports System.ComponentModel
Imports PropertyGridEx
Imports WeifenLuo.WinFormsUI.Docking

Imports DWSIM.DWSIM.Flowsheet.FlowsheetSolver
Imports DWSIM.DWSIM.SimulationObjects
Imports System.Drawing.Drawing2D
Imports System.Linq
Imports System.Threading.Tasks
Imports DWSIM.DWSIM.FormClasses

Public Class frmSurface
    Inherits DockContent

    Private m_connecting As Boolean = False
    Private m_stPoint As New Point

    Public Flowsheet As FormFlowsheet
    Public PGEx2 As PropertyGridEx.PropertyGridEx
    Public PGEx1 As PropertyGridEx.PropertyGridEx

    Public m_startobj, m_endobj As GraphicObject

    Public m_qt As DWSIM.GraphicObjects.QuickTableGraphic

    Public ticks As Integer

    Public Event ObjectSelected(ByVal sender As FormFlowsheet)

    Public Function ReturnForm(ByVal str As String) As IDockContent

        If str = Me.ToString Then
            Return Me
        Else
            Return Nothing
        End If

    End Function

    Public Sub SetupGraphicsSurface()

        'load up the design surface with the default bounds and margins
        Dim defSettings As Printing.PageSettings = _
            designSurfacePrintDocument.DefaultPageSettings
        With defSettings

            Dim bounds As Rectangle = .Bounds
            Dim horizRes As Integer = .PrinterResolution.X

            Dim vertRes As Integer = .PrinterResolution.Y

            Me.FlowsheetDesignSurface.SurfaceBounds = bounds

            'Me.FlowsheetDesignSurface.GridSize = 50
            Me.FlowsheetDesignSurface.SurfaceMargins = _
                New Rectangle(bounds.Left + .Margins.Left, _
                    bounds.Top + .Margins.Top, _
                    bounds.Width - .Margins.Left - .Margins.Right, _
                    bounds.Height - .Margins.Top - .Margins.Bottom)
        End With


    End Sub

    Private Sub designSurfacePrintDocument_PrintPage(ByVal sender As System.Object, _
            ByVal e As System.Drawing.Printing.PrintPageEventArgs) _
            Handles designSurfacePrintDocument.PrintPage
        Dim drawobj As GraphicObjectCollection = Me.FlowsheetDesignSurface.drawingObjects
        Me.FlowsheetDesignSurface.drawingObjects.PrintObjects(e.Graphics, -FlowsheetDesignSurface.HorizontalScroll.Value / FlowsheetDesignSurface.Zoom, -FlowsheetDesignSurface.VerticalScroll.Value / FlowsheetDesignSurface.Zoom)
    End Sub

    Private Sub frmSurface_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        If TypeOf Me.ParentForm Is FormFlowsheet Then
            Flowsheet = Me.ParentForm
        ElseIf Flowsheet Is Nothing Then
            Flowsheet = My.Application.ActiveSimulation
        End If

        If DWSIM.App.IsRunningOnMono Then
            Me.FlowLayoutPanel1.AutoSize = False
            Me.FlowLayoutPanel1.Height = 25
        End If

        PGEx1 = Me.Flowsheet.FormProps.PGEx1
        PGEx2 = Me.Flowsheet.FormProps.PGEx2

        AddHandler CopyFromTSMI.DropDownItemClicked, AddressOf MaterialStreamClickHandler

    End Sub

    Public Sub UpdateSelectedObject()

        If Not Me.FlowsheetDesignSurface.SelectedObject Is Nothing Then

            Flowsheet.FormProps.SuspendLayout()
            If Me.FlowsheetDesignSurface.SelectedObject.TipoObjeto = TipoObjeto.GO_Tabela Then
                Flowsheet.FormProps.LblNomeObj.Text = DWSIM.App.GetLocalString("Tabela")
                Flowsheet.FormProps.LblTipoObj.Text = DWSIM.App.GetLocalString("TabeladeDados")
                Flowsheet.FormProps.LblStatusObj.Text = "-"
                Flowsheet.FormProps.LblStatusObj.ForeColor = Color.FromKnownColor(KnownColor.ControlText)
            ElseIf Me.FlowsheetDesignSurface.SelectedObject.TipoObjeto = TipoObjeto.GO_MasterTable Then
                Flowsheet.FormProps.LblNomeObj.Text = DWSIM.App.GetLocalString("MasterTable")
                Flowsheet.FormProps.LblTipoObj.Text = DWSIM.App.GetLocalString("MasterTable")
                Flowsheet.FormProps.LblStatusObj.Text = "-"
                Flowsheet.FormProps.LblStatusObj.ForeColor = Color.FromKnownColor(KnownColor.ControlText)
            ElseIf Me.FlowsheetDesignSurface.SelectedObject.TipoObjeto = TipoObjeto.GO_SpreadsheetTable Then
                Flowsheet.FormProps.LblNomeObj.Text = DWSIM.App.GetLocalString("SpreadsheetTable")
                Flowsheet.FormProps.LblTipoObj.Text = DWSIM.App.GetLocalString("SpreadsheetTable")
                Flowsheet.FormProps.LblStatusObj.Text = "-"
                Flowsheet.FormProps.LblStatusObj.ForeColor = Color.FromKnownColor(KnownColor.ControlText)
            ElseIf Me.FlowsheetDesignSurface.SelectedObject.TipoObjeto = TipoObjeto.GO_Figura Then
                Flowsheet.FormProps.LblNomeObj.Text = DWSIM.App.GetLocalString("Figura")
                Flowsheet.FormProps.LblTipoObj.Text = DWSIM.App.GetLocalString("ImagemBitmap")
                Flowsheet.FormProps.LblStatusObj.Text = "-"
                Flowsheet.FormProps.LblStatusObj.ForeColor = Color.FromKnownColor(KnownColor.ControlText)
            ElseIf Me.FlowsheetDesignSurface.SelectedObject.TipoObjeto = TipoObjeto.GO_Texto Then
                Flowsheet.FormProps.LblNomeObj.Text = DWSIM.App.GetLocalString("Texto")
                Flowsheet.FormProps.LblTipoObj.Text = DWSIM.App.GetLocalString("CaixadeTexto")
                Flowsheet.FormProps.LblStatusObj.Text = "-"
                Flowsheet.FormProps.LblStatusObj.ForeColor = Color.FromKnownColor(KnownColor.ControlText)
            Else
                Flowsheet.FormProps.LblNomeObj.Text = Me.FlowsheetDesignSurface.SelectedObject.Tag
                Flowsheet.FormProps.LblTipoObj.Text = DWSIM.App.GetLocalString(Flowsheet.Collections.ObjectCollection.Item(Me.FlowsheetDesignSurface.SelectedObject.Name).Descricao)
                Select Case Me.FlowsheetDesignSurface.SelectedObject.Status
                    Case Status.Calculated
                        Flowsheet.FormProps.LblStatusObj.Text = DWSIM.App.GetLocalString("Calculado")
                        Flowsheet.FormProps.LblStatusObj.ForeColor = Color.SteelBlue
                    Case Status.Calculating
                        Flowsheet.FormProps.LblStatusObj.Text = DWSIM.App.GetLocalString("Calculando")
                        Flowsheet.FormProps.LblStatusObj.ForeColor = Color.YellowGreen
                    Case Status.ErrorCalculating
                        Flowsheet.FormProps.LblStatusObj.Text = DWSIM.App.GetLocalString("NoCalculado")
                        Flowsheet.FormProps.LblStatusObj.ForeColor = Color.Red
                    Case Status.Inactive
                        Flowsheet.FormProps.LblStatusObj.Text = DWSIM.App.GetLocalString("Inativo")
                        Flowsheet.FormProps.LblStatusObj.ForeColor = Color.Gray
                    Case Status.Idle
                        Flowsheet.FormProps.LblStatusObj.Text = DWSIM.App.GetLocalString("Calculado")
                        Flowsheet.FormProps.LblStatusObj.ForeColor = Color.SteelBlue
                End Select
            End If
            Flowsheet.PopulatePGEx2(Me.FlowsheetDesignSurface.SelectedObject)
            Try
                If Me.FlowsheetDesignSurface.SelectedObject.TipoObjeto = TipoObjeto.GO_Tabela Then
                    CType(Me.FlowsheetDesignSurface.SelectedObject, DWSIM.GraphicObjects.TableGraphic).PopulateGrid(PGEx1)
                ElseIf Me.FlowsheetDesignSurface.SelectedObject.TipoObjeto = TipoObjeto.GO_MasterTable Then
                    CType(Me.FlowsheetDesignSurface.SelectedObject, DWSIM.GraphicObjects.MasterTableGraphic).PopulateGrid(PGEx1, Flowsheet)
                ElseIf Me.FlowsheetDesignSurface.SelectedObject.TipoObjeto = TipoObjeto.GO_SpreadsheetTable Then
                    CType(Me.FlowsheetDesignSurface.SelectedObject, DWSIM.GraphicObjects.SpreadsheetTableGraphic).PopulateGrid(PGEx1)
                Else
                    Flowsheet.Collections.ObjectCollection(Me.FlowsheetDesignSurface.SelectedObject.Name).PopulatePropertyGrid(PGEx1, Flowsheet.Options.SelectedUnitSystem)
                End If
                Flowsheet.FormProps.ResumeLayout()
            Catch ex As Exception
                PGEx1.SelectedObject = Nothing
                MessageBox.Show(ex.Message & " - " & ex.StackTrace, DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
            Finally
                PGEx2.Refresh()
                PGEx1.Refresh()
            End Try

        Else

            If Not PGEx2 Is Nothing Then PGEx2.SelectedObject = Nothing
            If Not PGEx1 Is Nothing Then PGEx1.SelectedObject = Nothing

        End If

    End Sub

    Private Sub FlowsheetDesignSurface_KeyDown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles FlowsheetDesignSurface.KeyDown

        If e.KeyCode = Keys.E And e.Control Then
            Call Me.EditCompTSMI_Click(sender, e)
        ElseIf e.KeyCode = Keys.F5 Then
            If My.Computer.Keyboard.ShiftKeyDown Then
                Flowsheet.tsbDesat.Checked = False
                Flowsheet.tsbAtivar.Checked = True
                Flowsheet.Options.CalculatorActivated = True
            End If
            CalculateAll2(Me.Flowsheet, My.Settings.SolverMode)
        ElseIf e.KeyCode = Keys.F6 Then
            If Not Me.FlowsheetDesignSurface.SelectedObject Is Nothing Then RecalcularToolStripMenuItem_Click(sender, e)
        ElseIf e.KeyCode = Keys.D And e.Control Then
            Flowsheet.tsbAtivar.Checked = False
            Flowsheet.tsbDesat.Checked = True
            Flowsheet.Options.CalculatorActivated = False
            Flowsheet.FormSurface.LabelCalculator.Text = DWSIM.App.GetLocalString("CalculadorDesativado1")
        ElseIf e.KeyCode = Keys.A And e.Control Then
            Flowsheet.tsbDesat.Checked = False
            Flowsheet.tsbAtivar.Checked = True
            Flowsheet.Options.CalculatorActivated = True
            Flowsheet.FormSurface.LabelCalculator.Text = DWSIM.App.GetLocalString("CalculadorOcioso")
            Flowsheet.WriteToLog(DWSIM.App.GetLocalString("Calculadorativado"), Color.DimGray, DWSIM.FormClasses.TipoAviso.Informacao)
        ElseIf e.KeyCode = Keys.X And e.Control Then
            Flowsheet.tsmiCut_Click(Me, New EventArgs)
        ElseIf e.KeyCode = Keys.C And e.Control Then
            Flowsheet.tsmiCopy_Click(Me, New EventArgs)
        ElseIf e.KeyCode = Keys.V And e.Control Then
            Flowsheet.tsmiPaste_Click(Me, New EventArgs)
        ElseIf e.KeyCode = Keys.Delete Then
            Flowsheet.tsmiRemoveSelected_Click(Me, New EventArgs)
        End If

        If Not Me.FlowsheetDesignSurface.SelectedObject Is Nothing Then
            For Each go As GraphicObject In Me.FlowsheetDesignSurface.SelectedObjects.Values
                If e.KeyCode = Keys.Up Then
                    If e.Modifiers = Keys.Control Then
                        go.Y = go.Y - 1
                    Else
                        go.Y = go.Y - 5
                    End If
                ElseIf e.KeyCode = Keys.Down Then
                    If e.Modifiers = Keys.Control Then
                        go.Y = go.Y + 1
                    Else
                        go.Y = go.Y + 5
                    End If
                ElseIf e.KeyCode = Keys.Left Then
                    If e.Modifiers = Keys.Control Then
                        go.X = go.X - 1
                    Else
                        go.X = go.X - 5
                    End If
                ElseIf e.KeyCode = Keys.Right Then
                    If e.Modifiers = Keys.Control Then
                        go.X = go.X + 1
                    Else
                        go.X = go.X + 5
                    End If
                End If
            Next
            Me.FlowsheetDesignSurface.Invalidate()
        Else
            If e.KeyCode = Keys.Up Then
                If Me.FlowsheetDesignSurface.VerticalScroll.Value > 4 * Me.FlowsheetDesignSurface.VerticalScroll.SmallChange Then
                    Me.FlowsheetDesignSurface.VerticalScroll.Value -= 4 * Me.FlowsheetDesignSurface.VerticalScroll.SmallChange
                Else
                    Me.FlowsheetDesignSurface.VerticalScroll.Value = 0
                End If
            ElseIf e.KeyCode = Keys.Down Then
                Me.FlowsheetDesignSurface.VerticalScroll.Value += 4 * Me.FlowsheetDesignSurface.VerticalScroll.SmallChange
            ElseIf e.KeyCode = Keys.Left Then
                If Me.FlowsheetDesignSurface.HorizontalScroll.Value > 4 * Me.FlowsheetDesignSurface.HorizontalScroll.SmallChange Then
                    Me.FlowsheetDesignSurface.HorizontalScroll.Value -= 4 * Me.FlowsheetDesignSurface.HorizontalScroll.SmallChange
                Else
                    Me.FlowsheetDesignSurface.HorizontalScroll.Value = 0
                End If
            ElseIf e.KeyCode = Keys.Right Then
                Me.FlowsheetDesignSurface.HorizontalScroll.Value += 4 * Me.FlowsheetDesignSurface.HorizontalScroll.SmallChange
            End If
            Me.FlowsheetDesignSurface.Invalidate()
            Me.FlowsheetDesignSurface.Invalidate()
        End If

    End Sub

    Private Sub FlowsheetDesignSurface_SelectionChanged(ByVal sender As Object, _
            ByVal e As Microsoft.Msdn.Samples.DesignSurface.SelectionChangedEventArgs) Handles FlowsheetDesignSurface.SelectionChanged

        If Not e.SelectedObject Is Nothing Then

            If Not e.SelectedObject.IsConnector Then
                If Me.FlowsheetDesignSurface.SelectedObject.TipoObjeto = TipoObjeto.GO_Tabela Then
                    Flowsheet.FormProps.LblNomeObj.Text = DWSIM.App.GetLocalString("Tabela")
                    Flowsheet.FormProps.LblTipoObj.Text = DWSIM.App.GetLocalString("TabeladeDados")
                    Flowsheet.FormProps.LblStatusObj.Text = "-"
                    Flowsheet.FormProps.LblStatusObj.ForeColor = Color.FromKnownColor(KnownColor.ControlText)
                ElseIf Me.FlowsheetDesignSurface.SelectedObject.TipoObjeto = TipoObjeto.GO_MasterTable Then
                    Flowsheet.FormProps.LblNomeObj.Text = "MasterTable"
                    Flowsheet.FormProps.LblTipoObj.Text = DWSIM.App.GetLocalString("MasterTable")
                    Flowsheet.FormProps.LblStatusObj.Text = "-"
                    Flowsheet.FormProps.LblStatusObj.ForeColor = Color.FromKnownColor(KnownColor.ControlText)
                ElseIf Me.FlowsheetDesignSurface.SelectedObject.TipoObjeto = TipoObjeto.GO_SpreadsheetTable Then
                    Flowsheet.FormProps.LblNomeObj.Text = "SpreadsheetTable"
                    Flowsheet.FormProps.LblTipoObj.Text = DWSIM.App.GetLocalString("SpreadsheetTable")
                    Flowsheet.FormProps.LblStatusObj.Text = "-"
                    Flowsheet.FormProps.LblStatusObj.ForeColor = Color.FromKnownColor(KnownColor.ControlText)
                ElseIf Me.FlowsheetDesignSurface.SelectedObject.TipoObjeto = TipoObjeto.GO_Figura Then
                    Flowsheet.FormProps.LblNomeObj.Text = DWSIM.App.GetLocalString("Figura")
                    Flowsheet.FormProps.LblTipoObj.Text = DWSIM.App.GetLocalString("ImagemBitmap")
                    Flowsheet.FormProps.LblStatusObj.Text = "-"
                    Flowsheet.FormProps.LblStatusObj.ForeColor = Color.FromKnownColor(KnownColor.ControlText)
                ElseIf Me.FlowsheetDesignSurface.SelectedObject.TipoObjeto = TipoObjeto.GO_Texto Then
                    Flowsheet.FormProps.LblNomeObj.Text = DWSIM.App.GetLocalString("Texto")
                    Flowsheet.FormProps.LblTipoObj.Text = DWSIM.App.GetLocalString("CaixadeTexto")
                    Flowsheet.FormProps.LblStatusObj.Text = "-"
                    Flowsheet.FormProps.LblStatusObj.ForeColor = Color.FromKnownColor(KnownColor.ControlText)
                ElseIf Not Me.FlowsheetDesignSurface.SelectedObject.TipoObjeto = TipoObjeto.GO_TabelaRapida Then
                    Flowsheet.FormProps.LblNomeObj.Text = e.SelectedObject.Tag
                    Flowsheet.FormProps.LblTipoObj.Text = DWSIM.App.GetLocalString(e.SelectedObject.Description)
                    If e.SelectedObject.Active = False Then
                        Flowsheet.FormProps.LblStatusObj.Text = DWSIM.App.GetLocalString("Inativo")
                        Flowsheet.FormProps.LblStatusObj.ForeColor = Color.DimGray
                    Else
                        Select Case Me.FlowsheetDesignSurface.SelectedObject.Status
                            Case Status.Calculated
                                Flowsheet.FormProps.LblStatusObj.Text = DWSIM.App.GetLocalString("Calculado")
                                Flowsheet.FormProps.LblStatusObj.ForeColor = Color.SteelBlue
                            Case Status.Calculating
                                Flowsheet.FormProps.LblStatusObj.Text = DWSIM.App.GetLocalString("Calculando")
                                Flowsheet.FormProps.LblStatusObj.ForeColor = Color.YellowGreen
                            Case Status.ErrorCalculating
                                Flowsheet.FormProps.LblStatusObj.Text = DWSIM.App.GetLocalString("NoCalculado")
                                Flowsheet.FormProps.LblStatusObj.ForeColor = Color.Red
                            Case Status.Inactive
                                Flowsheet.FormProps.LblStatusObj.Text = DWSIM.App.GetLocalString("Inativo")
                                Flowsheet.FormProps.LblStatusObj.ForeColor = Color.Gray
                            Case Status.Idle
                                Flowsheet.FormProps.LblStatusObj.Text = DWSIM.App.GetLocalString("Calculado")
                                Flowsheet.FormProps.LblStatusObj.ForeColor = Color.SteelBlue
                        End Select
                    End If
                End If
                If Not Me.FlowsheetDesignSurface.SelectedObject Is Nothing Then
                    If Me.FlowsheetDesignSurface.SelectedObject.IsConnector = False Then
                        Flowsheet.PopulatePGEx2(Me.FlowsheetDesignSurface.SelectedObject)
                        Try
                            If Me.FlowsheetDesignSurface.SelectedObject.TipoObjeto = TipoObjeto.GO_Tabela Then
                                CType(Me.FlowsheetDesignSurface.SelectedObject, DWSIM.GraphicObjects.TableGraphic).PopulateGrid(PGEx1)
                            ElseIf Me.FlowsheetDesignSurface.SelectedObject.TipoObjeto = TipoObjeto.GO_MasterTable Then
                                CType(Me.FlowsheetDesignSurface.SelectedObject, DWSIM.GraphicObjects.MasterTableGraphic).PopulateGrid(PGEx1, Flowsheet)
                            ElseIf Me.FlowsheetDesignSurface.SelectedObject.TipoObjeto = TipoObjeto.GO_SpreadsheetTable Then
                                CType(Me.FlowsheetDesignSurface.SelectedObject, DWSIM.GraphicObjects.SpreadsheetTableGraphic).PopulateGrid(PGEx1)
                            Else
                                Flowsheet.Collections.ObjectCollection(Me.FlowsheetDesignSurface.SelectedObject.Name).PopulatePropertyGrid(PGEx1, Flowsheet.Options.SelectedUnitSystem)
                            End If
                            'Flowsheet.FormProps.ResumeLayout()
                        Catch ex As Exception
                            Flowsheet.WriteToLog("Error: " & ex.ToString, Color.Red, DWSIM.FormClasses.TipoAviso.Erro)
                        Finally
                            Flowsheet.FormSurface.Select()
                        End Try
                    Else
                        Me.FlowsheetDesignSurface.SelectedObject = Nothing
                    End If
                Else
                    PGEx2.SelectedObject = Nothing
                    PGEx1.SelectedObject = Nothing
                End If
                PGEx2.Refresh()
                PGEx1.Refresh()

                Flowsheet.ChangeEditMenuStatus(True)

            Else

                Me.FlowsheetDesignSurface.SelectedObject = Nothing

            End If

        Else

            PGEx2.SelectedObject = Nothing
            PGEx1.SelectedObject = Nothing

            Flowsheet.ChangeEditMenuStatus(False)

        End If

        If Me.FlowsheetDesignSurface.SelectedObject Is Nothing Then

            Flowsheet.FormProps.LblNomeObj.Text = DWSIM.App.GetLocalString("Nenhumselecionado")
            Flowsheet.FormProps.LblTipoObj.Text = "-"
            Flowsheet.FormProps.LblStatusObj.Text = "-"
            Flowsheet.FormProps.LblStatusObj.ForeColor = Color.FromKnownColor(KnownColor.ControlText)

        End If

    End Sub

    Private Sub ToolStripMenuItem6_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripMenuItem6.Click
        If DWSIM.App.IsRunningOnMono Then
            MessageBox.Show(DWSIM.App.GetLocalString("Unsupported_Feature"), "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
        Else
            If Me.FlowsheetDesignSurface.SelectedObject.Rotation + 90 >= 360 Then
                Me.FlowsheetDesignSurface.SelectedObject.Rotation = Math.Truncate((Me.FlowsheetDesignSurface.SelectedObject.Rotation + 90) / 360)
            Else
                Me.FlowsheetDesignSurface.SelectedObject.Rotation = Me.FlowsheetDesignSurface.SelectedObject.Rotation + 90
            End If
            Me.FlowsheetDesignSurface.Invalidate()
        End If
    End Sub

    Private Sub BToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles BToolStripMenuItem.Click
        If DWSIM.App.IsRunningOnMono Then
            MessageBox.Show(DWSIM.App.GetLocalString("Unsupported_Feature"), "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
        Else
            If Me.FlowsheetDesignSurface.SelectedObject.Rotation + 180 >= 360 Then
                Me.FlowsheetDesignSurface.SelectedObject.Rotation = Math.Truncate((Me.FlowsheetDesignSurface.SelectedObject.Rotation + 180) / 360)
            Else
                Me.FlowsheetDesignSurface.SelectedObject.Rotation = Me.FlowsheetDesignSurface.SelectedObject.Rotation + 180
            End If
            Me.FlowsheetDesignSurface.Invalidate()
        End If
    End Sub

    Private Sub ToolStripMenuItem7_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripMenuItem7.Click
        If DWSIM.App.IsRunningOnMono Then
            MessageBox.Show(DWSIM.App.GetLocalString("Unsupported_Feature"), "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
        Else
            If Me.FlowsheetDesignSurface.SelectedObject.Rotation + 270 >= 360 Then
                Me.FlowsheetDesignSurface.SelectedObject.Rotation = Math.Truncate((Me.FlowsheetDesignSurface.SelectedObject.Rotation + 270) / 360)
            Else
                Me.FlowsheetDesignSurface.SelectedObject.Rotation = Me.FlowsheetDesignSurface.SelectedObject.Rotation + 270
            End If
            Me.FlowsheetDesignSurface.Invalidate()
        End If
    End Sub

    Private Sub FlowsheetDesignSurface_StatusUpdate(ByVal sender As Object, ByVal e As Microsoft.Msdn.Samples.DesignSurface.StatusUpdateEventArgs) Handles FlowsheetDesignSurface.StatusUpdate
        Flowsheet.TSTBZoom.Text = Format(FlowsheetDesignSurface.Zoom, "#%")
    End Sub

    Private Sub FlowsheetDesignSurface_MouseClick(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles FlowsheetDesignSurface.MouseDown

        If e.Button = Windows.Forms.MouseButtons.Left Then

            If Me.FlowsheetDesignSurface.QuickConnect And My.Computer.Keyboard.CtrlKeyDown Then
                Dim mousePT As New Point(Flowsheet.gscTogoc(e.X, e.Y))
                Dim mpx = mousePT.X
                Dim mpy = mousePT.Y
                Me.m_stPoint = mousePT
                Dim myCTool As New ConnectToolGraphic(mousePT)
                myCTool.Name = "CTOOL1234567890"
                myCTool.Width = mousePT.X
                myCTool.Height = mousePT.Y
                Me.m_startobj = Me.FlowsheetDesignSurface.drawingObjects.FindObjectAtPoint(mousePT)
                Me.FlowsheetDesignSurface.drawingObjects.Add(myCTool)
                Me.m_connecting = True
            Else
                Me.FlowsheetDesignSurface.SelectRectangle = True
            End If

            Me.FlowsheetDesignSurface.Invalidate()

        End If

    End Sub

    Public Sub FlowsheetDesignSurface_MouseUp(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles FlowsheetDesignSurface.MouseUp

        If Me.m_connecting Then

            Me.m_connecting = False

            Me.FlowsheetDesignSurface.drawingObjects.Remove(Me.FlowsheetDesignSurface.drawingObjects.FindObjectWithName("CTOOL1234567890"))

            Me.m_endobj = Me.FlowsheetDesignSurface.drawingObjects.FindObjectAtPoint(Flowsheet.gscTogoc(e.X, e.Y))

            Me.FlowsheetDesignSurface.SelectRectangle = True
            Me.FlowsheetDesignSurface.Invalidate()

            If Not m_startobj Is Nothing And Not m_endobj Is Nothing Then
                If m_startobj.Name <> m_endobj.Name Then
                    Try
                        Flowsheet.ConnectObject(Me.m_startobj, Me.m_endobj)
                    Catch ex As Exception
                        Flowsheet.WriteToLog(ex.Message.ToString, Color.Red, DWSIM.FormClasses.TipoAviso.Erro)
                    End Try
                End If
            End If
        End If

        If e.Button = Windows.Forms.MouseButtons.Left Then

            If Not Flowsheet.ClickedToolStripMenuItem Is Nothing Then
                If Flowsheet.InsertingObjectToPFD Then

                    Dim gObj As GraphicObject = Nothing
                    Dim fillclr As Color = Color.WhiteSmoke
                    Dim lineclr As Color = Color.Red
                    Dim mousePT As Point = Flowsheet.gscTogoc(e.X, e.Y)
                    Dim mpx = mousePT.X '- SplitContainer1.SplitterDistance
                    Dim mpy = mousePT.Y '- ToolStripContainer1.TopToolStripPanel.Height
                    Dim tobj As TipoObjeto = TipoObjeto.Nenhum

                    Select Case Flowsheet.ClickedToolStripMenuItem.Name

                        Case "TSMIMaterialStream"
                            tobj = TipoObjeto.MaterialStream
                        Case "TSMIEnergyStream"
                            tobj = TipoObjeto.EnergyStream
                        Case "TSMIMixer"
                            tobj = TipoObjeto.NodeIn
                        Case "TSMISplitter"
                            tobj = TipoObjeto.NodeOut
                        Case "TSMICompressor"
                            tobj = TipoObjeto.Compressor
                        Case "TSMIExpander"
                            tobj = TipoObjeto.Expander
                        Case "TSMIPump"
                            tobj = TipoObjeto.Pump
                        Case "TSMIPipe"
                            tobj = TipoObjeto.Pipe
                        Case "TSMIValve"
                            tobj = TipoObjeto.Valve
                        Case "TSMISeparator"
                            tobj = TipoObjeto.Vessel
                        Case "TSMIHeater"
                            tobj = TipoObjeto.Heater
                        Case "TSMICooler"
                            tobj = TipoObjeto.Cooler
                        Case "TSMIOrificePlate"
                            tobj = TipoObjeto.OrificePlate
                        Case "TSMIComponentSeparator"
                            tobj = TipoObjeto.ComponentSeparator
                        Case "TSMIHeatExchanger"
                            tobj = TipoObjeto.HeatExchanger
                        Case "TSMITank"
                            tobj = TipoObjeto.Tank
                        Case "TSMIColShortcut"
                            tobj = TipoObjeto.ShortcutColumn
                        Case "TSMIColDist"
                            tobj = TipoObjeto.DistillationColumn
                        Case "TSMIColAbs"
                            tobj = TipoObjeto.AbsorptionColumn
                        Case "TSMIColAbsReb"
                            tobj = TipoObjeto.ReboiledAbsorber
                        Case "TSMIColAbsCond"
                            tobj = TipoObjeto.RefluxedAbsorber
                        Case "TSMIReactorConv"
                            tobj = TipoObjeto.RCT_Conversion
                        Case "TSMIReactorEquilibrium"
                            tobj = TipoObjeto.RCT_Equilibrium
                        Case "TSMIReactorGibbs"
                            tobj = TipoObjeto.RCT_Gibbs
                        Case "TSMIReactorCSTR"
                            tobj = TipoObjeto.RCT_CSTR
                        Case "TSMIReactorPFR"
                            tobj = TipoObjeto.RCT_PFR
                        Case "TSMIRecycle"
                            tobj = TipoObjeto.OT_Reciclo
                        Case "TSMIEnergyRecycle"
                            tobj = TipoObjeto.OT_EnergyRecycle
                        Case "TSMIAdjust"
                            tobj = TipoObjeto.OT_Ajuste
                        Case "TSMISpecification"
                            tobj = TipoObjeto.OT_Especificacao
                        Case "TSMICUO"
                            tobj = TipoObjeto.CustomUO
                        Case "TSMIExcelUO"
                            tobj = TipoObjeto.ExcelUO
                        Case "TSMICOUO"
                            tobj = TipoObjeto.CapeOpenUO
                        Case "TSMISolidsSeparator"
                            tobj = TipoObjeto.SolidSeparator
                        Case "TSMIFilter"
                            tobj = TipoObjeto.Filter
                        Case "TSMIFlowsheet"
                            tobj = TipoObjeto.FlowsheetUO
                    End Select

                    AddObjectToSurface(tobj, mpx, mpy)

                    Flowsheet.ClickedToolStripMenuItem = Nothing
                    Flowsheet.InsertingObjectToPFD = False

                    If tobj = TipoObjeto.MaterialStream Then
                        Call Me.EditCompTSMI_Click(Me.FlowsheetDesignSurface, New EventArgs())
                    End If

                End If

            End If

            'If Not Me.FlowsheetDesignSurface.SelectedObject Is Nothing Then

            '    If Me.FlowsheetDesignSurface.SelectedObject.IsConnector = False Then

            '        Flowsheet.PopulatePGEx2(Me.FlowsheetDesignSurface.SelectedObject)
            '        Try
            '            Flowsheet.Collections.ObjectCollection(Me.FlowsheetDesignSurface.SelectedObject.Name).PopulatePropertyGrid(PGEx1, Flowsheet.Options.SelectedUnitSystem)
            '            Flowsheet.FormProps.ResumeLayout()
            '        Catch ex As Exception
            '            PGEx1.SelectedObject = Nothing
            '            'MessageBox.Show(ex.Message & " - " & ex.StackTrace, DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
            '        Finally
            '            Flowsheet.FormSurface.Select()
            '        End Try

            '    Else

            '        Me.FlowsheetDesignSurface.SelectedObject = Nothing

            '    End If


            'Else

            '    PGEx2.SelectedObject = Nothing
            '    PGEx1.SelectedObject = Nothing

            'End If
            'PGEx2.Refresh()
            'PGEx1.Refresh()

        ElseIf e.Button = Windows.Forms.MouseButtons.Right Then

            If Not Me.FlowsheetDesignSurface.SelectedObject Is Nothing Then

                Me.CMS_Sel.Items("TSMI_Label").Text = Me.FlowsheetDesignSurface.SelectedObject.Tag
                Me.CMS_Sel.Show(MousePosition)

            Else

                Me.CMS_NoSel.Show(MousePosition)

            End If

        End If

        RaiseEvent ObjectSelected(Flowsheet)

    End Sub

    Private Sub FlowsheetDesignSurface_MouseMove(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles FlowsheetDesignSurface.MouseMove

        Dim px As Point = Flowsheet.gscTogoc(e.Location.X, e.Location.Y)
        Dim px2 As Point = Flowsheet.gscTogoc(e.Location.X + 20, e.Location.Y + 20)

        If Me.m_connecting Then

            Dim myCTool As ConnectToolGraphic = Me.FlowsheetDesignSurface.drawingObjects.FindObjectWithName("CTOOL1234567890")

            Dim mousePT As New Point(Flowsheet.gscTogoc(e.X, e.Y))

            myCTool.Width = mousePT.X
            myCTool.Height = mousePT.Y

        ElseIf Not FlowsheetDesignSurface.dragging Then

            Dim gobj As GraphicObject = Me.FlowsheetDesignSurface.drawingObjects.FindObjectAtPoint(px)

            If Not gobj Is Nothing Then

                If Me.m_qt Is Nothing And Not _
                    gobj.TipoObjeto = TipoObjeto.GO_TabelaRapida And Not _
                    gobj.TipoObjeto = TipoObjeto.GO_MasterTable And Not _
                    gobj.TipoObjeto = TipoObjeto.GO_SpreadsheetTable And Not _
                    gobj.TipoObjeto = TipoObjeto.GO_Tabela And Not _
                    gobj.TipoObjeto = TipoObjeto.GO_Figura And Not _
                    gobj.TipoObjeto = TipoObjeto.GO_Texto And Not _
                    gobj.TipoObjeto = TipoObjeto.Nenhum Then

                    If gobj.Calculated Then

                        If Flowsheet.Collections.ObjectCollection.ContainsKey(gobj.Name) Then

                            Dim obj As DWSIM.SimulationObjects.UnitOperations.BaseClass = Flowsheet.Collections.ObjectCollection(gobj.Name)

                            If obj.TabelaRapida Is Nothing Then

                                If Not obj.QTNodeTableItems Is Nothing Then

                                    Dim tabela As New DWSIM.GraphicObjects.QuickTableGraphic(obj, px2.X + 5, px2.Y + 5)
                                    obj.TabelaRapida = tabela
                                    obj.TabelaRapida.BaseOwner = obj
                                    tabela.Tag = obj.Nome
                                    tabela.Name = "QTAB-" & Guid.NewGuid.ToString
                                    tabela.HeaderText = gobj.Tag
                                    tabela.AdditionalInfo = Me.FlowsheetDesignSurface.Zoom
                                    Me.m_qt = tabela
                                    If Not Me.m_qt Is Nothing Then
                                        If FlowsheetDesignSurface.AutoScrollPosition.X + px2.X * FlowsheetDesignSurface.Zoom + m_qt.Width * FlowsheetDesignSurface.Zoom > FlowsheetDesignSurface.ClientRectangle.Width Then
                                            px2.X -= 50 + m_qt.Width / FlowsheetDesignSurface.Zoom
                                        End If
                                        If FlowsheetDesignSurface.AutoScrollPosition.Y + px2.Y * FlowsheetDesignSurface.Zoom + m_qt.Height * FlowsheetDesignSurface.Zoom > FlowsheetDesignSurface.ClientRectangle.Height Then
                                            px2.Y -= 50 + m_qt.Height / FlowsheetDesignSurface.Zoom
                                        End If
                                        Me.m_qt.SetPosition(px2)
                                    End If
                                    Me.FlowsheetDesignSurface.drawingObjects.Add(tabela)
                                    Me.ticks = 0

                                End If

                            Else

                                Me.m_qt = obj.TabelaRapida
                                Me.m_qt.AdditionalInfo = Me.FlowsheetDesignSurface.Zoom

                                If Not Me.m_qt Is Nothing Then
                                    If FlowsheetDesignSurface.AutoScrollPosition.X + px2.X * FlowsheetDesignSurface.Zoom + m_qt.Width * FlowsheetDesignSurface.Zoom > FlowsheetDesignSurface.ClientRectangle.Width Then
                                        px2.X -= 50 + m_qt.Width / FlowsheetDesignSurface.Zoom
                                    End If
                                    If FlowsheetDesignSurface.AutoScrollPosition.Y + px2.Y * FlowsheetDesignSurface.Zoom + m_qt.Height * FlowsheetDesignSurface.Zoom > FlowsheetDesignSurface.ClientRectangle.Height Then
                                        px2.Y -= 50 + m_qt.Height / FlowsheetDesignSurface.Zoom
                                    End If
                                    Me.m_qt.SetPosition(px2)
                                End If

                                If Not Me.FlowsheetDesignSurface.drawingObjects.Contains(obj.TabelaRapida) Then
                                    obj.TabelaRapida.BaseOwner = obj
                                    Me.FlowsheetDesignSurface.drawingObjects.Add(obj.TabelaRapida)
                                End If

                            End If

                        End If

                    End If

                ElseIf gobj.TipoObjeto = TipoObjeto.GO_TabelaRapida Then

                    If Me.FlowsheetDesignSurface.drawingObjects.Contains(Me.m_qt) Then
                        Me.FlowsheetDesignSurface.drawingObjects.Remove(Me.m_qt)
                    End If
                    Me.m_qt = Nothing
                    Me.ticks = 0

                End If

            Else

                Try
                    If Me.FlowsheetDesignSurface.drawingObjects.Contains(Me.m_qt) Then
                        Me.FlowsheetDesignSurface.drawingObjects.Remove(Me.m_qt)
                    End If
                    Me.m_qt = Nothing
                    Me.ticks = 0
                Catch ex As Exception
                    Console.WriteLine(ex.Message)
                End Try

            End If

        Else

            Try
                If Me.FlowsheetDesignSurface.drawingObjects.Contains(Me.m_qt) Then
                    Me.FlowsheetDesignSurface.drawingObjects.Remove(Me.m_qt)
                End If
                Me.m_qt = Nothing
                Me.ticks = 0
            Catch ex As Exception
                Console.WriteLine(ex.Message)
            End Try

        End If

        'If Not Me.m_qt Is Nothing Then Me.m_qt.SetPosition(px2)
        Me.FlowsheetDesignSurface.Invalidate()

    End Sub

    Private Sub ConfigurarToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ConfigurarToolStripMenuItem.Click

        Dim fct As New FormConfigureTable

        Flowsheet = My.Application.ActiveSimulation

        Dim obj As DWSIM.SimulationObjects.UnitOperations.BaseClass = Flowsheet.Collections.ObjectCollection(Me.FlowsheetDesignSurface.SelectedObject.Name)
        'obj.FillNodeItems()
        Dim ni As DWSIM.Outros.NodeItem
        If obj.NodeTableItems.Count > 0 Then
            For Each nti As DWSIM.Outros.NodeItem In obj.NodeTableItems.Values
                If DWSIM.App.GetPropertyName(nti.Text) = nti.Text Then
                    obj.NodeTableItems = New System.Collections.Generic.Dictionary(Of Integer, DWSIM.Outros.NodeItem)
                    obj.FillNodeItems()
                    Exit For
                End If
            Next
        End If
        If obj.NodeTableItems Is Nothing Then
            obj.NodeTableItems = New System.Collections.Generic.Dictionary(Of Integer, DWSIM.Outros.NodeItem)
            obj.FillNodeItems()
        End If
        For Each ni In obj.NodeTableItems.Values
            fct.NodeItems.Add(ni.Key, New DWSIM.Outros.NodeItem(ni.Text, ni.Value, ni.Unit, ni.Key, ni.Level, ni.ParentNode))
            fct.NodeItems(ni.Key).Checked = ni.Checked
        Next
        fct.objname = obj.Nome
        fct.ShowDialog(Me)

    End Sub

    Private Sub MostrarToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MostrarToolStripMenuItem.Click
        If Not Me.FlowsheetDesignSurface.SelectedObject Is Nothing Then
            If Me.MostrarToolStripMenuItem.Checked = True Then
                Dim obj As DWSIM.SimulationObjects.UnitOperations.BaseClass = Flowsheet.Collections.ObjectCollection(Me.FlowsheetDesignSurface.SelectedObject.Name)
                Dim tabela As New DWSIM.GraphicObjects.TableGraphic(obj, Me.FlowsheetDesignSurface.SelectedObject.X + Me.FlowsheetDesignSurface.SelectedObject.Width, Me.FlowsheetDesignSurface.SelectedObject.Y + Me.FlowsheetDesignSurface.SelectedObject.Height)
                obj.Tabela = tabela
                tabela.Tag = obj.Nome
                tabela.Name = "TAB-" & Guid.NewGuid.ToString
                tabela.HeaderText = Me.FlowsheetDesignSurface.SelectedObject.Tag
                Me.FlowsheetDesignSurface.drawingObjects.Add(tabela)
                Me.FlowsheetDesignSurface.Invalidate()
            Else
                Dim obj As DWSIM.SimulationObjects.UnitOperations.BaseClass = Flowsheet.Collections.ObjectCollection(Me.FlowsheetDesignSurface.SelectedObject.Name)
                Me.FlowsheetDesignSurface.drawingObjects.Remove(obj.Tabela)
                obj.Tabela = Nothing
                Me.FlowsheetDesignSurface.Invalidate()
            End If
        End If
    End Sub

    Private Sub CMS_Sel_Enter(ByVal sender As Object, ByVal e As System.EventArgs) Handles CMS_Sel.Opened

        Dim naoLimparListaDeDesconectar As Boolean = False

        Me.CMS_ItemsToDisconnect.Items.Clear()
        Me.CMS_ItemsToConnect.Items.Clear()
        Me.CopyFromTSMI.DropDownItems.Clear()

        Me.DesconectarDeToolStripMenuItem.Visible = False
        Me.ConectarAToolStripMenuItem.Visible = False
        Me.ToolStripSeparator3.Visible = False
        Me.CopyFromTSMI.Visible = False
        Me.RestoreTSMI.Visible = False

        DepurarObjetoToolStripMenuItem.Visible = Flowsheet.Collections.ObjectCollection.ContainsKey(Me.FlowsheetDesignSurface.SelectedObject.Name)

        If Me.FlowsheetDesignSurface.SelectedObject.TipoObjeto <> TipoObjeto.GO_Figura And _
            Me.FlowsheetDesignSurface.SelectedObject.TipoObjeto <> TipoObjeto.GO_Tabela And _
            Me.FlowsheetDesignSurface.SelectedObject.TipoObjeto <> TipoObjeto.GO_MasterTable And _
            Me.FlowsheetDesignSurface.SelectedObject.TipoObjeto <> TipoObjeto.GO_SpreadsheetTable And _
            Me.FlowsheetDesignSurface.SelectedObject.TipoObjeto <> TipoObjeto.GO_TabelaRapida And _
            Me.FlowsheetDesignSurface.SelectedObject.TipoObjeto <> TipoObjeto.DistillationColumn And _
            Me.FlowsheetDesignSurface.SelectedObject.TipoObjeto <> TipoObjeto.AbsorptionColumn And _
             Me.FlowsheetDesignSurface.SelectedObject.TipoObjeto <> TipoObjeto.ReboiledAbsorber And _
             Me.FlowsheetDesignSurface.SelectedObject.TipoObjeto <> TipoObjeto.RefluxedAbsorber And _
            Me.FlowsheetDesignSurface.SelectedObject.TipoObjeto <> TipoObjeto.GO_Texto Then

            Me.RecalcularToolStripMenuItem.Visible = True
            Me.ToolStripSeparator6.Visible = True
            Me.TabelaToolStripMenuItem.Visible = True
            Me.ClonarToolStripMenuItem.Visible = True
            Me.ExcluirToolStripMenuItem.Visible = True
            Me.HorizontalmenteToolStripMenuItem.Visible = True

            Try

                Dim obj As DWSIM.SimulationObjects.UnitOperations.BaseClass = Flowsheet.Collections.ObjectCollection(Me.FlowsheetDesignSurface.SelectedObject.Name)

                If Me.IsObjectDownstreamConnectable(obj.GraphicObject.Tag) Then
                    Dim arr As ArrayList = Me.ReturnDownstreamConnectibles(obj.GraphicObject.Tag)
                    Me.CMS_ItemsToConnect.Items.Clear()
                    If arr.Count <> 0 Then
                        Dim i As Integer = 0
                        Do
                            Me.CMS_ItemsToConnect.Items.Add(arr(i))
                            i = i + 1
                            Me.ConectarAToolStripMenuItem.Visible = True
                            Me.ToolStripSeparator3.Visible = True
                            Me.ConectarAToolStripMenuItem.DropDown = Me.CMS_ItemsToConnect
                        Loop Until i = arr.Count
                    End If
                Else
                    Dim arr As ArrayList = Me.ReturnDownstreamDisconnectables(obj.GraphicObject.Tag)
                    Me.CMS_ItemsToDisconnect.Items.Clear()
                    If arr.Count <> 0 Then
                        naoLimparListaDeDesconectar = True
                        Dim i As Integer = 0
                        Do
                            Me.CMS_ItemsToDisconnect.Items.Add(arr(i))
                            i = i + 1
                        Loop Until i = arr.Count
                        Me.DesconectarDeToolStripMenuItem.Visible = True
                        Me.ToolStripSeparator3.Visible = True
                        Me.DesconectarDeToolStripMenuItem.DropDown = Me.CMS_ItemsToDisconnect
                    End If
                End If

                If Me.IsObjectUpstreamConnectable(obj.GraphicObject.Tag) = False Then
                    Dim arr As ArrayList = Me.ReturnUpstreamDisconnectables(obj.GraphicObject.Tag)
                    If naoLimparListaDeDesconectar = False Then Me.CMS_ItemsToDisconnect.Items.Clear()
                    If arr.Count <> 0 Then
                        Dim i As Integer = 0
                        Do
                            Me.CMS_ItemsToDisconnect.Items.Add(arr(i))
                            i = i + 1
                        Loop Until i = arr.Count
                        Me.DesconectarDeToolStripMenuItem.Visible = True
                        Me.ToolStripSeparator3.Visible = True
                        Me.DesconectarDeToolStripMenuItem.DropDown = Me.CMS_ItemsToDisconnect
                    End If
                End If

                If obj.GraphicObject.FlippedH Then
                    Me.HorizontalmenteToolStripMenuItem.Checked = True
                Else
                    Me.HorizontalmenteToolStripMenuItem.Checked = False
                End If

                If obj.Tabela Is Nothing Then
                    Me.MostrarToolStripMenuItem.Checked = False
                Else
                    Me.MostrarToolStripMenuItem.Checked = True
                End If

                If Me.FlowsheetDesignSurface.SelectedObject.TipoObjeto = TipoObjeto.MaterialStream Then

                    EditCompTSMI.Visible = True
                    RestoreTSMI.Visible = True

                    Dim cancopy As Boolean

                    If Not obj.GraphicObject.InputConnectors(0).IsAttached Then
                        cancopy = True
                    Else
                        If obj.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.TipoObjeto = TipoObjeto.OT_Reciclo Then
                            cancopy = True
                        Else
                            cancopy = False
                        End If
                    End If

                    If cancopy Then
                        Me.CopyFromTSMI.Visible = True
                        For Each mstr As Streams.MaterialStream In Flowsheet.Collections.CLCS_MaterialStreamCollection.Values
                            If mstr.GraphicObject.Tag <> obj.GraphicObject.Tag Then
                                Dim newtsmi As New ToolStripMenuItem(mstr.GraphicObject.Tag)
                                If mstr.GraphicObject.Calculated Then CopyFromTSMI.DropDownItems.Add(newtsmi)
                            End If
                        Next
                    End If

                Else

                    EditCompTSMI.Visible = False
                    RestoreTSMI.Visible = False

                End If

            Catch ex As Exception
                CMS_Sel.Hide()
            End Try

        ElseIf Me.FlowsheetDesignSurface.SelectedObject.TipoObjeto = TipoObjeto.AbsorptionColumn Or _
        Me.FlowsheetDesignSurface.SelectedObject.TipoObjeto = TipoObjeto.DistillationColumn Or _
        Me.FlowsheetDesignSurface.SelectedObject.TipoObjeto = TipoObjeto.ReboiledAbsorber Or _
        Me.FlowsheetDesignSurface.SelectedObject.TipoObjeto = TipoObjeto.RefluxedAbsorber Then

            Me.RecalcularToolStripMenuItem.Visible = True
            Me.ToolStripSeparator6.Visible = True

            Me.ConectarAToolStripMenuItem.Visible = False
            Me.DesconectarDeToolStripMenuItem.Visible = False
            Me.EditCompTSMI.Visible = False

            Me.TabelaToolStripMenuItem.Visible = True
            Me.ClonarToolStripMenuItem.Visible = True
            Me.ExcluirToolStripMenuItem.Visible = True
            Me.HorizontalmenteToolStripMenuItem.Visible = True
            Dim obj As DWSIM.SimulationObjects.UnitOperations.BaseClass = Flowsheet.Collections.ObjectCollection(Me.FlowsheetDesignSurface.SelectedObject.Name)

            If obj.GraphicObject.FlippedH Then
                Me.HorizontalmenteToolStripMenuItem.Checked = True
            Else
                Me.HorizontalmenteToolStripMenuItem.Checked = False
            End If
            If obj.Tabela Is Nothing Then
                Me.MostrarToolStripMenuItem.Checked = False
            Else
                Me.MostrarToolStripMenuItem.Checked = True
            End If

        Else

            Me.TSMI_Label.Text = "Tabela"
            Me.TabelaToolStripMenuItem.Visible = False
            Me.ClonarToolStripMenuItem.Visible = False
            Me.HorizontalmenteToolStripMenuItem.Visible = False
            Me.ExcluirToolStripMenuItem.Visible = False
            Me.RecalcularToolStripMenuItem.Visible = False
            Me.ToolStripSeparator6.Visible = False
            Me.EditCompTSMI.Visible = False
            Me.RestoreTSMI.Visible = False

        End If
        'Me.InverterToolStripMenuItem.Visible = False

    End Sub

    Sub MaterialStreamClickHandler(ByVal sender As System.Object, ByVal e As ToolStripItemClickedEventArgs)

        Dim obj1 As Streams.MaterialStream = Flowsheet.Collections.ObjectCollection(Me.FlowsheetDesignSurface.SelectedObject.Name)

        Dim obj2 As Streams.MaterialStream = Flowsheet.GetFlowsheetSimulationObject(e.ClickedItem.Text)

        obj1.Assign(obj2)

        CMS_Sel.Hide()

        Application.DoEvents()

        CalculateObject(Flowsheet, obj1.Nome)

    End Sub

    Private Sub ToolStripMenuItem2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripMenuItem2.Click
        Me.Flowsheet = My.Application.ActiveSimulation
        With Flowsheet.FormProps
            .LblStatusObj.Text = "-"
            .LblStatusObj.ForeColor = Color.Black
            .LblNomeObj.Text = DWSIM.App.GetLocalString("Fluxograma")
            .LblTipoObj.Text = "-"
        End With

        Flowsheet.FormProps.FTSProps.SelectedItem = Flowsheet.FormProps.TSObj

        With Flowsheet.FormProps.PGEx2

            .PropertySort = PropertySort.NoSort
            .ShowCustomProperties = True
            Try
                .Item.Clear()
            Catch ex As Exception
            Finally
                .Item.Clear()
            End Try
            .Item.Add(DWSIM.App.GetLocalString("Cordofundo"), FlowsheetDesignSurface, "BackColor", False, "", "", True)
            With .Item(.Item.Count - 1)
                .DefaultType = GetType(System.Drawing.Color)
            End With
            .Item.Add(DWSIM.App.GetLocalString("Cordagrade"), FlowsheetDesignSurface, "GridColor", False, "", "", True)
            With .Item(.Item.Count - 1)
                .DefaultType = GetType(System.Drawing.Color)
            End With
            .Item.Add(DWSIM.App.GetLocalString("Espessuradagrade"), FlowsheetDesignSurface, "GridLineWidth", False, "", "", True)
            .Item.Add(DWSIM.App.GetLocalString("SnapToGrid"), FlowsheetDesignSurface, "SnapToGrid", False, "", "", True)
            .Item.Add(DWSIM.App.GetLocalString("GridSize"), FlowsheetDesignSurface, "GridSize", False, "", "", True)
            .Item.Add(DWSIM.App.GetLocalString("Largura"), FlowsheetDesignSurface.SurfaceBounds, "Width", False, "", "", True)
            .Item.Add(DWSIM.App.GetLocalString("Altura"), FlowsheetDesignSurface.SurfaceBounds, "Height", False, "", "", True)
            .Item.Add(DWSIM.App.GetLocalString("Larguradeimpresso"), FlowsheetDesignSurface.SurfaceMargins, "Width", False, "", "", True)
            .Item.Add(DWSIM.App.GetLocalString("Alturadeimpresso"), FlowsheetDesignSurface.SurfaceMargins, "Height", False, "", "", True)

            .Refresh()
        End With

    End Sub

    Private Sub ToolStripMenuItem5_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripMenuItem5.Click
        PreviewDialog.ShowDialog()
    End Sub

    Private Sub Timer1_Tick(ByVal sender As Object, ByVal e As System.EventArgs) Handles Timer1.Tick
        Me.ticks += 1
    End Sub

    Public Sub ClonarToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ClonarToolStripMenuItem.Click
        CloneObject(Me.FlowsheetDesignSurface.SelectedObject)
    End Sub

    Public Sub CloneObject(gobj As GraphicObject)

        Flowsheet = My.Application.ActiveSimulation

        Dim obj As DWSIM.SimulationObjects.UnitOperations.BaseClass = Flowsheet.Collections.ObjectCollection(gobj.Name)
        Dim newobj As DWSIM.SimulationObjects.UnitOperations.BaseClass = obj.Clone
        newobj.Tabela = Nothing
        newobj.TabelaRapida = Nothing

        Dim searchtext As String = gObj.Tag.Split("(")(0).Trim()

        Dim objcount As Integer = (From go As GraphicObject In Me.FlowsheetDesignSurface.drawingObjects Select go Where go.Tag.Contains(searchtext)).Count

        Dim mpx = Me.FlowsheetDesignSurface.SelectedObject.X + Me.FlowsheetDesignSurface.SelectedObject.Width * 1.1
        Dim mpy = Me.FlowsheetDesignSurface.SelectedObject.Y + Me.FlowsheetDesignSurface.SelectedObject.Height * 1.1

        Select Case Me.FlowsheetDesignSurface.SelectedObject.TipoObjeto

            Case TipoObjeto.OT_Ajuste
                Dim myDWOBJ As DWSIM.SimulationObjects.SpecialOps.Adjust = CType(newobj, DWSIM.SimulationObjects.SpecialOps.Adjust)
                With myDWOBJ.GraphicObject
                    .Calculated = False
                    .Name = "ADJ-" & Guid.NewGuid.ToString
                    .Tag = searchtext & " (" & (objcount + 1).ToString & ")"
                    .X = mpx
                    .Y = mpy
                    For Each con As ConnectionPoint In .InputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    For Each con As ConnectionPoint In .OutputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    If Not .SpecialConnectors Is Nothing Then
                        For Each con As ConnectionPoint In .SpecialConnectors
                            con.AttachedConnector = Nothing
                            con.IsAttached = False
                        Next
                    End If
                    .EnergyConnector.AttachedConnector = Nothing
                    .EnergyConnector.IsAttached = False
                End With
                myDWOBJ.Nome = myDWOBJ.GraphicObject.Name
                Flowsheet.Collections.AdjustCollection.Add(myDWOBJ.GraphicObject.Name, myDWOBJ.GraphicObject)
                Flowsheet.Collections.ObjectCollection.Add(myDWOBJ.Nome, myDWOBJ)
                Flowsheet.Collections.CLCS_AdjustCollection.Add(myDWOBJ.Nome, myDWOBJ)
                Me.FlowsheetDesignSurface.drawingObjects.Add(myDWOBJ.GraphicObject)
            Case TipoObjeto.OT_Especificacao
                Dim myDWOBJ As DWSIM.SimulationObjects.SpecialOps.Spec = CType(newobj, DWSIM.SimulationObjects.SpecialOps.Spec)
                With myDWOBJ.GraphicObject
                    .Calculated = False
                    .Name = "SPEC-" & Guid.NewGuid.ToString
                    .Tag = searchtext & " (" & (objcount + 1).ToString & ")"
                    .X = mpx
                    .Y = mpy
                    For Each con As ConnectionPoint In .InputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    For Each con As ConnectionPoint In .OutputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    If Not .SpecialConnectors Is Nothing Then
                        For Each con As ConnectionPoint In .SpecialConnectors
                            con.AttachedConnector = Nothing
                            con.IsAttached = False
                        Next
                    End If
                    .EnergyConnector.AttachedConnector = Nothing
                    .EnergyConnector.IsAttached = False
                End With
                myDWOBJ.Nome = myDWOBJ.GraphicObject.Name
                Flowsheet.Collections.SpecCollection.Add(myDWOBJ.GraphicObject.Name, myDWOBJ.GraphicObject)
                Flowsheet.Collections.ObjectCollection.Add(myDWOBJ.Nome, myDWOBJ)
                Flowsheet.Collections.CLCS_SpecCollection.Add(myDWOBJ.Nome, myDWOBJ)
                Me.FlowsheetDesignSurface.drawingObjects.Add(myDWOBJ.GraphicObject)
            Case TipoObjeto.OT_Reciclo
                Dim myDWOBJ As DWSIM.SimulationObjects.SpecialOps.Recycle = CType(newobj, DWSIM.SimulationObjects.SpecialOps.Recycle)
                With myDWOBJ.GraphicObject
                    .Calculated = False
                    .Name = "REC-" & Guid.NewGuid.ToString
                    .Tag = searchtext & " (" & (objcount + 1).ToString & ")"
                    .X = mpx
                    .Y = mpy
                    For Each con As ConnectionPoint In .InputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    For Each con As ConnectionPoint In .OutputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    If Not .SpecialConnectors Is Nothing Then
                        For Each con As ConnectionPoint In .SpecialConnectors
                            con.AttachedConnector = Nothing
                            con.IsAttached = False
                        Next
                    End If
                    .EnergyConnector.AttachedConnector = Nothing
                    .EnergyConnector.IsAttached = False
                End With
                myDWOBJ.Nome = myDWOBJ.GraphicObject.Name
                Flowsheet.Collections.RecycleCollection.Add(myDWOBJ.GraphicObject.Name, myDWOBJ.GraphicObject)
                Flowsheet.Collections.ObjectCollection.Add(myDWOBJ.Nome, myDWOBJ)
                Flowsheet.Collections.CLCS_RecycleCollection.Add(myDWOBJ.Nome, myDWOBJ)
                Me.FlowsheetDesignSurface.drawingObjects.Add(myDWOBJ.GraphicObject)
            Case TipoObjeto.OT_EnergyRecycle
                Dim myDWOBJ As DWSIM.SimulationObjects.SpecialOps.EnergyRecycle = CType(newobj, DWSIM.SimulationObjects.SpecialOps.EnergyRecycle)
                With myDWOBJ.GraphicObject
                    .Calculated = False
                    .Name = "EREC-" & Guid.NewGuid.ToString
                    .Tag = searchtext & " (" & (objcount + 1).ToString & ")"
                    .X = mpx
                    .Y = mpy
                    For Each con As ConnectionPoint In .InputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    For Each con As ConnectionPoint In .OutputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    If Not .SpecialConnectors Is Nothing Then
                        For Each con As ConnectionPoint In .SpecialConnectors
                            con.AttachedConnector = Nothing
                            con.IsAttached = False
                        Next
                    End If
                    .EnergyConnector.AttachedConnector = Nothing
                    .EnergyConnector.IsAttached = False
                End With
                myDWOBJ.Nome = myDWOBJ.GraphicObject.Name
                Flowsheet.Collections.EnergyRecycleCollection.Add(myDWOBJ.GraphicObject.Name, myDWOBJ.GraphicObject)
                Flowsheet.Collections.ObjectCollection.Add(myDWOBJ.Nome, myDWOBJ)
                Flowsheet.Collections.CLCS_EnergyRecycleCollection.Add(myDWOBJ.Nome, myDWOBJ)
                Me.FlowsheetDesignSurface.drawingObjects.Add(myDWOBJ.GraphicObject)
            Case TipoObjeto.NodeIn
                Dim myDWOBJ As DWSIM.SimulationObjects.UnitOps.Mixer = CType(newobj, DWSIM.SimulationObjects.UnitOps.Mixer)
                With myDWOBJ.GraphicObject
                    .Calculated = False
                    .Name = "MIX-" & Guid.NewGuid.ToString
                    .Tag = searchtext & " (" & (objcount + 1).ToString & ")"
                    .X = mpx
                    .Y = mpy
                    For Each con As ConnectionPoint In .InputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    For Each con As ConnectionPoint In .OutputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    If Not .SpecialConnectors Is Nothing Then
                        For Each con As ConnectionPoint In .SpecialConnectors
                            con.AttachedConnector = Nothing
                            con.IsAttached = False
                        Next
                    End If
                    .EnergyConnector.AttachedConnector = Nothing
                    .EnergyConnector.IsAttached = False
                End With
                myDWOBJ.Nome = myDWOBJ.GraphicObject.Name
                Flowsheet.Collections.MixerCollection.Add(myDWOBJ.GraphicObject.Name, myDWOBJ.GraphicObject)
                Flowsheet.Collections.ObjectCollection.Add(myDWOBJ.Nome, myDWOBJ)
                Flowsheet.Collections.CLCS_MixerCollection.Add(myDWOBJ.Nome, myDWOBJ)
                Me.FlowsheetDesignSurface.drawingObjects.Add(myDWOBJ.GraphicObject)
            Case TipoObjeto.NodeEn
                Dim myDWOBJ As DWSIM.SimulationObjects.UnitOps.EnergyMixer = CType(newobj, DWSIM.SimulationObjects.UnitOps.EnergyMixer)
                With myDWOBJ.GraphicObject
                    .Calculated = False
                    .Name = "MIX-ME_-" & Guid.NewGuid.ToString
                    .Tag = searchtext & " (" & (objcount + 1).ToString & ")"
                    .X = mpx
                    .Y = mpy
                    For Each con As ConnectionPoint In .InputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    For Each con As ConnectionPoint In .OutputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    If Not .SpecialConnectors Is Nothing Then
                        For Each con As ConnectionPoint In .SpecialConnectors
                            con.AttachedConnector = Nothing
                            con.IsAttached = False
                        Next
                    End If
                    .EnergyConnector.AttachedConnector = Nothing
                    .EnergyConnector.IsAttached = False
                End With
                myDWOBJ.Nome = myDWOBJ.GraphicObject.Name
                Flowsheet.Collections.MixerENCollection.Add(myDWOBJ.GraphicObject.Name, myDWOBJ.GraphicObject)
                Flowsheet.Collections.ObjectCollection.Add(myDWOBJ.Nome, myDWOBJ)
                Flowsheet.Collections.CLCS_EnergyMixerCollection.Add(myDWOBJ.Nome, myDWOBJ)
                Me.FlowsheetDesignSurface.drawingObjects.Add(myDWOBJ.GraphicObject)
            Case TipoObjeto.NodeOut
                Dim myDWOBJ As DWSIM.SimulationObjects.UnitOps.Splitter = CType(newobj, DWSIM.SimulationObjects.UnitOps.Splitter)
                With myDWOBJ.GraphicObject
                    .Calculated = False
                    .Name = "SPLT-" & Guid.NewGuid.ToString
                    .Tag = searchtext & " (" & (objcount + 1).ToString & ")"
                    .X = mpx
                    .Y = mpy
                    For Each con As ConnectionPoint In .InputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    For Each con As ConnectionPoint In .OutputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    If Not .SpecialConnectors Is Nothing Then
                        For Each con As ConnectionPoint In .SpecialConnectors
                            con.AttachedConnector = Nothing
                            con.IsAttached = False
                        Next
                    End If
                    .EnergyConnector.AttachedConnector = Nothing
                    .EnergyConnector.IsAttached = False
                End With
                myDWOBJ.Nome = myDWOBJ.GraphicObject.Name
                Flowsheet.Collections.SplitterCollection.Add(myDWOBJ.GraphicObject.Name, myDWOBJ.GraphicObject)
                Flowsheet.Collections.ObjectCollection.Add(myDWOBJ.Nome, myDWOBJ)
                Flowsheet.Collections.CLCS_SplitterCollection.Add(myDWOBJ.Nome, myDWOBJ)
                Me.FlowsheetDesignSurface.drawingObjects.Add(myDWOBJ.GraphicObject)
            Case TipoObjeto.Pump
                Dim myDWOBJ As DWSIM.SimulationObjects.UnitOps.Pump = CType(newobj, DWSIM.SimulationObjects.UnitOps.Pump)
                With myDWOBJ.GraphicObject
                    .Calculated = False
                    .Name = "PUMP-" & Guid.NewGuid.ToString
                    .Tag = searchtext & " (" & (objcount + 1).ToString & ")"
                    .X = mpx
                    .Y = mpy
                    For Each con As ConnectionPoint In .InputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    For Each con As ConnectionPoint In .OutputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    If Not .SpecialConnectors Is Nothing Then
                        For Each con As ConnectionPoint In .SpecialConnectors
                            con.AttachedConnector = Nothing
                            con.IsAttached = False
                        Next
                    End If
                    .EnergyConnector.AttachedConnector = Nothing
                    .EnergyConnector.IsAttached = False
                End With
                myDWOBJ.Nome = myDWOBJ.GraphicObject.Name
                Flowsheet.Collections.PumpCollection.Add(myDWOBJ.GraphicObject.Name, myDWOBJ.GraphicObject)
                Flowsheet.Collections.ObjectCollection.Add(myDWOBJ.Nome, myDWOBJ)
                Flowsheet.Collections.CLCS_PumpCollection.Add(myDWOBJ.Nome, myDWOBJ)
                Me.FlowsheetDesignSurface.drawingObjects.Add(myDWOBJ.GraphicObject)
            Case TipoObjeto.Tank
                Dim myDWOBJ As DWSIM.SimulationObjects.UnitOps.Tank = CType(newobj, DWSIM.SimulationObjects.UnitOps.Tank)
                With myDWOBJ.GraphicObject
                    .Calculated = False
                    .Name = "TANK-" & Guid.NewGuid.ToString
                    .Tag = searchtext & " (" & (objcount + 1).ToString & ")"
                    .X = mpx
                    .Y = mpy
                     For Each con As ConnectionPoint In .InputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    For Each con As ConnectionPoint In .OutputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    If Not .SpecialConnectors Is Nothing Then
                        For Each con As ConnectionPoint In .SpecialConnectors
                            con.AttachedConnector = Nothing
                            con.IsAttached = False
                        Next
                    End If
                    .EnergyConnector.AttachedConnector = Nothing
                    .EnergyConnector.IsAttached = False
                End With
                myDWOBJ.Nome = myDWOBJ.GraphicObject.Name
                Flowsheet.Collections.TankCollection.Add(myDWOBJ.GraphicObject.Name, myDWOBJ.GraphicObject)
                Flowsheet.Collections.ObjectCollection.Add(myDWOBJ.Nome, myDWOBJ)
                Flowsheet.Collections.CLCS_TankCollection.Add(myDWOBJ.Nome, myDWOBJ)
                Me.FlowsheetDesignSurface.drawingObjects.Add(myDWOBJ.GraphicObject)
            Case TipoObjeto.Vessel
                Dim myDWOBJ As DWSIM.SimulationObjects.UnitOps.Vessel = CType(newobj, DWSIM.SimulationObjects.UnitOps.Vessel)
                With myDWOBJ.GraphicObject
                    .Calculated = False
                    .Name = "SEP-" & Guid.NewGuid.ToString
                    .Tag = searchtext & " (" & (objcount + 1).ToString & ")"
                    .X = mpx
                    .Y = mpy
                    For Each con As ConnectionPoint In .InputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    For Each con As ConnectionPoint In .OutputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    If Not .SpecialConnectors Is Nothing Then
                        For Each con As ConnectionPoint In .SpecialConnectors
                            con.AttachedConnector = Nothing
                            con.IsAttached = False
                        Next
                    End If
                    .EnergyConnector.AttachedConnector = Nothing
                    .EnergyConnector.IsAttached = False
                End With
                myDWOBJ.Nome = myDWOBJ.GraphicObject.Name
                Flowsheet.Collections.SeparatorCollection.Add(myDWOBJ.GraphicObject.Name, myDWOBJ.GraphicObject)
                Flowsheet.Collections.ObjectCollection.Add(myDWOBJ.Nome, myDWOBJ)
                Flowsheet.Collections.CLCS_VesselCollection.Add(myDWOBJ.Nome, myDWOBJ)
                Me.FlowsheetDesignSurface.drawingObjects.Add(myDWOBJ.GraphicObject)
            Case TipoObjeto.MaterialStream
                Dim myDWOBJ As DWSIM.SimulationObjects.Streams.MaterialStream = CType(newobj, DWSIM.SimulationObjects.Streams.MaterialStream)
                With myDWOBJ.GraphicObject
                    .Calculated = False
                    .Name = "MSTR-" & Guid.NewGuid.ToString
                    .Tag = searchtext & " (" & (objcount + 1).ToString & ")"
                    .X = mpx
                    .Y = mpy
                    For Each con As ConnectionPoint In .InputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    For Each con As ConnectionPoint In .OutputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    If Not .SpecialConnectors Is Nothing Then
                        For Each con As ConnectionPoint In .SpecialConnectors
                            con.AttachedConnector = Nothing
                            con.IsAttached = False
                        Next
                    End If
                    .EnergyConnector.AttachedConnector = Nothing
                    .EnergyConnector.IsAttached = False
                End With
                myDWOBJ.Nome = myDWOBJ.GraphicObject.Name
                Flowsheet.Collections.MaterialStreamCollection.Add(myDWOBJ.GraphicObject.Name, myDWOBJ.GraphicObject)
                Flowsheet.Collections.ObjectCollection.Add(myDWOBJ.Nome, myDWOBJ)
                Flowsheet.Collections.CLCS_MaterialStreamCollection.Add(myDWOBJ.Nome, myDWOBJ)
                Me.FlowsheetDesignSurface.drawingObjects.Add(myDWOBJ.GraphicObject)
            Case TipoObjeto.EnergyStream
                Dim myDWOBJ As DWSIM.SimulationObjects.Streams.EnergyStream = CType(newobj, DWSIM.SimulationObjects.Streams.EnergyStream)
                With myDWOBJ.GraphicObject
                    .Calculated = False
                    .Name = "ESTR-" & Guid.NewGuid.ToString
                    .Tag = searchtext & " (" & (objcount + 1).ToString & ")"
                    .X = mpx
                    .Y = mpy
                    For Each con As ConnectionPoint In .InputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    For Each con As ConnectionPoint In .OutputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    If Not .SpecialConnectors Is Nothing Then
                        For Each con As ConnectionPoint In .SpecialConnectors
                            con.AttachedConnector = Nothing
                            con.IsAttached = False
                        Next
                    End If
                    .EnergyConnector.AttachedConnector = Nothing
                    .EnergyConnector.IsAttached = False
                End With
                myDWOBJ.Nome = myDWOBJ.GraphicObject.Name
                Flowsheet.Collections.EnergyStreamCollection.Add(myDWOBJ.GraphicObject.Name, myDWOBJ.GraphicObject)
                Flowsheet.Collections.ObjectCollection.Add(myDWOBJ.Nome, myDWOBJ)
                Flowsheet.Collections.CLCS_EnergyStreamCollection.Add(myDWOBJ.Nome, myDWOBJ)
                Me.FlowsheetDesignSurface.drawingObjects.Add(myDWOBJ.GraphicObject)
            Case TipoObjeto.Compressor
                Dim myDWOBJ As DWSIM.SimulationObjects.UnitOps.Compressor = CType(newobj, DWSIM.SimulationObjects.UnitOps.Compressor)
                With myDWOBJ.GraphicObject
                    .Calculated = False
                    .Name = "COMP-" & Guid.NewGuid.ToString
                    .Tag = searchtext & " (" & (objcount + 1).ToString & ")"
                    .X = mpx
                    .Y = mpy
                    For Each con As ConnectionPoint In .InputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    For Each con As ConnectionPoint In .OutputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    If Not .SpecialConnectors Is Nothing Then
                        For Each con As ConnectionPoint In .SpecialConnectors
                            con.AttachedConnector = Nothing
                            con.IsAttached = False
                        Next
                    End If
                    .EnergyConnector.AttachedConnector = Nothing
                    .EnergyConnector.IsAttached = False
                End With
                myDWOBJ.Nome = myDWOBJ.GraphicObject.Name
                Flowsheet.Collections.CompressorCollection.Add(myDWOBJ.GraphicObject.Name, myDWOBJ.GraphicObject)
                Flowsheet.Collections.ObjectCollection.Add(myDWOBJ.Nome, myDWOBJ)
                Flowsheet.Collections.CLCS_CompressorCollection.Add(myDWOBJ.Nome, myDWOBJ)
                Me.FlowsheetDesignSurface.drawingObjects.Add(myDWOBJ.GraphicObject)
            Case TipoObjeto.Expander
                Dim myDWOBJ As DWSIM.SimulationObjects.UnitOps.Expander = CType(newobj, DWSIM.SimulationObjects.UnitOps.Expander)
                With myDWOBJ.GraphicObject
                    .Calculated = False
                    .Name = "EXP-" & Guid.NewGuid.ToString
                    .Tag = searchtext & " (" & (objcount + 1).ToString & ")"
                    .X = mpx
                    .Y = mpy
                    For Each con As ConnectionPoint In .InputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    For Each con As ConnectionPoint In .OutputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    If Not .SpecialConnectors Is Nothing Then
                        For Each con As ConnectionPoint In .SpecialConnectors
                            con.AttachedConnector = Nothing
                            con.IsAttached = False
                        Next
                    End If
                    .EnergyConnector.AttachedConnector = Nothing
                    .EnergyConnector.IsAttached = False
                End With
                myDWOBJ.Nome = myDWOBJ.GraphicObject.Name
                Flowsheet.Collections.TurbineCollection.Add(myDWOBJ.GraphicObject.Name, myDWOBJ.GraphicObject)
                Flowsheet.Collections.ObjectCollection.Add(myDWOBJ.Nome, myDWOBJ)
                Flowsheet.Collections.CLCS_TurbineCollection.Add(myDWOBJ.Nome, myDWOBJ)
                Me.FlowsheetDesignSurface.drawingObjects.Add(myDWOBJ.GraphicObject)
            Case TipoObjeto.Cooler
                Dim myDWOBJ As DWSIM.SimulationObjects.UnitOps.Cooler = CType(newobj, DWSIM.SimulationObjects.UnitOps.Cooler)
                With myDWOBJ.GraphicObject
                    .Calculated = False
                    .Name = "COOL-" & Guid.NewGuid.ToString
                    .Tag = searchtext & " (" & (objcount + 1).ToString & ")"
                    .X = mpx
                    .Y = mpy
                    For Each con As ConnectionPoint In .InputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    For Each con As ConnectionPoint In .OutputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    If Not .SpecialConnectors Is Nothing Then
                        For Each con As ConnectionPoint In .SpecialConnectors
                            con.AttachedConnector = Nothing
                            con.IsAttached = False
                        Next
                    End If
                    .EnergyConnector.AttachedConnector = Nothing
                    .EnergyConnector.IsAttached = False
                End With
                myDWOBJ.Nome = myDWOBJ.GraphicObject.Name
                Flowsheet.Collections.CoolerCollection.Add(myDWOBJ.GraphicObject.Name, myDWOBJ.GraphicObject)
                Flowsheet.Collections.ObjectCollection.Add(myDWOBJ.Nome, myDWOBJ)
                Flowsheet.Collections.CLCS_CoolerCollection.Add(myDWOBJ.Nome, myDWOBJ)
                Me.FlowsheetDesignSurface.drawingObjects.Add(myDWOBJ.GraphicObject)
            Case TipoObjeto.Heater
                Dim myDWOBJ As DWSIM.SimulationObjects.UnitOps.Heater = CType(newobj, DWSIM.SimulationObjects.UnitOps.Heater)
                With myDWOBJ.GraphicObject
                    .Calculated = False
                    .Name = "HEAT-" & Guid.NewGuid.ToString
                    .Tag = searchtext & " (" & (objcount + 1).ToString & ")"
                    .X = mpx
                    .Y = mpy
                    For Each con As ConnectionPoint In .InputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    For Each con As ConnectionPoint In .OutputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    If Not .SpecialConnectors Is Nothing Then
                        For Each con As ConnectionPoint In .SpecialConnectors
                            con.AttachedConnector = Nothing
                            con.IsAttached = False
                        Next
                    End If
                    .EnergyConnector.AttachedConnector = Nothing
                    .EnergyConnector.IsAttached = False
                End With
                myDWOBJ.Nome = myDWOBJ.GraphicObject.Name
                Flowsheet.Collections.HeaterCollection.Add(myDWOBJ.GraphicObject.Name, myDWOBJ.GraphicObject)
                Flowsheet.Collections.ObjectCollection.Add(myDWOBJ.Nome, myDWOBJ)
                Flowsheet.Collections.CLCS_HeaterCollection.Add(myDWOBJ.Nome, myDWOBJ)
                Me.FlowsheetDesignSurface.drawingObjects.Add(myDWOBJ.GraphicObject)
            Case TipoObjeto.Pipe
                Dim myDWOBJ As DWSIM.SimulationObjects.UnitOps.Pipe = CType(newobj, DWSIM.SimulationObjects.UnitOps.Pipe)
                With myDWOBJ.GraphicObject
                    .Calculated = False
                    .Name = "PIPE-" & Guid.NewGuid.ToString
                    .Tag = searchtext & " (" & (objcount + 1).ToString & ")"
                    .X = mpx
                    .Y = mpy
                    For Each con As ConnectionPoint In .InputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    For Each con As ConnectionPoint In .OutputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    If Not .SpecialConnectors Is Nothing Then
                        For Each con As ConnectionPoint In .SpecialConnectors
                            con.AttachedConnector = Nothing
                            con.IsAttached = False
                        Next
                    End If
                    .EnergyConnector.AttachedConnector = Nothing
                    .EnergyConnector.IsAttached = False
                End With
                myDWOBJ.Nome = myDWOBJ.GraphicObject.Name
                Flowsheet.Collections.PipeCollection.Add(myDWOBJ.GraphicObject.Name, myDWOBJ.GraphicObject)
                Flowsheet.Collections.ObjectCollection.Add(myDWOBJ.Nome, myDWOBJ)
                Flowsheet.Collections.CLCS_PipeCollection.Add(myDWOBJ.Nome, myDWOBJ)
                Me.FlowsheetDesignSurface.drawingObjects.Add(myDWOBJ.GraphicObject)
            Case TipoObjeto.Valve
                Dim myDWOBJ As DWSIM.SimulationObjects.UnitOps.Valve = CType(newobj, DWSIM.SimulationObjects.UnitOps.Valve)
                With myDWOBJ.GraphicObject
                    .Calculated = False
                    .Name = "VALV-" & Guid.NewGuid.ToString
                    .Tag = searchtext & " (" & (objcount + 1).ToString & ")"
                    .X = mpx
                    .Y = mpy
                     For Each con As ConnectionPoint In .InputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    For Each con As ConnectionPoint In .OutputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    If Not .SpecialConnectors Is Nothing Then
                        For Each con As ConnectionPoint In .SpecialConnectors
                            con.AttachedConnector = Nothing
                            con.IsAttached = False
                        Next
                    End If
                    .EnergyConnector.AttachedConnector = Nothing
                    .EnergyConnector.IsAttached = False
                End With
                myDWOBJ.Nome = myDWOBJ.GraphicObject.Name
                Flowsheet.Collections.ValveCollection.Add(myDWOBJ.GraphicObject.Name, myDWOBJ.GraphicObject)
                Flowsheet.Collections.ObjectCollection.Add(myDWOBJ.Nome, myDWOBJ)
                Flowsheet.Collections.CLCS_ValveCollection.Add(myDWOBJ.Nome, myDWOBJ)
                Me.FlowsheetDesignSurface.drawingObjects.Add(myDWOBJ.GraphicObject)
            Case TipoObjeto.RCT_Conversion
                Dim myDWOBJ As DWSIM.SimulationObjects.Reactors.Reactor_Conversion = CType(newobj, DWSIM.SimulationObjects.Reactors.Reactor_Conversion)
                With myDWOBJ.GraphicObject
                    .Calculated = False
                    .Name = "RC-" & Guid.NewGuid.ToString
                    .Tag = searchtext & " (" & (objcount + 1).ToString & ")"
                    .X = mpx
                    .Y = mpy
                    For Each con As ConnectionPoint In .InputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    For Each con As ConnectionPoint In .OutputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    If Not .SpecialConnectors Is Nothing Then
                        For Each con As ConnectionPoint In .SpecialConnectors
                            con.AttachedConnector = Nothing
                            con.IsAttached = False
                        Next
                    End If
                    .EnergyConnector.AttachedConnector = Nothing
                    .EnergyConnector.IsAttached = False
                End With
                myDWOBJ.Nome = myDWOBJ.GraphicObject.Name
                Flowsheet.Collections.ReactorConversionCollection.Add(myDWOBJ.GraphicObject.Name, myDWOBJ.GraphicObject)
                Flowsheet.Collections.ObjectCollection.Add(myDWOBJ.Nome, myDWOBJ)
                Flowsheet.Collections.CLCS_ReactorConversionCollection.Add(myDWOBJ.Nome, myDWOBJ)
                Me.FlowsheetDesignSurface.drawingObjects.Add(myDWOBJ.GraphicObject)
            Case TipoObjeto.RCT_Equilibrium
                Dim myDWOBJ As DWSIM.SimulationObjects.Reactors.Reactor_Equilibrium = CType(newobj, DWSIM.SimulationObjects.Reactors.Reactor_Equilibrium)
                With myDWOBJ.GraphicObject
                    .Calculated = False
                    .Name = "RE-" & Guid.NewGuid.ToString
                    .Tag = searchtext & " (" & (objcount + 1).ToString & ")"
                    .X = mpx
                    .Y = mpy
                    For Each con As ConnectionPoint In .InputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    For Each con As ConnectionPoint In .OutputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    If Not .SpecialConnectors Is Nothing Then
                        For Each con As ConnectionPoint In .SpecialConnectors
                            con.AttachedConnector = Nothing
                            con.IsAttached = False
                        Next
                    End If
                    .EnergyConnector.AttachedConnector = Nothing
                    .EnergyConnector.IsAttached = False
                End With
                myDWOBJ.Nome = myDWOBJ.GraphicObject.Name
                Flowsheet.Collections.ReactorEquilibriumCollection.Add(myDWOBJ.GraphicObject.Name, myDWOBJ.GraphicObject)
                Flowsheet.Collections.ObjectCollection.Add(myDWOBJ.Nome, myDWOBJ)
                Flowsheet.Collections.CLCS_ReactorEquilibriumCollection.Add(myDWOBJ.Nome, myDWOBJ)
                Me.FlowsheetDesignSurface.drawingObjects.Add(myDWOBJ.GraphicObject)
            Case TipoObjeto.RCT_Gibbs
                Dim myDWOBJ As DWSIM.SimulationObjects.Reactors.Reactor_Gibbs = CType(newobj, DWSIM.SimulationObjects.Reactors.Reactor_Gibbs)
                With myDWOBJ.GraphicObject
                    .Calculated = False
                    .Name = "RG-" & Guid.NewGuid.ToString
                    .Tag = searchtext & " (" & (objcount + 1).ToString & ")"
                    .X = mpx
                    .Y = mpy
                     For Each con As ConnectionPoint In .InputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    For Each con As ConnectionPoint In .OutputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    If Not .SpecialConnectors Is Nothing Then
                        For Each con As ConnectionPoint In .SpecialConnectors
                            con.AttachedConnector = Nothing
                            con.IsAttached = False
                        Next
                    End If
                    .EnergyConnector.AttachedConnector = Nothing
                    .EnergyConnector.IsAttached = False
                End With
                myDWOBJ.Nome = myDWOBJ.GraphicObject.Name
                Flowsheet.Collections.ReactorGibbsCollection.Add(myDWOBJ.GraphicObject.Name, myDWOBJ.GraphicObject)
                Flowsheet.Collections.ObjectCollection.Add(myDWOBJ.Nome, myDWOBJ)
                Flowsheet.Collections.CLCS_ReactorGibbsCollection.Add(myDWOBJ.Nome, myDWOBJ)
                Me.FlowsheetDesignSurface.drawingObjects.Add(myDWOBJ.GraphicObject)
            Case TipoObjeto.RCT_CSTR
                Dim myDWOBJ As DWSIM.SimulationObjects.Reactors.Reactor_CSTR = CType(newobj, DWSIM.SimulationObjects.Reactors.Reactor_CSTR)
                With myDWOBJ.GraphicObject
                    .Calculated = False
                    .Name = "CSTR-" & Guid.NewGuid.ToString
                    .Tag = searchtext & " (" & (objcount + 1).ToString & ")"
                    .X = mpx
                    .Y = mpy
                    For Each con As ConnectionPoint In .InputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    For Each con As ConnectionPoint In .OutputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    If Not .SpecialConnectors Is Nothing Then
                        For Each con As ConnectionPoint In .SpecialConnectors
                            con.AttachedConnector = Nothing
                            con.IsAttached = False
                        Next
                    End If
                    .EnergyConnector.AttachedConnector = Nothing
                    .EnergyConnector.IsAttached = False
                End With
                myDWOBJ.Nome = myDWOBJ.GraphicObject.Name
                Flowsheet.Collections.ReactorCSTRCollection.Add(myDWOBJ.GraphicObject.Name, myDWOBJ.GraphicObject)
                Flowsheet.Collections.ObjectCollection.Add(myDWOBJ.Nome, myDWOBJ)
                Flowsheet.Collections.CLCS_ReactorCSTRCollection.Add(myDWOBJ.Nome, myDWOBJ)
                Me.FlowsheetDesignSurface.drawingObjects.Add(myDWOBJ.GraphicObject)
            Case TipoObjeto.RCT_PFR
                Dim myDWOBJ As DWSIM.SimulationObjects.Reactors.Reactor_PFR = CType(newobj, DWSIM.SimulationObjects.Reactors.Reactor_PFR)
                With myDWOBJ.GraphicObject
                    .Calculated = False
                    .Name = "PFR-" & Guid.NewGuid.ToString
                    .Tag = searchtext & " (" & (objcount + 1).ToString & ")"
                    .X = mpx
                    .Y = mpy
                    For Each con As ConnectionPoint In .InputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    For Each con As ConnectionPoint In .OutputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    If Not .SpecialConnectors Is Nothing Then
                        For Each con As ConnectionPoint In .SpecialConnectors
                            con.AttachedConnector = Nothing
                            con.IsAttached = False
                        Next
                    End If
                    .EnergyConnector.AttachedConnector = Nothing
                    .EnergyConnector.IsAttached = False
                End With
                myDWOBJ.Nome = myDWOBJ.GraphicObject.Name
                Flowsheet.Collections.ReactorPFRCollection.Add(myDWOBJ.GraphicObject.Name, myDWOBJ.GraphicObject)
                Flowsheet.Collections.ObjectCollection.Add(myDWOBJ.Nome, myDWOBJ)
                Flowsheet.Collections.CLCS_ReactorPFRCollection.Add(myDWOBJ.Nome, myDWOBJ)
                Me.FlowsheetDesignSurface.drawingObjects.Add(myDWOBJ.GraphicObject)
            Case TipoObjeto.HeatExchanger
                Dim myDWOBJ As DWSIM.SimulationObjects.UnitOps.HeatExchanger = CType(newobj, DWSIM.SimulationObjects.UnitOps.HeatExchanger)
                With myDWOBJ.GraphicObject
                    .Calculated = False
                    .Name = "HE-" & Guid.NewGuid.ToString
                    .Tag = searchtext & " (" & (objcount + 1).ToString & ")"
                    .X = mpx
                    .Y = mpy
                    For Each con As ConnectionPoint In .InputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    For Each con As ConnectionPoint In .OutputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    If Not .SpecialConnectors Is Nothing Then
                        For Each con As ConnectionPoint In .SpecialConnectors
                            con.AttachedConnector = Nothing
                            con.IsAttached = False
                        Next
                    End If
                    .EnergyConnector.AttachedConnector = Nothing
                    .EnergyConnector.IsAttached = False
                End With
                myDWOBJ.Nome = myDWOBJ.GraphicObject.Name
                Flowsheet.Collections.HeatExchangerCollection.Add(myDWOBJ.GraphicObject.Name, myDWOBJ.GraphicObject)
                Flowsheet.Collections.ObjectCollection.Add(myDWOBJ.Nome, myDWOBJ)
                Flowsheet.Collections.CLCS_HeatExchangerCollection.Add(myDWOBJ.Nome, myDWOBJ)
                Me.FlowsheetDesignSurface.drawingObjects.Add(myDWOBJ.GraphicObject)
            Case TipoObjeto.ShortcutColumn
                Dim myDWOBJ As DWSIM.SimulationObjects.UnitOps.ShortcutColumn = CType(newobj, DWSIM.SimulationObjects.UnitOps.ShortcutColumn)
                With myDWOBJ.GraphicObject
                    .Calculated = False
                    .Name = "SC-" & Guid.NewGuid.ToString
                    .Tag = searchtext & " (" & (objcount + 1).ToString & ")"
                    .X = mpx
                    .Y = mpy
                    For Each con As ConnectionPoint In .InputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    For Each con As ConnectionPoint In .OutputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    If Not .SpecialConnectors Is Nothing Then
                        For Each con As ConnectionPoint In .SpecialConnectors
                            con.AttachedConnector = Nothing
                            con.IsAttached = False
                        Next
                    End If
                    .EnergyConnector.AttachedConnector = Nothing
                    .EnergyConnector.IsAttached = False
                End With
                myDWOBJ.Nome = myDWOBJ.GraphicObject.Name
                Flowsheet.Collections.ShortcutColumnCollection.Add(myDWOBJ.GraphicObject.Name, myDWOBJ.GraphicObject)
                Flowsheet.Collections.ObjectCollection.Add(myDWOBJ.Nome, myDWOBJ)
                Flowsheet.Collections.CLCS_ShortcutColumnCollection.Add(myDWOBJ.Nome, myDWOBJ)
                Me.FlowsheetDesignSurface.drawingObjects.Add(myDWOBJ.GraphicObject)
            Case TipoObjeto.DistillationColumn
                Dim myDWOBJ As DWSIM.SimulationObjects.UnitOps.DistillationColumn = CType(newobj, DWSIM.SimulationObjects.UnitOps.DistillationColumn)
                With myDWOBJ.GraphicObject
                    .Calculated = False
                    .Name = "DC-" & Guid.NewGuid.ToString
                    .Tag = searchtext & " (" & (objcount + 1).ToString & ")"
                    .X = mpx
                    .Y = mpy
                    For Each con As ConnectionPoint In .InputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    For Each con As ConnectionPoint In .OutputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    If Not .SpecialConnectors Is Nothing Then
                        For Each con As ConnectionPoint In .SpecialConnectors
                            con.AttachedConnector = Nothing
                            con.IsAttached = False
                        Next
                    End If
                    .EnergyConnector.AttachedConnector = Nothing
                    .EnergyConnector.IsAttached = False
                End With
                myDWOBJ.Nome = myDWOBJ.GraphicObject.Name
                Flowsheet.Collections.DistillationColumnCollection.Add(myDWOBJ.GraphicObject.Name, myDWOBJ.GraphicObject)
                Flowsheet.Collections.ObjectCollection.Add(myDWOBJ.Nome, myDWOBJ)
                Flowsheet.Collections.CLCS_DistillationColumnCollection.Add(myDWOBJ.Nome, myDWOBJ)
                Me.FlowsheetDesignSurface.drawingObjects.Add(myDWOBJ.GraphicObject)
            Case TipoObjeto.AbsorptionColumn
                Dim myDWOBJ As DWSIM.SimulationObjects.UnitOps.AbsorptionColumn = CType(newobj, DWSIM.SimulationObjects.UnitOps.AbsorptionColumn)
                With myDWOBJ.GraphicObject
                    .Calculated = False
                    .Name = "ABS-" & Guid.NewGuid.ToString
                    .Tag = searchtext & " (" & (objcount + 1).ToString & ")"
                    .X = mpx
                    .Y = mpy
                    For Each con As ConnectionPoint In .InputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    For Each con As ConnectionPoint In .OutputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    If Not .SpecialConnectors Is Nothing Then
                        For Each con As ConnectionPoint In .SpecialConnectors
                            con.AttachedConnector = Nothing
                            con.IsAttached = False
                        Next
                    End If
                    .EnergyConnector.AttachedConnector = Nothing
                    .EnergyConnector.IsAttached = False
                End With
                myDWOBJ.Nome = myDWOBJ.GraphicObject.Name
                Flowsheet.Collections.AbsorptionColumnCollection.Add(myDWOBJ.GraphicObject.Name, myDWOBJ.GraphicObject)
                Flowsheet.Collections.ObjectCollection.Add(myDWOBJ.Nome, myDWOBJ)
                Flowsheet.Collections.CLCS_AbsorptionColumnCollection.Add(myDWOBJ.Nome, myDWOBJ)
                Me.FlowsheetDesignSurface.drawingObjects.Add(myDWOBJ.GraphicObject)
            Case TipoObjeto.ReboiledAbsorber
                Dim myDWOBJ As DWSIM.SimulationObjects.UnitOps.ReboiledAbsorber = CType(newobj, DWSIM.SimulationObjects.UnitOps.ReboiledAbsorber)
                With myDWOBJ.GraphicObject
                    .Calculated = False
                    .Name = "RBA-" & Guid.NewGuid.ToString
                    .Tag = searchtext & " (" & (objcount + 1).ToString & ")"
                    .X = mpx
                    .Y = mpy
                   For Each con As ConnectionPoint In .InputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    For Each con As ConnectionPoint In .OutputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    If Not .SpecialConnectors Is Nothing Then
                        For Each con As ConnectionPoint In .SpecialConnectors
                            con.AttachedConnector = Nothing
                            con.IsAttached = False
                        Next
                    End If
                    .EnergyConnector.AttachedConnector = Nothing
                    .EnergyConnector.IsAttached = False
                End With
                myDWOBJ.Nome = myDWOBJ.GraphicObject.Name
                Flowsheet.Collections.ReboiledAbsorberCollection.Add(myDWOBJ.GraphicObject.Name, myDWOBJ.GraphicObject)
                Flowsheet.Collections.ObjectCollection.Add(myDWOBJ.Nome, myDWOBJ)
                Flowsheet.Collections.CLCS_ReboiledAbsorberCollection.Add(myDWOBJ.Nome, myDWOBJ)
                Me.FlowsheetDesignSurface.drawingObjects.Add(myDWOBJ.GraphicObject)
            Case TipoObjeto.RefluxedAbsorber
                Dim myDWOBJ As DWSIM.SimulationObjects.UnitOps.RefluxedAbsorber = CType(newobj, DWSIM.SimulationObjects.UnitOps.RefluxedAbsorber)
                With myDWOBJ.GraphicObject
                    .Calculated = False
                    .Name = "RFA-" & Guid.NewGuid.ToString
                    .Tag = searchtext & " (" & (objcount + 1).ToString & ")"
                    .X = mpx
                    .Y = mpy
                   For Each con As ConnectionPoint In .InputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    For Each con As ConnectionPoint In .OutputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    If Not .SpecialConnectors Is Nothing Then
                        For Each con As ConnectionPoint In .SpecialConnectors
                            con.AttachedConnector = Nothing
                            con.IsAttached = False
                        Next
                    End If
                    .EnergyConnector.AttachedConnector = Nothing
                    .EnergyConnector.IsAttached = False
                End With
                myDWOBJ.Nome = myDWOBJ.GraphicObject.Name
                Flowsheet.Collections.RefluxedAbsorberCollection.Add(myDWOBJ.GraphicObject.Name, myDWOBJ.GraphicObject)
                Flowsheet.Collections.ObjectCollection.Add(myDWOBJ.Nome, myDWOBJ)
                Flowsheet.Collections.CLCS_RefluxedAbsorberCollection.Add(myDWOBJ.Nome, myDWOBJ)
                Me.FlowsheetDesignSurface.drawingObjects.Add(myDWOBJ.GraphicObject)
            Case TipoObjeto.ComponentSeparator
                Dim myDWOBJ As DWSIM.SimulationObjects.UnitOps.ComponentSeparator = CType(newobj, DWSIM.SimulationObjects.UnitOps.ComponentSeparator)
                With myDWOBJ.GraphicObject
                    .Calculated = False
                    .Name = "CS-" & Guid.NewGuid.ToString
                    .Tag = searchtext & " (" & (objcount + 1).ToString & ")"
                    .X = mpx
                    .Y = mpy
                    For Each con As ConnectionPoint In .InputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    For Each con As ConnectionPoint In .OutputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    If Not .SpecialConnectors Is Nothing Then
                        For Each con As ConnectionPoint In .SpecialConnectors
                            con.AttachedConnector = Nothing
                            con.IsAttached = False
                        Next
                    End If
                    .EnergyConnector.AttachedConnector = Nothing
                    .EnergyConnector.IsAttached = False
                End With
                myDWOBJ.Nome = myDWOBJ.GraphicObject.Name
                Flowsheet.Collections.ComponentSeparatorCollection.Add(myDWOBJ.GraphicObject.Name, myDWOBJ.GraphicObject)
                Flowsheet.Collections.ObjectCollection.Add(myDWOBJ.Nome, myDWOBJ)
                Flowsheet.Collections.CLCS_ComponentSeparatorCollection.Add(myDWOBJ.Nome, myDWOBJ)
                Me.FlowsheetDesignSurface.drawingObjects.Add(myDWOBJ.GraphicObject)
            Case TipoObjeto.SolidSeparator
                Dim myDWOBJ As DWSIM.SimulationObjects.UnitOps.SolidsSeparator = CType(newobj, DWSIM.SimulationObjects.UnitOps.SolidsSeparator)
                With myDWOBJ.GraphicObject
                    .Calculated = False
                    .Name = "SS-" & Guid.NewGuid.ToString
                    .Tag = searchtext & " (" & (objcount + 1).ToString & ")"
                    .X = mpx
                    .Y = mpy
                    For Each con As ConnectionPoint In .InputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    For Each con As ConnectionPoint In .OutputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    If Not .SpecialConnectors Is Nothing Then
                        For Each con As ConnectionPoint In .SpecialConnectors
                            con.AttachedConnector = Nothing
                            con.IsAttached = False
                        Next
                    End If
                    .EnergyConnector.AttachedConnector = Nothing
                    .EnergyConnector.IsAttached = False
                End With
                myDWOBJ.Nome = myDWOBJ.GraphicObject.Name
                Flowsheet.Collections.SolidsSeparatorCollection.Add(myDWOBJ.GraphicObject.Name, myDWOBJ.GraphicObject)
                Flowsheet.Collections.ObjectCollection.Add(myDWOBJ.Nome, myDWOBJ)
                Flowsheet.Collections.CLCS_SolidsSeparatorCollection.Add(myDWOBJ.Nome, myDWOBJ)
                Me.FlowsheetDesignSurface.drawingObjects.Add(myDWOBJ.GraphicObject)
            Case TipoObjeto.Filter
                Dim myDWOBJ As DWSIM.SimulationObjects.UnitOps.Filter = CType(newobj, DWSIM.SimulationObjects.UnitOps.Filter)
                With myDWOBJ.GraphicObject
                    .Calculated = False
                    .Name = "FT-" & Guid.NewGuid.ToString
                    .Tag = searchtext & " (" & (objcount + 1).ToString & ")"
                    .X = mpx
                    .Y = mpy
                     For Each con As ConnectionPoint In .InputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    For Each con As ConnectionPoint In .OutputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    If Not .SpecialConnectors Is Nothing Then
                        For Each con As ConnectionPoint In .SpecialConnectors
                            con.AttachedConnector = Nothing
                            con.IsAttached = False
                        Next
                    End If
                    .EnergyConnector.AttachedConnector = Nothing
                    .EnergyConnector.IsAttached = False
                End With
                myDWOBJ.Nome = myDWOBJ.GraphicObject.Name
                Flowsheet.Collections.FilterCollection.Add(myDWOBJ.GraphicObject.Name, myDWOBJ.GraphicObject)
                Flowsheet.Collections.ObjectCollection.Add(myDWOBJ.Nome, myDWOBJ)
                Flowsheet.Collections.CLCS_FilterCollection.Add(myDWOBJ.Nome, myDWOBJ)
                Me.FlowsheetDesignSurface.drawingObjects.Add(myDWOBJ.GraphicObject)
            Case TipoObjeto.OrificePlate
                Dim myDWOBJ As DWSIM.SimulationObjects.UnitOps.OrificePlate = CType(newobj, DWSIM.SimulationObjects.UnitOps.OrificePlate)
                With myDWOBJ.GraphicObject
                    .Calculated = False
                    .Name = "OP-" & Guid.NewGuid.ToString
                    .Tag = searchtext & " (" & (objcount + 1).ToString & ")"
                    .X = mpx
                    .Y = mpy
                    For Each con As ConnectionPoint In .InputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    For Each con As ConnectionPoint In .OutputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    If Not .SpecialConnectors Is Nothing Then
                        For Each con As ConnectionPoint In .SpecialConnectors
                            con.AttachedConnector = Nothing
                            con.IsAttached = False
                        Next
                    End If
                    .EnergyConnector.AttachedConnector = Nothing
                    .EnergyConnector.IsAttached = False
                End With
                myDWOBJ.Nome = myDWOBJ.GraphicObject.Name
                Flowsheet.Collections.OrificePlateCollection.Add(myDWOBJ.GraphicObject.Name, myDWOBJ.GraphicObject)
                Flowsheet.Collections.ObjectCollection.Add(myDWOBJ.Nome, myDWOBJ)
                Flowsheet.Collections.CLCS_OrificePlateCollection.Add(myDWOBJ.Nome, myDWOBJ)
                Me.FlowsheetDesignSurface.drawingObjects.Add(myDWOBJ.GraphicObject)
            Case TipoObjeto.CustomUO
                Dim myDWOBJ As DWSIM.SimulationObjects.UnitOps.CustomUO = CType(newobj, DWSIM.SimulationObjects.UnitOps.CustomUO)
                With myDWOBJ.GraphicObject
                    .Calculated = False
                    .Name = "UO-" & Guid.NewGuid.ToString
                    .Tag = searchtext & " (" & (objcount + 1).ToString & ")"
                    .X = mpx
                    .Y = mpy
                    For Each con As ConnectionPoint In .InputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    For Each con As ConnectionPoint In .OutputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    If Not .SpecialConnectors Is Nothing Then
                        For Each con As ConnectionPoint In .SpecialConnectors
                            con.AttachedConnector = Nothing
                            con.IsAttached = False
                        Next
                    End If
                    .EnergyConnector.AttachedConnector = Nothing
                    .EnergyConnector.IsAttached = False
                End With
                myDWOBJ.Nome = myDWOBJ.GraphicObject.Name
                Flowsheet.Collections.CustomUOCollection.Add(myDWOBJ.GraphicObject.Name, myDWOBJ.GraphicObject)
                Flowsheet.Collections.ObjectCollection.Add(myDWOBJ.Nome, myDWOBJ)
                Flowsheet.Collections.CLCS_CustomUOCollection.Add(myDWOBJ.Nome, myDWOBJ)
                Me.FlowsheetDesignSurface.drawingObjects.Add(myDWOBJ.GraphicObject)
            Case TipoObjeto.CapeOpenUO, TipoObjeto.FlowsheetUO
                MessageBox.Show("Cloning is not supported by CAPE-OPEN/Flowsheet Unit Operations.", "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
        End Select
        Me.FlowsheetDesignSurface.Invalidate()

    End Sub

    Private Sub CMS_ItemsToDisconnect_ItemClicked(ByVal sender As System.Object, ByVal e As System.Windows.Forms.ToolStripItemClickedEventArgs) Handles CMS_ItemsToDisconnect.ItemClicked

        Me.Flowsheet.DisconnectObject(Me.FlowsheetDesignSurface.SelectedObject, FormFlowsheet.SearchSurfaceObjectsByTag(e.ClickedItem.Text, Me.FlowsheetDesignSurface), True)

    End Sub

    Private Sub CMS_ItemsToConnect_ItemClicked(ByVal sender As System.Object, ByVal e As System.Windows.Forms.ToolStripItemClickedEventArgs) Handles CMS_ItemsToConnect.ItemClicked

        Call Me.Flowsheet.ConnectObject(Me.FlowsheetDesignSurface.SelectedObject, FormFlowsheet.SearchSurfaceObjectsByTag(e.ClickedItem.Text, Me.FlowsheetDesignSurface))

    End Sub

    Function IsObjectDownstreamConnectable(ByVal objTag As String) As Boolean

        Dim obj As GraphicObject = FormFlowsheet.SearchSurfaceObjectsByTag(objTag, Me.FlowsheetDesignSurface)

        If Not obj Is Nothing Then

            Dim cp As ConnectionPoint
            For Each cp In obj.OutputConnectors
                If cp.IsAttached = False Then Return True
            Next

        End If

        Return False

    End Function

    Function IsObjectUpstreamConnectable(ByVal objTag As String) As Boolean

        Dim obj As GraphicObject = FormFlowsheet.SearchSurfaceObjectsByTag(objTag, Me.FlowsheetDesignSurface)

        If Not obj Is Nothing Then

            Dim cp As ConnectionPoint
            For Each cp In obj.InputConnectors
                If cp.IsAttached = False Then Return True
            Next

        End If

        Return False

    End Function

    Function ReturnDownstreamConnectibles(ByVal objtag As String)

        Dim refobj As GraphicObject = FormFlowsheet.SearchSurfaceObjectsByTag(objtag, Me.FlowsheetDesignSurface)

        Dim obj As DWSIM.SimulationObjects.UnitOperations.BaseClass
        Dim cp As ConnectionPoint

        Dim conables As New ArrayList

        For Each obj In Me.Flowsheet.Collections.ObjectCollection.Values
            If obj.GraphicObject.Tag <> refobj.Tag Then
                If obj.GraphicObject.TipoObjeto <> TipoObjeto.GO_Texto And _
                    obj.GraphicObject.TipoObjeto <> TipoObjeto.GO_TabelaRapida And _
                    obj.GraphicObject.TipoObjeto <> TipoObjeto.GO_MasterTable And _
                    obj.GraphicObject.TipoObjeto <> TipoObjeto.GO_SpreadsheetTable And _
                    obj.GraphicObject.TipoObjeto <> TipoObjeto.GO_Tabela And _
                    obj.GraphicObject.TipoObjeto <> TipoObjeto.OT_Ajuste And _
                    obj.GraphicObject.TipoObjeto <> TipoObjeto.OT_Especificacao And _
                    obj.GraphicObject.TipoObjeto <> TipoObjeto.DistillationColumn And _
                    obj.GraphicObject.TipoObjeto <> TipoObjeto.AbsorptionColumn And _
                    obj.GraphicObject.TipoObjeto <> TipoObjeto.RefluxedAbsorber And _
                    obj.GraphicObject.TipoObjeto <> TipoObjeto.ReboiledAbsorber And _
                    obj.GraphicObject.TipoObjeto <> TipoObjeto.Nenhum Then

                    If refobj.TipoObjeto = TipoObjeto.MaterialStream Then
                        For Each cp In obj.GraphicObject.InputConnectors
                            If Not cp.IsAttached And Not conables.Contains(obj.GraphicObject.Tag) And Not _
                            obj.GraphicObject.TipoObjeto = TipoObjeto.MaterialStream And Not _
                            obj.GraphicObject.TipoObjeto = TipoObjeto.EnergyStream And _
                            cp.Type = ConType.ConIn Then conables.Add(obj.GraphicObject.Tag)
                        Next
                    ElseIf refobj.TipoObjeto = TipoObjeto.EnergyStream Then
                        If obj.GraphicObject.TipoObjeto <> TipoObjeto.Heater And _
                        obj.GraphicObject.TipoObjeto <> TipoObjeto.Pump And _
                        obj.GraphicObject.TipoObjeto <> TipoObjeto.Compressor And _
                        obj.GraphicObject.TipoObjeto <> TipoObjeto.MaterialStream Then
                            cp = obj.GraphicObject.EnergyConnector
                            If Not cp.IsAttached And Not conables.Contains(obj.GraphicObject.Tag) Then conables.Add(obj.GraphicObject.Tag)
                        ElseIf obj.GraphicObject.TipoObjeto = TipoObjeto.MaterialStream Then

                        Else
                            For Each cp In obj.GraphicObject.InputConnectors
                                If Not cp.IsAttached And Not conables.Contains(obj.GraphicObject.Tag) And Not _
                                obj.GraphicObject.TipoObjeto = TipoObjeto.MaterialStream And Not _
                                obj.GraphicObject.TipoObjeto = TipoObjeto.EnergyStream And _
                                cp.Type = ConType.ConEn Then conables.Add(obj.GraphicObject.Tag)
                            Next
                        End If
                    Else
                        For Each cp In obj.GraphicObject.InputConnectors
                            If Not cp.IsAttached And Not conables.Contains(obj.GraphicObject.Tag) Then conables.Add(obj.GraphicObject.Tag)
                        Next
                        If obj.GraphicObject.TipoObjeto = TipoObjeto.MaterialStream Then
                            cp = obj.GraphicObject.InputConnectors(0)
                            If Not cp.IsAttached And Not conables.Contains(obj.GraphicObject.Tag) Then conables.Add(obj.GraphicObject.Tag)
                        ElseIf obj.GraphicObject.TipoObjeto = TipoObjeto.EnergyStream Then
                            cp = obj.GraphicObject.InputConnectors(0)
                            If Not cp.IsAttached And Not refobj.EnergyConnector.IsAttached And Not conables.Contains(obj.GraphicObject.Tag) Then conables.Add(obj.GraphicObject.Tag)
                        End If
                    End If
                End If
            End If
        Next

        Return conables

    End Function

    Function ReturnDownstreamDisconnectables(ByVal objTag As String) As ArrayList

        Dim obj As GraphicObject = FormFlowsheet.SearchSurfaceObjectsByTag(objTag, Me.FlowsheetDesignSurface)

        Dim conables As New ArrayList

        If Not obj Is Nothing Then

            Dim cp As ConnectionPoint
            For Each cp In obj.OutputConnectors
                If cp.AttachedConnector.AttachedTo.TipoObjeto <> TipoObjeto.AbsorptionColumn And cp.AttachedConnector.AttachedTo.TipoObjeto <> TipoObjeto.DistillationColumn And _
                cp.AttachedConnector.AttachedTo.TipoObjeto <> TipoObjeto.RefluxedAbsorber And cp.AttachedConnector.AttachedTo.TipoObjeto <> TipoObjeto.ReboiledAbsorber Then
                    If cp.IsAttached = True And Not conables.Contains(cp.AttachedConnector.AttachedTo.Tag) Then conables.Add(cp.AttachedConnector.AttachedTo.Tag)
                End If
            Next
        End If

        Return conables

    End Function

    Function ReturnUpstreamDisconnectables(ByVal objTag As String) As ArrayList

        Dim obj As GraphicObject = FormFlowsheet.SearchSurfaceObjectsByTag(objTag, Me.FlowsheetDesignSurface)

        Dim conables As New ArrayList

        If Not obj Is Nothing Then

            Dim cp As ConnectionPoint
            For Each cp In obj.InputConnectors
                If cp.AttachedConnector.AttachedTo.TipoObjeto <> TipoObjeto.AbsorptionColumn And cp.AttachedConnector.AttachedTo.TipoObjeto <> TipoObjeto.DistillationColumn And _
                cp.AttachedConnector.AttachedTo.TipoObjeto <> TipoObjeto.RefluxedAbsorber And cp.AttachedConnector.AttachedTo.TipoObjeto <> TipoObjeto.ReboiledAbsorber Then
                    If cp.IsAttached = True And Not conables.Contains(cp.AttachedConnector.AttachedFrom.Tag) Then conables.Add(cp.AttachedConnector.AttachedFrom.Tag)
                End If
            Next

        End If

        Return conables

    End Function

    Private Sub LabelSimMode_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles LabelCalculator.TextChanged
        If Me.LabelCalculator.Text.Contains(DWSIM.App.GetLocalString("Calculando")) Then
            Me.PictureBox3.Image = My.Resources.weather_lightning
        Else
            Me.PictureBox3.Image = My.Resources.tick
        End If
    End Sub

    Private Sub ExcluirToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ExcluirToolStripMenuItem.Click
        Call Me.Flowsheet.DeleteSelectedObject(sender, e, Me.FlowsheetDesignSurface.SelectedObject)
    End Sub

    Private Sub HorizontalmenteToolStripMenuItem_Click_1(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles HorizontalmenteToolStripMenuItem.Click
        If Me.HorizontalmenteToolStripMenuItem.Checked Then
            Me.FlowsheetDesignSurface.SelectedObject.FlippedH = True
        Else
            Me.FlowsheetDesignSurface.SelectedObject.FlippedH = False
        End If
        Me.FlowsheetDesignSurface.Invalidate()
    End Sub

    Public Function AddObjectToSurface(ByVal type As TipoObjeto, ByVal x As Integer, ByVal y As Integer, Optional ByVal tag As String = "", Optional ByVal id As String = "") As String

        Flowsheet = My.Application.ActiveSimulation

        If Flowsheet.Collections.ObjectCounter Is Nothing Then Flowsheet.Collections.InitializeCounter()

        Dim gObj As GraphicObject = Nothing
        Dim fillclr As Color = Color.WhiteSmoke
        Dim lineclr As Color = Color.Red
        Dim mpx = x '- SplitContainer1.SplitterDistance
        Dim mpy = y '- ToolStripContainer1.TopToolStripPanel.Height

        Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("FLSH004"), Color.Black, DWSIM.FormClasses.TipoAviso.Dica)
        Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("FLSH006"), Color.Black, DWSIM.FormClasses.TipoAviso.Dica)

        Select Case type

            Case TipoObjeto.OT_Ajuste

                Dim myNode As New AdjustGraphic(mpx, mpy, 20, 20, 0)
                myNode.LineWidth = 2
                myNode.Fill = True
                myNode.FillColor = fillclr
                myNode.LineColor = lineclr
                myNode.Tag = "ADJ-" & Format(Flowsheet.Collections.ObjectCounter("ADJU"), "00#")
                Flowsheet.Collections.UpdateCounter("ADJU")
                If tag <> "" Then myNode.Tag = tag
                gObj = myNode
                gObj.Name = DWSIM.App.GetLocalString("AJ") & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.AdjustCollection.Add(gObj.Name, myNode)
                'If Not DWSIM.App.IsRunningOnMono Then Flowsheet.FormObjList.TreeViewObj.Nodes("NodeMX").Nodes.Add(gObj.Name, gObj.Tag).Name = gObj.Name
                'If Not DWSIM.App.IsRunningOnMono Then Flowsheet.FormObjList.TreeViewObj.Nodes("NodeMX").Nodes(gObj.Name).ContextMenuStrip = Flowsheet.FormObjList.ContextMenuStrip1
                'OBJETO DWSIM
                Dim myADJ As DWSIM.SimulationObjects.SpecialOps.Adjust = New DWSIM.SimulationObjects.SpecialOps.Adjust(myNode.Name, "Ajuste")
                myADJ.GraphicObject = myNode
                Flowsheet.Collections.ObjectCollection.Add(myNode.Name, myADJ)
                Flowsheet.Collections.CLCS_AdjustCollection.Add(myNode.Name, myADJ)

                Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("ADJT001"), Color.Black, DWSIM.FormClasses.TipoAviso.Dica)

            Case TipoObjeto.OT_Especificacao

                Dim myNode As New SpecGraphic(mpx, mpy, 20, 20, 0)
                myNode.LineWidth = 2
                myNode.Fill = True
                myNode.FillColor = fillclr
                myNode.LineColor = lineclr
                myNode.Tag = "SPEC-" & Format(Flowsheet.Collections.ObjectCounter("SPEC"), "00#")
                Flowsheet.Collections.UpdateCounter("SPEC")
                If tag <> "" Then myNode.Tag = tag
                gObj = myNode
                gObj.Name = "ES-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.SpecCollection.Add(gObj.Name, myNode)
                'If Not DWSIM.App.IsRunningOnMono Then Flowsheet.FormObjList.TreeViewObj.Nodes("NodeMX").Nodes.Add(gObj.Name, gObj.Tag).Name = gObj.Name
                'If Not DWSIM.App.IsRunningOnMono Then Flowsheet.FormObjList.TreeViewObj.Nodes("NodeMX").Nodes(gObj.Name).ContextMenuStrip = Flowsheet.FormObjList.ContextMenuStrip1
                'OBJETO DWSIM
                Dim myADJ As DWSIM.SimulationObjects.SpecialOps.Spec = New DWSIM.SimulationObjects.SpecialOps.Spec(myNode.Name, "Especificao")
                myADJ.GraphicObject = myNode
                Flowsheet.Collections.ObjectCollection.Add(myNode.Name, myADJ)
                Flowsheet.Collections.CLCS_SpecCollection.Add(myNode.Name, myADJ)

            Case TipoObjeto.OT_Reciclo

                Dim myNode As New RecycleGraphic(mpx, mpy, 20, 20, 0)
                myNode.LineWidth = 2
                myNode.Fill = True
                myNode.FillColor = fillclr
                myNode.LineColor = lineclr
                myNode.Tag = "REC-" & Format(Flowsheet.Collections.ObjectCounter("RECY"), "00#")
                Flowsheet.Collections.UpdateCounter("RECY")
                If tag <> "" Then myNode.Tag = tag
                gObj = myNode
                gObj.Name = "REC-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.RecycleCollection.Add(gObj.Name, myNode)
                'If Not DWSIM.App.IsRunningOnMono Then Flowsheet.FormObjList.TreeViewObj.Nodes("NodeMX").Nodes.Add(gObj.Name, gObj.Tag).Name = gObj.Name
                'If Not DWSIM.App.IsRunningOnMono Then Flowsheet.FormObjList.TreeViewObj.Nodes("NodeMX").Nodes(gObj.Name).ContextMenuStrip = Flowsheet.FormObjList.ContextMenuStrip1
                'OBJETO DWSIM
                Dim myADJ As DWSIM.SimulationObjects.SpecialOps.Recycle = New DWSIM.SimulationObjects.SpecialOps.Recycle(myNode.Name, "Reciclo")
                myADJ.GraphicObject = myNode
                Flowsheet.Collections.ObjectCollection.Add(myNode.Name, myADJ)
                Flowsheet.Collections.CLCS_RecycleCollection.Add(myNode.Name, myADJ)

                Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("RECY001"), Color.Black, DWSIM.FormClasses.TipoAviso.Dica)

            Case TipoObjeto.OT_EnergyRecycle

                Dim myNode As New EnergyRecycleGraphic(mpx, mpy, 20, 20, 0)
                myNode.LineWidth = 2
                myNode.Fill = True
                myNode.FillColor = fillclr
                myNode.LineColor = lineclr
                If Not Flowsheet.Collections.ObjectCounter.ContainsKey("EREC") Then
                    Flowsheet.Collections.ObjectCounter.Add("EREC", 0)
                End If
                myNode.Tag = "EREC-" & Format(Flowsheet.Collections.ObjectCounter("EREC"), "00#")
                Flowsheet.Collections.UpdateCounter("EREC")
                If tag <> "" Then myNode.Tag = tag
                gObj = myNode
                gObj.Name = "EREC-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.EnergyRecycleCollection.Add(gObj.Name, myNode)
                'If Not DWSIM.App.IsRunningOnMono Then Flowsheet.FormObjList.TreeViewObj.Nodes("NodeMX").Nodes.Add(gObj.Name, gObj.Tag).Name = gObj.Name
                'If Not DWSIM.App.IsRunningOnMono Then Flowsheet.FormObjList.TreeViewObj.Nodes("NodeMX").Nodes(gObj.Name).ContextMenuStrip = Flowsheet.FormObjList.ContextMenuStrip1
                'OBJETO DWSIM
                Dim myADJ As DWSIM.SimulationObjects.SpecialOps.EnergyRecycle = New DWSIM.SimulationObjects.SpecialOps.EnergyRecycle(myNode.Name, "EnergyRecycle")
                myADJ.GraphicObject = myNode
                Flowsheet.Collections.ObjectCollection.Add(myNode.Name, myADJ)
                Flowsheet.Collections.CLCS_EnergyRecycleCollection.Add(myNode.Name, myADJ)

            Case TipoObjeto.NodeIn

                Dim myNode As New NodeInGraphic(mpx, mpy, 20, 20, 0)
                myNode.LineWidth = 2
                myNode.Fill = True
                myNode.FillColor = fillclr
                myNode.LineColor = lineclr
                myNode.Tag = "MIX-" & Format(Flowsheet.Collections.ObjectCounter("MIXR"), "00#")
                Flowsheet.Collections.UpdateCounter("MIXR")
                If tag <> "" Then myNode.Tag = tag
                gObj = myNode
                gObj.Name = "MIST-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.MixerCollection.Add(gObj.Name, myNode)
                 'OBJETO DWSIM
                Dim myCOMIX As DWSIM.SimulationObjects.UnitOps.Mixer = New DWSIM.SimulationObjects.UnitOps.Mixer(myNode.Name, "Misturador")
                myCOMIX.GraphicObject = myNode
                Flowsheet.Collections.ObjectCollection.Add(myNode.Name, myCOMIX)
                Flowsheet.Collections.CLCS_MixerCollection.Add(myNode.Name, myCOMIX)

            Case TipoObjeto.NodeEn

                Dim myNode As New NodeEnGraphic(mpx, mpy, 20, 20, 0)
                myNode.LineWidth = 2
                myNode.Fill = True
                myNode.GradientMode = False
                myNode.FillColor = Color.LightYellow
                myNode.LineColor = lineclr
                myNode.Tag = "MIX-ME-" & Format(Flowsheet.Collections.ObjectCounter("MXEN"), "00#")
                Flowsheet.Collections.UpdateCounter("MXEN")
                If tag <> "" Then myNode.Tag = tag
                gObj = myNode
                gObj.Name = "MIST_ME-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.MixerENCollection.Add(gObj.Name, myNode)
                'OBJETO DWSIM
                Dim myCOMIX As DWSIM.SimulationObjects.UnitOps.EnergyMixer = New DWSIM.SimulationObjects.UnitOps.EnergyMixer(myNode.Name, "MisturadorMATEN")
                myCOMIX.GraphicObject = myNode
                Flowsheet.Collections.ObjectCollection.Add(myNode.Name, myCOMIX)
                Flowsheet.Collections.CLCS_EnergyMixerCollection.Add(myNode.Name, myCOMIX)

            Case TipoObjeto.NodeOut

                Dim myNodeo As New NodeOutGraphic(mpx, mpy, 20, 20, 0)
                myNodeo.LineWidth = 2
                myNodeo.Fill = True
                myNodeo.FillColor = fillclr
                myNodeo.LineColor = lineclr
                myNodeo.Tag = "SPLT-" & Format(Flowsheet.Collections.ObjectCounter("SPLI"), "00#")
                Flowsheet.Collections.UpdateCounter("SPLI")
                If tag <> "" Then myNodeo.Tag = tag
                gObj = myNodeo
                gObj.Name = "DIV-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.SplitterCollection.Add(gObj.Name, myNodeo)
                  'OBJETO DWSIM
                Dim myCOSP As DWSIM.SimulationObjects.UnitOps.Splitter = New DWSIM.SimulationObjects.UnitOps.Splitter(myNodeo.Name, "Divisor")
                myCOSP.GraphicObject = myNodeo
                Flowsheet.Collections.ObjectCollection.Add(myNodeo.Name, myCOSP)
                Flowsheet.Collections.CLCS_SplitterCollection.Add(myNodeo.Name, myCOSP)

            Case TipoObjeto.Pump

                Dim myPump As New PumpGraphic(mpx, mpy, 25, 25, 0)
                myPump.LineWidth = 2
                myPump.Fill = True
                myPump.FillColor = fillclr
                myPump.LineColor = lineclr
                myPump.Tag = "PUMP-" & Format(Flowsheet.Collections.ObjectCounter("PUMP"), "00#")
                Flowsheet.Collections.UpdateCounter("PUMP")
                If tag <> "" Then myPump.Tag = tag
                gObj = myPump
                gObj.Name = "BB-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.PumpCollection.Add(gObj.Name, myPump)
                 'OBJETO DWSIM
                Dim myCOSP As DWSIM.SimulationObjects.UnitOps.Pump = New DWSIM.SimulationObjects.UnitOps.Pump(myPump.Name, "Bomba")
                myCOSP.GraphicObject = myPump
                Flowsheet.Collections.ObjectCollection.Add(myPump.Name, myCOSP)
                Flowsheet.Collections.CLCS_PumpCollection.Add(myPump.Name, myCOSP)

            Case TipoObjeto.Tank

                Dim myTank As New TankGraphic(mpx, mpy, 50, 50, 0)
                myTank.LineWidth = 2
                myTank.Fill = True
                myTank.FillColor = fillclr
                myTank.LineColor = lineclr
                myTank.Tag = "TANK-" & Format(Flowsheet.Collections.ObjectCounter("TANK"), "00#")
                Flowsheet.Collections.UpdateCounter("TANK")
                If tag <> "" Then myTank.Tag = tag
                gObj = myTank
                gObj.Name = "TQ-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.TankCollection.Add(gObj.Name, myTank)
                'OBJETO DWSIM
                Dim myCOTK As DWSIM.SimulationObjects.UnitOps.Tank = New DWSIM.SimulationObjects.UnitOps.Tank(myTank.Name, "Tanque")
                myCOTK.GraphicObject = myTank
                Flowsheet.Collections.ObjectCollection.Add(myTank.Name, myCOTK)
                Flowsheet.Collections.CLCS_TankCollection.Add(myTank.Name, myCOTK)

            Case TipoObjeto.Vessel

                Dim myVessel As New VesselGraphic(mpx, mpy, 50, 70, 0)
                myVessel.LineWidth = 2
                myVessel.Fill = True
                myVessel.FillColor = fillclr
                myVessel.LineColor = lineclr
                myVessel.Tag = "SEP-" & Format(Flowsheet.Collections.ObjectCounter("VESS"), "00#")
                Flowsheet.Collections.UpdateCounter("VESS")
                If tag <> "" Then myVessel.Tag = tag
                gObj = myVessel
                gObj.Name = "SEP-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.SeparatorCollection.Add(gObj.Name, myVessel)
                 'OBJETO DWSIM
                Dim myCOVESSEL As DWSIM.SimulationObjects.UnitOps.Vessel = New DWSIM.SimulationObjects.UnitOps.Vessel(myVessel.Name, "VasoSeparadorGL")
                myCOVESSEL.GraphicObject = myVessel
                Flowsheet.Collections.ObjectCollection.Add(myVessel.Name, myCOVESSEL)
                Flowsheet.Collections.CLCS_VesselCollection.Add(myVessel.Name, myCOVESSEL)

            Case TipoObjeto.TPVessel

                Dim myVessel As New TPVesselGraphic(mpx, mpy, 50, 50, 0)
                myVessel.LineWidth = 2
                myVessel.Fill = True
                myVessel.FillColor = fillclr
                myVessel.LineColor = lineclr
                myVessel.Tag = "SEPTF-" & Format(Flowsheet.Collections.ObjectCounter("VES3"), "00#")
                Flowsheet.Collections.UpdateCounter("VES3")
                If tag <> "" Then myVessel.Tag = tag
                gObj = myVessel
                gObj.Name = "SEPTF-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.TPSeparatorCollection.Add(gObj.Name, myVessel)
            
            Case TipoObjeto.MaterialStream

                Dim myMStr As New MaterialStreamGraphic(mpx, mpy, 20, 20, 0)
                myMStr.LineWidth = 2
                myMStr.Fill = True
                myMStr.FillColor = fillclr
                myMStr.LineColor = lineclr
                myMStr.Tag = "MSTR-" & Format(Flowsheet.Collections.ObjectCounter("MSTR"), "00#")
                Flowsheet.Collections.UpdateCounter("MSTR")
                If tag <> "" Then myMStr.Tag = tag
                gObj = myMStr
                gObj.Name = "MAT-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.MaterialStreamCollection.Add(gObj.Name, myMStr)
                'OBJETO DWSIM
                Dim myCOMS As DWSIM.SimulationObjects.Streams.MaterialStream = New DWSIM.SimulationObjects.Streams.MaterialStream(myMStr.Name, "CorrentedeMatria", Flowsheet, Nothing)
                myCOMS.GraphicObject = myMStr
                Flowsheet.AddComponentsRows(myCOMS)
                Flowsheet.Collections.ObjectCollection.Add(myCOMS.Nome, myCOMS)
                Flowsheet.Collections.CLCS_MaterialStreamCollection.Add(myCOMS.Nome, myCOMS)

                Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("MSTR001"), Color.Black, DWSIM.FormClasses.TipoAviso.Dica)
                Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("MSTR002"), Color.Black, DWSIM.FormClasses.TipoAviso.Dica)
                Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("MSTR003"), Color.Black, DWSIM.FormClasses.TipoAviso.Dica)

            Case TipoObjeto.EnergyStream

                Dim myMStr As New EnergyStreamGraphic(mpx, mpy, 20, 20, 0)
                myMStr.LineWidth = 2
                myMStr.Fill = True
                myMStr.FillColor = Color.LightYellow
                myMStr.LineColor = lineclr
                myMStr.Tag = "ESTR-" & Format(Flowsheet.Collections.ObjectCounter("ESTR"), "00#")
                Flowsheet.Collections.UpdateCounter("ESTR")
                If tag <> "" Then myMStr.Tag = tag
                gObj = myMStr
                gObj.Name = "EN-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.EnergyStreamCollection.Add(gObj.Name, myMStr)
                'OBJETO DWSIM
                Dim myCOES As DWSIM.SimulationObjects.Streams.EnergyStream = New DWSIM.SimulationObjects.Streams.EnergyStream(myMStr.Name, "Correntedeenergia")
                myCOES.GraphicObject = myMStr
                Flowsheet.Collections.ObjectCollection.Add(myCOES.Nome, myCOES)
                Flowsheet.Collections.CLCS_EnergyStreamCollection.Add(myCOES.Nome, myCOES)

            Case TipoObjeto.Compressor

                Dim myComp As New CompressorGraphic(mpx, mpy, 25, 25, 0)
                myComp.LineWidth = 2
                myComp.Fill = True
                myComp.FillColor = fillclr
                myComp.LineColor = lineclr
                myComp.Tag = "COMP-" & Format(Flowsheet.Collections.ObjectCounter("COMP"), "00#")
                Flowsheet.Collections.UpdateCounter("COMP")
                If tag <> "" Then myComp.Tag = tag
                gObj = myComp
                gObj.Name = "COMP-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.CompressorCollection.Add(gObj.Name, myComp)
                 'OBJETO DWSIM
                Dim myCOCP As DWSIM.SimulationObjects.UnitOps.Compressor = New DWSIM.SimulationObjects.UnitOps.Compressor(myComp.Name, "CompressorAdiabtico")
                myCOCP.GraphicObject = myComp
                Flowsheet.Collections.ObjectCollection.Add(myComp.Name, myCOCP)
                Flowsheet.Collections.CLCS_CompressorCollection.Add(myComp.Name, myCOCP)

            Case TipoObjeto.Expander

                Dim myComp As New TurbineGraphic(mpx, mpy, 25, 25, 0)
                myComp.LineWidth = 2
                myComp.Fill = True
                myComp.FillColor = fillclr
                myComp.LineColor = lineclr
                myComp.Tag = "EXP-" & Format(Flowsheet.Collections.ObjectCounter("EXPN"), "00#")
                Flowsheet.Collections.UpdateCounter("EXPN")
                If tag <> "" Then myComp.Tag = tag
                gObj = myComp
                gObj.Name = "TURB-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.TurbineCollection.Add(gObj.Name, myComp)
                'OBJETO DWSIM
                Dim myCOCP As DWSIM.SimulationObjects.UnitOps.Expander = New DWSIM.SimulationObjects.UnitOps.Expander(myComp.Name, "TurbinaAdiabtica")
                myCOCP.GraphicObject = myComp
                Flowsheet.Collections.ObjectCollection.Add(myComp.Name, myCOCP)
                Flowsheet.Collections.CLCS_TurbineCollection.Add(myComp.Name, myCOCP)

            Case TipoObjeto.Cooler

                Dim myCool As New CoolerGraphic(mpx, mpy, 25, 25, 0)
                myCool.LineWidth = 2
                myCool.Fill = True
                myCool.FillColor = fillclr
                myCool.LineColor = lineclr
                myCool.Tag = "COOL-" & Format(Flowsheet.Collections.ObjectCounter("COOL"), "00#")
                Flowsheet.Collections.UpdateCounter("COOL")
                If tag <> "" Then myCool.Tag = tag
                gObj = myCool
                gObj.Name = "RESF-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.CoolerCollection.Add(gObj.Name, myCool)
               'OBJETO DWSIM
                Dim myCOCL As DWSIM.SimulationObjects.UnitOps.Cooler = New DWSIM.SimulationObjects.UnitOps.Cooler(myCool.Name, "Resfriador")
                myCOCL.GraphicObject = myCool
                Flowsheet.Collections.ObjectCollection.Add(myCool.Name, myCOCL)
                Flowsheet.Collections.CLCS_CoolerCollection.Add(myCool.Name, myCOCL)

            Case TipoObjeto.Heater

                Dim myHeat As New HeaterGraphic(mpx, mpy, 25, 25, 0)
                myHeat.LineWidth = 2
                myHeat.Fill = True
                myHeat.FillColor = fillclr
                myHeat.LineColor = lineclr
                myHeat.Tag = "HEAT-" & Format(Flowsheet.Collections.ObjectCounter("HEAT"), "00#")
                Flowsheet.Collections.UpdateCounter("HEAT")
                If tag <> "" Then myHeat.Tag = tag
                gObj = myHeat
                gObj.Name = "AQ-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.HeaterCollection.Add(gObj.Name, myHeat)
                 'OBJETO DWSIM
                Dim myCOCL As DWSIM.SimulationObjects.UnitOps.Heater = New DWSIM.SimulationObjects.UnitOps.Heater(myHeat.Name, "Aquecedor")
                myCOCL.GraphicObject = myHeat
                Flowsheet.Collections.ObjectCollection.Add(myHeat.Name, myCOCL)
                Flowsheet.Collections.CLCS_HeaterCollection.Add(myHeat.Name, myCOCL)

            Case TipoObjeto.Pipe

                Dim myPipe As New PipeGraphic(mpx, mpy, 50, 10, 0)
                myPipe.LineWidth = 2
                myPipe.Fill = True
                myPipe.FillColor = fillclr
                myPipe.LineColor = lineclr
                myPipe.Tag = "PIPE-" & Format(Flowsheet.Collections.ObjectCounter("PIPE"), "00#")
                Flowsheet.Collections.UpdateCounter("PIPE")
                If tag <> "" Then myPipe.Tag = tag
                gObj = myPipe
                gObj.Name = "TUB-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.PipeCollection.Add(gObj.Name, myPipe)
                 'OBJETO DWSIM
                Dim myCOPIPE As DWSIM.SimulationObjects.UnitOps.Pipe = New DWSIM.SimulationObjects.UnitOps.Pipe(myPipe.Name, "Tubulao")
                myCOPIPE.GraphicObject = myPipe
                Flowsheet.Collections.ObjectCollection.Add(myPipe.Name, myCOPIPE)
                Flowsheet.Collections.CLCS_PipeCollection.Add(myPipe.Name, myCOPIPE)

            Case TipoObjeto.Valve

                Dim myValve As New ValveGraphic(mpx, mpy, 20, 20, 0)
                myValve.LineWidth = 2
                myValve.Fill = True
                myValve.FillColor = fillclr
                myValve.LineColor = lineclr
                myValve.Tag = "VALV-" & Format(Flowsheet.Collections.ObjectCounter("VALV"), "00#")
                Flowsheet.Collections.UpdateCounter("VALV")
                If tag <> "" Then myValve.Tag = tag
                gObj = myValve
                gObj.Name = "VALV-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.ValveCollection.Add(gObj.Name, myValve)
                 'OBJETO DWSIM
                Dim myCOVALVE As DWSIM.SimulationObjects.UnitOps.Valve = New DWSIM.SimulationObjects.UnitOps.Valve(myValve.Name, "Vlvula")
                myCOVALVE.GraphicObject = myValve
                Flowsheet.Collections.ObjectCollection.Add(myValve.Name, myCOVALVE)
                Flowsheet.Collections.CLCS_ValveCollection.Add(myValve.Name, myCOVALVE)

            Case TipoObjeto.RCT_Conversion

                Dim myRconv As New ReactorConversionGraphic(mpx, mpy, 50, 50, 0)
                myRconv.LineWidth = 2
                myRconv.Fill = True
                myRconv.FillColor = fillclr
                myRconv.LineColor = lineclr
                myRconv.Tag = "RC-" & Format(Flowsheet.Collections.ObjectCounter("RCON"), "00#")
                Flowsheet.Collections.UpdateCounter("RCON")
                If tag <> "" Then myRconv.Tag = tag
                gObj = myRconv
                gObj.Name = "RC-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.ReactorConversionCollection.Add(gObj.Name, myRconv)
                 'OBJETO DWSIM
                Dim myCORCONV As DWSIM.SimulationObjects.Reactors.Reactor_Conversion = New DWSIM.SimulationObjects.Reactors.Reactor_Conversion(myRconv.Name, "ReatorConversao")
                myCORCONV.GraphicObject = myRconv
                Flowsheet.Collections.ObjectCollection.Add(myRconv.Name, myCORCONV)
                Flowsheet.Collections.CLCS_ReactorConversionCollection.Add(myRconv.Name, myCORCONV)

                Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("RCON001"), Color.Black, DWSIM.FormClasses.TipoAviso.Dica)

            Case TipoObjeto.RCT_Equilibrium

                Dim myReq As New ReactorEquilibriumGraphic(mpx, mpy, 50, 50, 0)
                myReq.LineWidth = 2
                myReq.Fill = True
                myReq.FillColor = fillclr
                myReq.LineColor = lineclr
                myReq.Tag = "RE-" & Format(Flowsheet.Collections.ObjectCounter("REQL"), "00#")
                Flowsheet.Collections.UpdateCounter("REQL")
                If tag <> "" Then myReq.Tag = tag
                gObj = myReq
                gObj.Name = "RE-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.ReactorEquilibriumCollection.Add(gObj.Name, myReq)
                 'OBJETO DWSIM
                Dim myCOREQ As DWSIM.SimulationObjects.Reactors.Reactor_Equilibrium = New DWSIM.SimulationObjects.Reactors.Reactor_Equilibrium(myReq.Name, "ReatorEquilibrio")
                myCOREQ.GraphicObject = myReq
                Flowsheet.Collections.ObjectCollection.Add(myReq.Name, myCOREQ)
                Flowsheet.Collections.CLCS_ReactorEquilibriumCollection.Add(myReq.Name, myCOREQ)

                Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("REQL001"), Color.Black, DWSIM.FormClasses.TipoAviso.Dica)

            Case TipoObjeto.RCT_Gibbs

                Dim myRgibbs As New ReactorGibbsGraphic(mpx, mpy, 50, 50, 0)
                myRgibbs.LineWidth = 2
                myRgibbs.Fill = True
                myRgibbs.FillColor = fillclr
                myRgibbs.LineColor = lineclr
                myRgibbs.Tag = "RG-" & Format(Flowsheet.Collections.ObjectCounter("RGIB"), "00#")
                Flowsheet.Collections.UpdateCounter("RGIB")
                If tag <> "" Then myRgibbs.Tag = tag
                gObj = myRgibbs
                gObj.Name = "RG-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.ReactorGibbsCollection.Add(gObj.Name, myRgibbs)
                'OBJETO DWSIM
                Dim myCORGIBBS As DWSIM.SimulationObjects.Reactors.Reactor_Gibbs = New DWSIM.SimulationObjects.Reactors.Reactor_Gibbs(myRgibbs.Name, "ReatorGibbs")
                myCORGIBBS.GraphicObject = myRgibbs
                Flowsheet.Collections.ObjectCollection.Add(myRgibbs.Name, myCORGIBBS)
                Flowsheet.Collections.CLCS_ReactorGibbsCollection.Add(myRgibbs.Name, myCORGIBBS)

                Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("RGIB001"), Color.Black, DWSIM.FormClasses.TipoAviso.Dica)

            Case TipoObjeto.RCT_CSTR

                Dim myRcstr As New ReactorCSTRGraphic(mpx, mpy, 50, 50, 0)
                myRcstr.LineWidth = 2
                myRcstr.Fill = True
                myRcstr.FillColor = fillclr
                myRcstr.LineColor = lineclr
                myRcstr.Tag = "CSTR-" & Format(Flowsheet.Collections.ObjectCounter("CSTR"), "00#")
                Flowsheet.Collections.UpdateCounter("CSTR")
                If tag <> "" Then myRcstr.Tag = tag
                gObj = myRcstr
                gObj.Name = "CSTR-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.ReactorCSTRCollection.Add(gObj.Name, myRcstr)
                 'OBJETO DWSIM
                Dim myCORCSTR As DWSIM.SimulationObjects.Reactors.Reactor_CSTR = New DWSIM.SimulationObjects.Reactors.Reactor_CSTR(myRcstr.Name, "ReatorCSTR")
                myCORCSTR.GraphicObject = myRcstr
                Flowsheet.Collections.ObjectCollection.Add(myRcstr.Name, myCORCSTR)
                Flowsheet.Collections.CLCS_ReactorCSTRCollection.Add(myRcstr.Name, myCORCSTR)

                Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("CSTR001"), Color.Black, DWSIM.FormClasses.TipoAviso.Dica)

            Case TipoObjeto.RCT_PFR

                Dim myRpfr As New ReactorPFRGraphic(mpx, mpy, 70, 20, 0)
                myRpfr.LineWidth = 2
                myRpfr.Fill = True
                myRpfr.FillColor = fillclr
                myRpfr.LineColor = lineclr
                myRpfr.Tag = "PFR-" & Format(Flowsheet.Collections.ObjectCounter("PFR_"), "00#")
                Flowsheet.Collections.UpdateCounter("PFR_")
                If tag <> "" Then myRpfr.Tag = tag
                gObj = myRpfr
                gObj.Name = "PFR-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.ReactorPFRCollection.Add(gObj.Name, myRpfr)
                'OBJETO DWSIM
                Dim myCOPFR As DWSIM.SimulationObjects.Reactors.Reactor_PFR = New DWSIM.SimulationObjects.Reactors.Reactor_PFR(myRpfr.Name, "ReatorPFR")
                myCOPFR.GraphicObject = myRpfr
                Flowsheet.Collections.ObjectCollection.Add(myRpfr.Name, myCOPFR)
                Flowsheet.Collections.CLCS_ReactorPFRCollection.Add(myRpfr.Name, myCOPFR)

                Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("PFR001"), Color.Black, DWSIM.FormClasses.TipoAviso.Dica)

            Case TipoObjeto.HeatExchanger

                Dim myHeatExchanger As New HeatExchangerGraphic(mpx, mpy, 30, 30, 0)
                myHeatExchanger.LineWidth = 2
                myHeatExchanger.Fill = True
                myHeatExchanger.FillColor = fillclr
                myHeatExchanger.LineColor = lineclr
                myHeatExchanger.Tag = "HE-" & Format(Flowsheet.Collections.ObjectCounter("HXCG"), "00#")
                Flowsheet.Collections.UpdateCounter("HXCG")
                If tag <> "" Then myHeatExchanger.Tag = tag
                gObj = myHeatExchanger
                gObj.Name = "HE-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.HeatExchangerCollection.Add(gObj.Name, myHeatExchanger)
                 'OBJETO DWSIM
                Dim myCOHE As DWSIM.SimulationObjects.UnitOps.HeatExchanger = New DWSIM.SimulationObjects.UnitOps.HeatExchanger(myHeatExchanger.Name, "HeatExchanger")
                myCOHE.GraphicObject = myHeatExchanger
                Flowsheet.Collections.ObjectCollection.Add(myHeatExchanger.Name, myCOHE)
                Flowsheet.Collections.CLCS_HeatExchangerCollection.Add(myHeatExchanger.Name, myCOHE)

            Case TipoObjeto.ShortcutColumn

                Dim mySC As New ShorcutColumnGraphic(mpx, mpy, 144, 180, 0)
                mySC.LineWidth = 2
                mySC.Fill = True
                mySC.FillColor = fillclr
                mySC.LineColor = lineclr
                mySC.Tag = "SC-" & Format(Flowsheet.Collections.ObjectCounter("SCOL"), "00#")
                Flowsheet.Collections.UpdateCounter("SCOL")
                If tag <> "" Then mySC.Tag = tag
                gObj = mySC
                gObj.Name = "SC-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.ShortcutColumnCollection.Add(gObj.Name, mySC)
                 'OBJETO DWSIM
                Dim myCOSC As DWSIM.SimulationObjects.UnitOps.ShortcutColumn = New DWSIM.SimulationObjects.UnitOps.ShortcutColumn(mySC.Name, "ShortcutColumn")
                myCOSC.GraphicObject = mySC
                Flowsheet.Collections.ObjectCollection.Add(mySC.Name, myCOSC)
                Flowsheet.Collections.CLCS_ShortcutColumnCollection.Add(mySC.Name, myCOSC)

            Case TipoObjeto.DistillationColumn

                Dim mySC As New DistillationColumnGraphic(mpx, mpy, 144, 180, 0)
                mySC.LineWidth = 2
                mySC.Fill = True
                mySC.FillColor = fillclr
                mySC.LineColor = lineclr
                mySC.Tag = "DC-" & Format(Flowsheet.Collections.ObjectCounter("DCOL"), "00#")
                Flowsheet.Collections.UpdateCounter("DCOL")
                If tag <> "" Then mySC.Tag = tag
                gObj = mySC
                gObj.Name = "DC-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.DistillationColumnCollection.Add(gObj.Name, mySC)
                 'OBJETO DWSIM
                Dim myCOSC As DWSIM.SimulationObjects.UnitOps.DistillationColumn = New DWSIM.SimulationObjects.UnitOps.DistillationColumn(mySC.Name, "DistillationColumn")
                myCOSC.GraphicObject = mySC
                Flowsheet.Collections.ObjectCollection.Add(mySC.Name, myCOSC)
                Flowsheet.Collections.CLCS_DistillationColumnCollection.Add(mySC.Name, myCOSC)

                Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("DCOL001"), Color.Black, DWSIM.FormClasses.TipoAviso.Dica)
                Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("DCOL002"), Color.Black, DWSIM.FormClasses.TipoAviso.Dica)
                Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("DCOL003"), Color.Black, DWSIM.FormClasses.TipoAviso.Dica)

            Case TipoObjeto.AbsorptionColumn

                Dim mySC As New AbsorptionColumnGraphic(mpx, mpy, 144, 180, 0)
                mySC.LineWidth = 2
                mySC.Fill = True
                mySC.FillColor = fillclr
                mySC.LineColor = lineclr
                mySC.Tag = "ABS-" & Format(Flowsheet.Collections.ObjectCounter("ACOL"), "00#")
                Flowsheet.Collections.UpdateCounter("ACOL")
                If tag <> "" Then mySC.Tag = tag
                gObj = mySC
                gObj.Name = "ABS-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.AbsorptionColumnCollection.Add(gObj.Name, mySC)
                'OBJETO DWSIM
                Dim myCOSC As DWSIM.SimulationObjects.UnitOps.AbsorptionColumn = New DWSIM.SimulationObjects.UnitOps.AbsorptionColumn(mySC.Name, "AbsorptionColumn")
                myCOSC.GraphicObject = mySC
                Flowsheet.Collections.ObjectCollection.Add(mySC.Name, myCOSC)
                Flowsheet.Collections.CLCS_AbsorptionColumnCollection.Add(mySC.Name, myCOSC)

                Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("DCOL001"), Color.Black, DWSIM.FormClasses.TipoAviso.Dica)
                Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("DCOL002"), Color.Black, DWSIM.FormClasses.TipoAviso.Dica)
                Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("DCOL003"), Color.Black, DWSIM.FormClasses.TipoAviso.Dica)

            Case TipoObjeto.ReboiledAbsorber

                Dim mySC As New ReboiledAbsorberGraphic(mpx, mpy, 144, 180, 0)
                mySC.LineWidth = 2
                mySC.Fill = True
                mySC.FillColor = fillclr
                mySC.LineColor = lineclr
                mySC.Tag = "RBA-" & Format(Flowsheet.Collections.ObjectCounter("RBAB"), "00#")
                Flowsheet.Collections.UpdateCounter("RBAB")
                If tag <> "" Then mySC.Tag = tag
                gObj = mySC
                gObj.Name = "RBA-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.ReboiledAbsorberCollection.Add(gObj.Name, mySC)
                'OBJETO DWSIM
                Dim myCOSC As DWSIM.SimulationObjects.UnitOps.ReboiledAbsorber = New DWSIM.SimulationObjects.UnitOps.ReboiledAbsorber(mySC.Name, "ReboiledAbsorber")
                myCOSC.GraphicObject = mySC
                Flowsheet.Collections.ObjectCollection.Add(mySC.Name, myCOSC)
                Flowsheet.Collections.CLCS_ReboiledAbsorberCollection.Add(mySC.Name, myCOSC)

                Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("DCOL001"), Color.Black, DWSIM.FormClasses.TipoAviso.Dica)
                Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("DCOL002"), Color.Black, DWSIM.FormClasses.TipoAviso.Dica)
                Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("DCOL003"), Color.Black, DWSIM.FormClasses.TipoAviso.Dica)

            Case TipoObjeto.RefluxedAbsorber

                Dim mySC As New RefluxedAbsorberGraphic(mpx, mpy, 144, 180, 0)
                mySC.LineWidth = 2
                mySC.Fill = True
                mySC.FillColor = fillclr
                mySC.LineColor = lineclr
                mySC.Tag = "RFA-" & Format(Flowsheet.Collections.ObjectCounter("RFAB"), "00#")
                Flowsheet.Collections.UpdateCounter("RFAB")
                If tag <> "" Then mySC.Tag = tag
                gObj = mySC
                gObj.Name = "RFA-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.RefluxedAbsorberCollection.Add(gObj.Name, mySC)
                 'OBJETO DWSIM
                Dim myCOSC As DWSIM.SimulationObjects.UnitOps.RefluxedAbsorber = New DWSIM.SimulationObjects.UnitOps.RefluxedAbsorber(mySC.Name, "RefluxedAbsorber")
                myCOSC.GraphicObject = mySC
                Flowsheet.Collections.ObjectCollection.Add(mySC.Name, myCOSC)
                Flowsheet.Collections.CLCS_RefluxedAbsorberCollection.Add(mySC.Name, myCOSC)

                Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("DCOL001"), Color.Black, DWSIM.FormClasses.TipoAviso.Dica)
                Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("DCOL002"), Color.Black, DWSIM.FormClasses.TipoAviso.Dica)
                Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("DCOL003"), Color.Black, DWSIM.FormClasses.TipoAviso.Dica)

            Case TipoObjeto.ComponentSeparator

                Dim myCSep As New ComponentSeparatorGraphic(mpx, mpy, 50, 50, 0)
                myCSep.LineWidth = 2
                myCSep.Fill = True
                myCSep.FillColor = fillclr
                myCSep.LineColor = lineclr
                If Not Flowsheet.Collections.ObjectCounter.ContainsKey("CSEP") Then
                    Flowsheet.Collections.ObjectCounter.Add("CSEP", 0)
                End If
                myCSep.Tag = "CS-" & Format(Flowsheet.Collections.ObjectCounter("CSEP"), "00#")
                Flowsheet.Collections.UpdateCounter("CSEP")
                If tag <> "" Then myCSep.Tag = tag
                gObj = myCSep
                gObj.Name = "CS-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.ComponentSeparatorCollection.Add(gObj.Name, myCSep)
                 'OBJETO DWSIM
                Dim myCOCSEP As DWSIM.SimulationObjects.UnitOps.ComponentSeparator = New DWSIM.SimulationObjects.UnitOps.ComponentSeparator(myCSep.Name, "ComponentSeparator")
                myCOCSEP.GraphicObject = myCSep
                Flowsheet.Collections.ObjectCollection.Add(myCSep.Name, myCOCSEP)
                Flowsheet.Collections.CLCS_ComponentSeparatorCollection.Add(myCSep.Name, myCOCSEP)

            Case TipoObjeto.SolidSeparator

                Dim myCSep As New SolidSeparatorGraphic(mpx, mpy, 50, 50, 0)
                myCSep.LineWidth = 2
                myCSep.Fill = True
                myCSep.FillColor = fillclr
                myCSep.LineColor = lineclr
                If Not Flowsheet.Collections.ObjectCounter.ContainsKey("SOLS") Then
                    Flowsheet.Collections.ObjectCounter.Add("SOLS", 0)
                End If
                myCSep.Tag = "SS-" & Format(Flowsheet.Collections.ObjectCounter("SOLS"), "00#")
                Flowsheet.Collections.UpdateCounter("SOLS")
                If tag <> "" Then myCSep.Tag = tag
                gObj = myCSep
                gObj.Name = "SS-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.SolidsSeparatorCollection.Add(gObj.Name, myCSep)
                'OBJETO DWSIM
                Dim myCOCSEP As DWSIM.SimulationObjects.UnitOps.SolidsSeparator = New DWSIM.SimulationObjects.UnitOps.SolidsSeparator(myCSep.Name, "SolidsSeparator")
                myCOCSEP.GraphicObject = myCSep
                Flowsheet.Collections.ObjectCollection.Add(myCSep.Name, myCOCSEP)
                Flowsheet.Collections.CLCS_SolidsSeparatorCollection.Add(myCSep.Name, myCOCSEP)

            Case TipoObjeto.Filter

                Dim myCSep As New FilterGraphic(mpx, mpy, 50, 50, 0)
                myCSep.LineWidth = 2
                myCSep.Fill = True
                myCSep.FillColor = fillclr
                myCSep.LineColor = lineclr
                If Not Flowsheet.Collections.ObjectCounter.ContainsKey("FILT") Then
                    Flowsheet.Collections.ObjectCounter.Add("FILT", 0)
                End If
                myCSep.Tag = "FT-" & Format(Flowsheet.Collections.ObjectCounter("FILT"), "00#")
                Flowsheet.Collections.UpdateCounter("FILT")
                If tag <> "" Then myCSep.Tag = tag
                gObj = myCSep
                gObj.Name = "FT-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.FilterCollection.Add(gObj.Name, myCSep)
                'OBJETO DWSIM
                Dim myCOCSEP As DWSIM.SimulationObjects.UnitOps.Filter = New DWSIM.SimulationObjects.UnitOps.Filter(myCSep.Name, "Filter")
                myCOCSEP.GraphicObject = myCSep
                Flowsheet.Collections.ObjectCollection.Add(myCSep.Name, myCOCSEP)
                Flowsheet.Collections.CLCS_FilterCollection.Add(myCSep.Name, myCOCSEP)

            Case TipoObjeto.OrificePlate

                Dim myOPL As New OrificePlateGraphic(mpx, mpy, 25, 25, 0)
                myOPL.LineWidth = 2
                myOPL.Fill = True
                myOPL.FillColor = fillclr
                myOPL.LineColor = lineclr
                If Not Flowsheet.Collections.ObjectCounter.ContainsKey("ORIF") Then
                    Flowsheet.Collections.ObjectCounter.Add("ORIF", 0)
                End If
                myOPL.Tag = "OP-" & Format(Flowsheet.Collections.ObjectCounter("ORIF"), "00#")
                Flowsheet.Collections.UpdateCounter("ORIF")
                If tag <> "" Then myOPL.Tag = tag
                gObj = myOPL
                gObj.Name = "OP-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.OrificePlateCollection.Add(gObj.Name, myOPL)
                 'OBJETO DWSIM
                Dim myCOOPL As DWSIM.SimulationObjects.UnitOps.OrificePlate = New DWSIM.SimulationObjects.UnitOps.OrificePlate(myOPL.Name, "OrificePlate")
                myCOOPL.GraphicObject = myOPL
                Flowsheet.Collections.ObjectCollection.Add(myOPL.Name, myCOOPL)
                Flowsheet.Collections.CLCS_OrificePlateCollection.Add(myOPL.Name, myCOOPL)

            Case TipoObjeto.CustomUO

                Dim myCUO As New CustomUOGraphic(mpx, mpy, 25, 25, 0)
                myCUO.LineWidth = 2
                myCUO.Fill = True
                myCUO.FillColor = fillclr
                myCUO.LineColor = lineclr
                If Not Flowsheet.Collections.ObjectCounter.ContainsKey("CUOP") Then
                    Flowsheet.Collections.ObjectCounter.Add("CUOP", 0)
                End If
                myCUO.Tag = "UO-" & Format(Flowsheet.Collections.ObjectCounter("CUOP"), "00#")
                Flowsheet.Collections.UpdateCounter("CUOP")
                If tag <> "" Then myCUO.Tag = tag
                gObj = myCUO
                gObj.Name = "UO-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.CustomUOCollection.Add(gObj.Name, myCUO)
                'OBJETO DWSIM
                Dim myCOCUO As DWSIM.SimulationObjects.UnitOps.CustomUO = New DWSIM.SimulationObjects.UnitOps.CustomUO(myCUO.Name, "CustomUnitOp")
                myCOCUO.GraphicObject = myCUO
                Flowsheet.Collections.ObjectCollection.Add(myCUO.Name, myCOCUO)
                Flowsheet.Collections.CLCS_CustomUOCollection.Add(myCUO.Name, myCOCUO)

                Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("CSUO001"), Color.Black, DWSIM.FormClasses.TipoAviso.Dica)

            Case TipoObjeto.ExcelUO

                Dim myEUO As New ExcelUOGraphic(mpx, mpy, 25, 25, 0)
                myEUO.LineWidth = 2
                myEUO.Fill = True
                myEUO.FillColor = fillclr
                myEUO.LineColor = lineclr
                If Not Flowsheet.Collections.ObjectCounter.ContainsKey("CEOP") Then
                    Flowsheet.Collections.ObjectCounter.Add("CEOP", 0)
                End If
                myEUO.Tag = "EXL-" & Format(Flowsheet.Collections.ObjectCounter("CEOP"), "00#")
                Flowsheet.Collections.UpdateCounter("CEOP")
                If tag <> "" Then myEUO.Tag = tag
                gObj = myEUO
                gObj.Name = "EXL-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.ExcelUOCollection.Add(gObj.Name, myEUO)
                'OBJETO DWSIM
                Dim myCOEUO As DWSIM.SimulationObjects.UnitOps.ExcelUO = New DWSIM.SimulationObjects.UnitOps.ExcelUO(myEUO.Name, "ExcelUnitOp")
                myCOEUO.GraphicObject = myEUO
                Flowsheet.Collections.ObjectCollection.Add(myEUO.Name, myCOEUO)
                Flowsheet.Collections.CLCS_ExcelUOCollection.Add(myEUO.Name, myCOEUO)

            Case TipoObjeto.FlowsheetUO

                Dim myEUO As New FlowsheetUOGraphic(mpx, mpy, 25, 25, 0)
                myEUO.LineWidth = 2
                myEUO.Fill = True
                myEUO.FillColor = fillclr
                myEUO.LineColor = lineclr
                If Not Flowsheet.Collections.ObjectCounter.ContainsKey("FLST") Then
                    Flowsheet.Collections.ObjectCounter.Add("FLST", 0)
                End If
                myEUO.Tag = "FS-" & Format(Flowsheet.Collections.ObjectCounter("FLST"), "00#")
                Flowsheet.Collections.UpdateCounter("FLST")
                If tag <> "" Then myEUO.Tag = tag
                gObj = myEUO
                gObj.Name = "FS-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.FlowsheetUOCollection.Add(gObj.Name, myEUO)
                'OBJETO DWSIM
                Dim myCOEUO As DWSIM.SimulationObjects.UnitOps.Flowsheet = New DWSIM.SimulationObjects.UnitOps.Flowsheet(myEUO.Name, "FlowsheetUnitOp")
                myCOEUO.GraphicObject = myEUO
                Flowsheet.Collections.ObjectCollection.Add(myEUO.Name, myCOEUO)
                Flowsheet.Collections.CLCS_FlowsheetUOCollection.Add(myEUO.Name, myCOEUO)

            Case TipoObjeto.CapeOpenUO

                Dim myCUO As New CapeOpenUOGraphic(mpx, mpy, 40, 40, 0)
                myCUO.LineWidth = 2
                myCUO.Fill = True
                myCUO.FillColor = fillclr
                myCUO.LineColor = lineclr
                If Not Flowsheet.Collections.ObjectCounter.ContainsKey("COOP") Then
                    Flowsheet.Collections.ObjectCounter.Add("COOP", 0)
                End If
                myCUO.Tag = "COUO-" & Format(Flowsheet.Collections.ObjectCounter("COOP"), "00#")
                Flowsheet.Collections.UpdateCounter("COOP")
                If tag <> "" Then myCUO.Tag = tag
                gObj = myCUO
                gObj.Name = "COUO-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.CapeOpenUOCollection.Add(gObj.Name, myCUO)
                'OBJETO DWSIM
                Dim myCOCUO As DWSIM.SimulationObjects.UnitOps.CapeOpenUO = New DWSIM.SimulationObjects.UnitOps.CapeOpenUO(myCUO.Name, "CapeOpenUnitOperation", gObj)
                myCOCUO.GraphicObject = myCUO
                Flowsheet.Collections.ObjectCollection.Add(myCUO.Name, myCOCUO)
                Flowsheet.Collections.CLCS_CapeOpenUOCollection.Add(myCUO.Name, myCOCUO)

                Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("CAPE001"), Color.Black, DWSIM.FormClasses.TipoAviso.Dica)

        End Select
     
        If Not gObj Is Nothing Then
            Me.FlowsheetDesignSurface.drawingObjects.Add(gObj)
            gObj.Draw(Me.FlowsheetDesignSurface.CreateGraphics)
            Me.FlowsheetDesignSurface.SelectedObject = gObj
            Me.FlowsheetDesignSurface.Invalidate()
            Application.DoEvents()
            If My.Application.PushUndoRedoAction Then Flowsheet.AddUndoRedoAction(New UndoRedoAction() With {.AType = UndoRedoActionType.ObjectAdded,
                                     .ObjID = gObj.Name,
                                     .NewValue = gObj,
                                     .Name = String.Format(DWSIM.App.GetLocalString("UndoRedo_ObjectAdded"), gObj.Tag)})
        End If

        Me.FlowsheetDesignSurface.Cursor = Cursors.Arrow

        Return gObj.Name

    End Function

    Private Sub FlowsheetDesignSurface_DragEnter(ByVal sender As System.Object, ByVal e As System.Windows.Forms.DragEventArgs) Handles FlowsheetDesignSurface.DragEnter
        Dim i As Integer
        For i = 0 To e.Data.GetFormats().Length - 1
            If e.Data.GetFormats()(i).Equals _
               ("System.Windows.Forms.DataGridViewRow") Then
                'The data from the drag source is moved to the target.
                e.Effect = DragDropEffects.Copy
            End If
        Next
    End Sub

    Private Sub FlowsheetDesignSurface_DragDrop(ByVal sender As System.Object, ByVal e As System.Windows.Forms.DragEventArgs) Handles FlowsheetDesignSurface.DragDrop

        If e.Effect = DragDropEffects.Copy Then
            Dim obj As DataGridViewRow = e.Data.GetData("System.Windows.Forms.DataGridViewRow")
            Dim tobj As TipoObjeto = TipoObjeto.Nenhum
            Dim p As Point = Me.FlowsheetDesignSurface.PointToClient(New Point(e.X, e.Y))
            Dim mousePT As Point = Flowsheet.gscTogoc(p.X, p.Y)
            Dim mpx = mousePT.X
            Dim mpy = mousePT.Y
            'Dim text As String = Flowsheet.FormObjListView.DataGridView1.Rows(obj.Index).Cells(0).Value.ToString.TrimEnd(" ")
            Select Case text
                Case "Ajuste"
                    tobj = TipoObjeto.OT_Ajuste
                Case "Especificao"
                    tobj = TipoObjeto.OT_Especificacao
                Case "Reciclo"
                    tobj = TipoObjeto.OT_Reciclo
                Case "EnergyRecycle"
                    tobj = TipoObjeto.OT_EnergyRecycle
                Case "Misturador"
                    tobj = TipoObjeto.NodeIn
                Case "Divisor"
                    tobj = TipoObjeto.NodeOut
                Case "Bomba"
                    tobj = TipoObjeto.Pump
                Case "Tanque"
                    tobj = TipoObjeto.Tank
                Case "VasoSeparadorGL"
                    tobj = TipoObjeto.Vessel
                Case "CorrentedeMatria"
                    tobj = TipoObjeto.MaterialStream
                Case "Correntedeenergia"
                    tobj = TipoObjeto.EnergyStream
                Case "CompressorAdiabtico"
                    tobj = TipoObjeto.Compressor
                Case "TurbinaAdiabtica"
                    tobj = TipoObjeto.Expander
                Case "Resfriador"
                    tobj = TipoObjeto.Cooler
                Case "Aquecedor"
                    tobj = TipoObjeto.Heater
                Case "Tubulao"
                    tobj = TipoObjeto.Pipe
                Case "Vlvula"
                    tobj = TipoObjeto.Valve
                Case "ReatorConversao"
                    tobj = TipoObjeto.RCT_Conversion
                Case "ReatorEquilibrio"
                    tobj = TipoObjeto.RCT_Equilibrium
                Case "ReatorGibbs"
                    tobj = TipoObjeto.RCT_Gibbs
                Case "ReatorCSTR"
                    tobj = TipoObjeto.RCT_CSTR
                Case "ReatorPFR"
                    tobj = TipoObjeto.RCT_PFR
                Case "HeatExchanger"
                    tobj = TipoObjeto.HeatExchanger
                Case "ShortcutColumn"
                    tobj = TipoObjeto.ShortcutColumn
                Case "DistillationColumn"
                    tobj = TipoObjeto.DistillationColumn
                Case "AbsorptionColumn"
                    tobj = TipoObjeto.AbsorptionColumn
                Case "ReboiledAbsorber"
                    tobj = TipoObjeto.ReboiledAbsorber
                Case "RefluxedAbsorber"
                    tobj = TipoObjeto.RefluxedAbsorber
                Case "ComponentSeparator"
                    tobj = TipoObjeto.ComponentSeparator
                Case "OrificePlate"
                    tobj = TipoObjeto.OrificePlate
                Case "CustomUnitOp"
                    tobj = TipoObjeto.CustomUO
                Case "ExcelUnitOp"
                    tobj = TipoObjeto.ExcelUO
                Case "CapeOpenUnitOperation"
                    tobj = TipoObjeto.CapeOpenUO
                Case "SolidsSeparator"
                    tobj = TipoObjeto.SolidSeparator
                Case "Filter"
                    tobj = TipoObjeto.Filter
                Case "FlowsheetUnitOp"
                    tobj = TipoObjeto.FlowsheetUO
            End Select

            AddObjectToSurface(tobj, mpx, mpy)

            If tobj = TipoObjeto.MaterialStream And Not My.Computer.Keyboard.ShiftKeyDown Then
                Call Me.EditCompTSMI_Click(Me.FlowsheetDesignSurface, New EventArgs())
            End If

        End If

    End Sub

    Public calcstart As Date

    Private Sub Timer2_Tick(ByVal sender As Object, ByVal e As System.EventArgs) Handles Timer2.Tick

        Dim ts As TimeSpan = Date.Now - calcstart

        Me.LabelTime.Text = ts.ToString("g")

    End Sub

    Public Sub RecalcularToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RecalcularToolStripMenuItem.Click

        Me.Flowsheet.tsmiRecalc_Click(sender, e)

    End Sub

    Private Sub EditCompTSMI_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles EditCompTSMI.Click

        If Not Me.FlowsheetDesignSurface.SelectedObject Is Nothing Then
            If Me.FlowsheetDesignSurface.SelectedObject.TipoObjeto = TipoObjeto.MaterialStream Then

                Dim mystr As DWSIM.SimulationObjects.Streams.MaterialStream = Flowsheet.Collections.CLCS_MaterialStreamCollection(Flowsheet.FormSurface.FlowsheetDesignSurface.SelectedObject.Name)

                Dim editable As Boolean = False

                If Not mystr.GraphicObject.InputConnectors(0).IsAttached Then
                    editable = True
                ElseIf mystr.GraphicObject.InputConnectors(0).IsAttached Then
                    If mystr.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.TipoObjeto = TipoObjeto.OT_Reciclo Then
                        editable = True
                    End If
                End If

                If editable Then

                    Dim selectionControl As New CompositionEditorForm
                    selectionControl.Text = mystr.GraphicObject.Tag & DWSIM.App.GetLocalString("EditComp")
                    selectionControl.Componentes = mystr.Phases(0).Componentes
                    selectionControl.NF = My.Application.ActiveSimulation.Options.NumberFormat
                    selectionControl.SU = My.Application.ActiveSimulation.Options.SelectedUnitSystem
                    selectionControl.Q = mystr.Phases(0).Properties.molarflow.GetValueOrDefault
                    selectionControl.W = mystr.Phases(0).Properties.massflow.GetValueOrDefault
                    selectionControl.Solvent = mystr.ReferenceSolvent
                    selectionControl.InitialComposition = mystr.InputComposition
                    selectionControl.Stream = mystr
                    selectionControl.tbTag.Text = mystr.GraphicObject.Tag

                    selectionControl.ShowDialog()

                    mystr.Phases(0).Componentes = selectionControl.Componentes
                    mystr.Phases(0).Properties.molarflow = selectionControl.Q
                    mystr.Phases(0).Properties.massflow = selectionControl.W
                    mystr.ReferenceSolvent = selectionControl.Solvent
                    mystr.InputComposition = selectionControl.InitialComposition

                    selectionControl.Dispose()
                    selectionControl = Nothing

                    'Call function to calculate flowsheet
                    Dim objargs As New DWSIM.Outros.StatusChangeEventArgs
                    With objargs
                        .Calculado = False
                        .Tag = mystr.GraphicObject.Tag
                        .Nome = mystr.Nome
                        .Tipo = mystr.GraphicObject.TipoObjeto
                        .Emissor = "PropertyGrid"
                    End With

                    Me.FlowsheetDesignSurface.SelectedObject = Nothing
                    Me.FlowsheetDesignSurface.SelectedObjects.Clear()

                    Flowsheet.CalculationQueue.Enqueue(objargs)

                    Application.DoEvents()

                    CalculateAll2(Flowsheet, My.Settings.SolverMode, , True)

                    Me.FlowsheetDesignSurface.SelectedObject = mystr.GraphicObject

                    If Not Me.FlowsheetDesignSurface.SelectedObjects.ContainsKey(mystr.Name) Then
                        Me.FlowsheetDesignSurface.SelectedObjects.Add(mystr.Name, mystr.GraphicObject)
                    End If

                    Me.Invalidate()

                Else

                    MessageBox.Show("The composition of this material stream is read-only.", "Information", MessageBoxButtons.OK, MessageBoxIcon.Information)

                End If

            End If
        End If


    End Sub

    Private Sub ToolStripMenuItem8_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripMenuItem8.Click
        DrawToBitmapScaled(2)
    End Sub

    Private Sub ToolStripMenuItem4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripMenuItem4.Click
        DrawToBitmapScaled(1)
    End Sub

    Private Sub ToolStripMenuItem1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripMenuItem1.Click
        DrawToBitmapScaled(0.5)
    End Sub

    Private Sub ToolStripMenuItem10_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripMenuItem10.Click
        DrawToBitmapScaled(3)
    End Sub

    Sub DrawToBitmapScaled(ByVal scale As Double)

        If DWSIM.App.IsRunningOnMono Then
            MessageBox.Show(DWSIM.App.GetLocalString("Unsupported_Feature"), "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
            Exit Sub
        End If

        Dim rect As Rectangle = New Rectangle(0, 0, scale * (Me.FlowsheetDesignSurface.Width - 14), scale * (Me.FlowsheetDesignSurface.Height - 14))
        Dim img As Image = New Bitmap(rect.Width, rect.Height)
        Dim g As Graphics = Graphics.FromImage(img)

        Try
            g.SmoothingMode = SmoothingMode.Default
            'get the dpi settings of the graphics context,
            'for example; 96dpi on screen, 600dpi for the printer
            'used to adjust grid and margin sizing.
            Me.FlowsheetDesignSurface.m_HorizRes = g.DpiX
            Me.FlowsheetDesignSurface.m_VertRes = g.DpiY

            Me.FlowsheetDesignSurface.DrawGrid(g)

            'handle the possibility that the viewport is scrolled,
            'adjust my origin coordintates to compensate
            Dim pt As Point = Me.FlowsheetDesignSurface.AutoScrollPosition
            g.TranslateTransform(pt.X * scale, pt.Y * scale)

            'draw the actual objects onto the page, on top of the grid

            For Each gr As GraphicObject In Me.FlowsheetDesignSurface.SelectedObjects.Values
                Me.FlowsheetDesignSurface.drawingObjects.DrawSelectedObject(g, gr, scale * Me.FlowsheetDesignSurface.Zoom)
            Next

            With Me.FlowsheetDesignSurface.drawingObjects
                'pass the graphics resolution onto the objects
                'so that images and other objects can be sized
                'correct taking the dpi into consideration.
                .HorizontalResolution = g.DpiX
                .VerticalResolution = g.DpiY
                'doesn't really draw the selected object, but instead the
                'selection indicator, a dotted outline around the selected object
                .DrawObjects(g, scale * Me.FlowsheetDesignSurface.Zoom, False)
                If Not Me.FlowsheetDesignSurface.SelectedObject Is Nothing Then
                    If Not Me.FlowsheetDesignSurface.SelectedObjects.ContainsKey(Me.FlowsheetDesignSurface.SelectedObject.Name) Then
                        .DrawSelectedObject(g, Me.FlowsheetDesignSurface.SelectedObject, scale * Me.FlowsheetDesignSurface.Zoom)
                    End If
                End If
            End With

            Clipboard.SetImage(img)

            Me.Flowsheet.WriteToLog("Image created and sent to clipboard sucessfully.", Color.Blue, DWSIM.FormClasses.TipoAviso.Informacao)

        Catch ex As Exception

            Me.Flowsheet.WriteToLog("Error capturing flowsheet snapshot: " & ex.ToString, Color.Red, DWSIM.FormClasses.TipoAviso.Erro)

        Finally

            img.Dispose()
            g.Dispose()

        End Try


    End Sub

    Public Sub New()

        ' This call is required by the designer.
        InitializeComponent()

    End Sub

    Private Sub FlowsheetDesignSurface_MouseDoubleClick(sender As Object, e As System.Windows.Forms.MouseEventArgs) Handles FlowsheetDesignSurface.MouseDoubleClick

        If Not Me.FlowsheetDesignSurface.SelectedObject Is Nothing Then
            Select Case Me.FlowsheetDesignSurface.SelectedObject.TipoObjeto
                Case TipoObjeto.MaterialStream
                    EditCompTSMI_Click(sender, e)
                Case TipoObjeto.FlowsheetUO
                    Dim myobj As DWSIM.SimulationObjects.UnitOps.Flowsheet = Flowsheet.Collections.CLCS_FlowsheetUOCollection(Flowsheet.FormSurface.FlowsheetDesignSurface.SelectedObject.Name)
                    If My.Computer.Keyboard.ShiftKeyDown Then
                        Dim viewform As New FlowsheetUOEditorForm
                        With viewform
                            .Text = Flowsheet.FormSurface.FlowsheetDesignSurface.SelectedObject.Tag
                            .fsuo = myobj
                            .ShowDialog()
                            .Dispose()
                        End With
                        viewform = Nothing
                    Else
                        If myobj.Initialized Then
                            Dim viewform As New FlowsheetUOViewerForm
                            With viewform
                                .Text = Flowsheet.FormSurface.FlowsheetDesignSurface.SelectedObject.Tag
                                .fsuo = myobj
                                .Show(Flowsheet.dckPanel)
                            End With
                        Else
                            Dim viewform As New FlowsheetUOEditorForm
                            With viewform
                                .Text = Flowsheet.FormSurface.FlowsheetDesignSurface.SelectedObject.Tag
                                .fsuo = myobj
                                .ShowDialog()
                                .Dispose()
                            End With
                            viewform = Nothing
                        End If
                    End If
                Case TipoObjeto.CapeOpenUO
                    Dim myobj As DWSIM.SimulationObjects.UnitOps.CapeOpenUO = Flowsheet.Collections.CLCS_CapeOpenUOCollection(Flowsheet.FormSurface.FlowsheetDesignSurface.SelectedObject.Name)
                    myobj.Edit(Me, New EventArgs)
                Case TipoObjeto.ExcelUO
                    Dim myobj As DWSIM.SimulationObjects.UnitOps.ExcelUO = Flowsheet.Collections.CLCS_ExcelUOCollection(Flowsheet.FormSurface.FlowsheetDesignSurface.SelectedObject.Name)
                    If My.Computer.Keyboard.ShiftKeyDown Then
                        Dim selectionControl As New ExcelUOEditorForm
                        selectionControl.Text = Flowsheet.FormSurface.FlowsheetDesignSurface.SelectedObject.Tag & " - " & "Excel UO Specification"
                        selectionControl.TbFileName.Text = myobj.Filename
                        selectionControl.ShowDialog()
                        myobj.Filename = selectionControl.TbFileName.Text
                    Else
                        If myobj.Filename <> "" Then
                            If My.Computer.FileSystem.FileExists(myobj.Filename) Then
                                If Not DWSIM.App.IsRunningOnMono Then
                                    Try
                                        Process.Start(myobj.Filename)
                                    Catch ex As Exception
                                    End Try
                                Else
                                    Try
                                        Process.Start(New ProcessStartInfo("xdg-open", myobj.Filename) With {.UseShellExecute = False})
                                    Catch ex As Exception

                                    End Try
                                End If
                            Else
                                MessageBox.Show(DWSIM.App.GetLocalString("Oarquivonoexisteoufo"), DWSIM.App.GetLocalString("Erroaoabrirarquivo"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            End If
                        End If
                    End If
                Case TipoObjeto.CustomUO
                    Dim myobj As DWSIM.SimulationObjects.UnitOps.CustomUO = Flowsheet.Collections.CLCS_CustomUOCollection(Flowsheet.FormSurface.FlowsheetDesignSurface.SelectedObject.Name)
                    If Not DWSIM.App.IsRunningOnMono Then
                        Dim selectionControl As New ScriptEditorForm
                        selectionControl.scripttext = myobj.ScriptText
                        selectionControl.language = myobj.Language
                        selectionControl.fontname = myobj.FontName
                        selectionControl.fontsize = myobj.FontSize
                        selectionControl.includes = myobj.Includes
                        selectionControl.highlightspaces = myobj.HighlightSpaces
                        selectionControl.Text = Flowsheet.FormSurface.FlowsheetDesignSurface.SelectedObject.Tag & " - " & DWSIM.App.GetLocalString("ScriptEditor")
                        selectionControl.ShowDialog(Me)
                        myobj.FontName = selectionControl.tscb1.SelectedItem
                        myobj.FontSize = selectionControl.tscb2.SelectedItem
                        myobj.Includes = selectionControl.includes
                        myobj.ScriptText = selectionControl.scripttext
                        myobj.HighlightSpaces = selectionControl.highlightspaces
                        selectionControl.Dispose()
                        selectionControl = Nothing
                    Else
                        Dim selectionControl As New ScriptEditorFormMono
                        selectionControl.scripttext = myobj.ScriptText
                        selectionControl.language = myobj.Language
                        selectionControl.fontname = myobj.FontName
                        selectionControl.fontsize = myobj.FontSize
                        selectionControl.includes = myobj.Includes
                        selectionControl.Text = Flowsheet.FormSurface.FlowsheetDesignSurface.SelectedObject.Tag & " - " & DWSIM.App.GetLocalString("ScriptEditor")
                        selectionControl.ShowDialog()
                        myobj.FontName = selectionControl.tscb1.SelectedItem
                        myobj.FontSize = selectionControl.tscb2.SelectedItem
                        myobj.Includes = selectionControl.includes
                        myobj.ScriptText = selectionControl.txtScript.Text
                        selectionControl.Dispose()
                        selectionControl = Nothing
                    End If
                Case TipoObjeto.OT_Ajuste
                    Dim selectionControl As New UI_AdjControlPanelForm
                    selectionControl.ShowDialog()
                    selectionControl.Dispose()
                    selectionControl = Nothing
                Case TipoObjeto.OT_Especificacao
                    Dim selectionControl As New UI_SpecControlPanelForm
                    selectionControl.ShowDialog()
                    selectionControl.Dispose()
                    selectionControl = Nothing
                Case TipoObjeto.AbsorptionColumn, TipoObjeto.DistillationColumn, TipoObjeto.ReboiledAbsorber, TipoObjeto.RefluxedAbsorber
                    Dim myobj As DWSIM.SimulationObjects.UnitOps.Column = Flowsheet.Collections.ObjectCollection(Flowsheet.FormSurface.FlowsheetDesignSurface.SelectedObject.Name)
                    If My.Computer.Keyboard.ShiftKeyDown Then
                        Dim selectionControl As New UIConnectionsEditorForm
                        selectionControl.ShowDialog()
                        selectionControl.Dispose()
                        selectionControl = Nothing
                    ElseIf My.Computer.Keyboard.CtrlKeyDown Then
                        Dim selectionControl As New UIInitialEstimatesEditorForm
                        selectionControl.ShowDialog()
                        selectionControl.Dispose()
                        selectionControl = Nothing
                    ElseIf My.Computer.Keyboard.AltKeyDown Then
                        Dim selectionControl As New UIStagesEditorForm
                        selectionControl.ShowDialog()
                        selectionControl.Dispose()
                        selectionControl = Nothing
                    Else
                        If myobj.Calculated Then
                            Dim selectionControl As New UIResultsForm
                            selectionControl.form = myobj.FlowSheet
                            selectionControl.ShowDialog()
                            selectionControl.Dispose()
                            selectionControl = Nothing
                        End If
                    End If
                Case TipoObjeto.Pipe
                    Dim myobj As DWSIM.SimulationObjects.UnitOps.Pipe = Flowsheet.Collections.ObjectCollection(Flowsheet.FormSurface.FlowsheetDesignSurface.SelectedObject.Name)
                    If My.Computer.Keyboard.ShiftKeyDown Then
                        Dim selectionControl As New ThermalProfileEditorForm
                        selectionControl.ThermalProfile = myobj.ThermalProfile
                        selectionControl.ShowDialog()
                        myobj.ThermalProfile = selectionControl.ThermalProfile
                        selectionControl.Dispose()
                        selectionControl = Nothing
                    ElseIf My.Computer.Keyboard.CtrlKeyDown Then
                        If myobj.Calculated Then
                            Dim selectionControl As New FormGraph
                            selectionControl.Profile = myobj.Profile
                            selectionControl.ShowDialog()
                            selectionControl.Dispose()
                            selectionControl = Nothing
                        End If
                    ElseIf My.Computer.Keyboard.AltKeyDown Then
                        If myobj.Calculated Then
                            Dim selectionControl As New FormTable
                            selectionControl.Profile = myobj.Profile
                            selectionControl.ShowDialog()
                            selectionControl.Dispose()
                            selectionControl = Nothing
                        End If
                    Else
                        Dim selectionControl As New PipeEditorForm
                        selectionControl.PipeEditor1.SystemOfUnits = My.Application.ActiveSimulation.Options.SelectedUnitSystem
                        selectionControl.PipeEditor1.NumberFormat = My.Application.ActiveSimulation.Options.NumberFormat
                        selectionControl.PipeEditor1.Profile = myobj.Profile
                        selectionControl.ShowDialog()
                        myobj.Profile = selectionControl.PipeEditor1.Profile
                        selectionControl.Dispose()
                        selectionControl = Nothing
                    End If
                Case TipoObjeto.RCT_PFR
                    Dim myobj As DWSIM.SimulationObjects.Reactors.Reactor_PFR = Flowsheet.Collections.ObjectCollection(Flowsheet.FormSurface.FlowsheetDesignSurface.SelectedObject.Name)
                    If myobj.Calculated Then
                        Dim selectionControl As New FormGraphPFR
                        selectionControl.form = myobj.FlowSheet
                        selectionControl.Points = myobj.points
                        selectionControl.ShowDialog()
                        selectionControl.Dispose()
                        selectionControl = Nothing
                    End If
                Case TipoObjeto.RCT_Gibbs
                    Dim myobj As DWSIM.SimulationObjects.Reactors.Reactor_Gibbs = Flowsheet.Collections.ObjectCollection(Flowsheet.FormSurface.FlowsheetDesignSurface.SelectedObject.Name)
                    If My.Computer.Keyboard.ShiftKeyDown Then
                        Dim selectionControl As New ElementMatrixEditorForm
                        selectionControl.elmat = myobj.ElementMatrix
                        selectionControl.Text = myobj.GraphicObject.Tag & " - " & DWSIM.App.GetLocalString("RGEditElementMatrix")
                        selectionControl.ShowDialog()
                        myobj.ElementMatrix = selectionControl.elmat
                        selectionControl.Dispose()
                        selectionControl = Nothing
                    Else
                        Dim selectionControl As New GibbsInitialEstimatesEditorForm
                        selectionControl.ie = myobj.InitialEstimates
                        selectionControl.gr = myobj
                        selectionControl.Text = myobj.GraphicObject.Tag & " - " & selectionControl.Text
                        selectionControl.form = myobj.FlowSheet
                        If selectionControl.gr.GraphicObject.InputConnectors(0).IsAttached Then
                            selectionControl.inlet = myobj.FlowSheet.Collections.CLCS_MaterialStreamCollection(selectionControl.gr.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name)
                        End If
                        If selectionControl.gr.GraphicObject.InputConnectors(0).IsAttached Then
                            selectionControl.inlet = myobj.FlowSheet.Collections.CLCS_MaterialStreamCollection(selectionControl.gr.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name)
                        End If
                        If selectionControl.gr.GraphicObject.OutputConnectors(0).IsAttached Then
                            selectionControl.outletv = myobj.FlowSheet.Collections.CLCS_MaterialStreamCollection(selectionControl.gr.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Name)
                        End If
                        If selectionControl.gr.GraphicObject.OutputConnectors(1).IsAttached Then
                            selectionControl.outletl = myobj.FlowSheet.Collections.CLCS_MaterialStreamCollection(selectionControl.gr.GraphicObject.OutputConnectors(1).AttachedConnector.AttachedTo.Name)
                        End If
                        selectionControl.ShowDialog()
                        myobj.InitialEstimates = selectionControl.ie
                        selectionControl.Dispose()
                        selectionControl = Nothing
                    End If
            End Select
        End If

    End Sub

    Private Sub CopiarDadosParaÁreaDeTransferênciaToolStripMenuItem_Click(sender As System.Object, e As System.EventArgs) Handles CopiarDadosParaÁreaDeTransferênciaToolStripMenuItem.Click

        Me.Flowsheet.tsmiExportData_Click(sender, e)

    End Sub

    Private Sub RestoreTSMI_Click(sender As Object, e As EventArgs) Handles RestoreTSMI.Click

        If Not Me.FlowsheetDesignSurface.SelectedObject Is Nothing Then

            If Me.FlowsheetDesignSurface.SelectedObject.TipoObjeto = TipoObjeto.MaterialStream Then

                Dim mystr As DWSIM.SimulationObjects.Streams.MaterialStream = Flowsheet.Collections.CLCS_MaterialStreamCollection(Flowsheet.FormSurface.FlowsheetDesignSurface.SelectedObject.Name)

                If Not mystr.GraphicObject.InputConnectors(0).IsAttached Then

                    'assign default values for temperature, pressure and mass flow
                    mystr.Phases(0).Properties.temperature = 298.15
                    mystr.Phases(0).Properties.pressure = 101325
                    mystr.Phases(0).Properties.massflow = 1

                    mystr.EqualizeOverallComposition()

                    Application.DoEvents()
                    CalculateMaterialStream(Flowsheet, mystr)
                    Application.DoEvents()
                    Call Flowsheet.FormSurface.UpdateSelectedObject()
                    Application.DoEvents()
                    Call Flowsheet.FormSurface.FlowsheetDesignSurface.Invalidate()
                    Application.DoEvents()
                    ProcessCalculationQueue(Flowsheet)
                    Application.DoEvents()

                ElseIf mystr.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.TipoObjeto = TipoObjeto.OT_Reciclo Then

                    'assign default values for temperature, pressure and mass flow
                    mystr.Phases(0).Properties.temperature = 298.15
                    mystr.Phases(0).Properties.pressure = 101325
                    mystr.Phases(0).Properties.massflow = 1

                    mystr.EqualizeOverallComposition()

                    Application.DoEvents()
                    CalculateMaterialStream(Flowsheet, mystr)
                    Application.DoEvents()
                    Call Flowsheet.FormSurface.UpdateSelectedObject()
                    Application.DoEvents()
                    Call Flowsheet.FormSurface.FlowsheetDesignSurface.Invalidate()
                    Application.DoEvents()
                    ProcessCalculationQueue(Flowsheet)
                    Application.DoEvents()

                Else

                    MessageBox.Show("The selected Material Stream is read-only.", "Information", MessageBoxButtons.OK, MessageBoxIcon.Information)

                End If

            End If
        End If
    End Sub

    Private Sub ExibirTudoToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles ExibirTudoToolStripMenuItem.Click
        Me.FlowsheetDesignSurface.ZoomAll()
        Me.FlowsheetDesignSurface.ZoomAll()
        Me.Invalidate()
    End Sub

    Private Sub ZoomPadrão100ToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles ZoomPadrão100ToolStripMenuItem.Click
        Me.FlowsheetDesignSurface.Zoom = 1
        Me.Invalidate()
    End Sub

    Private Sub CentralizarToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles CentralizarToolStripMenuItem.Click
        Me.FlowsheetDesignSurface.Center()
        Me.Invalidate()
    End Sub

    Private Sub FloatToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles FloatToolStripMenuItem.Click, DocumentToolStripMenuItem.Click,
                                                                            DockLeftToolStripMenuItem.Click, DockLeftAutoHideToolStripMenuItem.Click,
                                                                            DockRightAutoHideToolStripMenuItem.Click, DockRightToolStripMenuItem.Click,
                                                                            DockTopAutoHideToolStripMenuItem.Click, DockTopToolStripMenuItem.Click,
                                                                            DockBottomAutoHideToolStripMenuItem.Click, DockBottomToolStripMenuItem.Click

        For Each ts As ToolStripMenuItem In dckMenu.Items
            ts.Checked = False
        Next

        sender.Checked = True

        Select Case sender.Name
            Case "FloatToolStripMenuItem"
                Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.Float
            Case "DocumentToolStripMenuItem"
                Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.Document
            Case "DockLeftToolStripMenuItem"
                Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.DockLeft
            Case "DockLeftAutoHideToolStripMenuItem"
                Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.DockLeftAutoHide
            Case "DockRightAutoHideToolStripMenuItem"
                Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.DockRightAutoHide
            Case "DockRightToolStripMenuItem"
                Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.DockRight
            Case "DockBottomAutoHideToolStripMenuItem"
                Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.DockBottomAutoHide
            Case "DockBottomToolStripMenuItem"
                Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.DockBottom
            Case "DockTopAutoHideToolStripMenuItem"
                Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.DockTopAutoHide
            Case "DockTopToolStripMenuItem"
                Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.DockTop
            Case "HiddenToolStripMenuItem"
                Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.Hidden
        End Select

    End Sub

    Private Sub ToolStripMenuItem11_Click(sender As Object, e As EventArgs) Handles ToolStripMenuItem11.Click
        If DWSIM.App.IsRunningOnMono Then
            MessageBox.Show(DWSIM.App.GetLocalString("Unsupported_Feature"), "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
        Else
            Me.FlowsheetDesignSurface.SelectedObject.Rotation = 0
            Me.Invalidate()
        End If
    End Sub

    Private Sub ToolStripMenuItem12_Click(sender As Object, e As EventArgs) Handles ToolStripMenuItem12.Click
        If DWSIM.App.IsRunningOnMono Then
            MessageBox.Show(DWSIM.App.GetLocalString("Unsupported_Feature"), "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
        Else
            Me.FlowsheetDesignSurface.SelectedObject.Rotation = 90
            Me.Invalidate()
        End If
    End Sub

    Private Sub ToolStripMenuItem13_Click(sender As Object, e As EventArgs) Handles ToolStripMenuItem13.Click
        If DWSIM.App.IsRunningOnMono Then
            MessageBox.Show(DWSIM.App.GetLocalString("Unsupported_Feature"), "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
        Else
            Me.FlowsheetDesignSurface.SelectedObject.Rotation = 180
            Me.Invalidate()
        End If
    End Sub

    Private Sub ToolStripMenuItem14_Click(sender As Object, e As EventArgs) Handles ToolStripMenuItem14.Click
        If DWSIM.App.IsRunningOnMono Then
            MessageBox.Show(DWSIM.App.GetLocalString("Unsupported_Feature"), "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
        Else
            Me.FlowsheetDesignSurface.SelectedObject.Rotation = 270
            Me.Invalidate()
        End If
    End Sub

    Private Sub DepurarObjetoToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles DepurarObjetoToolStripMenuItem.Click
        If Not Me.FlowsheetDesignSurface.SelectedObject Is Nothing Then
            If Flowsheet.Collections.ObjectCollection.ContainsKey(Flowsheet.FormSurface.FlowsheetDesignSurface.SelectedObject.Name) Then
                Dim myobj As DWSIM.SimulationObjects.UnitOperations.BaseClass = Flowsheet.Collections.ObjectCollection(Flowsheet.FormSurface.FlowsheetDesignSurface.SelectedObject.Name)
                Dim frm As New FormTextBox
                With frm
                    .TextBox1.Text = "Please wait, debugging object..."
                    .TextBox1.SelectionStart = 0
                    .TextBox1.SelectionLength = 0
                    .TextBox1.ScrollBars = ScrollBars.Vertical
                    .Text = DWSIM.App.GetLocalString("Debugging") & " " & myobj.GraphicObject.Tag & "..."
                    .StartPosition = FormStartPosition.CenterScreen
                End With
                frm.Show(Me.Flowsheet)

                Task.Factory.StartNew(Function()
                                          Return myobj.GetDebugReport()
                                      End Function).ContinueWith(Sub(t)
                                                                     frm.TextBox1.Text = t.Result
                                                                     frm.TextBox1.SelectionStart = 0
                                                                     frm.TextBox1.SelectionLength = 0
                                                                 End Sub, TaskContinuationOptions.ExecuteSynchronously)

            End If
        End If
    End Sub

End Class
