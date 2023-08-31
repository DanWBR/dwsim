'    Copyright 2014 Gregor Reichert
'
'    This file is part of DWSIM.
'
'    DWSIM is free software: you can redistribute it and/or modify
'    it under the terms of the GNU General Public License as published by
'    the Free Software Foundation, either version 3 of the License, or
'    (at your option) any later version.
'
'    DWSIM is distributed in the hope that it will be useful,
'    but WITHOUT ANY WARRANTY; without even the implied warranty of
'    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
'    GNU General Public License for more details.
'
'    You should have received a copy of the GNU General Public License
'    along with DWSIM.  If not, see <http://www.gnu.org/licenses/>.

Imports DWSIM.Thermodynamics.BaseClasses
Imports DWSIM.Thermodynamics.PropertyPackages
Imports DWSIM.SharedClasses
Imports DWSIM.Thermodynamics.Streams

Public Class FormLLEDiagram

    Inherits UserControl

    Implements Interfaces.IAttachedUtility

    Dim mat As Streams.MaterialStream
    Dim Frm As FormFlowsheet

    Public su As New SystemsOfUnits.Units
    Public cv As New SystemsOfUnits.Converter
    Public nf As String
    Public Names() As String

    Public DiagMargins As New Rec

    Public LLECurve As New ArrayList
    Public LLEPoints As New ArrayList

    Dim P, T As Double

    Private Property loaded As Boolean = False

    Private Function Transform(ByVal G As Graphics, ByVal X As Double, ByVal Y As Double) As Point

        Dim R As New Point
        Dim Size, Shift As Double
        Size = Math.Min(G.VisibleClipBounds.Width - DiagMargins.Left - DiagMargins.Right, G.VisibleClipBounds.Height - DiagMargins.Top - DiagMargins.Bottom)
        Shift = (G.VisibleClipBounds.Width - Size - DiagMargins.Left - DiagMargins.Right) / 2 'shift diagram to horizontal center

        R.X = DiagMargins.Left + Shift + (X + Y / 2) * Size
        R.Y = G.VisibleClipBounds.Height - DiagMargins.Bottom - Y * Size

        Return R

    End Function

    Private Sub DrawDiagram(ByVal G As Graphics)

        Dim pt1, pt2 As System.Drawing.Point
        Dim P As PointF
        Dim MyPen As Pen
        Dim MyBrush As Brush
        Dim S1, S2 As String
        Dim TitleFont As Font = SystemFonts.MessageBoxFont
        Dim SubTitleFont As Font = SystemFonts.MessageBoxFont
        Dim stringSize As SizeF = New SizeF()
        Dim k As Integer
        Dim Ko1, KO2 As Konode

        G.Clear(Color.WhiteSmoke)

        DiagMargins.Left = 50
        DiagMargins.Right = 50
        DiagMargins.Top = 5
        DiagMargins.Bottom = 50

        'Draw Title
        S1 = DWSIM.App.GetLocalString("LLEDiagrama")
        S2 = cbComp1.Text & " / " & cbComp2.Text & " / " & cbComp3.Text

        stringSize = G.MeasureString(S1, TitleFont)
        G.DrawString(S1, TitleFont, Brushes.Black, Math.Round((G.VisibleClipBounds.Width - stringSize.Width) / 2), DiagMargins.Top)
        DiagMargins.Top += stringSize.Height

        stringSize = G.MeasureString(S2, SubTitleFont)
        G.DrawString(S2, SubTitleFont, Brushes.Black, Math.Round((G.VisibleClipBounds.Width - stringSize.Width) / 2), DiagMargins.Top)
        DiagMargins.Top += stringSize.Height

        stringSize = G.MeasureString(cbComp1.Text, SubTitleFont)
        DiagMargins.Top += stringSize.Height * 2
        pt1 = Transform(G, 1, 0)
        G.DrawString(cbComp1.Text, SubTitleFont, Brushes.Black, pt1.X - Math.Round(stringSize.Width / 2), pt1.Y)

        stringSize = G.MeasureString(cbComp2.Text, SubTitleFont)
        pt1 = Transform(G, 0, 1)
        G.DrawString(cbComp2.Text, SubTitleFont, Brushes.Black, pt1.X - Math.Round(stringSize.Width / 2), pt1.Y - stringSize.Height)

        stringSize = G.MeasureString(cbComp3.Text, SubTitleFont)
        pt1 = Transform(G, 0, 0)
        G.DrawString(cbComp3.Text, SubTitleFont, Brushes.Black, pt1.X - Math.Round(stringSize.Width / 2), pt1.Y)

        '=========================
        ' draw diagram background
        '=========================

        MyBrush = New SolidBrush(Color.Snow)

        pt1 = Transform(G, 0, 0)
        Dim point1 As New System.Drawing.Point(pt1.X, pt1.Y)
        pt1 = Transform(G, 1, 0)
        Dim point2 As New System.Drawing.Point(pt1.X, pt1.Y)
        pt1 = Transform(G, 0, 1)
        Dim point3 As New System.Drawing.Point(pt1.X, pt1.Y)
        Dim curvePoints As System.Drawing.Point() = {point1, point2, point3}
        G.FillPolygon(MyBrush, curvePoints)

        '===================================
        'Draw background of miscibility gap
        '===================================
        MyBrush.Dispose()
        MyBrush = New SolidBrush(Color.SkyBlue)
        If LLECurve.Count > 1 Then
            Dim MiscGap(2 * LLECurve.Count - 1) As System.Drawing.Point

            For k = 0 To LLECurve.Count - 1
                Ko1 = LLECurve(k)
                pt1 = Transform(G, Ko1.X11, Ko1.X12)
                MiscGap(k).X = pt1.X
                MiscGap(k).Y = pt1.Y
            Next
            For k = 0 To LLECurve.Count - 1
                Ko1 = LLECurve(LLECurve.Count - 1 - k)
                pt1 = Transform(G, Ko1.X21, Ko1.X22)
                MiscGap(LLECurve.Count + k).X = pt1.X
                MiscGap(LLECurve.Count + k).Y = pt1.Y
            Next

            G.FillPolygon(MyBrush, MiscGap)

        End If


        '=================
        'Draw border+grid
        '=================
        MyPen = Pens.Black.Clone
        MyPen.Width = 2

        pt1 = Transform(G, 0, 0)
        pt2 = Transform(G, 1, 0)
        G.DrawLine(MyPen, pt1, pt2)

        pt1 = Transform(G, 0, 0)
        pt2 = Transform(G, 0, 1)
        G.DrawLine(MyPen, pt1, pt2)

        pt1 = Transform(G, 1, 0)
        pt2 = Transform(G, 0, 1)
        G.DrawLine(MyPen, pt1, pt2)

        MyPen.Color = Color.Blue
        MyPen.Width = 1
        For k = 1 To 9
            pt1 = Transform(G, 0, k / 10)
            pt2 = Transform(G, 1 - k / 10, k / 10)
            G.DrawLine(MyPen, pt1, pt2)

            pt1 = Transform(G, k / 10, 0)
            pt2 = Transform(G, k / 10, 1 - k / 10)
            G.DrawLine(MyPen, pt1, pt2)

            pt1 = Transform(G, 0, k / 10)
            pt2 = Transform(G, k / 10, 0)
            G.DrawLine(MyPen, pt1, pt2)
        Next

        '=================
        'Draw LLE phase lines
        '=================
        MyBrush = New SolidBrush(Color.Yellow)
        MyPen.Color = Color.Black
        MyPen.Width = 2

        'draw phase border
        If LLECurve.Count > 1 Then
            For k = 0 To LLECurve.Count - 2
                Ko1 = LLECurve(k)
                KO2 = LLECurve(k + 1)
                pt1 = Transform(G, Ko1.X11, Ko1.X12)
                pt2 = Transform(G, KO2.X11, KO2.X12)
                G.DrawLine(MyPen, pt1, pt2)
                pt1 = Transform(G, Ko1.X21, Ko1.X22)
                pt2 = Transform(G, KO2.X21, KO2.X22)
                G.DrawLine(MyPen, pt1, pt2)
            Next
        End If

        'draw konodes
        MyPen.Width = 1
        For k = 0 To LLECurve.Count - 1
            Ko1 = LLECurve(k)
            pt1 = Transform(G, Ko1.X11, Ko1.X12)
            pt2 = Transform(G, Ko1.X21, Ko1.X22)
            G.DrawLine(MyPen, pt1, pt2)

            G.FillEllipse(MyBrush, pt1.X - 2, pt1.Y - 2, 5, 5)
            G.FillEllipse(MyBrush, pt2.X - 2, pt2.Y - 2, 5, 5)
            G.DrawEllipse(MyPen, pt1.X - 2, pt1.Y - 2, 5, 5)
            G.DrawEllipse(MyPen, pt2.X - 2, pt2.Y - 2, 5, 5)
        Next

        'draw additional points
        MyBrush.Dispose()
        MyBrush = New SolidBrush(Color.Red)
        For k = 0 To LLEPoints.Count - 1
            P = LLEPoints(k)
            pt1 = Transform(G, P.X, P.Y)
            G.FillEllipse(MyBrush, pt1.X - 2, pt1.Y - 2, 5, 5)
            G.DrawEllipse(MyPen, pt1.X - 2, pt1.Y - 2, 5, 5)
        Next

    End Sub
    Private Sub PanelDiag_Paint(sender As System.Object, e As System.Windows.Forms.PaintEventArgs) Handles PanelDiag.Paint
        DrawDiagram(e.Graphics)
    End Sub
    Private Sub PanelDiag_Resize(sender As System.Object, e As System.EventArgs) Handles PanelDiag.Resize
        PanelDiag.Refresh() 'redraw Diagram
    End Sub

    Private Function NewPt(ByVal ko As Konode, ByVal Length As Double, ByRef LastDir() As Double) As PointF
        Dim V, M, R, N As PointF
        Dim L, SP As Double

        M.X = (ko.X21 + ko.X11) / 2 'Center point of konode
        M.Y = (ko.X22 + ko.X12) / 2
        V.X = ko.X21 - ko.X11 'konode vector
        V.Y = ko.X22 - ko.X12
        L = ko.Distance

        N.X = -V.Y / L 'calc normal vector with length 1
        N.Y = V.X / L

        'calculate scalar product
        SP = N.X * LastDir(0) + N.Y * LastDir(1)

        'SP<0 means old and new vactor point to opposite side of conode. -> reverse direction of new vector
        If SP < 0 Then Length = -Length

        LastDir(0) = N.X * Length
        LastDir(1) = N.Y * Length

        R.X = M.X + Length * N.X
        R.Y = M.Y + Length * N.Y

        Return R
    End Function
    Private Function CheckValidComp(ByVal x1 As Double, ByVal x2 As Double) As Boolean
        Dim x3 As Double
        x3 = 1 - x1 - x2
        Return x1 >= 0 And x2 >= 0 And x3 >= 0 And x1 <= 1 And x2 <= 1 And x3 <= 1
    End Function

    Private Function CalcKonode(ByVal Pt As PointF) As Konode
        Dim Ko As New Konode

        'calculate compositions
        mat.Phases(0).Compounds(Names(cbComp1.SelectedIndex)).MoleFraction = Pt.X
        mat.Phases(0).Compounds(Names(cbComp2.SelectedIndex)).MoleFraction = Pt.Y
        mat.Phases(0).Compounds(Names(cbComp3.SelectedIndex)).MoleFraction = 1 - Pt.X - Pt.Y

        If My.Settings.EnableGPUProcessing Then
            Calculator.InitComputeDevice()
            Settings.gpu.EnableMultithreading()
        End If

        Try
            mat.CalcEquilibrium("tp", Nothing)
        Catch ex As Exception
            Dim euid As String = Guid.NewGuid().ToString()
            SharedClasses.ExceptionProcessing.ExceptionList.Exceptions.Add(euid, ex)
            mat.FlowSheet.ShowMessage(ex.Message.ToString, Interfaces.IFlowsheet.MessageType.GeneralError, euid)
        Finally
            If My.Settings.EnableGPUProcessing Then
                Settings.gpu.DisableMultithreading()
                Settings.gpu.FreeAll()
            End If
        End Try


        'added .GetValueOrDefault to avoid null object errors

        Ko.X11 = mat.Phases(3).Compounds(Names(cbComp1.SelectedIndex)).MoleFraction.GetValueOrDefault
        Ko.X12 = mat.Phases(3).Compounds(Names(cbComp2.SelectedIndex)).MoleFraction.GetValueOrDefault

        If mat.Phases(4).Properties.molarfraction > 0 Then
            Ko.X21 = mat.Phases(4).Compounds(Names(cbComp1.SelectedIndex)).MoleFraction.GetValueOrDefault
            Ko.X22 = mat.Phases(4).Compounds(Names(cbComp2.SelectedIndex)).MoleFraction.GetValueOrDefault
        Else
            Ko.X21 = mat.Phases(3).Compounds(Names(cbComp1.SelectedIndex)).MoleFraction.GetValueOrDefault
            Ko.X22 = mat.Phases(3).Compounds(Names(cbComp2.SelectedIndex)).MoleFraction.GetValueOrDefault
        End If

        Return Ko

    End Function

    Private Sub btnCalcDiagram_Click(sender As System.Object, e As System.EventArgs) Handles TSB_CalcDiagr.Click, btnCalcDiagram.Click

        Dim Ko, LastKo As New Konode
        Dim Pt As PointF
        Dim InitialPoints As New ArrayList
        Dim pp As PropertyPackages.PropertyPackage = Nothing
        Dim final, first, searchmode As Boolean
        Dim w, stepsize, D1, D2 As Double
        Dim dir(1) As Double
        Dim C(2) As Double
        Dim k As Integer

        '=============================
        ' check components are different
        '=============================
        If cbComp1.SelectedItem = cbComp2.SelectedItem Or
           cbComp1.SelectedItem = cbComp3.SelectedItem Or
           cbComp2.SelectedItem = cbComp3.SelectedItem Then

            MessageBox.Show(DWSIM.App.GetLocalString("LLEEnvError_DuplicateCompound"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
            Exit Sub
        End If

        '===============================
        ' assign components to datagrid
        '===============================
        DataGridView1.Columns(0).HeaderText = "[1] " & cbComp1.Text
        DataGridView1.Columns(1).HeaderText = "[1] " & cbComp2.Text
        DataGridView1.Columns(2).HeaderText = "[2] " & cbComp1.Text
        DataGridView1.Columns(3).HeaderText = "[2] " & cbComp2.Text
        DataGridView1.RowCount = 0

        '=============================
        ' assign components to phases
        '=============================

        For Each phase As BaseClasses.Phase In mat.Phases.Values
            phase.Compounds.Clear() 'delete old assignment
        Next

        Dim N As String
        N = Names(cbComp1.SelectedIndex)
        For Each phase As BaseClasses.Phase In mat.Phases.Values
            phase.Compounds.Add(N, New BaseClasses.Compound(N, ""))
            phase.Compounds(N).ConstantProperties = Frm.Options.SelectedComponents(N)
        Next
        N = Names(cbComp2.SelectedIndex)
        For Each phase As BaseClasses.Phase In mat.Phases.Values
            phase.Compounds.Add(N, New BaseClasses.Compound(N, ""))
            phase.Compounds(N).ConstantProperties = Frm.Options.SelectedComponents(N)
        Next
        N = Names(cbComp3.SelectedIndex)
        For Each phase As BaseClasses.Phase In mat.Phases.Values
            phase.Compounds.Add(N, New BaseClasses.Compound(N, ""))
            phase.Compounds(N).ConstantProperties = Frm.Options.SelectedComponents(N)
        Next

        '==========================================================
        ' define searching points for two phase region
        '==========================================================
        InitialPoints.Add(New PointF With {.X = 0.5, .Y = 0})
        InitialPoints.Add(New PointF With {.X = 0, .Y = 0.5})
        InitialPoints.Add(New PointF With {.X = 0.5, .Y = 0.5})
        InitialPoints.Add(New PointF With {.X = 0.25, .Y = 0})
        InitialPoints.Add(New PointF With {.X = 0.75, .Y = 0})
        InitialPoints.Add(New PointF With {.X = 0, .Y = 0.25})
        InitialPoints.Add(New PointF With {.X = 0, .Y = 0.75})
        InitialPoints.Add(New PointF With {.X = 0.25, .Y = 0.75})
        InitialPoints.Add(New PointF With {.X = 0.75, .Y = 0.25})

        InitialPoints.Add(New PointF With {.X = 0.125, .Y = 0.0})
        InitialPoints.Add(New PointF With {.X = 0.375, .Y = 0.0})
        InitialPoints.Add(New PointF With {.X = 0.625, .Y = 0.0})
        InitialPoints.Add(New PointF With {.X = 0.875, .Y = 0.0})
        InitialPoints.Add(New PointF With {.X = 0, .Y = 0.125})
        InitialPoints.Add(New PointF With {.X = 0, .Y = 0.375})
        InitialPoints.Add(New PointF With {.X = 0, .Y = 0.625})
        InitialPoints.Add(New PointF With {.X = 0, .Y = 0.875})
        InitialPoints.Add(New PointF With {.X = 0.125, .Y = 0.875})
        InitialPoints.Add(New PointF With {.X = 0.375, .Y = 0.625})
        InitialPoints.Add(New PointF With {.X = 0.625, .Y = 0.375})
        InitialPoints.Add(New PointF With {.X = 0.875, .Y = 0.125})

        '==========================
        ' assign property package
        '==========================
        For Each pp1 As PropertyPackage In Frm.Options.PropertyPackages.Values
            If k = cbPropPack.SelectedIndex Then pp = pp1
            k += 1
        Next

        mat.SetFlowsheet(Frm)
        pp.CurrentMaterialStream = mat
        P = SystemsOfUnits.Converter.ConvertToSI(su.pressure, tbP.Text)
        T = SystemsOfUnits.Converter.ConvertToSI(su.temperature, tbT.Text)
        mat.Phases(0).Properties.pressure = P
        mat.Phases(0).Properties.temperature = T

        LLECurve.Clear()
        LLEPoints.Clear()
        final = False
        stepsize = 0.03

        '===============================
        ' find starting point
        '===============================
        For Each IPt As PointF In InitialPoints
            Ko = CalcKonode(IPt)
            If Ko.Distance > 0.01 Then
                'starting points with 2 liquids was found
                first = True
                Pt.X = IPt.X
                Pt.Y = IPt.Y
                If (Pt.X > 0 And Pt.Y = 0) Then dir = {0, 1}
                If (Pt.X = 0 And Pt.Y > 0) Then dir = {1, 0}
                If (Pt.X > 0 And Pt.Y > 0) Then dir = {-1, -1}
                Exit For
            End If
        Next
        If Not first Then
            Me.Cursor = Cursors.Default
            PanelDiag.Refresh() 'redraw Diagram
            Exit Sub 'no two liquid phases found -> exit
        End If

        '==========================
        ' calculate konodes
        '==========================

        Dim counter As Integer = 0

        Do
            Try
                Ko = CalcKonode(Pt)
                w = Ko.Distance

                If (Ko.X21 + Ko.X22 > 0) And w > 0.001 Then 'if second liquid phase is existing
                    If first Then
                        LastKo = Ko.copy
                        first = False
                    Else
                        ' check if phases need to be swaped back due to phase ordering of flash 
                        D1 = (LastKo.X11 - Ko.X11) ^ 2 + (LastKo.X12 - Ko.X12) ^ 2 'distance of last point phase 1 to new point of phase 1
                        D2 = (LastKo.X11 - Ko.X21) ^ 2 + (LastKo.X12 - Ko.X22) ^ 2 'distance of last point phase 1 to new point of phase 2

                        If D2 < D1 Then 'check if phases are swaped
                            Ko.SwapPoints()
                        End If
                        LastKo = Ko.copy
                    End If

                    LLECurve.Add(New Konode With {.X11 = Ko.X11, .X12 = Ko.X12, .X21 = Ko.X21, .X22 = Ko.X22})
                    LLEPoints.Add(New PointF With {.X = (Ko.X11 + Ko.X21) / 2, .Y = (Ko.X12 + Ko.X22) / 2})

                    With DataGridView1
                        .RowCount += 1
                        .Rows(.RowCount - 1).Cells(0).Value = Ko.X11
                        .Rows(.RowCount - 1).Cells(1).Value = Ko.X12
                        .Rows(.RowCount - 1).Cells(2).Value = Ko.X21
                        .Rows(.RowCount - 1).Cells(3).Value = Ko.X22
                    End With
                Else
                    'outside miscibility gap
                    searchmode = True
                End If

                If (w < 0.001 And Not searchmode) Or stepsize < 0.001 Or counter > 30 Then Exit Do

            Catch ex As Exception
                MsgBox("Error" & vbCrLf & ex.Message)
                Exit Do
            End Try

            If final Then
                Exit Do
            Else
                If searchmode Then
                    stepsize /= 3
                    Pt = NewPt(LastKo, stepsize, dir)
                    searchmode = False
                Else
                    'calculate new global composition as perpendicular point to last center of konode
                    Pt = NewPt(Ko, stepsize, dir)
                End If

            End If

            If Not CheckValidComp(Pt.X, Pt.Y) Then 'adjust new point if invalid 
                w = 0
                C(0) = Pt.X
                C(1) = Pt.Y
                C(2) = 1 - Pt.X - Pt.Y
                For k = 0 To 2
                    If C(k) < 0 Then C(k) = 0
                    If C(k) > 1 Then C(k) = 1
                    w += C(k)
                Next
                Pt.X = C(0) / w
                Pt.Y = C(1) / w

                final = True
            End If

            counter += 1

        Loop

        PanelDiag.Refresh() 'redraw Diagram

    End Sub
    Private Sub PrintDocument1_PrintPage(sender As System.Object, e As System.Drawing.Printing.PrintPageEventArgs) Handles PrintDocument1.PrintPage
        DrawDiagram(e.Graphics)
    End Sub
    Private Sub TSB_Print_Click(sender As System.Object, e As System.EventArgs) Handles TSB_Print.Click
        PrintDocument1.Print()
    End Sub
    Private Sub TSB_PrinterSetup_Click(sender As System.Object, e As System.EventArgs) Handles TSB_PrinterSetup.Click
        PrintDialog1.ShowDialog()
    End Sub
    Private Sub FormLLEDiagram_HelpRequested(sender As System.Object, hlpevent As System.Windows.Forms.HelpEventArgs) Handles MyBase.HelpRequested
        DWSIM.App.HelpRequested("UT_Triangular_LLE.htm")
    End Sub

    Public Sub New()

        ' Dieser Aufruf ist für den Designer erforderlich.
        InitializeComponent()

        ' Fügen Sie Initialisierungen nach dem InitializeComponent()-Aufruf hinzu.

    End Sub

    Public Sub Initialize() Implements Interfaces.IAttachedUtility.Initialize

        ExtensionMethods.ChangeDefaultFont(Me)

        Frm = AttachedTo.GetFlowsheet
        mat = New MaterialStream("", "", Frm, Nothing)

        If Me.Frm.Options.SelectedComponents.Count > 2 Then

            Populate()

            cbComp1.SelectedIndex = 0
            cbComp2.SelectedIndex = 1
            cbComp3.SelectedIndex = 2

            cbPropPack.SelectedIndex = 0

            Me.tbT.Text = Format(SystemsOfUnits.Converter.ConvertFromSI(su.temperature, 298.15), nf)

            Me.tbP.Text = Format(SystemsOfUnits.Converter.ConvertFromSI(su.pressure, 101400), nf)

            DataGridView1.Columns(0).HeaderText = "[1] " & cbComp1.Text
            DataGridView1.Columns(1).HeaderText = "[1] " & cbComp2.Text
            DataGridView1.Columns(2).HeaderText = "[2] " & cbComp1.Text
            DataGridView1.Columns(3).HeaderText = "[2] " & cbComp2.Text

        Else

            MessageBox.Show(DWSIM.App.GetLocalString("LLEEnvError_ThreeCompoundsMinimum"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)

        End If

        loaded = True

    End Sub

    Public Sub Populate() Implements Interfaces.IAttachedUtility.Populate

        ReDim Names(Me.Frm.Options.SelectedComponents.Count - 1)

        su = Frm.Options.SelectedUnitSystem
        nf = Frm.Options.NumberFormat

        Dim i As Integer

        Dim s1, s2, s3, s4 As Object
        s1 = cbComp1.SelectedItem
        s2 = cbComp2.SelectedItem
        s3 = cbComp3.SelectedItem
        s4 = cbPropPack.SelectedItem

        cbComp1.Items.Clear()
        cbComp2.Items.Clear()
        cbComp3.Items.Clear()

        For Each co As ConstantProperties In Frm.Options.SelectedComponents.Values
            cbComp1.Items.Add((co.Name))
            cbComp2.Items.Add((co.Name))
            cbComp3.Items.Add((co.Name))
            Names(i) = co.Name
            i += 1
        Next

        Me.cbPropPack.Items.Clear()
        For Each pp As PropertyPackage In Me.Frm.Options.PropertyPackages.Values
            Me.cbPropPack.Items.Add(pp.Tag & " (" & pp.ComponentName & ")")
        Next

        Me.lblT.Text = su.temperature
        Me.lblP.Text = su.pressure

        Try
            cbComp1.SelectedItem = s1
            cbComp2.SelectedItem = s2
            cbComp3.SelectedItem = s3
            cbPropPack.SelectedItem = s4
        Catch ex As Exception

        End Try

    End Sub

    Private Sub FormLLEDiagram_Shown(sender As Object, e As EventArgs) Handles Me.Load

        If Not loaded Then Initialize()

    End Sub

    Public Property AttachedTo As Interfaces.ISimulationObject Implements Interfaces.IAttachedUtility.AttachedTo

    Public Function GetPropertyList() As List(Of String) Implements Interfaces.IAttachedUtility.GetPropertyList
        Return New List(Of String)(New String() {"Name", "AutoUpdate", "P", "T", "Comp1", "Comp2", "Comp3", "PP"})
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
            Case "P"
                Return Double.Parse(tbP.Text)
            Case "T"
                Return Double.Parse(tbT.Text)
            Case "Comp1"
                Return cbComp1.SelectedItem.ToString
            Case "Comp2"
                Return cbComp2.SelectedItem.ToString
            Case "Comp3"
                Return cbComp3.SelectedItem.ToString
            Case "PP"
                Return cbPropPack.SelectedIndex
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
            Case "P"
                tbP.Text = pvalue
            Case "T"
                tbT.Text = pvalue
            Case "Comp1"
                cbComp1.SelectedItem = pvalue
            Case "Comp2"
                cbComp2.SelectedItem = pvalue
            Case "Comp3"
                cbComp3.SelectedItem = pvalue
            Case "PP"
                cbPropPack.SelectedIndex = pvalue
        End Select
    End Sub

    Public Sub Update1() Implements Interfaces.IAttachedUtility.Update
        btnCalcDiagram_Click(Me, New EventArgs)
    End Sub

    Public Function GetUtilityType() As FlowsheetUtility Implements Interfaces.IAttachedUtility.GetUtilityType
        Return FlowsheetUtility.PhaseEnvelopeTernary
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

Public Class Rec
    Public Left, Right, Top, Bottom As UInteger
End Class

Public Class Konode
    Public X11, X12, X21, X22 As Double
    Public Function copy() As Konode
        Dim R As New Konode
        R.X11 = X11
        R.X12 = X12
        R.X21 = X21
        R.X22 = X22
        Return R
    End Function
    Public Sub SwapPoints()
        Dim x, y As Double
        x = X11
        y = X12
        X11 = X21
        X12 = X22
        X21 = x
        X22 = y
    End Sub
    Public Function Distance() As Double
        Dim dx, dy As Double
        dx = X21 - X11
        dy = X22 - X12
        Return Math.Sqrt(dx ^ 2 + dy ^ 2)
    End Function
End Class