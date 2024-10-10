'    Copyright 2008 Daniel Wagner O. de Medeiros
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

Public Class FormHYD_DET

    Inherits System.Windows.Forms.Form

    Public res As Object
    Public sI As Boolean = True
    Public nomes() As String
    Public model As Integer
    Public P, T As Double

    Public m_aux As DWSIM.Utilities.HYD.AuxMethods

    Dim Frm As FormFlowsheet

    Public su As New SystemsOfUnits.Units
    Public cv As New SystemsOfUnits.Converter
    Public nf As String

    Public Sub FormDET_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        m_aux = New DWSIM.Utilities.HYD.AuxMethods

        Me.Frm = My.Application.ActiveSimulation
        Me.su = Frm.Options.SelectedUnitSystem
        Me.nf = Frm.Options.NumberFormat

        'unidades
        Dim uP, uT As String
        uP = su.pressure
        uT = su.temperature

        Label15.Text = uP
        Label16.Text = uT
        Label18.Text = uT

        Dim n As Integer = nomes.Length

        Dim xwH As Double

        If sI = True Then
            xwH = 0.88
        Else
            xwH = 0.9
        End If

        Grid1.Rows.Clear()

        Dim i As Integer
        i = 0
        Do
            Grid1.Rows.Add()
            Grid1.Rows(i).Cells(0).Value = (nomes(i))
            i = i + 1
        Loop Until i = n

        Dim Vx, VxHC, Vy, Vh, VxH As Object

        Vx = res(4)
        Vy = res(5)
        Vh = res(6)
        Try
            VxH = res(7)
            VxHC = res(8)
        Catch
            VxHC = res(7)
            VxH = New Object(n - 1) {}
            i = 0
            Do
                VxH(i) = 0
                i = i + 1
            Loop Until i = UBound(VxH) + 1
        End Try

        Dim Vx2(Grid1.Rows.Count - 1), Vy2(Grid1.Rows.Count - 1), Vh2(Grid1.Rows.Count - 1)
        Dim Vn(Grid1.Rows.Count - 1), Vzprel(Grid1.Rows.Count - 1), Vids(Grid1.Rows.Count - 1)

        i = 0
        Do
            Vn(i) = nomes(i)
            Vzprel(i) = nomes(i)
            i += 1
        Loop Until i = Grid1.Rows.Count
        Vids = m_aux.RetornarIDsParaCalculoDeHidratos(nomes)

        Dim soma = 0
        i = 0
        Do
            soma += Vx(i)
            i = i + 1
        Loop Until i = UBound(Vx) + 1

        i = 0
        Do
            If Vids(i) > 100 Then
                soma += Vzprel(i)
            End If
            i = i + 1
        Loop Until i = Grid1.Rows.Count

        ReDim Preserve VxH(Grid1.Rows.Count - 1)

        i = 0
        Do
            If Vids(i) > 100 Then
                If soma <> 0.0# Then Vx2(i) = Vzprel(i) / soma Else Vx2(i) = 0.0#
                Vy2(i) = 0
                Vh2(i) = 0
                VxH(i) = 0
            Else
                If soma <> 0.0# Then Vx2(i) = Vx(i) / soma Else Vx2(i) = 0.0#
                Vy2(i) = Vy(i)
                Vh2(i) = Vh(i) * (1 - xwH)
            End If
            If Vids(i) = 13 Then Vh2(i) = xwH
            i = i + 1
        Loop Until i = Grid1.Rows.Count

        'vdwP,KS,SL     {Td, act, (FG)PQAG, (FG)PQHYD, Vx, Vy, Vh}
        'CG             {Td, act}

        Dim Phases As String = ""
        If T < res(0) Then Phases = DWSIM.App.GetLocalString("SlidoGeloGseHidrato")
        If T > res(0) Then Phases = DWSIM.App.GetLocalString("LquidoguaGseHidrato2")
        If Math.Abs(T - res(0)) < 0.01 Then Phases = DWSIM.App.GetLocalString("SlidoGeloLquidoguaGs")
        If soma = 0.0# Then Phases = DWSIM.App.GetLocalString("VaporAndHydrate")

        If model <> 2 Then

            Label8.Text = Format(SystemsOfUnits.Converter.ConvertFromSI(su.pressure, P), nf)
            Label9.Text = Format(SystemsOfUnits.Converter.ConvertFromSI(su.temperature, T), nf)
            Label10.Text = Phases
            Label11.Text = Format(SystemsOfUnits.Converter.ConvertFromSI(su.temperature, res(0)), nf)
            Label12.Text = Format(res(1), "####0.########")
            Label13.Text = Format(res(2) / 1000, "####0.0000#")
            Label14.Text = Format(res(3) / 1000, "####0.0000#")

            i = 0
            Do
                Grid1.Rows(i).Cells(1).Value = Format(VxHC(i), "####0.0000")
                Grid1.Rows(i).Cells(2).Value = Format(Vx2(i), "####0.0000")
                Grid1.Rows(i).Cells(3).Value = Format(Vy2(i), "####0.0000")
                Grid1.Rows(i).Cells(4).Value = Format(Vh2(i), "####0.0000")
                'Grid1.Rows(i).Cells(4).Value = Format(VxH(i), "####0.0000")
                i = i + 1
            Loop Until i = Grid1.Rows.Count

        Else

            Label8.Text = Format(SystemsOfUnits.Converter.ConvertFromSI(su.pressure, P), nf)
            Label9.Text = Format(SystemsOfUnits.Converter.ConvertFromSI(su.temperature, T), nf)
            Label10.Text = Phases
            Label11.Text = Format(SystemsOfUnits.Converter.ConvertFromSI(su.temperature, res(0)), nf)
            Label12.Text = Format(res(1), "####0.########")
            Label13.Text = DWSIM.App.GetLocalString("ND") 'Format(res(2) / 1000, "####0.0000#")
            Label14.Text = DWSIM.App.GetLocalString("ND") 'Format(res(3) / 1000, "####0.0000#")

            i = 0
            Do
                Grid1.Rows(i).Cells(1).Value = Format(VxHC(i), "####0.0000")
                Grid1.Rows(i).Cells(2).Value = Format(Vx2(i), "####0.0000")
                Grid1.Rows(i).Cells(3).Value = Format(Vy2(i), "####0.0000")
                Grid1.Rows(i).Cells(4).Value = Format(Vh2(i), "####0.0000")
                'Grid1.Rows(i).Cells(4).Value = Format(VxH(i), "####0.0000")
                i = i + 1
            Loop Until i = Grid1.Rows.Count

        End If

        ChangeDefaultFont()

    End Sub

    Private Sub FormHYD_DET_HelpRequested(sender As System.Object, hlpevent As System.Windows.Forms.HelpEventArgs) Handles MyBase.HelpRequested

        DWSIM.App.HelpRequested("UT_HydrateDissociation.htm")

    End Sub

End Class