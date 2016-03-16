'    Excel/Spreadsheet Unit Calculation Routines 
'    Copyright 2014 Gregor Reichert, 2015 Daniel Medeiros
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

Imports Microsoft.MSDN.Samples.GraphicObjects
Imports DWSIM.DWSIM.Flowsheet.FlowsheetSolver
Imports DWSIM.DWSIM.SimulationObjects.UnitOps.Auxiliary
Imports Excel = NetOffice.ExcelApi
Imports NetOffice.ExcelApi.Enums
Imports GS = GemBox.Spreadsheet

Namespace DWSIM.SimulationObjects.UnitOps.Auxiliary

    <System.Serializable()> Public Class ExcelParameter
        Public Name As String = ""
        Public Value As Double = 0.0#
        Public Unit As String = ""
        Public Annotation As String = ""
    End Class

End Namespace

Namespace DWSIM.SimulationObjects.UnitOps

    <System.Serializable()> Public Class ExcelUO

        Inherits SimulationObjects_UnitOpBaseClass

        Protected m_DQ As Nullable(Of Double)
        Protected m_FileName As String = ""
        Protected m_InputParams As New Dictionary(Of String, ExcelParameter)
        Protected m_OutputParams As New Dictionary(Of String, ExcelParameter)

        Public Property InputParams() As Dictionary(Of String, ExcelParameter)
            Get
                Return m_InputParams
            End Get
            Set(value As Dictionary(Of String, ExcelParameter))
                m_InputParams = value
            End Set
        End Property
        Public Property OutputParams() As Dictionary(Of String, ExcelParameter)
            Get
                Return m_OutputParams
            End Get
            Set(value As Dictionary(Of String, ExcelParameter))
                m_OutputParams = value
            End Set
        End Property

        Public Property Filename() As String
            Get
                Return m_FileName
            End Get
            Set(ByVal value As String)
                m_FileName = value
            End Set
        End Property

        Public Sub New(ByVal nome As String, ByVal descricao As String)
            MyBase.CreateNew()
            Me.m_ComponentName = nome
            Me.m_ComponentDescription = descricao
            Me.FillNodeItems()
            Me.QTFillNodeItems()
        End Sub

        Public Property DeltaQ() As Nullable(Of Double)
            Get
                Return m_DQ
            End Get
            Set(ByVal value As Nullable(Of Double))
                m_DQ = value
            End Set
        End Property

        Public Sub New()
            MyBase.New()
        End Sub

        Private Function ExctractFilepath(ByVal S As String) As String
            Dim P1, P2 As Integer
            P1 = InStr(1, S, "(") + 1
            P2 = InStrRev(S, "\") + 1

            Return Mid(S, P1, P2 - P1)
        End Function

        Public Overrides Function Calculate(Optional ByVal args As Object = Nothing) As Integer

            Dim form As Global.DWSIM.FormFlowsheet = Me.FlowSheet
            Dim objargs As New DWSIM.Outros.StatusChangeEventArgs
            Dim k, ci, co As Integer

            Dim excelType As Type = Nothing

            If Not DWSIM.App.IsRunningOnMono Then excelType = Type.GetTypeFromProgID("Excel.Application")

            If Not DWSIM.App.IsRunningOnMono And Not excelType Is Nothing Then

                Dim excelProxy As Object = Activator.CreateInstance(excelType)

                Using xcl As New Excel.Application(Nothing, excelProxy)

                    For Each CurrAddin As Excel.AddIn In xcl.AddIns
                        If CurrAddin.Installed Then
                            CurrAddin.Installed = False
                            CurrAddin.Installed = True
                        End If
                    Next

                    'xcl.Visible = True 'uncomment for debugging

                    Dim mybook As Excel.Workbook
                    Dim AppPath = Application.StartupPath

                    'Load Excel definition file
                    If My.Computer.FileSystem.FileExists(Filename) Then
                        mybook = xcl.Workbooks.Open(Filename)
                    Else
                        xcl.Quit()
                        'xcl.Dispose()
                        Throw New Exception("Definition file '" & Filename & "' :" & DWSIM.App.GetLocalString("Oarquivonoexisteoufo"))
                    End If

                    xcl.Calculation = XlCalculation.xlCalculationManual

                    Dim mysheetIn As Excel.Worksheet = mybook.Sheets("Input")
                    Dim mysheetOut As Excel.Worksheet = mybook.Sheets("Output")
                    '=====================================================================================================

                    If Not Me.GraphicObject.InputConnectors(4).IsAttached Then 'Check if Energy stream existing
                        'Call function to calculate flowsheet
                        With objargs
                            .Calculado = False
                            .Nome = Me.Nome
                            .Tipo = TipoObjeto.ExcelUO
                        End With
                        mybook.Close(saveChanges:=False)
                        xcl.Quit()
                        'xcl.Dispose()
                        'CalculateFlowsheet(FlowSheet, objargs, Nothing)
                        Throw New Exception(DWSIM.App.GetLocalString("Nohcorrentedeenergia1"))
                    End If

                    'check if at least one input and output connection is available
                    For k = 0 To 3
                        If GraphicObject.InputConnectors(k).IsAttached Then ci += 1
                        If GraphicObject.OutputConnectors(k).IsAttached Then co += 1
                    Next
                    If ci = 0 Or co = 0 Then
                        'Call function to calculate flowsheet
                        With objargs
                            .Calculado = False
                            .Nome = Me.Nome
                            .Tipo = TipoObjeto.ExcelUO
                        End With
                        mybook.Close(saveChanges:=False)
                        xcl.Quit()
                        'xcl.Dispose()
                        'CalculateFlowsheet(FlowSheet, objargs, Nothing)
                        Throw New Exception(DWSIM.App.GetLocalString("Verifiqueasconexesdo"))
                    End If

                    Dim Ti, Pi, Hi, Wi, T2, P2, H2, Hin, Hout, Win, Wout, MassBal As Double
                    Dim es As DWSIM.SimulationObjects.Streams.EnergyStream = form.Collections.CLCS_EnergyStreamCollection(Me.GraphicObject.InputConnectors(4).AttachedConnector.AttachedFrom.Name)
                    Dim ParName As String
                    Dim i As Integer

                    '======= write data to Excel ==============================================================
                    mysheetIn.Range("B5:E8").Value = "" 'delete Name, T, P, H of streams
                    mysheetIn.Range("A12:E150").Value = "" 'delete molar flows of streams

                    '======= write stream names to Excel =========
                    For k = 0 To 3
                        If GraphicObject.InputConnectors(k).IsAttached Then
                            mysheetIn.Cells(5, 2 + k).Value = Me.GraphicObject.InputConnectors(k).AttachedConnector.AttachedFrom.Tag
                        Else
                            mysheetIn.Cells(5, 2 + k).Value = ""
                        End If
                        If GraphicObject.OutputConnectors(k).IsAttached Then
                            mysheetOut.Cells(5, 2 + k).Value = Me.GraphicObject.OutputConnectors(k).AttachedConnector.AttachedTo.Tag
                        Else
                            mysheetOut.Cells(5, 2 + k).Value = ""
                        End If
                    Next

                    '======== write input parameters ============
                    k = 0
                    For Each EP As ExcelParameter In InputParams.Values
                        mysheetIn.Cells(5 + k, 8).Formula = EP.Value
                        k += 1
                    Next

                    '======== Input streams to unit =======
                    Dim S As DWSIM.SimulationObjects.Streams.MaterialStream
                    For k = 0 To 3
                        If Me.GraphicObject.InputConnectors(k).IsAttached Then
                            S = form.Collections.CLCS_MaterialStreamCollection(Me.GraphicObject.InputConnectors(k).AttachedConnector.AttachedFrom.Name)
                            Me.PropertyPackage.CurrentMaterialStream = S
                            Ti = S.Fases(0).SPMProperties.temperature.GetValueOrDefault.ToString
                            Pi = S.Fases(0).SPMProperties.pressure.GetValueOrDefault.ToString
                            Hi = S.Fases(0).SPMProperties.enthalpy.GetValueOrDefault.ToString
                            Wi = S.Fases(0).SPMProperties.massflow.GetValueOrDefault.ToString
                            Hin += Hi * Wi
                            Win += Wi

                            '======= transfer data to Excel ===========================================================
                            mysheetIn.Cells(6, 2 + k).Value = Ti
                            mysheetIn.Cells(7, 2 + k).Value = Pi
                            mysheetIn.Cells(8, 2 + k).Value = Hi

                            Dim dy As Integer = 0
                            For Each comp As DWSIM.ClassesBasicasTermodinamica.Substancia In S.Fases(0).Componentes.Values
                                mysheetIn.Cells(12 + dy, 1).Value = comp.ConstantProperties.Name
                                mysheetOut.Cells(12 + dy, 1).Value = comp.ConstantProperties.Name
                                mysheetIn.Cells(12 + dy, 2 + k).Value = comp.MolarFlow
                                dy += 1
                            Next
                        Else
                            mysheetIn.Cells(6, 2 + k).Value = ""
                            mysheetIn.Cells(7, 2 + k).Value = ""
                            mysheetIn.Cells(8, 2 + k).Value = ""
                            Dim dy As Integer = 0
                            For Each comp As DWSIM.ClassesBasicasTermodinamica.Substancia In Me.PropertyPackage.CurrentMaterialStream.Fases(0).Componentes.Values
                                mysheetIn.Cells(12 + dy, 1).Value = comp.ConstantProperties.Name
                                mysheetOut.Cells(12 + dy, 1).Value = comp.ConstantProperties.Name
                                mysheetIn.Cells(12 + dy, 2 + k).Value = ""
                                mysheetIn.Cells(12 + dy, 3 + k).Value = ""
                                dy += 1
                            Next
                        End If
                    Next

                    xcl.Calculate()

                    '======= read results from Excel =============================================================
                    Dim Vmol As New Dictionary(Of String, Double)
                    Dim v As Double
                    Dim SMass, SMole As Double

                    For k = 0 To 3 'run through all streams to execute TP-flash
                        If Me.GraphicObject.OutputConnectors(k).IsAttached Then
                            Me.PropertyPackage.CurrentMaterialStream = form.Collections.CLCS_MaterialStreamCollection(Me.GraphicObject.OutputConnectors(k).AttachedConnector.AttachedTo.Name)

                            T2 = mysheetOut.Cells(6, 2 + k).Value
                            P2 = mysheetOut.Cells(7, 2 + k).Value

                            'Atribuir valores à corrente de matéria conectada à jusante
                            With Me.PropertyPackage.CurrentMaterialStream
                                .Fases(0).SPMProperties.temperature = T2
                                .Fases(0).SPMProperties.pressure = P2

                                Dim comp As DWSIM.ClassesBasicasTermodinamica.Substancia
                                i = 0
                                SMole = 0
                                SMass = 0
                                Vmol.Clear()
                                For Each comp In .Fases(0).Componentes.Values
                                    v = mysheetOut.Cells(12 + i, 2 + k).Value
                                    Vmol.Add(comp.Nome, v)
                                    SMole += Vmol(comp.Nome)
                                    SMass += Vmol(comp.Nome) * comp.ConstantProperties.Molar_Weight / 1000
                                    i += 1
                                Next
                                For Each comp In .Fases(0).Componentes.Values
                                    comp.FracaoMolar = Vmol(comp.Nome) / SMole
                                    comp.FracaoMassica = Vmol(comp.Nome) * comp.ConstantProperties.Molar_Weight / SMass / 1000
                                Next
                                .Fases(0).SPMProperties.massflow = SMass

                                Try
                                    Dim tmp = Me.PropertyPackage.DW_CalcEquilibrio_ISOL(PropertyPackages.FlashSpec.T, PropertyPackages.FlashSpec.P, T2, P2, 0)
                                    H2 = tmp(4)
                                    .Fases(0).SPMProperties.enthalpy = H2
                                Catch ex As Exception
                                    mybook.Close(saveChanges:=True)
                                    xcl.Quit()
                                    xcl.Dispose()
                                    Throw New Exception("Flash calculation error")
                                End Try

                                Hout += H2 * SMass
                                Wout += SMass
                            End With

                        End If
                    Next

                    '======= caclculate output stream data ====================================================

                    Me.DeltaQ = Hout - Hin

                    'Corrente de energia - atualizar valor da potência (kJ/s)
                    With es
                        .Energia = Me.DeltaQ.GetValueOrDefault
                        .GraphicObject.Calculated = True
                    End With

                    '======== read output parameters from Excel table =========================================
                    k = 0
                    OutputParams.Clear()
                    Do
                        Dim ExlPar As New ExcelParameter

                        ParName = mysheetOut.Cells(5 + k, 7).Value
                        If ParName <> "" Then
                            ExlPar.Name = ParName
                            ExlPar.Value = mysheetOut.Cells(5 + k, 8).Value
                            ExlPar.Unit = mysheetOut.Cells(5 + k, 9).Value
                            ExlPar.Annotation = mysheetOut.Cells(5 + k, 10).Value
                            OutputParams.Add(ExlPar.Name, ExlPar)

                            k += 1
                        End If
                    Loop While ParName <> ""


                    '=============== clean up Excel stuff ================================================================
                    mybook.Close(saveChanges:=True)
                    xcl.Quit()
                    'xcl.Dispose()

                    MassBal = 100 * (Wout - Win) / (Win)
                    If Math.Abs(MassBal) > 0.001 Then
                        form.WriteToLog(Me.GraphicObject.Tag & ": " & "Mass balance error: " & MassBal & "%", Color.Red, FormClasses.TipoAviso.Erro)
                    End If

                    'Call function to calculate flowsheet
                    With objargs
                        .Calculado = True
                        .Nome = Me.Nome
                        .Tag = Me.GraphicObject.Tag
                        .Tipo = TipoObjeto.ExcelUO
                    End With

                End Using

            Else

                'use GemBox to read and write data

                GS.SpreadsheetInfo.SetLicense("FREE-LIMITED-KEY")

                Dim xcl As GS.ExcelFile = Nothing

                Dim AppPath = Application.StartupPath

                'Load Excel definition file
                If My.Computer.FileSystem.FileExists(Filename) Then
                    xcl = GS.ExcelFile.Load(Filename)
                Else
                    Throw New Exception("Definition file '" & Filename & "' :" & DWSIM.App.GetLocalString("Oarquivonoexisteoufo"))
                End If

                Dim mysheetIn As GS.ExcelWorksheet = xcl.Worksheets("Input")
                Dim mysheetOut As GS.ExcelWorksheet = xcl.Worksheets("Output")
                '=====================================================================================================

                If Not Me.GraphicObject.InputConnectors(4).IsAttached Then 'Check if Energy stream existing
                    'Call function to calculate flowsheet
                    With objargs
                        .Calculado = False
                        .Nome = Me.Nome
                        .Tipo = TipoObjeto.ExcelUO
                    End With
                    Throw New Exception(DWSIM.App.GetLocalString("Nohcorrentedeenergia1"))
                End If

                'check if at least one input and output connection is available
                For k = 0 To 3
                    If GraphicObject.InputConnectors(k).IsAttached Then ci += 1
                    If GraphicObject.OutputConnectors(k).IsAttached Then co += 1
                Next
                If ci = 0 Or co = 0 Then
                    'Call function to calculate flowsheet
                    With objargs
                        .Calculado = False
                        .Nome = Me.Nome
                        .Tipo = TipoObjeto.ExcelUO
                    End With
                    Throw New Exception(DWSIM.App.GetLocalString("Verifiqueasconexesdo"))
                End If

                Dim Ti, Pi, Hi, Wi, T2, P2, H2, Hin, Hout, Win, Wout, MassBal As Double
                Dim es As DWSIM.SimulationObjects.Streams.EnergyStream = form.Collections.CLCS_EnergyStreamCollection(Me.GraphicObject.InputConnectors(4).AttachedConnector.AttachedFrom.Name)
                Dim ParName As String
                Dim i As Integer

                '======= write data to Excel ==============================================================
                mysheetIn.Cells.GetSubrange("B5", "E8").Value = "" 'delete Name, T, P, H of streams
                mysheetIn.Cells.GetSubrange("A12", "E150").Value = "" 'delete molar flows of streams

                '======= write stream names to Excel =========
                For k = 0 To 3
                    If GraphicObject.InputConnectors(k).IsAttached Then
                        mysheetIn.Cells(4, 1 + k).Value = Me.GraphicObject.InputConnectors(k).AttachedConnector.AttachedFrom.Tag
                    Else
                        mysheetIn.Cells(4, 1 + k).Value = ""
                    End If
                    If GraphicObject.OutputConnectors(k).IsAttached Then
                        mysheetOut.Cells(4, 1 + k).Value = Me.GraphicObject.OutputConnectors(k).AttachedConnector.AttachedTo.Tag
                    Else
                        mysheetOut.Cells(4, 1 + k).Value = ""
                    End If
                Next

                '======== write input parameters ============
                k = 0
                For Each EP As ExcelParameter In InputParams.Values
                    mysheetIn.Cells(4 + k, 7).Formula = EP.Value
                    k += 1
                Next

                '======== Input streams to unit =======
                Dim S As DWSIM.SimulationObjects.Streams.MaterialStream
                For k = 0 To 3
                    If Me.GraphicObject.InputConnectors(k).IsAttached Then
                        S = form.Collections.CLCS_MaterialStreamCollection(Me.GraphicObject.InputConnectors(k).AttachedConnector.AttachedFrom.Name)
                        Me.PropertyPackage.CurrentMaterialStream = S
                        Ti = S.Fases(0).SPMProperties.temperature.GetValueOrDefault
                        Pi = S.Fases(0).SPMProperties.pressure.GetValueOrDefault
                        Hi = S.Fases(0).SPMProperties.enthalpy.GetValueOrDefault
                        Wi = S.Fases(0).SPMProperties.massflow.GetValueOrDefault
                        Hin += Hi * Wi
                        Win += Wi

                        '======= transfer data to Excel ===========================================================
                        mysheetIn.Cells(5, 1 + k).Value = Ti
                        mysheetIn.Cells(6, 1 + k).Value = Pi
                        mysheetIn.Cells(7, 1 + k).Value = Hi

                        Dim dy As Integer = 0
                        For Each comp As DWSIM.ClassesBasicasTermodinamica.Substancia In S.Fases(0).Componentes.Values
                            mysheetIn.Cells(11 + dy, 0).Value = comp.ConstantProperties.Name
                            mysheetOut.Cells(11 + dy, 0).Value = comp.ConstantProperties.Name
                            mysheetIn.Cells(11 + dy, 1 + k).Value = comp.MolarFlow.GetValueOrDefault
                            dy += 1
                        Next
                    Else
                        mysheetIn.Cells(5, 1 + k).Value = ""
                        mysheetIn.Cells(6, 1 + k).Value = ""
                        mysheetIn.Cells(7, 1 + k).Value = ""
                        Dim dy As Integer = 0
                        For Each comp As DWSIM.ClassesBasicasTermodinamica.Substancia In Me.PropertyPackage.CurrentMaterialStream.Fases(0).Componentes.Values
                            mysheetIn.Cells(11 + dy, 0).Value = comp.ConstantProperties.Name
                            mysheetOut.Cells(11 + dy, 0).Value = comp.ConstantProperties.Name
                            mysheetIn.Cells(11 + dy, 1 + k).Value = ""
                            mysheetIn.Cells(11 + dy, 2 + k).Value = ""
                            dy += 1
                        Next
                    End If
                Next

                'open spreadsheet to be calculated manually by the user.

                xcl.Save(Filename)

                If DWSIM.App.IsRunningOnMono Then
                    Dim p As New Process()
                    With p
                        .StartInfo.FileName = "xdg-open"
                        .StartInfo.Arguments = Filename
                        .StartInfo.UseShellExecute = False
                        .Start()
                        MessageBox.Show("Click 'OK' once the spreadsheet formula updating process is finished.")
                    End With
                Else
                    Process.Start(Filename)
                    MessageBox.Show("Click 'OK' once the spreadsheet formula updating process is finished.")
                End If

                'Load Excel definition file
                xcl = GS.ExcelFile.Load(Filename)

                mysheetIn = xcl.Worksheets("Input")
                mysheetOut = xcl.Worksheets("Output")

                '======= read results from sheet =============================================================
                Dim Vmol As New Dictionary(Of String, Double)
                Dim v As Double
                Dim SMass, SMole As Double

                For k = 0 To 3 'run through all streams to execute TP-flash
                    If Me.GraphicObject.OutputConnectors(k).IsAttached Then
                        Me.PropertyPackage.CurrentMaterialStream = form.Collections.CLCS_MaterialStreamCollection(Me.GraphicObject.OutputConnectors(k).AttachedConnector.AttachedTo.Name)

                        T2 = mysheetOut.Cells(5, 1 + k).Value
                        P2 = mysheetOut.Cells(6, 1 + k).Value

                        'Atribuir valores à corrente de matéria conectada à jusante
                        With Me.PropertyPackage.CurrentMaterialStream
                            .Fases(0).SPMProperties.temperature = T2
                            .Fases(0).SPMProperties.pressure = P2

                            Dim comp As DWSIM.ClassesBasicasTermodinamica.Substancia
                            i = 0
                            SMole = 0
                            SMass = 0
                            Vmol.Clear()
                            For Each comp In .Fases(0).Componentes.Values
                                v = mysheetOut.Cells(11 + i, 1 + k).Value
                                Vmol.Add(comp.Nome, v)
                                SMole += Vmol(comp.Nome)
                                SMass += Vmol(comp.Nome) * comp.ConstantProperties.Molar_Weight / 1000
                                i += 1
                            Next
                            For Each comp In .Fases(0).Componentes.Values
                                comp.FracaoMolar = Vmol(comp.Nome) / SMole
                                comp.FracaoMassica = Vmol(comp.Nome) * comp.ConstantProperties.Molar_Weight / SMass / 1000
                            Next
                            .Fases(0).SPMProperties.massflow = SMass

                            Try
                                Dim tmp = Me.PropertyPackage.DW_CalcEquilibrio_ISOL(PropertyPackages.FlashSpec.T, PropertyPackages.FlashSpec.P, T2, P2, 0)
                                H2 = tmp(4)
                                .Fases(0).SPMProperties.enthalpy = H2
                            Catch ex As Exception
                                Throw New Exception("Flash calculation error")
                            End Try

                            Hout += H2 * SMass
                            Wout += SMass
                        End With

                    End If
                Next

                '======= caclculate output stream data ====================================================

                Me.DeltaQ = Hout - Hin

                'Corrente de energia - atualizar valor da potência (kJ/s)
                With es
                    .Energia = Me.DeltaQ.GetValueOrDefault
                    .GraphicObject.Calculated = True
                End With

                '======== read output parameters from Excel table =========================================
                k = 0
                OutputParams.Clear()
                Do
                    Dim ExlPar As New ExcelParameter

                    ParName = mysheetOut.Cells(4 + k, 6).Value
                    If ParName <> "" Then
                        ExlPar.Name = ParName
                        ExlPar.Value = mysheetOut.Cells(4 + k, 7).Value
                        ExlPar.Unit = mysheetOut.Cells(4 + k, 8).Value
                        ExlPar.Annotation = mysheetOut.Cells(4 + k, 9).Value
                        OutputParams.Add(ExlPar.Name, ExlPar)

                        k += 1
                    End If
                Loop While ParName <> ""

                MassBal = 100 * (Wout - Win) / (Win)
                If Math.Abs(MassBal) > 0.001 Then
                    form.WriteToLog(Me.GraphicObject.Tag & ": " & "Mass balance error: " & MassBal & "%", Color.Red, FormClasses.TipoAviso.Erro)
                End If

                'Call function to calculate flowsheet
                With objargs
                    .Calculado = True
                    .Nome = Me.Nome
                    .Tag = Me.GraphicObject.Tag
                    .Tipo = TipoObjeto.ExcelUO
                End With

            End If

            '===========================================

            form.CalculationQueue.Enqueue(objargs)

        End Function

        Public Overrides Function DeCalculate() As Integer
            Dim k As Integer
            Dim form As Global.DWSIM.FormFlowsheet = Me.FlowSheet

            For k = 0 To 3
                If Me.GraphicObject.OutputConnectors(k).IsAttached Then

                    'Zerar valores da corrente de matéria conectada a jusante
                    With form.Collections.CLCS_MaterialStreamCollection(Me.GraphicObject.OutputConnectors(k).AttachedConnector.AttachedTo.Name)
                        .Fases(0).SPMProperties.temperature = Nothing
                        .Fases(0).SPMProperties.pressure = Nothing
                        .Fases(0).SPMProperties.enthalpy = Nothing
                        .Fases(0).SPMProperties.molarfraction = 1
                        .Fases(0).SPMProperties.massfraction = 1
                        Dim comp As DWSIM.ClassesBasicasTermodinamica.Substancia
                        Dim i As Integer = 0
                        For Each comp In .Fases(0).Componentes.Values
                            comp.FracaoMolar = 0
                            comp.FracaoMassica = 0
                            i += 1
                        Next
                        .Fases(0).SPMProperties.massflow = Nothing
                        .Fases(0).SPMProperties.molarflow = Nothing
                        .GraphicObject.Calculated = False
                    End With

                End If
            Next

            'Corrente de energia - atualizar valor da potência (kJ/s)
            If Me.GraphicObject.EnergyConnector.IsAttached Then
                With form.Collections.CLCS_EnergyStreamCollection(Me.GraphicObject.EnergyConnector.AttachedConnector.AttachedTo.Name)
                    .Energia = Nothing
                    .GraphicObject.Calculated = False
                End With
            End If

            'Call function to calculate flowsheet
            Dim objargs As New DWSIM.Outros.StatusChangeEventArgs
            With objargs
                .Calculado = False
                .Nome = Me.Nome
                .Tag = Me.GraphicObject.Tag
                .Tipo = TipoObjeto.Heater
            End With

            form.CalculationQueue.Enqueue(objargs)

        End Function

        Public Overloads Overrides Sub UpdatePropertyNodes(ByVal su As SistemasDeUnidades.Unidades, ByVal nf As String)

            Dim Conversor As New DWSIM.SistemasDeUnidades.Conversor
            If Me.NodeTableItems Is Nothing Then
                Me.NodeTableItems = New System.Collections.Generic.Dictionary(Of Integer, DWSIM.Outros.NodeItem)
                Me.FillNodeItems()
            End If

            For Each nti As Outros.NodeItem In Me.NodeTableItems.Values
                nti.Value = GetPropertyValue(nti.Text, FlowSheet.Options.SelectedUnitSystem)
                nti.Unit = GetPropertyUnit(nti.Text, FlowSheet.Options.SelectedUnitSystem)
            Next

            If Me.QTNodeTableItems Is Nothing Then
                Me.QTNodeTableItems = New System.Collections.Generic.Dictionary(Of Integer, DWSIM.Outros.NodeItem)
                Me.QTFillNodeItems()
            End If

            'read input and output parameters from associated Excel table if not existing yet (=during loading from file)
            If Filename <> "" And InputParams.Count = 0 And OutputParams.Count = 0 Then

                Dim excelType As Type = Nothing

                If Not DWSIM.App.IsRunningOnMono Then excelType = Type.GetTypeFromProgID("Excel.Application")

                If Not DWSIM.App.IsRunningOnMono And Not excelType Is Nothing Then

                    Dim excelProxy As Object = Activator.CreateInstance(excelType)

                    Using xcl As New Excel.Application(Nothing, excelProxy)

                        For Each CurrAddin As Excel.AddIn In xcl.AddIns
                            If CurrAddin.Installed Then
                                CurrAddin.Installed = False
                                CurrAddin.Installed = True
                            End If
                        Next

                        'xcl.Visible = True 'uncomment for debugging

                        Dim mybook As Excel.Workbook
                        Dim AppPath = Application.StartupPath
                        Dim ParName As String
                        Dim i As Integer

                        'Load Excel definition file
                        If My.Computer.FileSystem.FileExists(Filename) Then
                            mybook = xcl.Workbooks.Open(Filename, True, True)
                            Dim mysheetIn As Excel.Worksheet = mybook.Sheets("Input")
                            Dim mysheetOut As Excel.Worksheet = mybook.Sheets("Output")

                            InputParams.Clear()
                            i = 0
                            Do
                                Dim ExlPar As New ExcelParameter

                                ParName = mysheetIn.Cells(5 + i, 7).Value
                                If ParName <> "" Then
                                    ExlPar.Name = ParName
                                    ExlPar.Value = mysheetIn.Cells(5 + i, 8).Value
                                    ExlPar.Unit = mysheetIn.Cells(5 + i, 9).Value
                                    ExlPar.Annotation = mysheetIn.Cells(5 + i, 10).Value
                                    InputParams.Add(ExlPar.Name, ExlPar)

                                    i += 1
                                End If
                            Loop While ParName <> ""

                            OutputParams.Clear()
                            i = 0
                            Do
                                Dim ExlPar As New ExcelParameter

                                ParName = mysheetOut.Cells(5 + i, 7).Value
                                If ParName <> "" Then
                                    ExlPar.Name = ParName
                                    ExlPar.Value = mysheetOut.Cells(5 + i, 8).Value
                                    ExlPar.Unit = mysheetOut.Cells(5 + i, 9).Value
                                    ExlPar.Annotation = mysheetOut.Cells(5 + i, 10).Value
                                    OutputParams.Add(ExlPar.Name, ExlPar)

                                    i += 1
                                End If
                            Loop While ParName <> ""

                            mybook.Close(False)

                        End If

                        xcl.Quit()
                        xcl.Dispose()

                    End Using

                Else

                    'use GemBox to read and write data

                    GS.SpreadsheetInfo.SetLicense("FREE-LIMITED-KEY")

                    Dim xcl As GS.ExcelFile = Nothing

                    Dim AppPath = Application.StartupPath
                    Dim ParName As String
                    Dim i As Integer

                    'Load Excel definition file
                    If My.Computer.FileSystem.FileExists(Filename) Then
                        xcl = GS.ExcelFile.Load(Filename)
                        Dim mysheetIn As GS.ExcelWorksheet = xcl.Worksheets("Input")
                        Dim mysheetOut As GS.ExcelWorksheet = xcl.Worksheets("Output")

                        InputParams.Clear()
                        i = 0
                        Do
                            Dim ExlPar As New ExcelParameter

                            ParName = mysheetIn.Cells(4 + i, 6).Value
                            If ParName <> "" Then
                                ExlPar.Name = ParName
                                ExlPar.Value = mysheetIn.Cells(4 + i, 7).Value
                                ExlPar.Unit = mysheetIn.Cells(4 + i, 8).Value
                                ExlPar.Annotation = mysheetIn.Cells(4 + i, 9).Value
                                InputParams.Add(ExlPar.Name, ExlPar)

                                i += 1
                            End If
                        Loop While ParName <> ""

                        OutputParams.Clear()
                        i = 0
                        Do
                            Dim ExlPar As New ExcelParameter

                            ParName = mysheetOut.Cells(5 + i, 7).Value
                            If ParName <> "" Then
                                ExlPar.Name = ParName
                                ExlPar.Value = mysheetIn.Cells(4 + i, 7).Value
                                ExlPar.Unit = mysheetIn.Cells(4 + i, 8).Value
                                ExlPar.Annotation = mysheetIn.Cells(4 + i, 9).Value
                                OutputParams.Add(ExlPar.Name, ExlPar)

                                i += 1
                            End If
                        Loop While ParName <> ""

                    End If


                End If

            End If

                With Me.QTNodeTableItems

                    Dim valor As String

                    If Me.DeltaQ.HasValue Then
                        valor = Format(Conversor.ConverterDoSI(su.spmp_heatflow, Me.DeltaQ), nf)
                    Else
                        valor = DWSIM.App.GetLocalString("NC")
                    End If
                    .Item(0).Value = valor
                    .Item(0).Unit = su.spmp_heatflow

                End With

        End Sub

        Public Overrides Sub QTFillNodeItems()

            With QTNodeTableItems

                .Clear()
                .Add(0, New DWSIM.Outros.NodeItem("Q", "", "", 0, 0, ""))

            End With
        End Sub

        Public Overrides Sub PopulatePropertyGrid(ByVal pgrid As PropertyGridEx.PropertyGridEx, ByVal su As SistemasDeUnidades.Unidades)
            Dim Conversor As New DWSIM.SistemasDeUnidades.Conversor

            With pgrid

                .PropertySort = PropertySort.Categorized
                .ShowCustomProperties = True
                .Item.Clear()

                MyBase.PopulatePropertyGrid(pgrid, su)

                Dim ent1, ent2, ent3, ent4, saida1, saida2, saida3, saida4, energ As String
                If Me.GraphicObject.InputConnectors(0).IsAttached = True Then
                    ent1 = Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Tag
                Else
                    ent1 = ""
                End If
                If Me.GraphicObject.InputConnectors(1).IsAttached = True Then
                    ent2 = Me.GraphicObject.InputConnectors(1).AttachedConnector.AttachedFrom.Tag
                Else
                    ent2 = ""
                End If
                If Me.GraphicObject.InputConnectors(2).IsAttached = True Then
                    ent3 = Me.GraphicObject.InputConnectors(2).AttachedConnector.AttachedFrom.Tag
                Else
                    ent3 = ""
                End If
                If Me.GraphicObject.InputConnectors(3).IsAttached = True Then
                    ent4 = Me.GraphicObject.InputConnectors(3).AttachedConnector.AttachedFrom.Tag
                Else
                    ent4 = ""
                End If


                If Me.GraphicObject.OutputConnectors(0).IsAttached = True Then
                    saida1 = Me.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Tag
                Else
                    saida1 = ""
                End If
                If Me.GraphicObject.OutputConnectors(1).IsAttached = True Then
                    saida2 = Me.GraphicObject.OutputConnectors(1).AttachedConnector.AttachedTo.Tag
                Else
                    saida2 = ""
                End If
                If Me.GraphicObject.OutputConnectors(2).IsAttached = True Then
                    saida3 = Me.GraphicObject.OutputConnectors(2).AttachedConnector.AttachedTo.Tag
                Else
                    saida3 = ""
                End If
                If Me.GraphicObject.OutputConnectors(3).IsAttached = True Then
                    saida4 = Me.GraphicObject.OutputConnectors(3).AttachedConnector.AttachedTo.Tag
                Else
                    saida4 = ""
                End If

                If Me.GraphicObject.InputConnectors(4).IsAttached = True Then
                    energ = Me.GraphicObject.InputConnectors(4).AttachedConnector.AttachedFrom.Tag
                Else
                    energ = ""
                End If

                '==== Streams (1) =======================
                '==== Input streams ===
                .Item.Add(DWSIM.App.GetLocalString("Correntedeentrada1"), ent1, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .CustomEditor = New DWSIM.Editors.Streams.UIInputMSSelector
                End With
                .Item.Add(DWSIM.App.GetLocalString("Correntedeentrada2"), ent2, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .CustomEditor = New DWSIM.Editors.Streams.UIInputMSSelector
                End With
                .Item.Add(DWSIM.App.GetLocalString("Correntedeentrada3"), ent3, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .CustomEditor = New DWSIM.Editors.Streams.UIInputMSSelector
                End With
                .Item.Add(DWSIM.App.GetLocalString("Correntedeentrada4"), ent4, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .CustomEditor = New DWSIM.Editors.Streams.UIInputMSSelector
                End With

                '==== Output streams ===
                .Item.Add(DWSIM.App.GetLocalString("Correntedesaida1"), saida1, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .CustomEditor = New DWSIM.Editors.Streams.UIOutputMSSelector
                End With
                .Item.Add(DWSIM.App.GetLocalString("Correntedesaida2"), saida2, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .CustomEditor = New DWSIM.Editors.Streams.UIOutputMSSelector
                End With
                .Item.Add(DWSIM.App.GetLocalString("Correntedesaida3"), saida3, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .CustomEditor = New DWSIM.Editors.Streams.UIOutputMSSelector
                End With
                .Item.Add(DWSIM.App.GetLocalString("Correntedesaida4"), saida4, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .CustomEditor = New DWSIM.Editors.Streams.UIOutputMSSelector
                End With

                '==== Energy stream ===
                .Item.Add(DWSIM.App.GetLocalString("Correntedeenergia"), energ, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .CustomEditor = New DWSIM.Editors.Streams.UIInputESSelector
                End With


                '==== Input Parameters (2) =======================
                '======== Input parameters from Excel ============
                For Each Prop As ExcelParameter In InputParams.Values
                    .Item.Add(FT(Prop.Name, Prop.Unit), Prop.Value, False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), Prop.Annotation, True)
                Next

                .Item.Add(DWSIM.App.GetLocalString("ExcelUOEditor"), Me, "Filename", False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("ExcelFile"), True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .CustomEditor = New DWSIM.Editors.ExcelUO.UIExcelUOEditor
                End With

                '==== Results (3) =================================
                '======== Output parameters from Excel ============
                For Each Prop As ExcelParameter In OutputParams.Values
                    .Item.Add(FT(Prop.Name, Prop.Unit), Prop.Value, True, DWSIM.App.GetLocalString("Resultados3"), Prop.Annotation, True)
                Next

                '======== heat due to enthalpy balance ============
                Dim valor = Format(Conversor.ConverterDoSI(su.spmp_heatflow, Me.DeltaQ.GetValueOrDefault), FlowSheet.Options.NumberFormat)
                .Item.Add(FT(DWSIM.App.GetLocalString("CalorFornecido"), su.spmp_heatflow), valor, True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("Quantidadedecalortro"), True)
                .Item(.Item.Count - 1).Tag = New Object() {FlowSheet.Options.NumberFormat, su.spmp_heatflow, "E"}

                '========== Error message =========================
                If Me.GraphicObject.Calculated = False Then
                    .Item.Add(DWSIM.App.GetLocalString("Mensagemdeerro"), Me, "ErrorMessage", True, DWSIM.App.GetLocalString("Miscelnea4"), DWSIM.App.GetLocalString("Mensagemretornadaqua"), True)
                    With .Item(.Item.Count - 1)
                        .DefaultType = GetType(System.String)
                    End With
                End If

            End With

        End Sub

        Public Overrides Function GetPropertyValue(ByVal prop As String, Optional ByVal su As SistemasDeUnidades.Unidades = Nothing) As Object

            If su Is Nothing Then su = New DWSIM.SistemasDeUnidades.UnidadesSI
            Dim cv As New DWSIM.SistemasDeUnidades.Conversor
            Dim value As Double = 0

            Dim propType As String = prop.Split("_")(0)
            Dim propID As String = prop.Split("_")(1)

            Select Case propType
                Case "Calc"
                    value = Conversor.ConverterDoSI(su.spmp_heatflow, DeltaQ.GetValueOrDefault)
                Case "In"
                    value = InputParams(propID).Value
                Case "Out"
                    value = OutputParams(propID).Value
            End Select

            Return value
        End Function

        Public Overloads Overrides Function GetProperties(ByVal proptype As SimulationObjects_BaseClass.PropertyType) As String()

            Dim proplist As New ArrayList

            Select Case proptype
                Case PropertyType.RO
                    proplist.Add("Calc_dQ")
                    For Each P As ExcelParameter In OutputParams.Values
                        proplist.Add("Out_" + P.Name)
                    Next
                Case PropertyType.RW
                    proplist.Add("Calc_dQ")
                    For Each P As ExcelParameter In OutputParams.Values
                        proplist.Add("Out_" + P.Name)
                    Next
                    For Each P As ExcelParameter In InputParams.Values
                        proplist.Add("In_" + P.Name)
                    Next
                Case PropertyType.WR
                    For Each P As ExcelParameter In InputParams.Values
                        proplist.Add("In_" + P.Name)
                    Next
                Case PropertyType.ALL
                    proplist.Add("Calc_dQ")
                    For Each P As ExcelParameter In InputParams.Values
                        proplist.Add("In_" + P.Name)
                    Next
                    For Each P As ExcelParameter In OutputParams.Values
                        proplist.Add("Out_" + P.Name)
                    Next
            End Select

            Return proplist.ToArray(GetType(System.String))
            proplist = Nothing
        End Function

        Public Overrides Function SetPropertyValue(ByVal prop As String, ByVal propval As Object, Optional ByVal su As DWSIM.SistemasDeUnidades.Unidades = Nothing) As Object
            If su Is Nothing Then su = New DWSIM.SistemasDeUnidades.UnidadesSI
            Dim cv As New DWSIM.SistemasDeUnidades.Conversor

            Dim propType As String = prop.Split("_")(0)
            Dim propID As String = prop.Split("_")(1)

            Select Case propType
                Case "Calc"
                    DeltaQ = Conversor.ConverterParaSI(su.spmp_heatflow, propval)
                Case "In"
                    InputParams(propID).Value = propval
                Case "Out"
                    OutputParams(propID).Value = propval
            End Select

            Return 1
        End Function

        Public Overrides Function GetPropertyUnit(ByVal prop As String, Optional ByVal su As SistemasDeUnidades.Unidades = Nothing) As Object
            If su Is Nothing Then su = New DWSIM.SistemasDeUnidades.UnidadesSI
            Dim cv As New DWSIM.SistemasDeUnidades.Conversor
            Dim value As String = ""

            Dim propType As String = prop.Split("_")(0)
            Dim propID As String = prop.Split("_")(1)

            Select Case propType
                Case "Calc"
                    value = su.spmp_heatflow
                Case "In"
                    value = InputParams(propID).Unit
                Case "Out"
                    value = OutputParams(propID).Unit
            End Select

            Return value
        End Function
    End Class

End Namespace
