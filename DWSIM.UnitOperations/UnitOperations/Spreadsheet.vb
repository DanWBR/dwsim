'    Excel/Spreadsheet Unit Calculation Routines 
'    Copyright 2014 Gregor Reichert, 2015 Daniel Wagner
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


Imports Excel = NetOffice.ExcelApi
Imports NetOffice.ExcelApi.Enums
Imports GS = GemBox.Spreadsheet
Imports DWSIM.Thermodynamics
Imports DWSIM.Thermodynamics.Streams
Imports DWSIM.SharedClasses
Imports System.Windows.Forms
Imports DWSIM.UnitOperations.UnitOperations.Auxiliary
Imports DWSIM.Interfaces.Enums
Imports System.IO

Namespace UnitOperations.Auxiliary

    <System.Serializable()> Public Class ExcelParameter
        Public Name As String = ""
        Public Value As Double = 0.0#
        Public Unit As String = ""
        Public Annotation As String = ""
    End Class

End Namespace

Namespace UnitOperations

    <System.Serializable()> Public Class ExcelUO

        Inherits UnitOpBaseClass
        Public Overrides Property ObjectClass As SimulationObjectClass = SimulationObjectClass.UserModels

        Public Overrides ReadOnly Property HasPropertiesForDynamicMode As Boolean = False

        Public Overrides ReadOnly Property SupportsDynamicMode As Boolean = True

        <NonSerialized> <Xml.Serialization.XmlIgnore> Public f As EditingForm_SpreadsheetUO

        Protected m_DQ As Nullable(Of Double)
        Protected m_FileName As String = ""
        Protected m_InputParams As New Dictionary(Of String, ExcelParameter)
        Protected m_OutputParams As New Dictionary(Of String, ExcelParameter)
        Public ParamsLoaded As Boolean = False

        Public Property FileIsEmbedded As Boolean = False
        Public Property EmbeddedFileName As String = ""

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

        Public Sub New(ByVal name As String, ByVal description As String)

            MyBase.CreateNew()

            Me.ComponentName = name
            Me.ComponentDescription = description

        End Sub

        Public Overrides Function CloneXML() As Object
            Dim obj As ICustomXMLSerialization = New ExcelUO()
            obj.LoadData(Me.SaveData)
            Return obj
        End Function

        Public Overrides Function CloneJSON() As Object
            Return Newtonsoft.Json.JsonConvert.DeserializeObject(Of ExcelUO)(Newtonsoft.Json.JsonConvert.SerializeObject(Me))
        End Function
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

        Public Overrides Sub RunDynamicModel()

            Calculate()

        End Sub

        Public Overrides Sub Calculate(Optional ByVal args As Object = Nothing)

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "Calculate", If(GraphicObject IsNot Nothing, GraphicObject.Tag, "Temporary Object") & " (" & GetDisplayName() & ")", GetDisplayName() & " Calculation Routine", True)

            IObj?.SetCurrent()

            Dim k, ci, co As Integer

            Dim su = FlowSheet.FlowsheetOptions.SelectedUnitSystem

            Dim excelType As Type = Nothing

            If Not FileIsEmbedded Then

                If Not File.Exists(Filename) Then
                    'try to find the file in the current directory.
                    Dim fname = Path.GetFileName(Filename)
                    Dim newpath = Path.Combine(Path.GetDirectoryName(FlowSheet.FilePath), fname)
                    If File.Exists(newpath) Then
                        Filename = Path.GetFullPath(newpath)
                    End If
                End If

                If Not File.Exists(Filename) Then
                    Throw New Exception("Definition file '" & Filename & "' :" & FlowSheet.GetTranslatedString("Oarquivonoexisteoufo"))
                End If

            Else

                If Not FlowSheet.FileDatabaseProvider.CheckIfExists(EmbeddedFileName) Then
                    Throw New Exception("Definition file '" & EmbeddedFileName & "' :" & FlowSheet.GetTranslatedString("Oarquivonoexisteoufo"))
                End If

            End If

            If Not Calculator.IsRunningOnMono Then excelType = Type.GetTypeFromProgID("Excel.Application")

            If Not Calculator.IsRunningOnMono And Not excelType Is Nothing Then

                Dim excelProxy As Object = Activator.CreateInstance(excelType)

                Using xcl As New Excel.Application(Nothing, excelProxy)

                    For Each CurrAddin As Excel.AddIn In xcl.AddIns
                        If CurrAddin.Installed Then
                            CurrAddin.Installed = False
                            CurrAddin.Installed = True
                        End If
                    Next

                    Dim mybook As Excel.Workbook
                    Dim AppPath = Application.StartupPath

                    Dim tmpfile As String = ""

                    If FileIsEmbedded Then
                        tmpfile = Path.ChangeExtension(SharedClasses.Utility.GetTempFileName(), Path.GetExtension(EmbeddedFileName))
                        FlowSheet.FileDatabaseProvider.ExportFile(EmbeddedFileName, tmpfile)
                        'Load Excel definition file
                        mybook = xcl.Workbooks.Open(tmpfile)
                    Else
                        'Load Excel definition file
                        mybook = xcl.Workbooks.Open(Filename)
                    End If

                    'xcl.Visible = True 'uncomment for debugging
                    xcl.Calculation = XlCalculation.xlCalculationManual

                    Dim mysheetIn As Excel.Worksheet = mybook.Sheets("Input")
                    Dim mysheetOut As Excel.Worksheet = mybook.Sheets("Output")
                    '=====================================================================================================

                    'check if at least one input and output connection is available
                    For k = 0 To 3
                        If GraphicObject.InputConnectors(k).IsAttached Then ci += 1
                        If GraphicObject.OutputConnectors(k).IsAttached Then co += 1
                    Next
                    If ci = 0 Or co = 0 Then
                        mybook.Close(saveChanges:=False)
                        xcl.Quit()
                        'xcl.Dispose()
                        'CalculateFlowsheet(FlowSheet, objargs, Nothing)
                        Throw New Exception(FlowSheet.GetTranslatedString("Verifiqueasconexesdo"))
                    End If

                    Dim Ti, Pi, Hi, Wi, T2, P2, H2, Hin, Hout, Win, Wout, MassBal As Double

                    Dim es As Streams.EnergyStream = Nothing

                    If GetInletEnergyStream(4) IsNot Nothing Then
                        es = FlowSheet.SimulationObjects(Me.GraphicObject.InputConnectors(4).AttachedConnector.AttachedFrom.Name)
                    End If

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
                    Dim S As MaterialStream
                    For k = 0 To 3
                        If Me.GraphicObject.InputConnectors(k).IsAttached Then
                            S = FlowSheet.SimulationObjects(Me.GraphicObject.InputConnectors(k).AttachedConnector.AttachedFrom.Name)
                            Me.PropertyPackage.CurrentMaterialStream = S
                            Ti = S.Phases(0).Properties.temperature.GetValueOrDefault
                            Pi = S.Phases(0).Properties.pressure.GetValueOrDefault
                            Hi = S.Phases(0).Properties.enthalpy.GetValueOrDefault
                            Wi = S.Phases(0).Properties.massflow.GetValueOrDefault
                            Hin += Hi * Wi
                            Win += Wi

                            '======= transfer data to Excel ===========================================================
                            mysheetIn.Cells(6, 2 + k).Value = Ti
                            mysheetIn.Cells(7, 2 + k).Value = Pi
                            mysheetIn.Cells(8, 2 + k).Value = Hi

                            Dim dy As Integer = 0
                            For Each comp As BaseClasses.Compound In S.Phases(0).Compounds.Values
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
                            For Each comp As BaseClasses.Compound In Me.PropertyPackage.CurrentMaterialStream.Phases(0).Compounds.Values
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
                            Me.PropertyPackage.CurrentMaterialStream = FlowSheet.SimulationObjects(Me.GraphicObject.OutputConnectors(k).AttachedConnector.AttachedTo.Name)

                            T2 = mysheetOut.Cells(6, 2 + k).Value
                            P2 = mysheetOut.Cells(7, 2 + k).Value

                            'Atribuir valores a corrente de materia conectada a jusante
                            With Me.PropertyPackage.CurrentMaterialStream
                                .Phases(0).Properties.temperature = T2
                                .Phases(0).Properties.pressure = P2

                                Dim comp As BaseClasses.Compound
                                i = 0
                                SMole = 0
                                SMass = 0
                                Vmol.Clear()
                                For Each comp In .Phases(0).Compounds.Values
                                    v = mysheetOut.Cells(12 + i, 2 + k).Value
                                    Vmol.Add(comp.Name, v)
                                    SMole += Vmol(comp.Name)
                                    SMass += Vmol(comp.Name) * comp.ConstantProperties.Molar_Weight / 1000
                                    i += 1
                                Next
                                For Each comp In .Phases(0).Compounds.Values
                                    comp.MoleFraction = Vmol(comp.Name) / SMole
                                    comp.MassFraction = Vmol(comp.Name) * comp.ConstantProperties.Molar_Weight / SMass / 1000
                                Next
                                .Phases(0).Properties.massflow = SMass

                                Try
                                    Dim tmp = Me.PropertyPackage.CalculateEquilibrium2(FlashCalculationType.PressureTemperature, P2, T2, 0)
                                    H2 = tmp.CalculatedEnthalpy
                                    .Phases(0).Properties.enthalpy = H2
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

                    Dim hfin, hfout As Double

                    k = 0
                    For Each ic In GraphicObject.InputConnectors
                        If ic.IsAttached And ic.Type = GraphicObjects.ConType.ConIn Then
                            hfin += GetInletMaterialStream(k).GetOverallHeatOfFormation()
                        End If
                        k += 1
                    Next

                    k = 0
                    For Each oc In GraphicObject.OutputConnectors
                        If oc.IsAttached Then
                            hfout += GetOutletMaterialStream(k).GetOverallHeatOfFormation()
                        End If
                        k += 1
                    Next

                    Me.DeltaQ = Hout - Hin + hfout - hfin

                    'energy stream - update energy flow value (kW)
                    If es IsNot Nothing Then
                        With es
                            .EnergyFlow = Me.DeltaQ.GetValueOrDefault
                            .GraphicObject.Calculated = True
                        End With
                    End If

                    '======== read input/output parameters from Excel table =========================================
                    k = 0
                    InputParams.Clear()
                    Do
                        Dim ExlPar As New ExcelParameter

                        ParName = mysheetIn.Cells(5 + k, 7).Value
                        If ParName <> "" Then
                            ExlPar.Name = ParName
                            Try
                                ExlPar.Value = mysheetIn.Cells(5 + k, 8).Value
                            Catch ex As Exception
                                ExlPar.Value = Nothing
                            End Try

                            ExlPar.Unit = mysheetIn.Cells(5 + k, 9).Value
                            ExlPar.Annotation = mysheetIn.Cells(5 + k, 10).Value
                            InputParams.Add(ExlPar.Name, ExlPar)

                            k += 1
                        End If
                    Loop While ParName <> ""

                    k = 0
                    OutputParams.Clear()
                    Do
                        Dim ExlPar As New ExcelParameter

                        ParName = mysheetOut.Cells(5 + k, 7).Value
                        If ParName <> "" Then
                            ExlPar.Name = ParName
                            Try
                                ExlPar.Value = mysheetOut.Cells(5 + k, 8).Value
                            Catch ex As Exception
                                ExlPar.Value = Nothing
                            End Try
                            ExlPar.Unit = mysheetOut.Cells(5 + k, 9).Value
                            ExlPar.Annotation = mysheetOut.Cells(5 + k, 10).Value
                            OutputParams.Add(ExlPar.Name, ExlPar)

                            k += 1
                        End If
                    Loop While ParName <> ""
                    ParamsLoaded = True

                    mybook.Close(saveChanges:=True)
                    xcl.Quit()

                    MassBal = 100 * (Wout - Win) / (Win)
                    If Math.Abs(MassBal) > 0.001 Then
                        FlowSheet.ShowMessage(Me.GraphicObject.Tag & ": " & "Mass balance error: " & MassBal & "%", IFlowsheet.MessageType.GeneralError)
                    End If

                    If File.Exists(tmpfile) Then
                        Try
                            File.Delete(tmpfile)
                        Catch ex As Exception
                        End Try
                    End If

                End Using

            Else

                'use GemBox to read and write data

                GS.SpreadsheetInfo.SetLicense("FREE-LIMITED-KEY")

                Dim xcl As GS.ExcelFile = Nothing

                Dim AppPath = Application.StartupPath

                Dim tmpfile As String = ""

                If FileIsEmbedded Then
                    tmpfile = Path.ChangeExtension(SharedClasses.Utility.GetTempFileName(), Path.GetExtension(EmbeddedFileName))
                    FlowSheet.FileDatabaseProvider.ExportFile(EmbeddedFileName, tmpfile)
                    'Load Excel definition file
                    xcl = GS.ExcelFile.Load(tmpfile)
                Else
                    'Load Excel definition file
                    If My.Computer.FileSystem.FileExists(Filename) Then
                        xcl = GS.ExcelFile.Load(Filename)
                    Else
                        Throw New Exception("Definition file '" & Filename & "' :" & FlowSheet.GetTranslatedString("Oarquivonoexisteoufo"))
                    End If
                End If

                Dim mysheetIn As GS.ExcelWorksheet = xcl.Worksheets("Input")
                Dim mysheetOut As GS.ExcelWorksheet = xcl.Worksheets("Output")
                '=====================================================================================================

                If Not Me.GraphicObject.InputConnectors(4).IsAttached Then 'Check if Energy stream existing
                    Throw New Exception(FlowSheet.GetTranslatedString("NohcorrentedeEnergyFlow1"))
                End If

                'check if at least one input and output connection is available
                For k = 0 To 3
                    If GraphicObject.InputConnectors(k).IsAttached Then ci += 1
                    If GraphicObject.OutputConnectors(k).IsAttached Then co += 1
                Next
                If ci = 0 Or co = 0 Then
                    Throw New Exception(FlowSheet.GetTranslatedString("Verifiqueasconexesdo"))
                End If

                Dim Ti, Pi, Hi, Wi, T2, P2, H2, Hin, Hout, Win, Wout, MassBal As Double
                Dim es As Streams.EnergyStream = FlowSheet.SimulationObjects(Me.GraphicObject.InputConnectors(4).AttachedConnector.AttachedFrom.Name)
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
                Dim S As MaterialStream
                For k = 0 To 3
                    If Me.GraphicObject.InputConnectors(k).IsAttached Then
                        S = FlowSheet.SimulationObjects(Me.GraphicObject.InputConnectors(k).AttachedConnector.AttachedFrom.Name)
                        Me.PropertyPackage.CurrentMaterialStream = S
                        Ti = S.Phases(0).Properties.temperature.GetValueOrDefault
                        Pi = S.Phases(0).Properties.pressure.GetValueOrDefault
                        Hi = S.Phases(0).Properties.enthalpy.GetValueOrDefault
                        Wi = S.Phases(0).Properties.massflow.GetValueOrDefault
                        Hin += Hi * Wi
                        Win += Wi

                        '======= transfer data to Excel ===========================================================
                        mysheetIn.Cells(5, 1 + k).Value = Ti
                        mysheetIn.Cells(6, 1 + k).Value = Pi
                        mysheetIn.Cells(7, 1 + k).Value = Hi

                        Dim dy As Integer = 0
                        For Each comp As BaseClasses.Compound In S.Phases(0).Compounds.Values
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
                        For Each comp As BaseClasses.Compound In Me.PropertyPackage.CurrentMaterialStream.Phases(0).Compounds.Values
                            mysheetIn.Cells(11 + dy, 0).Value = comp.ConstantProperties.Name
                            mysheetOut.Cells(11 + dy, 0).Value = comp.ConstantProperties.Name
                            mysheetIn.Cells(11 + dy, 1 + k).Value = ""
                            mysheetIn.Cells(11 + dy, 2 + k).Value = ""
                            dy += 1
                        Next
                    End If
                Next

                'open spreadsheet to be calculated manually by the user.

                If FileIsEmbedded Then
                    xcl.Save(tmpfile)
                Else
                    xcl.Save(Filename)
                End If

                If Calculator.IsRunningOnMono Then
                    If GlobalSettings.Settings.RunningPlatform = Settings.Platform.Linux Then
                        Dim p As New Process()
                        With p
                            .StartInfo.FileName = "xdg-open"
                            .StartInfo.Arguments = Filename
                            .StartInfo.UseShellExecute = False
                            .Start()
                            MessageBox.Show("Click 'OK' once the spreadsheet formula updating process is finished.")
                        End With
                    Else 'macOS
                        Dim p As New Process()
                        With p
                            .StartInfo.FileName = "open"
                            .StartInfo.Arguments = Filename
                            .StartInfo.UseShellExecute = False
                            .Start()
                            MessageBox.Show("Click 'OK' once the spreadsheet formula updating process is finished.")
                        End With
                    End If
                Else
                    Process.Start(Filename)
                    MessageBox.Show("Click 'OK' once the spreadsheet formula updating process is finished.")
                End If

                If FileIsEmbedded Then
                    'Load Excel definition file
                    xcl = GS.ExcelFile.Load(tmpfile)
                Else
                    'Load Excel definition file
                    xcl = GS.ExcelFile.Load(Filename)
                End If

                mysheetIn = xcl.Worksheets("Input")
                mysheetOut = xcl.Worksheets("Output")

                '======= read results from sheet =============================================================
                Dim Vmol As New Dictionary(Of String, Double)
                Dim v As Double
                Dim SMass, SMole As Double

                For k = 0 To 3 'run through all streams to execute TP-flash
                    If Me.GraphicObject.OutputConnectors(k).IsAttached Then
                        Me.PropertyPackage.CurrentMaterialStream = FlowSheet.SimulationObjects(Me.GraphicObject.OutputConnectors(k).AttachedConnector.AttachedTo.Name)

                        T2 = mysheetOut.Cells(5, 1 + k).Value
                        P2 = mysheetOut.Cells(6, 1 + k).Value

                        'Atribuir valores a corrente de materia conectada a jusante
                        With Me.PropertyPackage.CurrentMaterialStream
                            .Phases(0).Properties.temperature = T2
                            .Phases(0).Properties.pressure = P2

                            Dim comp As BaseClasses.Compound
                            i = 0
                            SMole = 0
                            SMass = 0
                            Vmol.Clear()
                            For Each comp In .Phases(0).Compounds.Values
                                v = mysheetOut.Cells(11 + i, 1 + k).Value
                                Vmol.Add(comp.Name, v)
                                SMole += Vmol(comp.Name)
                                SMass += Vmol(comp.Name) * comp.ConstantProperties.Molar_Weight / 1000
                                i += 1
                            Next
                            For Each comp In .Phases(0).Compounds.Values
                                comp.MoleFraction = Vmol(comp.Name) / SMole
                                comp.MassFraction = Vmol(comp.Name) * comp.ConstantProperties.Molar_Weight / SMass / 1000
                            Next
                            .Phases(0).Properties.massflow = SMass
                            .DefinedFlow = FlowSpec.Mass

                            Try
                                IObj?.SetCurrent()
                                Dim tmp = Me.PropertyPackage.CalculateEquilibrium2(FlashCalculationType.PressureTemperature, P2, T2, 0)
                                H2 = tmp.CalculatedEnthalpy
                                .Phases(0).Properties.enthalpy = H2
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

                'energy stream - update energy flow value (kW)
                If es IsNot Nothing Then
                    With es
                        .EnergyFlow = Me.DeltaQ.GetValueOrDefault
                        .GraphicObject.Calculated = True
                    End With
                End If

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
                    FlowSheet.ShowMessage(Me.GraphicObject.Tag & ": " & "Mass balance error: " & MassBal & "%", IFlowsheet.MessageType.GeneralError)
                End If

                If File.Exists(tmpfile) Then
                    Try
                        File.Delete(tmpfile)
                    Catch ex As Exception
                    End Try
                End If

            End If

            IObj?.Close()

        End Sub

        Public Overrides Sub DeCalculate()

            Dim k As Integer

            For k = 0 To 3
                If Me.GraphicObject.OutputConnectors(k).IsAttached Then

                    'Zerar valores da corrente de materia conectada a jusante
                    With Me.GetOutletMaterialStream(k)
                        .Phases(0).Properties.temperature = Nothing
                        .Phases(0).Properties.pressure = Nothing
                        .Phases(0).Properties.enthalpy = Nothing
                        .Phases(0).Properties.molarfraction = 1
                        .Phases(0).Properties.massfraction = 1
                        Dim comp As BaseClasses.Compound
                        Dim i As Integer = 0
                        For Each comp In .Phases(0).Compounds.Values
                            comp.MoleFraction = 0
                            comp.MassFraction = 0
                            i += 1
                        Next
                        .Phases(0).Properties.massflow = Nothing
                        .Phases(0).Properties.molarflow = Nothing
                        .GraphicObject.Calculated = False
                    End With

                End If
            Next

            'energy stream - update energy flow value (kW)
            If Me.GraphicObject.EnergyConnector.IsAttached Then
                With Me.GetEnergyStream
                    .EnergyFlow = Nothing
                    .GraphicObject.Calculated = False
                End With
            End If

        End Sub

        Public Sub ReadExcelParams()

            'read input and output parameters from associated Excel table 

            If Not ParamsLoaded Then

                If FileIsEmbedded Then

                    If Not FlowSheet.FileDatabaseProvider.CheckIfExists(EmbeddedFileName) Then Exit Sub

                Else

                    If Not File.Exists(Filename) Then Exit Sub

                End If

                Dim excelType As Type = Nothing

                If Not Calculator.IsRunningOnMono Then excelType = Type.GetTypeFromProgID("Excel.Application")

                If Not Calculator.IsRunningOnMono And Not excelType Is Nothing Then

                    Dim excelProxy As Object = Activator.CreateInstance(excelType)

                    Using xcl As New Excel.Application(Nothing, excelProxy)

                        For Each CurrAddin As Excel.AddIn In xcl.AddIns
                            If CurrAddin.Installed Then
                                CurrAddin.Installed = False
                                CurrAddin.Installed = True
                            End If
                        Next

                        Dim mybook As Excel.Workbook
                        Dim AppPath = Application.StartupPath
                        Dim ParName As String
                        Dim i As Integer

                        Dim tmpfile As String = ""

                        If FileIsEmbedded Then

                            tmpfile = Path.ChangeExtension(SharedClasses.Utility.GetTempFileName(), Path.GetExtension(EmbeddedFileName))
                            FlowSheet.FileDatabaseProvider.ExportFile(EmbeddedFileName, tmpfile)
                            'Load Excel definition file
                            mybook = xcl.Workbooks.Open(tmpfile, True, True)

                        Else

                            'Load Excel definition file
                            mybook = xcl.Workbooks.Open(Filename, True, True)

                        End If

                        Dim mysheetIn As Excel.Worksheet = mybook.Sheets("Input")
                        Dim mysheetOut As Excel.Worksheet = mybook.Sheets("Output")

                        'xcl.Visible = True 'uncomment for debugging

                        InputParams.Clear()
                        i = 0
                        Do
                            Dim ExlPar As New ExcelParameter

                            ParName = mysheetIn.Cells(5 + i, 7).Value
                            If ParName <> "" Then
                                ExlPar.Name = ParName
                                Try
                                    ExlPar.Value = mysheetIn.Cells(5 + i, 8).Value
                                Catch ex As Exception
                                    ExlPar.Value = Nothing
                                End Try

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
                                Try
                                    ExlPar.Value = mysheetOut.Cells(5 + i, 8).Value
                                Catch ex As Exception
                                    ExlPar.Value = Nothing
                                End Try

                                ExlPar.Unit = mysheetOut.Cells(5 + i, 9).Value
                                ExlPar.Annotation = mysheetOut.Cells(5 + i, 10).Value
                                OutputParams.Add(ExlPar.Name, ExlPar)

                                i += 1
                            End If
                        Loop While ParName <> ""

                        mybook.Close(saveChanges:=False)

                        xcl.Quit()

                        ParamsLoaded = True

                        If File.Exists(tmpfile) Then
                            Try
                                File.Delete(tmpfile)
                            Catch ex As Exception
                            End Try
                        End If

                    End Using

                Else

                    'use GemBox to read and write data

                    GS.SpreadsheetInfo.SetLicense("FREE-LIMITED-KEY")

                    Dim xcl As GS.ExcelFile = Nothing

                    Dim AppPath = Application.StartupPath
                    Dim ParName As String
                    Dim i As Integer

                    Dim tmpfile As String = ""

                    If FileIsEmbedded Then
                        tmpfile = Path.ChangeExtension(SharedClasses.Utility.GetTempFileName(), Path.GetExtension(EmbeddedFileName))
                        FlowSheet.FileDatabaseProvider.ExportFile(EmbeddedFileName, tmpfile)
                        'Load Excel definition file
                        xcl = GS.ExcelFile.Load(tmpfile)
                    Else
                        'Load Excel definition file
                        xcl = GS.ExcelFile.Load(Filename)
                    End If

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

                    ParamsLoaded = True

                    If File.Exists(tmpfile) Then
                        Try
                            File.Delete(tmpfile)
                        Catch ex As Exception
                        End Try
                    End If

                End If

            End If

        End Sub
        Public Overrides Function GetPropertyValue(ByVal prop As String, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As Object

            Dim val0 As Object = MyBase.GetPropertyValue(prop, su)

            If Not val0 Is Nothing Then
                Return val0
            Else
                If su Is Nothing Then su = New SystemsOfUnits.SI
                Dim cv As New SystemsOfUnits.Converter
                Dim value As Double = 0

                Dim propType As String = prop.Split("_")(0)
                Dim propID As String = prop.Split("_")(1)

                Select Case propType
                    Case "Calc"
                        value = SystemsOfUnits.Converter.ConvertFromSI(su.heatflow, DeltaQ.GetValueOrDefault)
                    Case "In"
                        If InputParams.ContainsKey(propID) Then value = InputParams(propID).Value
                    Case "Out"
                        If OutputParams.ContainsKey(propID) Then value = OutputParams(propID).Value
                End Select

                Return value
            End If

        End Function

        Public Overloads Overrides Function GetProperties(ByVal proptype As Interfaces.Enums.PropertyType) As String()

            Dim proplist As New ArrayList
            Dim basecol = MyBase.GetProperties(proptype)
            If basecol.Length > 0 Then proplist.AddRange(basecol)

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

        Public Overrides Function SetPropertyValue(ByVal prop As String, ByVal propval As Object, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As Boolean

            If MyBase.SetPropertyValue(prop, propval, su) Then Return True

            If su Is Nothing Then su = New SystemsOfUnits.SI
            Dim cv As New SystemsOfUnits.Converter

            Dim propType As String = prop.Split("_")(0)
            Dim propID As String = prop.Split("_")(1)

            Select Case propType
                Case "Calc"
                    DeltaQ = SystemsOfUnits.Converter.ConvertToSI(su.heatflow, propval)
                Case "In"
                    If InputParams.ContainsKey(propID) Then InputParams(propID).Value = propval
                Case "Out"
                    If OutputParams.ContainsKey(propID) Then OutputParams(propID).Value = propval
            End Select

            Return 1
        End Function

        Public Overrides Function GetPropertyUnit(ByVal prop As String, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As String
            Dim u0 As String = MyBase.GetPropertyUnit(prop, su)

            If u0 <> "NF" Then
                Return u0
            Else
                If su Is Nothing Then su = New SystemsOfUnits.SI
                Dim cv As New SystemsOfUnits.Converter
                Dim value As String = ""

                Dim propType As String = prop.Split("_")(0)
                Dim propID As String = prop.Split("_")(1)

                Select Case propType
                    Case "Calc"
                        value = su.heatflow
                    Case "In"
                        Return "" 'If InputParams.ContainsKey(propID) Then value = InputParams(propID).Unit
                    Case "Out"
                        Return "" 'If OutputParams.ContainsKey(propID) Then value = OutputParams(propID).Unit
                End Select

                Return value
            End If
        End Function

        Public Overrides Sub DisplayEditForm()

            If f Is Nothing Then
                f = New EditingForm_SpreadsheetUO With {.SimObject = Me}
                f.ShowHint = GlobalSettings.Settings.DefaultEditFormLocation
                f.Tag = "ObjectEditor"
                Me.FlowSheet.DisplayForm(f)
            Else
                If f.IsDisposed Then
                    f = New EditingForm_SpreadsheetUO With {.SimObject = Me}
                    f.ShowHint = GlobalSettings.Settings.DefaultEditFormLocation
                    f.Tag = "ObjectEditor"
                    Me.FlowSheet.DisplayForm(f)
                Else
                    f.Activate()
                End If
            End If

        End Sub

        Public Overrides Sub UpdateEditForm()
            If f IsNot Nothing Then
                If Not f.IsDisposed Then
                    f.UIThread(Sub() f.UpdateInfo())
                End If
            End If
        End Sub

        Public Overrides Function GetIconBitmap() As Object
            Return My.Resources.table
        End Function

        Public Overrides Function GetDisplayDescription() As String
            Return ResMan.GetLocalString("EXLUO_Desc")
        End Function

        Public Overrides Function GetDisplayName() As String
            Return ResMan.GetLocalString("EXLUO_Name")
        End Function

        Public Overrides Sub CloseEditForm()
            If f IsNot Nothing Then
                If Not f.IsDisposed Then
                    f.Close()
                    f = Nothing
                End If
            End If
        End Sub

        Public Overrides ReadOnly Property MobileCompatible As Boolean
            Get
                Return False
            End Get
        End Property

        Public Overrides Function GetReport(su As IUnitsOfMeasure, ci As Globalization.CultureInfo, numberformat As String) As String


            Dim str As New Text.StringBuilder

            Dim istr, ostr As MaterialStream
            istr = Me.GetInletMaterialStream(0)
            ostr = Me.GetOutletMaterialStream(0)

            istr.PropertyPackage.CurrentMaterialStream = istr

            str.AppendLine("Spreadsheet Block: " & Me.GraphicObject.Tag)
            str.AppendLine("Property Package: " & Me.PropertyPackage.ComponentName)
            str.AppendLine()
            str.AppendLine("Calculation parameters")
            str.AppendLine()
            str.AppendLine("    Spreadsheet Path: " & Filename)
            str.AppendLine()
            str.AppendLine("Input Parameters")
            str.AppendLine()
            For Each par In InputParams.Values
                str.AppendLine("    " + par.Name + ": " + par.Value.ToString(numberformat) + " " + par.Unit)
            Next
            str.AppendLine()
            str.AppendLine("Output Parameters")
            str.AppendLine()
            For Each par In OutputParams.Values
                str.AppendLine("    " + par.Name + ": " + par.Value.ToString(numberformat) + " " + par.Unit)
            Next

            Return str.ToString

        End Function

    End Class

End Namespace
