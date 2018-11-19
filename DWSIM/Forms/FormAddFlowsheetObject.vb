Imports System.Reflection
Imports System.Linq

Public Class FormAddFlowsheetObject

    Public Flowsheet As FormFlowsheet

    Dim typelist As New List(Of Type)
    Dim objlist As New Dictionary(Of String, Interfaces.ISimulationObject)

    Private Sub FormAddFlowsheetObject_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        Dim calculatorassembly = My.Application.Info.LoadedAssemblies.Where(Function(x) x.FullName.Contains("DWSIM.Thermodynamics,")).FirstOrDefault
        Dim unitopassembly = My.Application.Info.LoadedAssemblies.Where(Function(x) x.FullName.Contains("DWSIM.UnitOperations")).FirstOrDefault
        Dim availableTypes As New List(Of Type)()

        availableTypes.AddRange(calculatorassembly.GetTypes().Where(Function(x) If(x.GetInterface("DWSIM.Interfaces.ISimulationObject") IsNot Nothing, True, False)))
        availableTypes.AddRange(unitopassembly.GetTypes().Where(Function(x) If(x.GetInterface("DWSIM.Interfaces.ISimulationObject") IsNot Nothing, True, False)))

        Dim add As Boolean = True

        For Each item In availableTypes
            If Not item.IsAbstract Then
                Dim obj = DirectCast(Activator.CreateInstance(item), Interfaces.ISimulationObject)
                If Not Flowsheet.MobileCompatibilityMode Then
                    add = True
                Else
                    add = obj.MobileCompatible
                End If
                If add Then
                    obj.SetFlowsheet(Flowsheet)
                    objlist.Add(obj.GetDisplayName, obj)
                    ListBox1.Items.Add(obj.GetDisplayName)
                End If
            End If
        Next

        ListBox1.SelectedIndex = 0

    End Sub

    Private Sub ListBox1_SelectedIndexChanged(sender As Object, e As EventArgs) Handles ListBox1.SelectedIndexChanged

        Dim obj = objlist(ListBox1.SelectedItem.ToString)

        Dim fi = FileVersionInfo.GetVersionInfo(obj.GetType.Assembly.Location)

        lblName.Text = obj.GetDisplayName
        lblVersion.Text = obj.GetVersion.ToString
        lblLibrary.Text = fi.OriginalFilename
        lblLibVersion.Text = obj.GetType.Assembly.GetName.Version.ToString

        txtDesc.Text = obj.GetDisplayDescription

        txtAbout.Text = fi.FileDescription + vbCrLf + fi.Comments + vbCrLf + fi.LegalCopyright

        PictureBox1.Image = obj.GetIconBitmap

    End Sub

    Private Sub btnCancel_Click(sender As Object, e As EventArgs) Handles btnCancel.Click
        Me.Close()
    End Sub

    Private Sub btnOK_Click(sender As Object, e As EventArgs) Handles btnOK.Click

        Dim tobj As ObjectType = ObjectType.Nenhum

        Select Case objlist(ListBox1.SelectedItem.ToString).GetType.Name
            Case "Adjust"
                tobj = ObjectType.OT_Adjust
            Case "Spec"
                tobj = ObjectType.OT_Spec
            Case "Recycle"
                tobj = ObjectType.OT_Recycle
            Case "EnergyRecycle"
                tobj = ObjectType.OT_EnergyRecycle
            Case "Mixer"
                tobj = ObjectType.NodeIn
            Case "Splitter"
                tobj = ObjectType.NodeOut
            Case "Pump"
                tobj = ObjectType.Pump
            Case "Tank"
                tobj = ObjectType.Tank
            Case "Vessel"
                tobj = ObjectType.Vessel
            Case "MaterialStream"
                tobj = ObjectType.MaterialStream
            Case "EnergyStream"
                tobj = ObjectType.EnergyStream
            Case "Compressor"
                tobj = ObjectType.Compressor
            Case "Expander"
                tobj = ObjectType.Expander
            Case "Cooler"
                tobj = ObjectType.Cooler
            Case "Heater"
                tobj = ObjectType.Heater
            Case "Pipe"
                tobj = ObjectType.Pipe
            Case "Valve"
                tobj = ObjectType.Valve
            Case "Reactor_Conversion"
                tobj = ObjectType.RCT_Conversion
            Case "Reactor_Equilibrium"
                tobj = ObjectType.RCT_Equilibrium
            Case "Reactor_Gibbs"
                tobj = ObjectType.RCT_Gibbs
            Case "Reactor_CSTR"
                tobj = ObjectType.RCT_CSTR
            Case "Reactor_PFR"
                tobj = ObjectType.RCT_PFR
            Case "HeatExchanger"
                tobj = ObjectType.HeatExchanger
            Case "ShortcutColumn"
                tobj = ObjectType.ShortcutColumn
            Case "DistillationColumn"
                tobj = ObjectType.DistillationColumn
            Case "AbsorptionColumn"
                tobj = ObjectType.AbsorptionColumn
            Case "ReboiledAbsorber"
                tobj = ObjectType.ReboiledAbsorber
            Case "RefluxedAbsorber"
                tobj = ObjectType.RefluxedAbsorber
            Case "ComponentSeparator"
                tobj = ObjectType.ComponentSeparator
            Case "OrificePlate"
                tobj = ObjectType.OrificePlate
            Case "CustomUO"
                tobj = ObjectType.CustomUO
            Case "ExcelUO"
                tobj = ObjectType.ExcelUO
            Case "CapeOpenUO"
                tobj = ObjectType.CapeOpenUO
            Case "SolidsSeparator"
                tobj = ObjectType.SolidSeparator
            Case "Filter"
                tobj = ObjectType.Filter
            Case "Flowsheet"
                tobj = ObjectType.FlowsheetUO
        End Select

        Flowsheet.AddObject(tobj, 30, 30, "")

        Me.Close()

    End Sub

End Class