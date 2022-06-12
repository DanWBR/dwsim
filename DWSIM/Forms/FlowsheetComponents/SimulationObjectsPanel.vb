Imports System.Reflection
Imports System.Linq

Public Class SimulationObjectsPanel

    Inherits UserControl

    Public Flowsheet As Interfaces.IFlowsheet

    'Public ObjectList As New List(Of Interfaces.ISimulationObject)

    Private Sub Simulation_Objects_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        ExtensionMethods.ChangeDefaultFont(Me)

        Dim add As Boolean = True

        Dim litems As New List(Of ListItem)

        'add chemsep model

        Dim csmodel = My.Application.MainWindowForm.aTypeList.Where(Function(x) x.FullName.ToLower.Contains("distillationcolumn")).SingleOrDefault
        Dim comodel = My.Application.MainWindowForm.aTypeList.Where(Function(x) x.FullName.ToLower.Contains("capeopenuo")).SingleOrDefault
        Dim csobj = DirectCast(Activator.CreateInstance(csmodel), Interfaces.ISimulationObject)
        Dim coobj = DirectCast(Activator.CreateInstance(comodel), Interfaces.ISimulationObject)
        If Not Flowsheet.MobileCompatibilityMode Then
            Dim li As New ListItem
            li.lblName.Text = "ChemSep Column"
            li.lblName.Font = New Font(SystemFonts.MessageBoxFont.FontFamily, 7.0, System.Drawing.FontStyle.Bold)
            li.ToolTip1.SetToolTip(li.lblName, "ChemSep Rigorous Separation Column (CAPE-OPEN)")
            li.Image.Image = csobj.GetIconBitmap
            li.ToolTip1.SetToolTip(li.Image, "ChemSep Rigorous Separation Column (CAPE-OPEN)")
            li.ObjectTypeInfo = coobj.GetType
            li.Tag = csobj.ObjectClass
            Me.PanelColumns.Controls.Add(li)
            csobj = Nothing
            coobj = Nothing
        End If

        'add other models

        For Each item In My.Application.MainWindowForm.aTypeList
            If Not item.IsAbstract Then
                Dim obj = DirectCast(Activator.CreateInstance(item), Interfaces.ISimulationObject)
                If Not Flowsheet.MobileCompatibilityMode Then
                    add = obj.GetType.GetProperty("Visible").GetValue(obj)
                Else
                    add = obj.MobileCompatible
                End If
                If add Then
                    obj.SetFlowsheet(Flowsheet)
                    Dim li As New ListItem
                    li.lblName.Text = obj.GetDisplayName
                    li.lblName.Font = New Font(SystemFonts.MessageBoxFont.FontFamily, 7.0, System.Drawing.FontStyle.Bold)
                    li.ToolTip1.SetToolTip(li.lblName, obj.GetDisplayDescription)
                    li.Image.Image = obj.GetIconBitmap
                    li.ToolTip1.SetToolTip(li.Image, obj.GetDisplayDescription)
                    li.ObjectTypeInfo = obj.GetType
                    li.Tag = obj.ObjectClass
                    litems.Add(li)
                    obj = Nothing
                End If
            End If
        Next

        For Each item In My.Application.MainWindowForm.ExternalUnitOperations
            Dim obj = item.Value
            If Not Flowsheet.MobileCompatibilityMode Then
                add = obj.GetType.GetProperty("Visible").GetValue(obj)
            Else
                add = obj.MobileCompatible
            End If
            If add Then
                obj.SetFlowsheet(Flowsheet)
                Dim li As New ListItem
                li.lblName.Text = obj.GetDisplayName
                li.lblName.Font = New Font(SystemFonts.MessageBoxFont.FontFamily, 7.0, System.Drawing.FontStyle.Bold)
                li.ToolTip1.SetToolTip(li.lblName, obj.GetDisplayDescription)
                li.Image.Image = obj.GetIconBitmap
                li.ToolTip1.SetToolTip(li.Image, obj.GetDisplayDescription)
                li.ObjectTypeInfo = obj.GetType
                li.Tag = obj.ObjectClass
                litems.Add(li)
                obj = Nothing
            End If
        Next

        For Each item In litems
            Select Case DirectCast(item.Tag, Interfaces.Enums.SimulationObjectClass)
                Case SimulationObjectClass.CAPEOPEN
                    Me.PanelUser.Controls.Add(item)
                Case SimulationObjectClass.Columns
                    Me.PanelColumns.Controls.Add(item)
                Case SimulationObjectClass.Exchangers
                    Me.PanelExchangers.Controls.Add(item)
                Case SimulationObjectClass.Logical, SimulationObjectClass.Inputs, SimulationObjectClass.Switches
                    Me.PanelLogical.Controls.Add(item)
                Case SimulationObjectClass.MixersSplitters
                    Me.PanelMixers.Controls.Add(item)
                Case SimulationObjectClass.Other
                    Me.PanelOther.Controls.Add(item)
                Case SimulationObjectClass.PressureChangers
                    Me.PanelPressure.Controls.Add(item)
                Case SimulationObjectClass.Reactors
                    Me.PanelReactors.Controls.Add(item)
                Case SimulationObjectClass.Separators
                    Me.PanelSeparators.Controls.Add(item)
                Case SimulationObjectClass.Solids
                    Me.PanelSolids.Controls.Add(item)
                Case SimulationObjectClass.Streams
                    Me.PanelStreams.Controls.Add(item)
                Case SimulationObjectClass.UserModels
                    Me.PanelUser.Controls.Add(item)
                Case SimulationObjectClass.Indicators
                    Me.PanelIndicators.Controls.Add(item)
                Case SimulationObjectClass.Controllers
                    Me.PanelControllers.Controls.Add(item)
                Case SimulationObjectClass.CleanPowerSources,
                     SimulationObjectClass.Electrolyzers
                    Me.PanelCE.Controls.Add(item)
            End Select
        Next

        If Not FormMain.IsPro Then
            ProFeatures.Functions.AddProUnitOps(New FlowLayoutPanel() {PanelMixers, PanelColumns, PanelExchangers, PanelLogical, PanelPressure, PanelUser})
        End If

    End Sub

End Class