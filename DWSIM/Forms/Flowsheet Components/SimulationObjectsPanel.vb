Imports System.Reflection
Imports System.Linq

Public Class SimulationObjectsPanel

    Inherits WeifenLuo.WinFormsUI.Docking.DockContent

    Public Flowsheet As Interfaces.IFlowsheet

    Private Sub Simulation_Objects_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        SplitContainer1.SplitterDistance = My.Settings.ObjectSelectorSplitterDistance1
        SplitContainer2.SplitterDistance = My.Settings.ObjectSelectorSplitterDistance2
        SplitContainer3.SplitterDistance = My.Settings.ObjectSelectorSplitterDistance3

        Dim calculatorassembly = My.Application.Info.LoadedAssemblies.Where(Function(x) x.FullName.Contains("DWSIM.Thermodynamics")).SingleOrDefault
        Dim unitopassembly = My.Application.Info.LoadedAssemblies.Where(Function(x) x.FullName.Contains("DWSIM.UnitOperations")).SingleOrDefault
        Dim availableTypes As New List(Of Type)()

        availableTypes.AddRange(calculatorassembly.GetTypes().Where(Function(x) If(x.GetInterface("ISimulationObject", True) IsNot Nothing, True, False)))
        availableTypes.AddRange(unitopassembly.GetTypes().Where(Function(x) If(x.GetInterface("ISimulationObject", True) IsNot Nothing, True, False)))

        For Each item In availableTypes.OrderBy(Function(x) x.Name)
            If Not item.IsAbstract Then
                Dim obj = DirectCast(Activator.CreateInstance(item), Interfaces.ISimulationObject)
                obj.SetFlowsheet(Flowsheet)
                Dim li As New ListItem
                li.lblName.Text = obj.GetDisplayName
                li.lblDescription.Text = obj.GetDisplayDescription
                li.Image.Image = obj.GetIconBitmap
                li.ObjectTypeInfo = obj.GetType
                If item.Name.Contains("Stream") Then
                    PanelStreams.Controls.Add(li)
                ElseIf item.Name.Contains("Flowsheet") Or item.Name.Contains("Custom") Or item.Name.Contains("Excel") Or item.Name.Contains("CapeOpen") Then
                    PanelCustomOps.Controls.Add(li)
                ElseIf item.Name.Contains("Recycle") Or item.Name.Contains("Spec") Or item.Name.Contains("Adjust") Then
                    PanelLogicalOps.Controls.Add(li)
                Else
                    PanelUnitOps.Controls.Add(li)
                End If
                obj = Nothing
            End If
        Next

    End Sub

    Private Sub SplitContainer1_SplitterMoved(sender As Object, e As SplitterEventArgs) Handles SplitContainer1.SplitterMoved, SplitContainer2.SplitterMoved, SplitContainer3.SplitterMoved
        My.Settings.ObjectSelectorSplitterDistance1 = SplitContainer1.SplitterDistance
        My.Settings.ObjectSelectorSplitterDistance2 = SplitContainer2.SplitterDistance
        My.Settings.ObjectSelectorSplitterDistance3 = SplitContainer3.SplitterDistance
    End Sub

End Class