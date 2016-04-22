Imports System.Reflection
Imports System.Linq

Public Class SimulationObjectsPanel

    Inherits WeifenLuo.WinFormsUI.Docking.DockContent

    Public Flowsheet As Interfaces.IFlowsheet

    Private Sub Simulation_Objects_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        Dim calculatorassembly = My.Application.Info.LoadedAssemblies.Where(Function(x) x.FullName.Contains("DWSIM.Thermodynamics")).SingleOrDefault
        Dim unitopassembly = My.Application.Info.LoadedAssemblies.Where(Function(x) x.FullName.Contains("DWSIM.UnitOperations")).SingleOrDefault
        Dim availableTypes As New List(Of Type)()

        availableTypes.AddRange(calculatorassembly.GetTypes().Where(Function(x) If(x.GetInterface("ISimulationObject", True) IsNot Nothing, True, False)))
        availableTypes.AddRange(unitopassembly.GetTypes().Where(Function(x) If(x.GetInterface("ISimulationObject", True) IsNot Nothing, True, False)))

        For Each item In availableTypes
            If Not item.IsAbstract Then
                Dim obj = DirectCast(Activator.CreateInstance(item), Interfaces.ISimulationObject)
                obj.SetFlowsheet(Flowsheet)
                Dim li As New ListItem
                li.lblName.Text = obj.GetDisplayName
                li.lblDescription.Text = obj.GetDisplayDescription
                li.Image.Image = obj.GetIconBitmap
                li.ObjectTypeInfo = obj.GetType
                ObjectsPanel.Controls.Add(li)
                obj = Nothing
            End If
        Next

    End Sub

End Class