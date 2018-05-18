Imports System.Reflection
Imports System.Linq

Public Class SimulationObjectsPanel

    Inherits WeifenLuo.WinFormsUI.Docking.DockContent

    Public Flowsheet As Interfaces.IFlowsheet

    Public ObjectList As New List(Of Interfaces.ISimulationObject)

    Private Sub Simulation_Objects_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        Me.AutoHidePortion = 580

        Dim calculatorassembly = My.Application.Info.LoadedAssemblies.Where(Function(x) x.FullName.Contains("DWSIM.Thermodynamics,")).FirstOrDefault
        Dim unitopassembly = My.Application.Info.LoadedAssemblies.Where(Function(x) x.FullName.Contains("DWSIM.UnitOperations")).FirstOrDefault
        Dim availableTypes As New List(Of Type)()

        availableTypes.AddRange(calculatorassembly.GetTypes().Where(Function(x) If(x.GetInterface("DWSIM.Interfaces.ISimulationObject") IsNot Nothing, True, False)))
        availableTypes.AddRange(unitopassembly.GetTypes().Where(Function(x) If(x.GetInterface("DWSIM.Interfaces.ISimulationObject") IsNot Nothing, True, False)))

        Dim add As Boolean = True

        Dim litems As New List(Of ListItem)

        For Each item In availableTypes.OrderBy(Function(x) x.Name)
            If Not item.IsAbstract Then
                Dim obj = DirectCast(Activator.CreateInstance(item), Interfaces.ISimulationObject)
                If Not Flowsheet.MobileCompatibilityMode Then
                    add = obj.GetType.GetProperty("Visible").GetValue(obj)
                Else
                    add = obj.MobileCompatible
                End If
                If add Then
                    ObjectList.Add(obj)
                    obj.SetFlowsheet(Flowsheet)
                    Dim li As New ListItem
                    li.lblName.Text = obj.GetDisplayName
                    li.lblDescription.Text = obj.GetDisplayDescription
                    li.Image.Image = obj.GetIconBitmap
                    li.ObjectTypeInfo = obj.GetType
                    litems.Add(li)
                    obj = Nothing
                End If
            End If
        Next

        Me.PanelItems.Controls.AddRange(litems.OrderBy(Function(x) x.lblName.Text).ToArray)

    End Sub

    Private Sub tbFilterList_TextChanged(sender As Object, e As EventArgs) Handles tbFilterList.TextChanged
        If tbFilterList.Text = "" Then
            For Each item As ListItem In Me.PanelItems.Controls
                item.Visible = True
            Next
        Else
            For Each item As ListItem In Me.PanelItems.Controls
                item.Visible = item.lblName.Text.ToLower.Contains(tbFilterList.Text.ToLower) Or item.lblDescription.Text.ToLower.Contains(tbFilterList.Text.ToLower)
            Next
        End If
    End Sub

End Class