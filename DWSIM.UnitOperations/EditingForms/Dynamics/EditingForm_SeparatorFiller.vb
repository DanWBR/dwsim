Imports DWSIM.ExtensionMethods
Imports DWSIM.Interfaces.Enums
Imports DWSIM.Interfaces.Enums.GraphicObjects
Imports DWSIM.Thermodynamics.Streams

Public Class EditingForm_SeparatorFiller

    Public Separator As UnitOperations.UnitOpBaseClass

    Private Sub EditingForm_SeparatorFiller_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        Dim units = Separator.FlowSheet.FlowsheetOptions.SelectedUnitSystem
        Dim nf = Separator.FlowSheet.FlowsheetOptions.NumberFormat

        lblPressure.Text = units.pressure

        Dim pressure As Double = Separator.GetDynamicProperty("Operating Pressure")

        tbPressure.Text = pressure.ConvertFromSI(units.pressure).ToString(nf)

        Dim mslist As String() = Separator.FlowSheet.GraphicObjects.Values.Where(Function(x) x.ObjectType = ObjectType.MaterialStream).Select(Function(m) m.Tag).ToArray

        cbStreams.Items.Clear()
        cbStreams.Items.AddRange(mslist)

        If mslist.Count > 0 Then cbStreams.SelectedIndex = 0

        lblVessel.Text = Separator.GraphicObject.Tag

        ChangeDefaultFont()

    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click

        Close()

    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click

        Dim units = Separator.FlowSheet.FlowsheetOptions.SelectedUnitSystem
        Dim nf = Separator.FlowSheet.FlowsheetOptions.NumberFormat

        Dim stream As MaterialStream = Separator.FlowSheet.GetFlowsheetSimulationObject(cbStreams.SelectedItem.ToString())

        Dim volume As Double = Separator.GetDynamicProperty("Volume")

        tbResults.Clear()
        tbResults.Text += "Separator Volume: " + volume.ConvertFromSI(units.volume).ToString(nf) + " " + units.volume + vbCrLf

        Dim pressure As Double = tbPressure.Text.ToDoubleFromCurrent().ConvertToSI(units.pressure)

        tbResults.Text += "Separator Pressure: " + pressure.ConvertFromSI(units.pressure).ToString(nf) + " " + units.pressure + vbCrLf

        Separator.SetDynamicProperty("Operating Pressure", pressure)

        tbResults.Text += "Updating Separator Pressure... OK" + vbCrLf

        Dim astream As MaterialStream = DirectCast(Separator.AccumulationStream.CloneXML(), MaterialStream)

        tbResults.Text += "Cloning Accumulation Stream... OK" + vbCrLf

        astream.Assign(stream)

        tbResults.Text += String.Format("Copying Specifications from '{0}'... OK", stream.GraphicObject.Tag) + vbCrLf

        Try
            astream.SetPressure(pressure)
            astream.SpecType = StreamSpec.Temperature_and_Pressure
            astream.PropertyPackage = Separator.PropertyPackage
            astream.PropertyPackage.CurrentMaterialStream = astream
            astream.Calculate()
            tbResults.Text += "Flashing Stream... OK" + vbCrLf
        Catch ex As Exception
            tbResults.Text += "Flashing Stream... Error" + vbCrLf
            tbResults.Text += vbCrLf
            tbResults.Text += ex.ToString() + vbCrLf
            tbResults.Text += "Accumulation Stream NOT updated. Please try again."
            Exit Sub
        End Try

        Dim density = astream.Phases(0).Properties.density.GetValueOrDefault

        astream.SetMassFlow(density * volume)

        Separator.AccumulationStream = DirectCast(astream.CloneXML(), MaterialStream)

        tbResults.Text += "Setting Accumulation Stream Properties... OK" + vbCrLf

        tbResults.Text += "Finished Successfully!"

        Separator.UpdateDynamicsEditForm()

    End Sub

End Class