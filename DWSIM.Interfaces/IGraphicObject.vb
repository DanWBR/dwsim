'    DWSIM Interface definitions
'    Copyright 2010-2017 Daniel Wagner O. de Medeiros
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

Imports DWSIM.Interfaces.Enums.GraphicObjects

''' <summary>
''' This interface defines the basic properties for a graphical representation of an object in the flowsheet PFD.
''' </summary>
<InterfaceType(ComInterfaceType.InterfaceIsIDispatch)> Public Interface IGraphicObject

    Property DoubleClickAction As Action(Of Object)

    Property DrawOverride As Action(Of Object)

    Sub PositionConnectors()

    Sub Draw(surface As Object)

    Property Editor As Object

    Function Clone() As IGraphicObject

    Property ShapeOverride() As ShapeIcon

    Property Status() As Status

    Property Description() As String

    Property AdditionalInfo() As Object

    Property Shape() As Integer

    Property FlippedH() As Boolean

    Property FlippedV() As Boolean

    Property ObjectType() As ObjectType

    Property IsEnergyStream() As Boolean

    Property Active() As Boolean

    Property Tag() As String

    Property AutoSize() As Boolean

    Property IsConnector() As Boolean

    Property X() As Single

    Property Y() As Single

    Property Name() As String

    Property Height() As Integer

    Property Width() As Integer

    Property InputConnectors() As System.Collections.Generic.List(Of IConnectionPoint)

    Property OutputConnectors() As System.Collections.Generic.List(Of IConnectionPoint)

    Property SpecialConnectors() As System.Collections.Generic.List(Of IConnectionPoint)

    Property EnergyConnector() As IConnectionPoint

    Property Rotation As Integer

    Property Calculated As Boolean

    Property Position As IPoint

    Property Selected As Boolean

    Property Owner As ISimulationObject

    Function HitTest(zoomedSelection As Object) As Boolean

    Property Extensions As Dictionary(Of String, IGraphicObjectExtension)

    Sub DisplayControlPanelModeEditor()

    Property ControlPanelModeEditorDisplayDelegate As Action

    Property DrawMode As Integer

    Property FontStyle As FontStyle

    Property Flowsheet As IFlowsheet

    Function GetPointValue(type As PointValueType, Xref As Integer, Yref As Integer, args As List(Of Object)) As Double

    Function GetIconAsBitmap() As System.Drawing.Bitmap

    Function GetIconAsStream() As IO.MemoryStream

    Sub ReleaseReferences()

    Property DrawLabel As Boolean

End Interface

<InterfaceType(ComInterfaceType.InterfaceIsIDispatch)> Public Interface IConnectionPoint

    Property X() As Integer

    Property Y() As Integer

    Property Type() As ConType

    Property Direction() As ConDir

    Property AttachedConnector() As IConnectorGraphicObject

    Property IsAttached() As Boolean

    Property ConnectorName() As String

    Property Position As IPoint

    Property IsEnergyConnector As Boolean

    Property Active As Boolean

End Interface

<InterfaceType(ComInterfaceType.InterfaceIsIDispatch)> Public Interface IConnectorGraphicObject

    Property AttachedFromConnectorIndex() As Integer

    Property AttachedToConnectorIndex() As Integer

    Property AttachedToEnergy() As Boolean

    Property AttachedFromEnergy() As Boolean

    Property AttachedFrom() As IGraphicObject

    Property AttachedTo() As IGraphicObject

    Property AttachedToOutput() As Boolean

    Property AttachedFromInput() As Boolean

    Property Straight As Boolean

End Interface

<InterfaceType(ComInterfaceType.InterfaceIsIDispatch)> Public Interface IGraphicObjectExtension

    Sub Draw(surface As Object)

    Function Clone() As IGraphicObjectExtension

    Property Description() As String

    Property Active() As Boolean

    Property Tag() As String

    Property Name() As String

    Property Height() As Integer

    Property Width() As Integer

    Property RelativePosition As IPoint

    Property Selected As Boolean

    Property Owner As IGraphicObject

    Function HitTest(zoomedSelection As Object) As Boolean

End Interface
