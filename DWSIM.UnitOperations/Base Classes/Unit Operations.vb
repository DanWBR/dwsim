'    Flowsheet Object Base Classes 
'    Copyright 2008-2014 Daniel Wagner O. de Medeiros
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

Imports System.Runtime.Serialization.Formatters.Binary
Imports System.Runtime.Serialization
Imports System.IO
Imports System.Linq
Imports CapeOpen
Imports System.Runtime.Serialization.Formatters
Imports System.Runtime.InteropServices.Marshal
Imports System.Runtime.InteropServices
Imports System.Text
Imports DWSIM.Interfaces.Interfaces
Imports DWSIM.Interfaces.Enums.GraphicObjects
Imports DWSIM.Interfaces.Enums
Imports System.Windows.Forms
Imports DWSIM.SharedClasses

Namespace UnitOperations

    <System.Serializable()> <ComVisible(True)> Public MustInherit Class UnitOpBaseClass

        Inherits SharedClasses.UnitOperations.BaseClass

        'CAPE-OPEN Unit Operation Support
        Implements ICapeIdentification, ICapeUnit, ICapeUtilities, ICapeUnitReport

        'CAPE-OPEN Persistence Interface
        Implements IPersistStreamInit

        'CAPE-OPEN Error Interfaces
        Implements ECapeUser, ECapeUnknown, ECapeRoot

        <NonSerialized> <Xml.Serialization.XmlIgnore> Public _pp As Interfaces.IPropertyPackage
        Public _ppid As String = ""

        Protected _capeopenmode As Boolean = False

        Public Sub New()
            MyBase.CreateNew()
        End Sub

        Public Overrides Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean

            MyBase.LoadData(data)

            Try
                Me._ppid = (From xel As XElement In data Select xel Where xel.Name = "PropertyPackage").SingleOrDefault.Value
            Catch

            End Try

            Return True

        End Function

        Public Overrides Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement)

            Dim elements As List(Of XElement) = MyBase.SaveData()

            Dim ppid As String = ""
            If _ppid <> "" Then
                ppid = _ppid
            ElseIf Not _pp Is Nothing Then
                ppid = _pp.Name
            Else
                ppid = ""
            End If
            elements.Add(New XElement("PropertyPackage", ppid))

            Return elements

        End Function

#Region "   DWSIM Specific"

        Public Overrides Function GetDebugReport() As String

            Me.DebugMode = True
            Me.DebugText = ""

            Try

                Calculate(Nothing)

                Me.DebugText += vbCrLf & vbCrLf & "Calculated OK."

            Catch ex As Exception

                Dim st As New StackTrace(ex, True)
                Dim frame As StackFrame = st.GetFrame(0)
                Dim fileName As String = Path.GetFileName(frame.GetFileName)
                Dim methodName As String = frame.GetMethod().Name
                Dim line As Integer = frame.GetFileLineNumber()

                AppendDebugLine(String.Format("ERROR: exception raised on file {0}, method {1}, line {2}:", fileName, methodName, line))
                AppendDebugLine(ex.Message.ToString)

            Finally

                Me.DebugMode = False

            End Try

            Return DebugText

        End Function

        ''' <summary>
        ''' Gets or sets the property package associated with this object.
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        <Xml.Serialization.XmlIgnore()> Public Overrides Property PropertyPackage() As Interfaces.IPropertyPackage
            Get
                If Not _pp Is Nothing Then Return _pp
                If _ppid Is Nothing Then _ppid = ""
                If FlowSheet.PropertyPackages.ContainsKey(_ppid) Then
                    Return FlowSheet.PropertyPackages(_ppid)
                Else
                    For Each pp As Interfaces.IPropertyPackage In Me.FlowSheet.PropertyPackages.Values
                        _ppid = pp.UniqueID
                        Return pp
                        Exit For
                    Next
                End If
                Return Nothing
            End Get
            Set(ByVal value As Interfaces.IPropertyPackage)
                If value IsNot Nothing Then
                    _ppid = value.UniqueID
                    _pp = value
                Else
                    _pp = Nothing
                End If
            End Set
        End Property

        ''' <summary>
        ''' Calculates the object.
        ''' </summary>
        ''' <param name="args"></param>
        ''' <remarks></remarks>
        Public Sub Solve(Optional ByVal args As Object = Nothing)

            Calculated = False

            Calculate(args)

            Calculated = True
            LastUpdated = Date.Now

        End Sub

        ''' <summary>
        ''' Decalculates the object.
        ''' </summary>
        ''' <remarks></remarks>
        Public Overridable Overloads Sub DeCalculate()
            MyBase.DeCalculate()
        End Sub

        ''' <summary>
        ''' Decalculates the object.
        ''' </summary>
        ''' <remarks></remarks>
        Public Sub Unsolve()

            DeCalculate()

            Calculated = False

        End Sub

        Sub CreateCOPorts()
            _ports = New CapeOpen.PortCollection()
            For Each c As Interfaces.IConnectionPoint In Me.GraphicObject.InputConnectors
                Select Case c.Type
                    Case ConType.ConIn
                        _ports.Add(New UnitPort("Inlet" + Me.GraphicObject.InputConnectors.IndexOf(c).ToString, "", CapePortDirection.CAPE_INLET, CapePortType.CAPE_MATERIAL))
                        With _ports(_ports.Count - 1)
                            If c.IsAttached Then
                                .Connect(Me.FlowSheet.GetFlowsheetSimulationObject(c.AttachedConnector.AttachedFrom.Tag))
                            End If
                        End With
                    Case ConType.ConEn
                        _ports.Add(New UnitPort("Inlet" + Me.GraphicObject.InputConnectors.IndexOf(c).ToString, "", CapePortDirection.CAPE_INLET, CapePortType.CAPE_ENERGY))
                        With _ports(_ports.Count - 1)
                            If c.IsAttached Then
                                .Connect(Me.FlowSheet.GetFlowsheetSimulationObject(c.AttachedConnector.AttachedFrom.Tag))
                            End If
                        End With
                End Select
            Next
            For Each c As Interfaces.IConnectionPoint In Me.GraphicObject.OutputConnectors
                Select Case c.Type
                    Case ConType.ConOut
                        _ports.Add(New UnitPort("Outlet" + Me.GraphicObject.OutputConnectors.IndexOf(c).ToString, "", CapePortDirection.CAPE_OUTLET, CapePortType.CAPE_MATERIAL))
                        With _ports(_ports.Count - 1)
                            If c.IsAttached Then
                                .Connect(Me.FlowSheet.GetFlowsheetSimulationObject(c.AttachedConnector.AttachedTo.Tag))
                            End If
                        End With
                    Case ConType.ConEn
                        _ports.Add(New UnitPort("Outlet" + Me.GraphicObject.OutputConnectors.IndexOf(c).ToString, "", CapePortDirection.CAPE_OUTLET, CapePortType.CAPE_ENERGY))
                        With _ports(_ports.Count - 1)
                            If c.IsAttached Then
                                .Connect(Me.FlowSheet.GetFlowsheetSimulationObject(c.AttachedConnector.AttachedTo.Tag))
                            End If
                        End With
                End Select
            Next
        End Sub

        Sub CreateCOParameters()
            _parameters = New CapeOpen.ParameterCollection
            Dim props() = Me.GetProperties(PropertyType.ALL)
            For Each s As String In props
                _parameters.Add(New CapeOpen.RealParameter(s, Me.GetPropertyValue(s), 0.0#, Me.GetPropertyUnit(s)))
                With _parameters.Item(_parameters.Count - 1)
                    .Mode = CapeParamMode.CAPE_OUTPUT
                End With
            Next
        End Sub

#End Region

#Region "   CAPE-OPEN ICapeIdentification"

        Public Overrides Property ComponentDescription() As String = "" Implements CapeOpen.ICapeIdentification.ComponentDescription

        Public Overrides Property ComponentName() As String = "" Implements CapeOpen.ICapeIdentification.ComponentName

#End Region

#Region "   CAPE-OPEN Unit Operation internal support"

        Protected _ports As CapeOpen.PortCollection
        Protected _parameters As CapeOpen.ParameterCollection
        Protected _simcontext As Object = Nothing

        ''' <summary>
        ''' Calculates the Unit Operation.
        ''' </summary>
        ''' <remarks>The Flowsheet Unit performs its calculation, that is, computes the variables that are missing at
        ''' this stage in the complete description of the input and output streams and computes any public
        ''' parameter value that needs to be displayed. Calculate will be able to do progress monitoring
        ''' and checks for interrupts as required using the simulation context. At present, there are no
        ''' standards agreed for this.
        ''' It is recommended that Flowsheet Units perform a suitable flash calculation on all output
        ''' streams. In some cases a Simulation Executive will be able to perform a flash calculation but
        ''' the writer of a Flowsheet Unit is in the best position to decide the correct flash to use.
        ''' Before performing the calculation, this method should perform any final validation tests that
        ''' are required. For example, at this point the validity of Material Objects connected to ports can
        ''' be checked.
        ''' There are no input or output arguments for this method.</remarks>
        Public Overridable Sub Calculate1() Implements CapeOpen.ICapeUnit.Calculate
            'do CAPE calculation here
            MyBase.Calculate()
        End Sub

        ''' <summary>
        ''' Return an interface to a collection containing the list of unit ports (e.g. ICapeUnitCollection).
        ''' </summary>
        ''' <value></value>
        ''' <returns>A reference to the interface on the collection containing the specified ports</returns>
        ''' <remarks>Return the collection of unit ports (i.e. ICapeUnitCollection). These are delivered as a
        ''' collection of elements exposing the interfaces ICapeUnitPort</remarks>
        <Xml.Serialization.XmlIgnore()> Public ReadOnly Property ports() As Object Implements CapeOpen.ICapeUnit.ports
            Get
                If Not Me._capeopenmode Then
                    If Not Me.GraphicObject.ObjectType = ObjectType.CapeOpenUO Then
                        _ports = New CapeOpen.PortCollection
                        For Each c As Interfaces.IConnectionPoint In Me.GraphicObject.InputConnectors
                            If c.Type = ConType.ConIn Then
                                _ports.Add(New UnitPort(c.ConnectorName, "", CapePortDirection.CAPE_INLET, CapePortType.CAPE_MATERIAL))
                            ElseIf c.Type = ConType.ConEn Then
                                _ports.Add(New UnitPort(c.ConnectorName, "", CapePortDirection.CAPE_INLET, CapePortType.CAPE_ENERGY))
                            End If
                            With _ports(_ports.Count - 1)
                                If c.IsAttached And Not c.AttachedConnector Is Nothing Then .Connect(Me.FlowSheet.SimulationObjects(c.AttachedConnector.AttachedFrom.Name))
                            End With
                        Next
                        For Each c As Interfaces.IConnectionPoint In Me.GraphicObject.OutputConnectors
                            If c.Type = ConType.ConOut Then
                                _ports.Add(New UnitPort(c.ConnectorName, "", CapePortDirection.CAPE_OUTLET, CapePortType.CAPE_MATERIAL))
                            ElseIf c.Type = ConType.ConEn Then
                                _ports.Add(New UnitPort(c.ConnectorName, "", CapePortDirection.CAPE_OUTLET, CapePortType.CAPE_ENERGY))
                            End If
                            With _ports(_ports.Count - 1)
                                If c.IsAttached And Not c.AttachedConnector Is Nothing Then .Connect(Me.FlowSheet.SimulationObjects(c.AttachedConnector.AttachedTo.Name))
                            End With
                        Next
                    End If
                End If
                Return _ports
            End Get
        End Property

        ''' <summary>Validates the Unit Operation.</summary>
        ''' <param name="message">An optional message describing the cause of the validation failure.</param>
        ''' <returns>TRUE if the Unit is valid</returns>
        ''' <remarks>Sets the flag that indicates whether the Flowsheet Unit is valid by validating the ports and
        ''' parameters of the Flowsheet Unit. For example, this method could check that all mandatory
        ''' ports have connections and that the values of all parameters are within bounds.
        ''' Note that the Simulation Executive can call the Validate routine at any time, in particular it
        ''' may be called before the executive is ready to call the Calculate method. This means that
        ''' Material Objects connected to unit ports may not be correctly configured when Validate is
        ''' called. The recommended approach is for this method to validate parameters and ports but not
        ''' Material Object configuration. A second level of validation to check Material Objects can be
        ''' implemented as part of Calculate, when it is reasonable to expect that the Material Objects
        ''' connected to ports will be correctly configured.</remarks>
        Public Overridable Function Validate1(ByRef message As String) As Boolean Implements CapeOpen.ICapeUnit.Validate
            'do CAPE validation here
            message = "Validation OK"
            Return True
        End Function

        ''' <summary>
        ''' Get the flag that indicates whether the Flowsheet Unit is valid (e.g. some parameter values
        ''' have changed but they have not been validated by using Validate). It has three possible
        ''' values:
        ''' ·  notValidated(CAPE_NOT_VALIDATED): the unit’s validate() method has not been
        ''' called since the last operation that could have changed the validation status of the unit, for
        ''' example an update to a parameter value of a connection to a port.
        ''' ·  invalid(CAPE_INVALID): the last time the unit’s validate() method was called it returned
        ''' false.
        ''' ·  valid(CAPE_VALID): the last time the unit’s validate() method was called it returned true.
        ''' </summary>
        ''' <value>CAPE_VALID meaning the Validate method returned success; CAPE_INVALID meaing the Validate 
        ''' method returned failure; CAPE_NOT_VALIDATED meaning that the Validate method needs to be called 
        ''' to determine whether the unit is valid or not.</value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Overridable ReadOnly Property ValStatus() As CapeOpen.CapeValidationStatus Implements CapeOpen.ICapeUnit.ValStatus
            Get
                Return CapeValidationStatus.CAPE_VALID
            End Get
        End Property

        ''' <summary>
        ''' The PMC displays its user interface and allows the Flowsheet User to interact with it. If no user interface is
        ''' available it returns an error.</summary>
        ''' <remarks></remarks>
        Public Overridable Sub Edit1() Implements CapeOpen.ICapeUtilities.Edit
            Throw New CapeNoImplException("ICapeUtilities.Edit() Method not implemented.")
        End Sub

        ''' <summary>
        ''' Initially, this method was only present in the ICapeUnit interface. Since ICapeUtilities.Initialize is now
        ''' available for any kind of PMC, ICapeUnit. Initialize is deprecated.
        ''' The PME will order the PMC to get initialized through this method. Any initialisation that could fail must be
        ''' placed here. Initialize is guaranteed to be the first method called by the client (except low level methods such
        ''' as class constructors or initialization persistence methods). Initialize has to be called once when the PMC is
        ''' instantiated in a particular flowsheet.
        ''' When the initialization fails, before signalling an error, the PMC must free all the resources that were
        ''' allocated before the failure occurred. When the PME receives this error, it may not use the PMC anymore.
        ''' The method terminate of the current interface must not either be called. Hence, the PME may only release
        ''' the PMC through the middleware native mechanisms.
        ''' </summary>
        ''' <remarks></remarks>
        Public Overridable Sub Initialize() Implements CapeOpen.ICapeUtilities.Initialize
            'do CAPE Initialization here
            CreateCOPorts()
            CreateCOParameters()
        End Sub

        ''' <summary>
        ''' Returns an ICapeCollection interface.
        ''' </summary>
        ''' <value></value>
        ''' <returns>CapeInterface (ICapeCollection)</returns>
        ''' <remarks>This interface will contain a collection of ICapeParameter interfaces.
        ''' This method allows any client to access all the CO Parameters exposed by a PMC. Initially, this method was
        ''' only present in the ICapeUnit interface. Since ICapeUtilities.GetParameters is now available for any kind of
        ''' PMC, ICapeUnit.GetParameters is deprecated. Consult the “Open Interface Specification: Parameter
        ''' Common Interface” document for more information about parameter. Consult the “Open Interface
        ''' Specification: Collection Common Interface” document for more information about collection.
        ''' If the PMC does not support exposing its parameters, it should raise the ECapeNoImpl error, instead of
        ''' returning a NULL reference or an empty Collection. But if the PMC supports parameters but has for this call
        ''' no parameters, it should return a valid ICapeCollection reference exposing zero parameters.</remarks>
        <Xml.Serialization.XmlIgnore()> Public ReadOnly Property parameters() As Object Implements CapeOpen.ICapeUtilities.parameters
            Get
                Return _parameters
            End Get
        End Property

        ''' <summary>
        ''' Allows the PME to convey the PMC a reference to the former’s simulation context. 
        ''' </summary>
        ''' <value>The reference to the PME’s simulation context class. For the PMC to
        ''' use this class, this reference will have to be converted to each of the
        ''' defined CO Simulation Context interfaces.</value>
        ''' <remarks>The simulation context
        ''' will be PME objects which will expose a given set of CO interfaces. Each of these interfaces will allow the
        ''' PMC to call back the PME in order to benefit from its exposed services (such as creation of material
        ''' templates, diagnostics or measurement unit conversion). If the PMC does not support accessing the
        ''' simulation context, it is recommended to raise the ECapeNoImpl error.
        ''' Initially, this method was only present in the ICapeUnit interface. Since ICapeUtilities.SetSimulationContext
        ''' is now available for any kind of PMC, ICapeUnit. SetSimulationContext is deprecated.</remarks>
        <Xml.Serialization.XmlIgnore()> Public WriteOnly Property simulationContext() As Object Implements CapeOpen.ICapeUtilities.simulationContext
            Set(ByVal value As Object)
                _simcontext = value
            End Set
        End Property

        ''' <summary>
        ''' Initially, this method was only present in the ICapeUnit interface. Since ICapeUtilities.Terminate is now
        ''' available for any kind of PMC, ICapeUnit.Terminate is deprecated.
        ''' The PME will order the PMC to get destroyed through this method. Any uninitialization that could fail must
        ''' be placed here. ‘Terminate’ is guaranteed to be the last method called by the client (except low level methods
        ''' such as class destructors). ‘Terminate’ may be called at any time, but may be only called once.
        ''' When this method returns an error, the PME should report the user. However, after that the PME is not
        ''' allowed to use the PMC anymore.
        ''' The Unit specification stated that “Terminate may check if the data has been saved and return an error if
        ''' not.” It is suggested not to follow this recommendation, since it’s the PME responsibility to save the state of
        ''' the PMC before terminating it. In the case that a user wants to close a simulation case without saving it, it’s
        ''' better to leave the PME to handle the situation instead of each PMC providing a different implementation.
        ''' </summary>
        ''' <remarks></remarks>
        Public Overridable Sub Terminate1() Implements CapeOpen.ICapeUtilities.Terminate
            If Not _simcontext Is Nothing Then
                If IsComObject(_simcontext) Then
                    ReleaseComObject(_simcontext)
                Else
                    _simcontext = Nothing
                End If
            End If
            Me.Dispose()
        End Sub

#End Region

#Region "   CAPE-OPEN Persistence Implementation"

        Protected m_dirty As Boolean = True

        Public Sub GetClassID(ByRef pClassID As System.Guid) Implements IPersistStreamInit.GetClassID
            pClassID = New Guid(UnitOpBaseClass.ClassId)
        End Sub

        Public Sub GetSizeMax(ByRef pcbSize As Long) Implements IPersistStreamInit.GetSizeMax
            pcbSize = 1024 * 1024
        End Sub

        Public Overridable Sub InitNew() Implements IPersistStreamInit.InitNew
            'do nothing
        End Sub

        Public Function IsDirty() As Integer Implements IPersistStreamInit.IsDirty
            Return m_dirty
        End Function

        Public Overridable Sub Load(ByVal pStm As System.Runtime.InteropServices.ComTypes.IStream) Implements IPersistStreamInit.Load

            ' Read the length of the string  
            Dim arrLen As Byte() = New [Byte](3) {}
            pStm.Read(arrLen, arrLen.Length, IntPtr.Zero)

            ' Calculate the length  
            Dim cb As Integer = BitConverter.ToInt32(arrLen, 0)

            ' Read the stream to get the string    
            Dim bytes As Byte() = New Byte(cb - 1) {}
            Dim pcb As New IntPtr()
            pStm.Read(bytes, bytes.Length, pcb)
            If System.Runtime.InteropServices.Marshal.IsComObject(pStm) Then System.Runtime.InteropServices.Marshal.ReleaseComObject(pStm)

            ' Deserialize byte array    

            Dim memoryStream As New System.IO.MemoryStream(bytes)

            Try

                Dim domain As AppDomain = AppDomain.CurrentDomain
                AddHandler domain.AssemblyResolve, New ResolveEventHandler(AddressOf MyResolveEventHandler)

                Dim myarr As ArrayList

                Dim mySerializer As Binary.BinaryFormatter = New Binary.BinaryFormatter(Nothing, New System.Runtime.Serialization.StreamingContext())
                myarr = mySerializer.Deserialize(memoryStream)

                Me._parameters = myarr(0)
                Me._ports = myarr(1)

                myarr = Nothing
                mySerializer = Nothing

                RemoveHandler domain.AssemblyResolve, New ResolveEventHandler(AddressOf MyResolveEventHandler)

            Catch p_Ex As System.Exception

                System.Windows.Forms.MessageBox.Show(p_Ex.ToString())

            End Try

            memoryStream.Close()

        End Sub

        Public Overridable Sub Save(ByVal pStm As System.Runtime.InteropServices.ComTypes.IStream, ByVal fClearDirty As Boolean) Implements IPersistStreamInit.Save

            Dim props As New ArrayList

            With props

                .Add(_parameters)
                .Add(_ports)

            End With

            Dim mySerializer As Binary.BinaryFormatter = New Binary.BinaryFormatter(Nothing, New System.Runtime.Serialization.StreamingContext())
            Dim mstr As New MemoryStream
            mySerializer.Serialize(mstr, props)
            Dim bytes As Byte() = mstr.ToArray()
            mstr.Close()

            ' construct length (separate into two separate bytes)    

            Dim arrLen As Byte() = BitConverter.GetBytes(bytes.Length)
            Try

                ' Save the array in the stream    
                pStm.Write(arrLen, arrLen.Length, IntPtr.Zero)
                pStm.Write(bytes, bytes.Length, IntPtr.Zero)
                If System.Runtime.InteropServices.Marshal.IsComObject(pStm) Then System.Runtime.InteropServices.Marshal.ReleaseComObject(pStm)

            Catch p_Ex As System.Exception

                System.Windows.Forms.MessageBox.Show(p_Ex.ToString())

            End Try

            If fClearDirty Then
                m_dirty = False
            End If

        End Sub

        Protected Function MyResolveEventHandler(ByVal sender As Object, ByVal args As ResolveEventArgs) As System.Reflection.Assembly
            Return Me.[GetType]().Assembly
        End Function

#End Region

#Region "   CAPE-OPEN Error Interfaces"

        Sub ThrowCAPEException(ByRef ex As Exception, ByVal name As String, ByVal description As String, ByVal interf As String, ByVal moreinfo As String, ByVal operation As String, ByVal scope As String, ByVal code As Integer)

            _code = code
            _description = description
            _interfacename = interf
            _moreinfo = moreinfo
            _operation = operation
            _scope = scope

            Throw ex

        End Sub

        Private _name, _description, _interfacename, _moreinfo, _operation, _scope As String, _code As Integer

        Public ReadOnly Property Name2() As String Implements CapeOpen.ECapeRoot.Name
            Get
                Return Me.Name
            End Get
        End Property

        Public ReadOnly Property code() As Integer Implements CapeOpen.ECapeUser.code
            Get
                Return _code
            End Get
        End Property

        Public ReadOnly Property description() As String Implements CapeOpen.ECapeUser.description
            Get
                Return _description
            End Get
        End Property

        Public ReadOnly Property interfaceName() As String Implements CapeOpen.ECapeUser.interfaceName
            Get
                Return _interfacename
            End Get
        End Property

        Public ReadOnly Property moreInfo() As String Implements CapeOpen.ECapeUser.moreInfo
            Get
                Return _moreinfo
            End Get
        End Property

        Public ReadOnly Property operation() As String Implements CapeOpen.ECapeUser.operation
            Get
                Return _operation
            End Get
        End Property

        Public ReadOnly Property scope1() As String Implements CapeOpen.ECapeUser.scope
            Get
                Return _scope
            End Get
        End Property

#End Region

#Region "   CAPE-OPEN Reports"

        Protected _reports As String() = New String() {"log", "last run", "validation results"}
        Protected _selreport As String = ""
        Protected _calclog As String = ""
        Protected _lastrun As String = ""
        Protected _valres As String = ""

        Public Sub ProduceReport(ByRef message As String) Implements CapeOpen.ICapeUnitReport.ProduceReport
            Select Case _selreport
                Case "log"
                    message = _calclog
                Case "last run"
                    message = _lastrun
                Case "validation results"
                    message = _valres
            End Select
        End Sub

        Public ReadOnly Property reports() As Object Implements CapeOpen.ICapeUnitReport.reports
            Get
                Return _reports
            End Get
        End Property

        Public Property selectedReport() As String Implements CapeOpen.ICapeUnitReport.selectedReport
            Get
                Return _selreport
            End Get
            Set(ByVal value As String)
                _selreport = value
            End Set
        End Property

#End Region

    End Class

    <System.Serializable()> Public MustInherit Class SpecialOpBaseClass

        Inherits SharedClasses.UnitOperations.BaseClass

        Public Sub New()
            MyBase.CreateNew()
        End Sub

    End Class

End Namespace

Namespace SpecialOps.Helpers

    <System.Serializable()> Public Class SpecialOpObjectInfo

        Implements ISpecialOpObjectInfo

        Sub New()

        End Sub

        Public Property ID As String = "" Implements ISpecialOpObjectInfo.ID

        Public Property Name As String = "" Implements ISpecialOpObjectInfo.Name

        Public Property PropertyName As String = "" Implements ISpecialOpObjectInfo.PropertyName

        Public Property Type As String = "" Implements ISpecialOpObjectInfo.Type

    End Class

End Namespace

