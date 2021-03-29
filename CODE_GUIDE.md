# Source Code Guide

This document guides you through DWSIM's Source Code structure and information flow during common calculation scenarios. Its main purpose is to show you **how things work behind the scenes**.

## Useful documents for starters

As stated in [this video](https://www.youtube.com/watch?v=Zbs3baMpoBM), the first reference for creating the data and class structure for DWSIM is a paper published by **William Barrett**, named [Development of a chemical process modeling environment based on CAPE-OPEN interface standards and the Microsoft .NET framework](https://www.sciencedirect.com/science/article/abs/pii/S009813540500219X). This paper provides a very good insight on the CAPE-OPEN standards, and how to implement them in order to create a useful process simulator. Many aspects of DWSIM, i.e. the existence of a separate class (object) for the graphical representation of flowsheet objects, were inspired by it.

After reading the above paper, you could take a look at the [CAPE-OPEN standards](https://www.colan.org/) for more insight on the data structures and functions required by CAPE-OPEN objects. DWSIM implemented CAPE-OPEN interfaces in later versions. The initial implementation of basic classes (compounds, reactions, material stream, etc) looked like CAPE-OPEN objects, but many functions had to be created and changes had to be done to these initial classes in order to have functional CAPE-OPEN objects as in the most recent versions.

## High-level aspects ##

DWSIM is programmed mainly in Visual Basic .NET language, because it was a natural evolution from VBA, which was the language that I (Daniel) was working with at the end of my graduation period. I knew very little about C++/C# at the time. I'm able to understand and write C# and Python code now, but not C++.

With the launch of Visual Basic 2005 Express Edition, I started working on basic DWSIM classes (Thermodynamics, Unit Operations, PFD, Flowsheet Solver), tightly bonded together and centered around a Windows Forms MDI application (the main window + child windows - the flowsheets). Today, the Classic UI is reminescent of this original decision, but it has been improved, cleaned up and separated from the base classes and calculation routines.

In DWSIM 5 and newer versions you'll find two main Graphical User Interfaces: **Classic UI**, based on the original code in Windows Forms, and the **Cross-Platform UI**, created in C# from scratch based on a multiplatform UI library called [Eto.Forms](https://github.com/picoe/Eto). The Cross-Platform UI was created to make DWSIM look "native" and run faster on macOS and Linux. If you're on Windows, you can run the Cross-Platform UI too, but I don't recommend that because the Classic UI has more features and is the GUI of choice for the vast majority of users - 90% of the users download the 64-bit version of DWSIM for Windows.

Just for the sake of curiosity, the Android and iOS versions of DWSIM are based on the Cross-Platform UI code, replacing the Eto.Forms code parts with platform-specific UI commands.

## Libraries

### DWSIM.Interfaces

The first and more important library is [**DWSIM.Interfaces**](https://github.com/DanWBR/dwsim6/tree/windows/DWSIM.Interfaces). It contains the interfaces which should be implemented by the Shared code and Classic UI/Cross-Platform UI specific projects. Note that this is different than the CAPE-OPEN interfaces, which are defined in the CapeOpen.dll library, which can be located [here](https://github.com/DanWBR/dwsim6/blob/windows/DWSIM/References/CapeOpen.dll). The most important objects in DWSIM implement both its own interfaces and the CAPE-OPEN ones:

**Material Stream** object:

```vbnet
Public Class MaterialStream

  Inherits UnitOperations.BaseClass

  'CAPE-OPEN 1.0
  Implements ICapeIdentification, ICapeThermoMaterialObject, ICapeThermoCalculationRoutine, ICapeThermoEquilibriumServer, ICapeThermoPropertyPackage, ICapeThermoMaterialTemplate

  'CAPE-OPEN 1.1
  Implements ICapeThermoMaterial, ICapeThermoCompounds, ICapeThermoPhases, ICapeThermoUniversalConstant, ICapeThermoPropertyRoutine, ICapeThermoEquilibriumRoutine, ICapeThermoMaterialContext

  'CAPE-OPEN Error Interfaces
  Implements ECapeUser, ECapeUnknown, ECapeRoot

  'DWSIM IMaterialStream interface
  Implements Interfaces.IMaterialStream
```

**Flowsheet** object (Classic UI):

```vbnet
Public Class FormFlowsheet

  Inherits Form

  'CAPE-OPEN PME/COSE Interfaces
  Implements CapeOpen.ICapeCOSEUtilities, CapeOpen.ICapeMaterialTemplateSystem, CapeOpen.ICapeDiagnostic,
                CapeOpen.ICapeFlowsheetMonitoring, CapeOpen.ICapeSimulationContext, CapeOpen.ICapeIdentification

  'DWSIM IFlowsheet interface
  Implements Interfaces.IFlowsheet, Interfaces.IFlowsheetBag, Interfaces.IFlowsheetGUI, Interfaces.IFlowsheetCalculationQueue

```
The Flowsheet object from the Cross-Platform UI is not CAPE-OPEN compliant, so it only implements DWSIM interfaces:

```vbnet
Public MustInherit Class FlowsheetBase

  Implements IFlowsheet, IFlowsheetCalculationQueue
```

The DWSIM.Interfaces library ensures that a simulation saved in the Classic UI can be loaded in the Cross-Platform UI, which are totally different implementations of the simulation objects. Take, for instance, the [implementation of the Dynamics Manager in the Classic UI](https://github.com/DanWBR/dwsim6/blob/windows/DWSIM/Forms/Flowsheet%20Components/FormDynamicsManager.vb) versus the [Dynamic Manager in the Cross-Platform UI](https://github.com/DanWBR/dwsim6/blob/windows/DWSIM.UI.Desktop.Editors/Dynamics/DynamicsManagerControl.cs).

### DWSIM.GlobalSettings

The Global Settings library contains all the shared settings (solver configuration, user databases, most recent files, etc) used by the currently running DWSIM application, be it the Classic UI or the Cross-Platform UI one. It is also used when DWSIM is running in automation mode or even as an Excel Add-In.

### DWSIM.SharedClasses

The most important libraries in DWSIM are: **DWSIM.Thermodynamics** and **DWSIM.UnitOperations**. As there is some shared code between these two, I chose to create a separate library to host it. For instance, the **System of Units** classes and Unit Conversion functions, and the base class for a flowsheet object ([SimulationObjectBaseClass](https://github.com/DanWBR/dwsim6/blob/windows/DWSIM.SharedClasses/Base%20Class/SimulationObjectBaseClasses.vb), which implements [ISimulationObject](https://github.com/DanWBR/dwsim6/blob/windows/DWSIM.Interfaces/ISimulationObject.vb), the base interface for all Simulation Objects), which is inherited by the Material Stream (contained in the Thermo library) and the remaining Unit Operation and Energy Stream classes (contained in the Unit Operations library).

The Shared Classes library also contains many other functions which are used by both UIs, like checking for updates, loading FOSSEE flowsheets, and other utilities.

### DWSIM.Thermodynamics.*

The Thermodynamics libraries contains the Property Packages and associated Thermo and Property models, Flash Algorithms and their respective editors.

The main classes defined in DWSIM.Thermodynamics are:

* [**CompoundConstantProperties**](https://github.com/DanWBR/dwsim6/blob/fd71e980d234f423059a119aaff58df2fc9e7cfb/DWSIM.Thermodynamics/Base%20Classes/ThermodynamicsBase.vb#L1395): contains all the unique parameters that define a compound (i.e. name, molecular weight, normal boiling point, critical temperature, etc).

* [**Compound**](https://github.com/DanWBR/dwsim6/blob/fd71e980d234f423059a119aaff58df2fc9e7cfb/DWSIM.Thermodynamics/Base%20Classes/ThermodynamicsBase.vb#L36): contains a **ConstantProperties** object as well as mutable parameters like mass/mole/volumetric flows and fractions, within the context of a Material Stream.

* [**Reaction**](https://github.com/DanWBR/dwsim6/blob/fd71e980d234f423059a119aaff58df2fc9e7cfb/DWSIM.Thermodynamics/Base%20Classes/ThermodynamicsBase.vb#L242): contains all parameters that define a chemical reaction.

* [**ReactionSet**](https://github.com/DanWBR/dwsim6/blob/fd71e980d234f423059a119aaff58df2fc9e7cfb/DWSIM.Thermodynamics/Base%20Classes/ThermodynamicsBase.vb#L521): contains a list of associated **Reaction** objects.

* [**PhaseProperties**](https://github.com/DanWBR/dwsim6/blob/fd71e980d234f423059a119aaff58df2fc9e7cfb/DWSIM.Thermodynamics/Base%20Classes/ThermodynamicsBase.vb#L1224): defines the properties of a Phase (Vapor, Liquid, Solid...) or a mixture of phases.

* [**Phase**](https://github.com/DanWBR/dwsim6/blob/fd71e980d234f423059a119aaff58df2fc9e7cfb/DWSIM.Thermodynamics/Base%20Classes/ThermodynamicsBase.vb#L147): defines a fluid phase. Contains a **PhaseProperties** object and a list of **Compound** objects.

* [**Material Stream**](https://github.com/DanWBR/dwsim6/blob/fd71e980d234f423059a119aaff58df2fc9e7cfb/DWSIM.Thermodynamics/Material%20Stream/MaterialStream.vb): defines flow of matter from one point to another in a flowsheet, or the transport of matter between unit operations. Contains a list of **Phase** objects and lots of helper parameters and methods.

* [**Property Package**](): A Property Package is a collection of phase equilibria and property calculation methods, specific to a chemical engineering application. The Property Package performs almost all of its calculations on an associated **MaterialStream** object. When the [equilibrium](https://github.com/DanWBR/dwsim6/blob/fd71e980d234f423059a119aaff58df2fc9e7cfb/DWSIM.Thermodynamics/Property%20Packages/PropertyPackage.vb#L2204) and [property](https://github.com/DanWBR/dwsim6/blob/fd71e980d234f423059a119aaff58df2fc9e7cfb/DWSIM.Thermodynamics/Property%20Packages/PengRobinson.vb#L257) calculations are performed, the Property Package updates the **Phase** objects in the Material Stream with the calculated total and per-compound phase flows, as well as the phase properties.

The [Flash Algorithm objects](https://github.com/DanWBR/dwsim6/blob/fd71e980d234f423059a119aaff58df2fc9e7cfb/DWSIM.Thermodynamics/Flash%20Algorithms/UniversalFlash.vb) are used by the Property Packages to calculate the distribution of a defined mixture of compounds in different phases according to a group of specified properties, like Temperature, Pressure, Enthalpy, Entropy and Vaporized Fraction.

### DWSIM.Math.*

The Math libraries contain shared math code, like optimization, regression, sorting and other utility classes.

### DWSIM.Drawing.*

The Drawing libraries contains the code required to draw the objects in the flowsheet, as well as the PFD surface class itself. Every single object in the flowsheet  must have a graphical representation, implementing the [IGraphicObject](https://github.com/DanWBR/dwsim6/blob/windows/DWSIM.Interfaces/IGraphicObject.vb) interface. All simulation objects have a [GraphicObject](https://github.com/DanWBR/dwsim6/blob/windows/DWSIM.Drawing/GraphicObjects/GraphicObject.vb) property which contains the "graphical" part of it. 

The GraphicObject class defines the inlet and outlet ports (connection points) for all simulation objects. They have a list of **InputConnectors** and **OutputConnectors**. These connectors contain the information about which object is connected to another object, and through which connection points.

To get information about the connections between flowsheet objects, you must then look for their graphical representations (GraphicObjects). The simulation objects *per se* only contain information about themselves, though their graphical representations also carry a property named **Owner**, which contains a reference to the corresponding simulation objects. This makes sure that everyone knows everyone in the flowsheet.

The Drawing code is based on the [SkiaSharp](https://github.com/mono/SkiaSharp) library. SkiaSharp is a cross-platform 2D graphics API for .NET platforms based on Google's Skia Graphics Library. It provides a comprehensive 2D API that can be used across mobile, server and desktop models to render images.

Using SkiaSharp enables the Classic and Cross-Platform UI to share the same drawing code.

### DWSIM.UI.*

The DWSIM.UI.* libraries contains the Cross-Platform UI code. When compiled, the last project in the group generates **DWSIM.UI.Desktop.exe**, the Cross-Platform UI launcher.

### DWSIM

The DWSIM project contains the Classic UI code. When compiled, it generates the Classic UI executable, **DWSIM.exe**.

