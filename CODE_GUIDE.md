# Source Code Guide

This document guides you through DWSIM's Source Code structure and information flow during common calculation scenarios. Its main purpose is to show you **how things work behind the scenes**.

## Useful documents for starters

As stated in [this video](https://www.youtube.com/watch?v=Zbs3baMpoBM), the first reference for creating the data and class structure for DWSIM is a paper published by **William Barrett**, named [Development of a chemical process modeling environment based on CAPE-OPEN interface standards and the Microsoft .NET framework](https://www.sciencedirect.com/science/article/abs/pii/S009813540500219X). This paper provides a very good insight on the CAPE-OPEN standards, and how to implement them in order to create a useful process simulator. Many aspects of DWSIM, i.e. the existence of a separate class (object) for the graphical representation of flowsheet objects, were inspired by it.

After reading the above paper, you could take a look at the [CAPE-OPEN standards](https://www.colan.org/) for more insight on the data structures and functions required by CAPE-OPEN objects. DWSIM implemented CAPE-OPEN interfaces in later versions. The initial implementation of basic classes (compounds, reactions, material stream, etc) looked like CAPE-OPEN objects, but many functions had to be created and changes had to be done to these initial classes in order to have functional CAPE-OPEN objects as in the most recent versions.

## High-level aspects

DWSIM is programmed mainly in Visual Basic .NET language, because it was a natural evolution from VBA, which was the language that I (Daniel) was working with at the end of my graduation period. I knew very little about C++/C# at the time. I'm able to understand and write C# and Python code now, but not C++.

With the launch of Visual Basic 2005 Express Edition, I started working on basic DWSIM classes (Thermodynamics, Unit Operations, PFD, Flowsheet Solver), tightly bonded together and centered around a Windows Forms MDI application (the main window + child windows - the flowsheets). Today, the Classic UI is reminescent of this original decision, but it has been improved, cleaned up and separated from the base classes and calculation routines.

In DWSIM 5 or newer you'll find two main Graphical User Interfaces: **Classic UI**, based on the original code in Windows Forms, and the **Cross-Platform UI**, created in C# from scratch based on a multiplatform UI library called [Eto.Forms](https://github.com/picoe/Eto). The Cross-Platform UI was created to make DWSIM look "native" and run faster on macOS and Linux. If you're on Windows, you can run the Cross-Platform UI too, but I don't recommend that because the Classic UI has more features and is the GUI of choice for the vast majority of users - 90% of the users download the 64-bit version of DWSIM for Windows.

Just for the sake of curiosity, the Android and iOS versions of DWSIM are based on the Cross-Platform UI code, replacing the Eto.Forms code parts with platform-specific UI commands.

### Libraries

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
