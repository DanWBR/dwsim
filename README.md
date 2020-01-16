[![Build status](https://ci.appveyor.com/api/projects/status/dpng11yi6dusqenr?svg=true)](https://ci.appveyor.com/project/DanWBR/dwsim5)
[![GitHub issues](https://img.shields.io/github/issues/DanWBR/dwsim5.svg)](https://github.com/DanWBR/dwsim5/issues)
[![tickets](https://img.shields.io/badge/view-tickets-blackgray.svg)](https://sourceforge.net/p/dwsim/tickets/)
[![forums](https://img.shields.io/badge/join-the%20forums-yellowgreen.svg)](https://sourceforge.net/p/dwsim/discussion/?source=navbar)
[![wiki](https://img.shields.io/badge/visit-website-blackblue.svg)](http://dwsim.inforside.com.br)
[![donate](https://img.shields.io/badge/make%20a-donation-greenblue.svg)](https://sourceforge.net/p/dwsim/donate/)

## DWSIM - Open Source Process Simulator
Copyright 2017-2020 Daniel Medeiros

Copyright 2008-2016 Daniel Medeiros, Gregor Reichert, Gustavo León


DWSIM is a software for modeling, simulation and optimization of steady-state chemical processes.

### License

DWSIM is licensed under the GNU General Public License (GPL) Version 3.

### Supported Operating Systems

- Windows (32/64-bit x86) with .NET Framework 4.6.1 or newer
- Linux (32-bit armhf / 64-bit x86) with Mono 5.8 or newer
- macOS (formerly OS X) 10.7 or newer

### Project Details

**Project Name** | **Description**
------------ | -------------
**Shared Projects**
DWSIM.ExtensionMethods|Extension Methods
DWSIM.ExtensionMethods.Eto|Extension Methods for Eto.Forms
DWSIM.FlowsheetSolver|Flowsheet Solver library
DWSIM.GlobalSettings|Shared global settings
DWSIM.Interfaces|Interface definitions
DWSIM.Libraries.OctaveSharp|Octave interface 
DWSIM.Libraries.PythonLink|Python interface 
DWSIM.MathOps|Math library
DWSIM.MathOps.RandomOps|Random number generator library
DWSIM.MathOps.SwarmOps|Optimization library
DWSIM.SharedClasses|Shared/Base class definitions
DWSIM.Thermodynamics|Thermo library
DWSIM.Thermodynamics.Databases.ChEDLThermoLink|ChEDL Thermo Python Library Database Linker/Parser
DWSIM.Thermodynamics.Databases.ChemeoLink|Cheméo Database Linker/Parser
DWSIM.Thermodynamics.Databases.DDBStructureLink|DDB UNIFAC/MODFAC Structure Database Linker/Parser
DWSIM.Thermodynamics.Databases.KDBLink|KDB Database Linker/Parser
DWSIM.Thermodynamics.CoolPropInterface|Interface for CoolProp native library
DWSIM.UnitOperations|Unit Operations library
DWSIM.XMLSerializer|Custom XML Serializer
**Classic UI Projects (Windows Forms)**
DWSIM|Main DWSIM GUI with Tools and Utilities
DWSIM.Automation|Automation Library
DWSIM.Automation.Tests|Automation Testing Application
DWSIM.Apps.AzureServer|Azure Solver Server 
DWSIM.Apps.TCPServer|TCP/IP Solver Server
DWSIM.Controls.DockPanel|DockPanel control
DWSIM.Controls.DockPanel.ThemeVS2012Light|Theme for DockPanel control
DWSIM.Controls.PropertyGridEx|Extended Property Grid Control
DWSIM.Controls.TabStrip|TabStrip control
DWSIM.Controls.ZedGraph|Custom ZedGraph Library
DWSIM.DrawingTools|Flowsheet Drawing Tools
DWSIM.Plugins.NaturalGas|Natural Gas Properties Plugin
DWSIM.SKiaSharp.Views.Desktop|SkiaSharp Controls for Windows Forms
**Cross-Platform UI Projects**
DWSIM.DrawingTools.SkiaSharp|Flowsheet Drawing Tools (SkiaSharp-based)
DWSIM.DrawingTools.SkiaSharp.Extended|Additional Flowsheet Drawing Tools (SkiaSharp-based)
DWSIM.FlowsheetBase|Flowsheet Base Class for Cross-Platform UI
DWSIM.UI.Desktop|Cross-Platform UI App Launcher
DWSIM.UI.Desktop.Editors|Cross-Platform UI Editors (Views/Forms)
DWSIM.UI.Desktop.Forms|Cross-Platform UI Main App Forms
DWSIM.UI.Desktop.GTK|Cross-Platform UI GTK# Platform-specific code
DWSIM.UI.Desktop.Mac|Cross-Platform UI MonoMac Platform-specific code
DWSIM.UI.Desktop.Shared|Cross-Platform UI shared code
DWSIM.UI.Desktop.WinForms|Cross-Platform UI Windows Forms Platform-specific code
DWSIM.UI.Desktop.WPF|Cross-Platform UI Windows Presentation Foundation (WPF) Platform-specific code

### Compiling

- DWSIM can be compiled using Visual Studio 2017 or newer on Windows.
- To compile everything, change the Build target to 'Debug' or 'Release' and 'x64'.