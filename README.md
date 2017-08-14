## DWSIM - Open Source Process Simulator
Copyright 2008-2017 Daniel Medeiros, Gregor Reichert, Gustavo León

DWSIM is a software for modeling, simulation and optimization of steady-state chemical processes.

### License

DWSIM is licensed under the GNU General Public License (GPL) Version 3.

### Supported Operating Systems

- 64-bit Windows with .NET Framework 4.5 or newer
- 64-bit Linux with Mono 5.0 or newer
- 64-bit macOS with Mono 5.0 or newer

### Project Details

**Project Name** | **Description**
------------ | -------------
**Shared Projects**
DWSIM.Apps.Updater|Updater Application
DWSIM.ExtensionMethods|Extension Methods
DWSIM.FileDownloader|File downloader for automatic updates
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
DWSIM.Apps.RunAsx86|Runs DWSIM in 32-bit mode (Windows)
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
**Cross-Platform UI Projects**
DWSIM.Controls.MonoMac.TextEditor|Control for script code editing on Mac platform
DWSIM.DrawingTools.SkiaSharp|Flowsheet Drawing Tools (SkiaSharp-based)
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

- The whole DWSIM project can be compiled using Visual Studio 2013 or newer, only on Windows.
- To get pre-release [Eto.Forms](https://github.com/picoe/Eto) packages, add https://www.myget.org/F/eto/ as a NuGet package source.
- To compile the Classic UI executable ('DWSIM' project), change the Build target to 'Debug/Release' => 'Any CPU'.
- To compile the Cross-Platform UI executable ('DWSIM.UI.Desktop' project), change the Build target to 'Debug/Release' => 'x64'.
