==================================================
DWSIM - Open Source Process Simulator
Version 5.8 Update 8
Copyright (c) 2017-2020 Daniel Medeiros
Copyright (c) 2008-2016 Daniel Medeiros, Gregor Reichert, Gustavo Leon
==================================================

DWSIM is a software for modeling, simulation and optimization of steady-state chemical processes.

==================================================
DISCLAIMER
==================================================

The data and information within DWSIM has been obtained from a wide variety of literature sources.  While reasonable care has been exercised in the collection of data and testing of this software, the author of DWSIM disclaims any warranty, expressed or implied, as to the accuracy or reliability of the data or calculations contained therein. The results of calculations obtained from DWSIM yield approximate results, which will not always be suitable for every application.

The software is designed for use by trained professional personnel and is not a substitute for sound professional judgment.  It is the sole responsibility of the user to validate the data presented by DWSIM and to determine whether the results of this program are accurate and suitable for any specific purpose.  No guarantee of accuracy or fitness for any purpose is expressed or implied.  The author strongly recommends that the data be checked against other sources and/or methods before use and application.  The author shall not be held liable for any direct, indirect, consequential or incidental damages incurred through use of the data or calculations. 

==================================================
LICENSE
==================================================

DWSIM is licensed under the GNU General Public License (GPL) Version 3.
 
==================================================
SYSTEM REQUIREMENTS
==================================================

OS:             

Windows: 32/64-bit Vista/7/8/10
Linux: 64-bit x86 Ubuntu 12.04 or newer / 32-bit armhf Raspbian/Ubuntu MATE
macOS: 10.7 (OS X Lion) or newer

Software:

Microsoft .NET Framework 4.6.1 or newer (Windows) 
Mono Framework 5.14.0 or newer (Linux)

CPU:			

1.0 GHz dual-core processor (minimum)

Memory:         

2 GB RAM

Graphics Card:	

128 MB with OpenGL hardware acceleration

Display:        

1280x800 display resolution is recommended as a minimum

Disk space:		

500 MB for program files

==================================================
VERSION HISTORY / CHANGELOG
==================================================

The full changelog, including code changes and their authors can be viewed at https://github.com/DanWBR/dwsim5/commits/windows

Version 5.8 Update 8

- [NEW] Added a Pervaporation sample
- [NEW] Added support for external Unit Operations
- [NEW] Added the ability to load and use images as flowsheet object icons for Python Script UOs
- [FIX] Fixed some automation bugs
- [FIX] Fixed Pipe insulation calculation
- [FIX] Bug fixes

Version 5.8 Update 7

- [NEW] Added Performance Curves and Polytropic Process support to Compressor and Expander
- [FIX] Fixed single compound phase detection
- [FIX] Other bug fixes

Version 5.8 Update 6

- [NEW] Define the number of grouping rows in Master Property Table
- [FIX] Fixed loading and saving of simulation files in mobile version format (Android/iOS)
- [FIX] Fixed Absorber Column in Liquid-Liquid Extraction mode
- [FIX] Fixed the Filter property editor in Cross-Platform UI
- [FIX] Fixed Filter cake resistance unit conversion
- [FIX] Fixed the Script Editor not working on Classic UI
- [FIX] Other bug fixes

Version 5.8 Update 5

- [NEW] Integrated Solution Inspector with object editors
- [CHG] Enhanced compound selection panel in simulation setup wizard (Classic UI)
- [FIX] Fixed a bug with Master Tables
- [FIX] Fixed Excel Add-In Settings panel not loading correctly
- [FIX] Other bug fixes

Version 5.8 Update 4

- [NEW] Added a Tag property to compounds for custom ordering
- [NEW] Added Compound ordering capability to Cross-Platform UI (CPUI)
- [NEW] Enhanced object connections editor (CPUI)
- [FIX] Fixed Spreadsheet Copy/Paste (CPUI)
- [FIX] Fixed Compressor/Expander calculation mode selection (CPUI)

Version 5.8 Update 3

- [FIX] Fixed Valve Kv calculation mode for Gas/Steam
- [FIX] Fixed loading of properties for some Unit Operations
- [FIX] Other minor bug fixes

Version 5.8 Update 2

- [NEW] Added a new VLLE Flash Algorithm tweak to check for alternative liquid phase compositions on initial VLE PT Flash
- [FIX] Fixed Rigorous Column automatic initialization
- [FIX] Fixed number of trial compositions not changing on Flash Algorithm editor

Version 5.8 Update 1

- [CHG] Changed target .NET Framework version to 4.6.1 on Windows
- [CHG] Reduced the size of the User Guide PDF file
- [FIX] Fixed Automation feature
- [FIX] Fixed some issues with ReoGrid formula parsing
- [FIX] Enhanced reliability of liquid phase instability detection

Version 5.8

- [NEW] Enhanced Spreadsheet Tool
- [NEW] 2D Charts Tool
- [NEW] Enhanced Script Editor (Cross-Platform UI)
- [NEW] Dynamic Script Snippets
- [CHG] Redesigned Cross-Platform UI
- [CHG] Redesigned Property Package editors
- [FIX] Fixed some unit conversions for reaction basis
- [FIX] Fixed text to number conversions
- [FIX] Fixed flowsheet dragging while zoomed
- [FIX] Fixed PFR heat balance
- [FIX] Fixed liquid phase viscosity calculation for supercritical compounds
- [FIX] Fixed Splitter ratios calculation mode
- [FIX] Workaround for math expression parsing errors

Version 5.7 Update 14

- [CHG] Added 'This' to python vars (Python Script UO)
- [FIX] Fixed flowsheet dragging while zoomed
- [FIX] Fixed exception when launching the Script Editor (Classic UI)
- [FIX] Fixed Energy Stream connection editors for Python Script UO (Classic UI)

Version 5.7 Update 13

- [NEW] Insert IronPython Script Snippets (Classic UI)
- [CHG] Restored Reference Object functionality to Controller/Adjust Logical Block
- [CHG] Restored Maximum Iterations editor to Recycle Logical Block
- [CHG] Optimized Isothermal Compressibility calculation
- [FIX] Fixed layout of object editors (Classic UI)
- [FIX] Fixed Thermo library registration
- [FIX] Fixed Molar Volume value in Material Stream report
- [FIX] Minor bug fixes

Version 5.7 Update 12

- [NEW] Added the ability to work with reaction parameters in optimization and sensitivity analysis tools (Classic UI)
- [NEW] Added the ability to load external Property Packages
- [NEW] Added solid fugacity config options to Property Packages
- [FIX] Fixed sorting of optimizer variable properties (Classic UI)
- [FIX] Fixed textbox text color on Property Package config editor (Cross-Platform UI)

Version 5.7 Update 11

- [FIX] Fixed PR/SRK volume translation correction for C1-C10, H2S, N2 and CO2
- [FIX] Restored Eto packages to v2.4.1 for Linux compatibility
- [FIX] Fixed splitter split ratio trackbar precision / added editable textboxes (Classic UI)
- [FIX] Fixed Flowsheet.AddObject() not working in automation mode

Version 5.7 Update 10

- [NEW] Exposing CAPE-OPEN object on CAPE-OPEN Unit Operation ('GetCAPEOPENObject()' method)
- [FIX] Fixed compound grid row double click (Classic UI)
- [FIX] Fixed Valve calculation for Liquid Service Kv mode

Version 5.7 Update 9

- [CHG] Changed Welcome Panel button icons (Classic UI)
- [FIX] Fixed ChemSep column mass/energy balance warnings
- [FIX] Restored GUI language selection (Classic UI)
- [FIX] Compound search textbox crash (Classic UI)
- [FIX] eNRTL fix
- [FIX] Electrolyte Flash warning message
- [FIX] Fixed Advanced Property Package Settings window not on top (Classic UI)
- [FIX] Fixed units mismatch on assay manager (Classic UI)
- [FIX] Other bug fixes

Version 5.7 Update 8

- [NEW] Highlight phase areas in Binary Envelope (Cross-Platform UI)
- [NEW] Context menu for charts (Cross-Platform UI)
- [NEW] Split Material/Energy Streams and Insert Recycles (Classic UI)
- [NEW] Order compound lists (Classic UI)
- [NEW] Exposing petroleum cold flow properties from utility (Classic UI)
- [FIX] Fixed solver infinite loop detection
- [FIX] Other bug fixes

Version 5.7 Update 7

- [NEW] Enhanced Phase/Binary Envelopes in Cross-Platform UI
- [FIX] Fixed compound adding by pressing ENTER on the search box (Classic UI)
- [FIX] Fixed Phase Identification Parameter in Phase Envelope
- [FIX] Fixed merged ToolStrips in Classic UI
- [FIX] Minor bug fixes and tweaks in Cross-Platform UI

Version 5.7 Update 6

- [NEW] IronPython Script Debugging (Classic UI)
- [CHG] Updated components info in about boxes
- [FIX] Fixed Custom Calculation Order not working (Classic UI)
- [FIX] Fixed Pressure units in Valve's Kv calculation mode
- [FIX] Other minor bug fixes

Version 5.7 Update 5

- [NEW] Added Python standard libraries
- [NEW] Enable/disable custom Touch Bar controls on macOS
- [CHG] Electrolyte flash enhancements
- [FIX] Fixed double conversion from string
- [FIX] Fixed remaining CefSharp processes after shutdown (Classic UI)
- [FIX] Fixed compound issues on Classic UI

Version 5.7 Update 4

- [NEW] New syntax-highlighting script editors on macOS and Linux (New UI)
- [FIX] Fixed editors not updating on Compound and Property Package/Flash Algorithm changes (New UI)
- [FIX] SLE-only Fixed binary envelope (Classic UI)

Version 5.7 Update 3

- [NEW] Spreadsheet cell formatting tools (Classic UI)
- [FIX] Fixed importing of property units to spreadsheet (Classic UI)
- [FIX] Fixed pressure drop on Gibbs and Equilibrium reactors

Version 5.7 Update 2

- [NEW] Structured object result reports (New UI)
- [NEW] User-defined expressions for velocity constants (Kinetic Reactions)
- [CHG] Redesigned reaction editors (New UI)
- [CHG] Redesigned flowsheet analysis tools (New UI)
- [FIX] Fixed PFD delayed connector drawing

Version 5.7 Update 1

- [NEW] Exposing additional reactor properties to tables and analysis tools
- [NEW] Shortcut selection buttons on table editors (Classic UI)
- [FIX] Fixed Electrolyte DB
- [FIX] Fixed pressure drop on Gibbs and Equilibrium reactors

Version 5.7

- [NEW] Electrolyte NRTL model for Sour Gas and Amine systems
- [NEW] Copy Flowsheet as image to clipboard
- [NEW] New Web Panel (Classic UI)
- [CHG] Redesigned Welcome Screen (Classic UI)
- [CHG] Redesigned Compound Selector (Classic UI)
- [CHG] Updated ChEDL Compound Database
- [CHG] Restored 'print flowsheet' function (Classic UI)
- [FIX] Fixed object cloning
- [FIX] Fixed SLE flash
- [FIX] Fixed english dynamic viscosity units
- [FIX] Fixed mirrored characters on flowsheet objects
- [FIX] Bug fixes

Version 5.6 Update 12

- [NEW] Change calculation order (Cross-Platform UI)
- [NEW] User-Defined (Python) Flash Algorithm
- [NEW] User can now override Unit Operation and Material Stream calculation routines using Python Scripts

Version 5.6 Update 11

- [NEW] Added the possibility to change the calculation order on Classic UI (hold Ctrl+Alt before solving the flowsheet)
- [NEW] User can now override K-value Fugacity, Enthalpy and Entropy calculations at Property Package level using Python Scripts
- [FIX] Fixed Side Draw flowrate input (Classic UI)

Version 5.6 Update 10

- [FIX] Fixed white background on Dark Mode (macOS)
- [FIX] Immiscible VLLE Flash fixes, restored missing compound selection

Version 5.6 Update 9

- [NEW] New Excel Add-In 'GetPropUnits' function (Windows only)
- [FIX] Fixed calculation of special properties (Excel Add-In)
- [FIX] Fixed Pipe Segment's Outlet P/T spec mode
- [FIX] Fixed some Classic UI issues with High-DPI displays

Version 5.6 Update 8

- [FIX] Fixed Flowsheet Unit Operation block
- [FIX] Fixed Spreadsheet mass/mole fraction writing

Version 5.6 Update 7

- [FIX] Fixed 'Create and Connect' buttons (Classic UI)
- [FIX] Fixed Heater/Cooler report (New UI)

Version 5.6 Update 6

- [NEW] Select Classic UI Flowsheet Renderer (CPU or OpenGL)
- [CHG] Added ChEDL compound database to Classic UI
- [FIX] Fixed Spreadsheet Table crash
- [FIX] Fixes to some flash algorithms
- [FIX] Other fixes and enhancements

Version 5.6 Update 5

- [NEW] Load FOSSEE Flowsheets and Samples from the File menu (Classic UI)
- [NEW] Customize Systems of Units on Simulation Settings Wizard (Classic UI)
- [NEW] Added ChemSep Column directly from the Object Palette
- [FIX] Other bug fixes and enhancements

Version 5.6 Update 4

- [FIX] Fixed loading of FOSSEE Flowsheets

Version 5.6 Update 3

- [FIX] Fixed Flowsheet Zoom
- [FIX] Fixed Petroleum Characterization

Version 5.6 Update 2

- [FIX] Fixed bugs in Gibbs and Equilibrium reactors
- [FIX] Fixed Oveerhead Vapor connection (Rig Dist Column)
- [FIX] Minor bug fixes and enhancements

Version 5.6 Update 1

- [FIX] Fixed bugs in Gibbs and Equilibrium reactors
- [FIX] Fixed Energy Recycle connection
- [FIX] Fixed some drawing issues
- [FIX] Minor bug fixes and enhancements

Version 5.6

- [NEW] Unified Process Flowsheet Drawing interface
- [CHG] Redesigned Distillation/Absorption Column connection editor
- [FIX] Fixed Steam Tables Enthalpy/Entropy calculation near saturation points
- [FIX] Fixed a regression in Nested Loops VLE Flash
- [FIX] Minor bug fixes and enhancements

Version 5.5 Update 1

- [CHG] Updated ChemSep database
- [FIX] Bug fixes

Version 5.5

- [NEW] Added an option to use legacy property editors
- [NEW] Unit tooltips on property editors
- [NEW] Random property units on floating tables
- [NEW] Added more Flash functions to Excel interface
- [NEW] Restored Command Line funcionality
- [CHG] Redesigned Compound Creator
- [FIX] Fixed PFR bugs
- [FIX] Exposing Script UO variables (CPUI)
- [FIX] Fixed reaction editors
- [FIX] Fixed Excel interface
- [FIX] Other bug fixes and enhancements

Version 5.4 Update 3

- [NEW] New Property Packages for CoolProp Incompressible Fluids & Mixtures
- [FIX] Fixed PFR viscosity calculation
- [FIX] Fixed a bug with Tx flashes

Version 5.4 Update 2

- [FIX] Fixed handling of solids by the Gibbs reactor
- [FIX] Other bug fixes

Version 5.4 Update 1

- [FIX] Bug fixes

Version 5.4

- [NEW] A new Global Flash Algorithm supporting one solid, one vapor and two liquid phases, is now the default one
- [NEW] Updated Inspector Window for Classic UI
- [CHG] Enhanced reliability of the liquid phase stability test, removes the need for stability search key compound selection
- [CHG] Missing NRTL/UNIQUAC Interaction Parameters warning only for non-zero compound amounts
- [FIX] Fixed broken sample simulations

Version 5.3

- [NEW] Support for Dark Mode on macOS Mojave (10.14)
- [NEW] Support for High DPI displays on Linux with configurable Scaling Factor (Cross-Platform UI)
- [NEW] Download and Open FOSSEE Flowsheets directly from DWSIM
- [NEW] Configurable warning for missing interaction parameters on Activity Coefficient-based Models
- [NEW] Cross-Platform UI users can now edit initial estimates for Distillation and Absorption Columns
- [CHG] Now requires Mono v5.14 or newer on Linux, which fixes many Classic UI (Windows Forms) glitches on this platform
- [CHG] Prevented unreliable UNIQUAC interaction parameters from loading
- [CHG] Bypass CAPE-OPEN Unit Operations on macOS and Linux
- [FIX] Minor bug fixes

Version 5.2 Update 22

- [CHG] Improved font rendering on Cross-Platform UI PFD Designer
- [FIX] Fixed stoichiometric coefficient input on reaction editor (Classic UI)

Version 5.2 Update 21

- [FIX] Fixed a regression in Heat Exchanger model

Version 5.2 Update 20

- [FIX] Fixed Absorption Column setup on Cross-Platform UI

Version 5.2 Update 19

- [FIX] Fixed a regression in Steam Tables Property Package

Version 5.2 Update 18

- [NEW] Liquid Viscosity Pressure Correction
- [FIX] Fixed Letsou-Stiel Liquid Viscosity estimation

Version 5.2 Update 17

- [CHG] Liquid Density calculation fixes and enhancements
- [FIX] Minor bug fixes

Version 5.2 Update 16

- [NEW] Exposed Gibbs Reactor Numerical Derivative Perturbation
- [FIX] Bug fixes (PFR, Orifice Plate)

Version 5.2 Update 15

- [CHG] Performance updates
- [FIX] Fixed Pipe Segment UO diameter unit conversion
- [FIX] Do not throw CAPE-OPEN UO 'Edit()' not implemented exception

Version 5.2 Update 14

- [NEW] Added support for OpenCL GPU acceleration on macOS and Linux
- [FIX] Fixed Classic UI Material Stream Editor

Version 5.2 Update 13

- [NEW] Enhancements to input textboxes
- [NEW] Enter math expressions on input textboxes

Version 5.2 Update 12

- [NEW] New Kv calculation mode for Valve model
- [NEW] User-Defined Pipe Wall Material
- [NEW] Unit locking mode for Spreadsheet (Classic UI)
- [FIX] Fixed a bug with Pump calculations
- [FIX] Fixed simulation comments not restoring correctly on Classic UI

Version 5.2 Update 11

- [NEW] Heat Exchanger thermal efficiency calculation mode
- [CHG] Material Stream editor updates (Classic UI)
- [FIX] Fixed Flowsheet Object cloning
- [FIX] Fixed Liquid Phase Enthalpy calculation for some Property Packages

Version 5.2 Update 10

- [FIX] Fixed a bug with the CAPE-OPEN UO Socket
- [FIX] Fixed Adjust Control Panel (Classic UI)
- [FIX] Fixed Update reminder

Version 5.2 Update 9

- [NEW] Added Floating Table for compound amounts in Material Streams
- [NEW] Copy data from chart objects
- [FIX] Fixed bugs with the PFR and Equilibrium Reactors
- [FIX] Fixed Adjust Control Panel (Classic UI)

Version 5.2 Update 8

- [NEW] Solution Inspector enhancements
- [FIX] Fixed loading of mobile-saved simulation files
- [FIX] Fixed a bug with the Equilibrium Reactor
- [FIX] Other bug fixes

Version 5.2 Update 7

- [FIX] Fixed a bug with the Compound Creator Wizard
- [FIX] Fixed a bug with the SVLE Flash Algorithm
- [FIX] CAPE-OPEN fixes
- [FIX] Fixed pipe segment diameter quick input (Classic UI)
- [FIX] Other fixes and enhancements

Version 5.2 Update 6

- [NEW] Added Quality Check to Petroleum Characterization Tools
- [CHG] Enhanced Petroleum Characterization Tools on Cross-Platform UI
- [CHG] Removed Update checkboxes on Classic UI
- [FIX] Fixed missing Expander on Cross-Platform UI
- [FIX] Bug fixes

Version 5.2 Update 5

- [NEW] Data copy from property tables to clipboard (Cross-Platform UI)
- [CHG] Limited the maximum mumber of simultaneous opened object editors
- [FIX] Fixed data copy from property table to clipboard (Classic UI)
- [FIX] Bug fixes

Version 5.2 Update 4

- [FIX] Bug fixes

Version 5.2 Update 3

- [FIX] Fixed a bug with the PFR model
- [FIX] Fixed a bug with the Heat Exchanger model
- [FIX] Fixed a bug with the Compound Separator model
- [FIX] Report tool fixes (Classic UI)
- [FIX] Touch Bar fix for macOS < 10.12.2

Version 5.2 Update 2

- [FIX] Fixed Python.NET issues on Linux and macOS
- [FIX] Fixed flowsheet object removal on Cross-Platform UI
- [FIX] Fixed error when connecting rigorous distillation column reboiler duty on Cross-Platform UI
- [FIX] Fixed a bug with the spreadsheet
- [FIX] Restored backup function on Classic UI
- [FIX] Fixed mouse wheel absent bug on Classic UI

Version 5.2 Update 1

- [CHG] Added more Solution Inspector model descriptions (NL SVLE Flash, Conversion Reactor, Compressor, Cooler, Expander, Heater, Mixer and Pump)
- [FIX] Fixed an issue with the Shell-and-Tube Heat Exchanger model
- [FIX] Fixed Material Stream Vapor Fraction editing on the New UI
- [FIX] Fixed an issue with the Nested Loops SVLE (Eutectic) PH/PS Flashes

Version 5.2

- [NEW] Solution Inspector - understand the models behind the simulation
- [NEW] Cross-Platform UI Enhancements: Drag-and-Drop to add Flowsheet Objects, object property editor Panels and much more
- [NEW] Added support for Touch Bar on MacBook Pro 2016/2017 models
- [NEW] Enhanced support for High DPI displays
- [NEW] Added support for neutral compounds on HetCat Reaction Expressions
- [NEW] Exposed Pipe Segment calculation results to analysis tools
- [NEW] Added support for Classic UI on macOS (work in progress)
- [NEW] Added an option to calculate Phase Enthalpies based on liquid phase Cp values on Activity Coefficient-based PPs
- [CHG] Relocated Flowsheet Solver Controls on the Classic UI
- [CHG] Exposing all flash specs on MS editor (Cross-Platform UI)
- [CHG] Updated the target version of .NET Framework to v4.7.1
- [CHG] Removed dependency of Mono Framework from macOS (OS X) version
- [FIX] Rigorous Column fixes for Liquid-Liquid Extraction
- [FIX] PFR zero-flow fix
- [FIX] Fixed Master Property Table not updating upon loading
- [FIX] Fixed Pipe Segment Quantity
- [FIX] Fixed Flash Algorithm temperature limits
- [FIX] Fixed Equilibrium Reactor calculation restart
- [FIX] Fixed issues with Petroleum Characterization Utilities

Version 5.1 Update 12

- [NEW] Exposed CSTR Outlet Temperature for Analysis tools
- [CHG] Updated method for calculating Cp and Cp/T integrals
- [FIX] Fixed ComboBox width for GTK# (Cross-Platform UI)
- [FIX] Fixed System of Units ComboBox selection not working (Classic UI)
- [FIX] Fixed Raoult's Law PP Entropy calculations
- [FIX] Pipe Profile fixes (Classic UI)

Version 5.1 Update 11

- [CHG] UI enhancements
- [FIX] Fixed CSTR and PFR heat balances
- [FIX] Fixed issues with Petroleum Characterization utilities
- [FIX] Fixed Raoult's Law entropy calculations

Version 5.1 Update 10

- [FIX] Fixed HetCat reaction support on CSTR
- [FIX] Fixed reaction temperature limits not working on Equilibrium and CSTR reactors
- [FIX] Fixed loading of duplicate compounds (Classic UI)

Version 5.1 Update 9

- [NEW] Exposed Pipe Segment hydraulic and thermal properties to analysis tools
- [NEW] Exposed Equilibrium Reactor convergence parameters
- [CHG] Classic UI updates
- [FIX] Fixed bugs with Compound Creator Wizard running on the Classic UI
- [FIX] Fixed a bug with the Gibbs Reactor

Version 5.1 Update 8

- [NEW] Added more information about errors listed in the log window (Classic UI)
- [FIX] Rewritten Conversion Reactor model
- [FIX] Fixed CSTR connections (New UI)
- [FIX] CoolProp fixes

Version 5.1 Update 7

- [NEW] Added support for Raspberry Pi 2/3 (Linux 32-bit ARM Platform)
- [NEW] Added a Natural Gas Water Saturator sample
- [CHG] Heat Capacity Difference workaround for SVLE Flash
- [FIX] Ordered list of cape-open objects in selector
- [FIX] Fixed CAPE-OPEN UO validation/calculation
- [FIX] Fixed a bug with valve sizing
- [FIX] Fixed missing temperature fields on Kinetic Reaction editor (New UI)

Version 5.1 Update 6

- [NEW] Added new sample flowsheet: NGPU with Turbo-Expansion (Windows only)
- [NEW] PFD enhancements (New UI): edit flowsheet object appearance, horizontal/vertical flip, rotation and other properties
- [FIX] Bug fixes

Version 5.1 Update 5

- [FIX] Fixed Sour Water model
- [FIX] Fixed a bug with the Flowsheet UO
- [FIX] Fixed molar flow separation spec in Compound Separator
- [FIX] Fixed Material Stream phase property cloning
- [FIX] Fixed section diameter conversion (Pipe Segment, New UI)

Version 5.1 Update 4

- [CHG] Updated CoolProp libraries (Windows/Linux)
- [FIX] Fixed Console Redirection (Classic UI)
- [FIX] Fixed issues with CoolProp Property Package
- [FIX] Fixed issues with CAPE-OPEN Property Package Socket

Version 5.1 Update 3

- [NEW] Exposed Plugins Interface to Cross-Platform UI
- [CHG] New procedure for calculation of dew points in Natural Gas properties Plugin
- [CHG] Improved compound compatibility with CoolProp Property Package
- [CHG] Nested Loops VLE Flash: forcing PH/PS fast mode off with single compound
- [FIX] Fixed issues with Formation Properties in Compound Creation tools
- [FIX] Fixed Flowsheet Solver not handling interconnected energy streams

Version 5.1 Update 2

- [CHG] Updated Compound Creator Wizard to get data from online sources
- [FIX] Exposing missing Rigorous Column specs (New UI)
- [FIX] Minor bug fixes

Version 5.1 Update 1

- [FIX] Fixed issues with the Classic UI on High DPI displays
- [FIX] Fixed inverted Material Stream list (New UI)
- [FIX] Fixed text flowsheet object removal (New UI)

Version 5.1

- [NEW] Simulation Setup Wizard (New UI)
- [NEW] Compound Creator Wizard
- [NEW] Calculated Phase Properties can now be overriden by the user
- [NEW] Force compound to be in the solid phase only
- [NEW] Added full profile information for Pipe Segment result reports (New UI)
- [FIX] Added missing CoolProp and Petalas-Aziz libraries for macOS

Version 5.0 Update 12

- [FIX] Fixed bugs in the SVLE flash algorithm
- [FIX] Fixed bugs in the Pipe Segment model
- [FIX] Fixed bugs in the CSTR editor (Classic UI)

Version 5.0 Update 11

- [NEW] Advanced EOS and ThermoC models are now exposed to Excel and CAPE-OPEN
- [CHG] Excel Add-In enhancements and bug fixes
- [FIX] Minor bug fixes

Version 5.0 Update 10

- [NEW] Added support for ThermoC Property Package
- [FIX] Fixed crashes during loading of simulations on macOS
- [FIX] Fixed Material Stream cloning on new UI
- [FIX] Fixed viscosity calculations for petroleum fractions
- [FIX] Fixed velocity units in Kinetic Reaction editor (new UI)

Version 5.0 Update 9

- [NEW] Anchored Property Lists
- [NEW] Flowsheet Charts
- [CHG] Updated syntax highlighting for macOS/Linux script editors
- [FIX] Minor bug fixes and enhancements

Version 5.0 Update 8

- [NEW] Added IPOPT libraries for macOS
- [NEW] Script Manager: Added an option to run scripts asynchronously
- [NEW] Added dynamic properties to Flowsheet
- [NEW] Redesigned Script Manager (New UI)
- [CHG] Removed FPROPS and PC-SAFT (original) Property Packages
- [FIX] Fixed logical block editors to include name/status (New UI)
- [FIX] Fixed floating property table behavior on macOS (New UI)
- [FIX] Added missing property editors for column solver methods and pump curves (New UI) 
- [FIX] Fixed HetCat reaction rate unit selector (New UI)
- [FIX] Fixed IPOPT link path on Linux
- [FIX] Fixed python script execution on Linux and macOS
- [FIX] Fixed some translation issues

Version 5.0 Update 7

- [FIX] Fixed loading Property Packages from the Advanced EOS Library on the new UI
- [FIX] Fixed missing translations 

Version 5.0 Update 6

- [NEW] Added hardware (OpenGL) renderer option for PFD area (accessible through 'Global Settings' > 'Flowsheet Renderer')
- [NEW] Added Mass/Energy balance check to Unit Operations ('Simulation Setup' > 'Settings')
- [FIX] Fixed new object placement on New UI
- [FIX] Fixed file extension when saving a simulation on the New UI
- [FIX] Fixed Log Window on Classic UI 
- [FIX] Fixed GTK# renderer on macOS
- [FIX] Reworked Sensitivity Analysis results viewing on New UI
- [FIX] Fixed solver selection missing from Rigorous Column editors on New UI

Version 5.0 Update 5

- [FIX] Fixed reporting issues on macOS/Linux
- [FIX] Fixed values of Enthalpy/Entropy/Cp on Material Stream result reports

Version 5.0 Update 4

- [NEW] Exposed Dynamic Properties to Sensitivity Analysis and Optimization utilities
- [NEW] Added ODS/ODT results reporting (New UI)
- [FIX] Load File/Spreadsheet fixes (Classic UI)
- [FIX] Fixed an error with the Sensitivity Analysis utility (New UI)

Version 5.0 Update 2/3

- [CHG] Updated Linux files
- [FIX] Added missing Orifice Plate Editor/Results
- [FIX] Fixed error on spreadsheet value writing
- [FIX] Fixed an issue with the Python Script UO on Linux

Version 5.0 Update 1

- [FIX] Fixed script selection/linking on new UI
- [FIX] Added missing WriteMessage() function to Flowsheet class on new UI
- [FIX] Fixed flowsheet mouse click issues on macOS platform

Version 5.0

- [NEW] New Cross-Platform User Interface (Windows/Linux/macOS)
- [NEW] Simulation Objects now feature Dynamic Properties
- [CHG] Reworked Classic interface to increase PFD area
- [CHG] Native libraries are now included on each platform-specific installer/package
- [FIX] Fixed Equation 107 in compound creator
- [FIX] Fixed ODE solver in PFR code
- [FIX] Various bug fixes and minor enhancements 

Version 4.3 Update 14

- [NEW] Exposed Gibbs Reactor convergence parameters for fine-tuning
- [FIX] Reactor model bug fixes
- [FIX] Fixed Compound Creator JSON export

Version 4.3 Update 13

- [NEW] Added Peng-Robinson interaction parameters for H2O/NH3/CO2/H2S systems
- [CHG] Updated Python Script subsystem
- [FIX] Sour Water model bug fixes
- [FIX] Fixed Equilibrium/Gibbs Reactor adiabatic calculation mode
- [FIX] Compound database loading fix
- [FIX] Fixed PR interaction parameter set retrieval

Version 4.3 Update 12

- [NEW] New samples: Gasoline & Biodiesel Combustion using Cantera
- [NEW] Custom ordering of Master Property Table items
- [NEW] Flowsheet Scripts now support Python.NET
- [FIX] Fixed management of Optimization and Sensitivity Analysis cases
- [FIX] Fixed a bug with the Gibbs Reactor solver 

Version 4.3 Update 11

- [NEW] Python.NET is now an option to parse Python scripts on the Script Unit Operation - enables 
        interoperability between DWSIM and a full Python 2.7 ecosystem
- [NEW] New Sample: Cantera (https://github.com/Cantera/cantera/wiki) Integration with DWSIM
- [FIX] Fixed a bug with the Gibbs Reactor

Version 4.3 Update 10

- [NEW] Quick-edit button for Systems of Units
- [CHG] Changed default number formatting
- [FIX] Flowsheet Solver fixes

Version 4.3 Update 8/9

- [CHG] Enhanced Gibbs Reactor model, now support multiphase chemical equilibrium
- [CHG] Updated Simultaneous Adjust convergence criteria to compare against individual tolerances
- [FIX] Fixed XML serialization of Strings
- [FIX] Fixed Poynting correction for supercritical fluids
- [FIX] Fixed Compound Separator mass balance issues

Version 4.3 Update 7

- [CHG] Redesigned Object Palette
- [FIX] Minor bug fixes

Version 4.3 Update 6

- [NEW] Pump Curve database system
- [FIX] Fixed Flowsheet Object cloning
- [FIX] Fixed issues when adding external compounds to existing simulations
- [FIX] Fixed issues with expression parsing for KDB compounds

Version 4.3 Update 5

- [NEW] Added 'Commercial Copper' to Pipe UO wall material selection
- [CHG] Changed Recycle mass flow error calculation
- [FIX] Fixed heat balance for the Conversion Reactor
- [FIX] Fixed initialization issues on Windows XP
- [FIX] Fixed bugs on CAPE-OPEN UO
- [FIX] Fixed bugs on Flowsheet UO
- [FIX] Fixed a bug on the Flowsheet Solver
- [FIX] Fixed a bug with the Orifice Plate UO Editor

Version 4.3 Update 4

- [NEW] Implemented Poynting Correction Factor for activity coefficient models, enabled by default
- [NEW] More German translations 
- [FIX] Fixed an issue with Nested-Loops immiscible flash algorithm
- [FIX] Fixed flowsheet drag-and-drop when running on Mono
- [FIX] Fixed solver deactivation after an infinite loop error message

Version 4.3 Update 3

- [CHG] Added an option to control decimal separator parsing for spreadsheet formulas
- [FIX] User compound database loading fix
- [FIX] Fixed issues with Material Stream cloning

Version 4.3 Update 2

- [FIX] Fixed an issue with Data Regression utility
- [FIX] Fixed a stack overflow error when cloning a Material Stream

Version 4.3 Update 1

- [FIX] Fixed issues with solid calculations
- [CHG] Moved Compound Data XML Exporting function to Pure Compound Property Viewer

Version 4.3

- [NEW] Import Compound from Online Databases (KDB, Chemeo, DDB GC Structure Info)
- [NEW] Import Compound from ChEDL Thermo Python Library (https://github.com/CalebBell/thermo)
- [NEW] Data Regression Tool: Import VLE data from Online KDB Database
- [NEW] Export/Import Compound data to/from JSON file format
- [NEW] Add objects to the flowsheet from the context menu
- [NEW] Spreadsheet Undo/Redo
- [NEW] Exposing Spec/Adjust properties
- [CHG] Data Regression Tool/Compound Creator UI design enhancements
- [CHG] Changed location of settings file for Mono environments to Documents Folder -> DWSIM Application Data
- [FIX] Fixed Spreadsheet data saving/loading when cell formula contains '>' or '<'
- [FIX] Fixed native libraries extraction procedures
- [FIX] Minor fixes and enhancements

Version 4.2 Update 3

- [NEW] Added support for the Advanced EOS Library
- [FIX] Fixed issue with flowsheet calculation when a Material Stream editor window is opened
- [FIX] Fixed issue when saving a simulation with scripts
- [FIX] Fixed issue with the Heat Exchanger model
- [FIX] Fixed a bug with the Simple LLE flash algorithm
- [CHG] Natural Gas plugin Water Dew Point calculation enhancements

Version 4.2 Update 2

- [FIX] Fixed outlet temperature in Gibbs/Equilibrium reactors
- [FIX] Fixed experimental regression data values not updating when changing units on Compound Creator
- [FIX] Fixed floating table positioning when close to flowsheet borders

Version 4.2 Update 1

- [CHG] Enhancements and fixes for Rigorous Column model solvers
- [FIX] Fixed issues with Material Stream editor

Version 4.2

- [NEW] Added support for COM/.NET Automation
- [NEW] Added string variable support to Script UO
- [CHG] Manage user databases directly from Excel Add-In Options window
- [FIX] Fixed Spreadsheet UO not reading input variables
- [FIX] Fixed issues with Steam Tables Enthalpy/Entropy calculations
- [FIX] Fixed Steam Tables liquid density calculation
- [FIX] Fixed a bug with Heat Exchanger Shell & Tube calculation mode
- [FIX] Other bug fixes and enhancements

Version 4.1 Update 12

- [FIX] Fixed translation issues
- [FIX] Fixed PH/PS Flash calculation through the Excel interface

Version 4.1 Update 11

- [NEW] More German translations
- [CHG] Updated Rigorous Column solvers (NS/IO)
- [FIX] Fixed CSTR editor issue

Version 4.1 Update 10

- [FIX] Implemented data validation for input tables (issue #147 https://sourceforge.net/p/dwsim/tickets/147/)
- [FIX] Fixed unwanted object dragging when 'Close Editors on Deselecting' is enabled (issue #154 https://sourceforge.net/p/dwsim/tickets/154/)
- [FIX] Fixed double array value check on XML serialization (issue #155 https://sourceforge.net/p/dwsim/tickets/155/)

Version 4.1 Update 9

- [FIX] Fixed a bug regression in binary envelope utility
- [FIX] Fixed PH-Flash with Raoult's Law Property Package
- [CHG] CSTR model updates

Version 4.1 Update 8

- [NEW] Updated CSTR model, now supports multiple phases and adds vapor outlet
- [CHG] Update Material Stream flow and composition after removing a compound
- [FIX] Fixed CAPE-OPEN UO Energy Stream connection
- [FIX] Fixed CAPE-OPEN UO error not breaking flowsheet calculation
- [FIX] Fixed CAPE-OPEN UO Array property handling
- [FIX] Fixed data storage for regression case
- [FIX] Fixed display of missing properties on tables
- [FIX] Minor bug fixes and enhancements

Version 4.1 Update 7

- [NEW] Enhanced compatibility with Wine on Linux/macOS
- [CHG] Changed default solver settings
- [CHG] Rewritten Simultaneous Correction and Inside-Out column solvers
- [FIX] Fixed heat balance of Conversion, Equilibrium and Gibbs reactors in Adiabatic Mode
- [FIX] Fixed issues with Flowsheet Block and Rigorous Column editors
- [FIX] Minor bug fixes and enhancements

Version 4.1 Update 6

- [FIX] Added missing libraries

Version 4.1 Update 5

- [FIX] Fixed bugs with Flowsheet and Gas-Liquid Separator Unit Operations

Version 4.1 Update 4

- [FIX] Fixed Recycle convergence issues

Version 4.1 Update 3

- [FIX] Fixed an issue with the Material Stream editor
- [FIX] Fixed CUDA GPU code loading

Version 4.1 Update 2

- [FIX] Fixed Solid Phase Enthalpy/Entropy calculation for some Property Packages
- [CHG] Steam Tables Property Package can now be used in simulations with multiple compounds

Version 4.1 Update 1

- [FIX] Fixed conversion reactor expression parsing

Version 4.1

- [NEW] Added the ability to load, edit and save simulations created on mobile devices (Android/iOS)
- [NEW] Material Stream editor enhancements
- [NEW] Biodiesel production sample
- [NEW] Added more thermophysical phase properties: Isothermal Compressibility, Bulk Modulus, Joule-Thomson Coefficient, Speed of Sound, Internal Energy, Gibbs Free Energy and Helmholtz Free Energy 
- [NEW] New Compound properties menu in Material Stream editor, added Infinite Dilution Diffusivity Coefficients
- [CHG] Updated Molarity/Molality input mechanism
- [FIX] General bug fixes

Version 4.0 Update 35

- [FIX] Fixed bugs with the Rigorous Column model

Version 4.0 Update 34

- [FIX] Fixed PFR & CSTR reaction heats

Version 4.0 Update 33

- [FIX] Fixed bugs with the Compound Creator
- [FIX] Fixed a bug with assignment of formation properties when loading user compounds

Version 4.0 Update 32

- [FIX] FIxed a bug with the Material Stream editor

Version 4.0 Update 31

- [FIX] CAPE-OPEN PP interface workaround for ChemSep
- [FIX] Fixed Sensitivity Analysis variable units
- [CHG] Orifice Plate diameter input

Version 4.0 Update 30

- [NEW] Added the ability to display charts on another window, zoomed copy and save
- [NEW] New Welcome Screen
- [FIX] Fixed PFR/CSTR heat balance
- [FIX] Fixed bugs with the Compound Creator

Version 4.0 Update 28/29

- [FIX] Added missing library from previous update

Version 4.0 Update 27

- [FIX] PFR/CSTR bug fixes
- [FIX] Fixed liquid density for user-defined compounds

Version 4.0 Update 26

- [CHG] CoolProp database compounds can now be used with most Property Packages
- [FIX] Fixed bugs with the CoolProp Property Package
- [FIX] Fixed Shortcut and Rigorous Column connection editors

Version 4.0 Update 25

- [FIX] Fixed bugs with the Steam Tables Property Package

Version 4.0 Update 24

- [FIX] Fixed Splitter Mass/Mole Flow Spec mode

Version 4.0 Update 23

- [FIX] Fixed Steam Tables property calculation on supercritical region
- [FIX] Other minor fixes

Version 4.0 Update 22

- [FIX] Fixed liquid phase reactions on Equilibrium reactors
- [FIX] Fixed Outlet Pressure units on Expander editor

Version 4.0 Update 21

- [FIX] Fixed writing of Spreadsheet Variables to Flowsheet
- [FIX] Fixed some unit conversions
- [CHG] More German translations

Version 4.0 Cumulative Update 2 (Update 20)

Cumulative Update 2 (Update 20) includes all changes from Update 1 to 19 as well as:

- [FIX] Fixed Petroleum Characterization utilities not working
- [CHG] More German translations

Version 4.0 Update 19

- [FIX] Fixed Specification Block calculation
- [FIX] Fixed Solid Phase Enthalpy calculation on Activity Coefficient models

Version 4.0 Update 16/17/18

- [FIX] Fixed PR calculation bug when adding compounds after creating a simulation
- [FIX] Fixed Steam Tables Enthalpy/Entropy calculation
- [FIX] Fixed Rigorous Column Reboiler/Condenser Pressure setting
- [FIX] Fixed Splitter Editor ratio sliders
- [CHG] More German translations

Version 4.0 Update 15

- [FIX] Fixed Steam Tables Property Package
- [FIX] Fixed input for Material Streams connected to Recycles
- [FIX] Fixed Script Editor auto-completion

Version 4.0 Update 14

- [FIX] Fixed number precision loss on saved XML files

Version 4.0 Update 13

- [FIX] Fixed cryptographic issues on FIPS-compliant systems
- [FIX] Fixed opening sample files on read-only installations

Version 4.0 Update 12

- [CHG] Exposing convergence tolerances in Pipe Segment editor

Version 4.0 Update 11

- [FIX] Fixes to Extended UNIQUAC, Sour Water and Steam Tables Property Packages
- [FIX] Fixed Energy Stream connection to Pipe Segment UO

Version 4.0 Cumulative Update 1 (Update 10)

Cumulative Update 1 (Update 10) includes all changes from Update 1 to 9 as well as:

- [FIX] Fixed Electrolyte PVF Flash

Version 4.0 Update 9

- [FIX] Fixed command buttons not showing in Data Regression Utility

Version 4.0 Update 8

- [FIX] Electrolytes subsystem stability and reliability enhancements
- [FIX] Fixed Material Stream, Heater/Cooler, Adjust and Spec editors
- [FIX] Minor bug fixes and enhancements

Version 4.0 Update 7

- [FIX] Fixed Excel Add-In 'CalcProp' function for mass-based properties

Version 4.0 Update 6

- [NEW] Added an option to hide solid phase from list of present phases (for ChemSep compatibility)
- [NEW] Sour Water PP: Added an option to ignore vapor fraction bounds during flash calculations

Version 4.0 Update 5

- [FIX] Fixed missing Compound Spec selection on Rigorous Column editor
- [FIX] Fixed non-converging samples
- [FIX] Fixed bugs in the Flowsheet UO

Version 4.0 Update 4

- [FIX] Fixed loading of DWSIM 3.x simulations with logical blocks
- [FIX] Fixed bugs in the Flowsheet UO

Version 4.0 Update 3

- [FIX] Fixed Gibbs Reactor Editor
- [FIX] Fixed Pipe insulation k calculation
- [FIX] Fixed Material Stream editor composition Clear/Equalize buttons

Version 4.0 Update 2

- [FIX] Fixed issues with Adjust and Simultaneous Adjust Solver

Version 4.0 Update 1

- [FIX] Fixed issues with Adjust and Spec logical blocks

Version 4.0 Release

- [FIX] Fixed enthalpy and entropy calculation for Activity Coefficient-based Property Packages

Version 4.0 Release Candidate 2

- [FIX] Fixed Material Stream flowrate editing
- [FIX] Other bug fixes and enhancements

Version 4.0 Release Candidate 1

- [NEW] Petalas-Aziz pressure drop calculation model now available for Windows 64-bit and Linux 32/64-bit
- [CHG] Updated documentation
- [CHG] Updated CoolProp library to v6.0.0
- [CHG] Pipe Segment editor enhancements
- [FIX] Compound Creator fixes
- [FIX] Other bug fixes and enhancements

Version 4.0 Preview 4

- [FIX] Fixed missing Shortcut Column editor
- [FIX] Restored user-defined interaction parameter funcionality
- [FIX] Fixed ideal gas enthalpy/entropy calculation
- [FIX] Other bug fixes and enhancements

Version 4.0 Preview 3

- [FIX] Bug fixes

Version 4.0 Preview 2

- [NEW] Excel Add-In ribbon menu
- [CHG] Redesigned Material Stream editor
- [FIX] Various bug fixes and enhancements

Version 4.0 Preview 1

- [NEW] Update User Interface
- [NEW] Automatic Updates
- [NEW] Flowsheet utilities: Split/Merge Energy/Material Streams, Resize Objects, Search Bar, Add Rectangle
- [NEW] Export Report data to ODT/ODS files
- [NEW] Heat Exchanger Pinch Point calculation mode
- [NEW] Pipe Segment ambient temperature gradient
- [CHG] Calculation speed improvements up to 50%
- [CHG] Changed Property Tables (Floating, Simple, Master and Linked) behavior
- [CHG] Utilities are now linked to Flowsheet Objects, can be added multiple times and saved/restored with simulation files
- [FIX] Many bug fixes and improvements

Version 3.7 Build 5981

- [FIX] Bug fixes to reactor models
- [FIX] Bug fix for Nested Loops VLLE and Simple LLE flash algorithms
- [FIX] Fixed a bug in the Data Regression utility
- [FIX] Fixed pipe wall conductivity calculation
- [FIX] Fixed CAPE-OPEN Property Package Manager 64-bit registration

Version 3.7 Build 5965

- [NEW] Added more optimizer options to Data Regression utility and Gibbs Minimization flash algorithm
- [NEW] Added the capability of creating electrolyte compounds to the Compound Creator
- [NEW] Added interaction parameter bounds to the Data Regression utility
- [FIX] Several bug fixes and enhancements to PFR and CSTR models
- [FIX] Minor bug fixes

Version 3.7 Build 5952

- [CHG] Changed target .NET Framework to v4.0 for Windows XP compatibility
- [FIX] Bug fixes

Version 3.7 Build 5905

- [NEW] Implemented inline unit conversion for Property Grid
- [NEW] Script Manager comment/uncomment/indent lines
- [CHG] flowsheet object selection rectangle now works from all directions
- [CHG] Updated cut/paste shortcut key behavior
- [FIX] Bug fixes and enhancements

Version 3.7 Build 5901

- [NEW] Added Undo/Redo capability for Flowsheet Objects, Compounds and Property Packages
- [NEW] Added cross-simulation Cut/Copy/Paste capability for Flowsheet Objects
- [NEW] New 'Edit' menu
- [NEW] Added data copy button to Spreadsheet and Material Stream list (Mono only)
- [NEW] Added SRK fugacity GPU calc routine
- [CHG] Minor changes and UI enhancements
- [FIX] Fixed Spreadsheet UO calculation taking too long to finish
- [FIX] Fixed Nested Loops PT/PV Flash mismatch for single-compound mixtures 
- [FIX] General bug fixes

Version 3.6 Build 5884

- [FIX] Performance enhancements and bug fixes

Version 3.6 Build 5876

- [NEW] DWSIM now runs in 64-bit mode on 64-bit operating systems. To run in 32-bit mode, start "DWSIM_32.bat"
- [NEW] New Pipe calculation modes: Specify Outlet Pressure / Outlet Temperature
- [NEW] Sour Water Property Package
- [CHG] Updated User Interface 
- [CHG] Enabled CoolProp Property Package for 64-bit Linux
- [CHG] Changed XML report style
- [FIX] Various stability enhancements and bug fixes when running in Mono mode

Version 3.5 Build 5800

- [NEW] IAPWS-08 Seawater Property Package
- [NEW] Black-Oil Property Package
- [NEW] Debug Object tool
- [CHG] Enhanced Flowsheet Solver to increase calculation speed 
- [CHG] Enhanced Compound Creator to handle black-oil fluids
- [CHG] Updated flowsheet drawing code, added new alignment tools
- [FIX] General bug fixes and optimizations

Version 3.4 Build 5740

- [NEW] Added Spreadsheet Cell support to Sensitivity Analysis and Optimization tools
- [NEW] 'Power' specification added to Pump model
- [NEW] New Heat Exchanger calculation mode
- [NEW] Added API help link to script editors
- [NEW] Added 'Replace' function to compounds added to the simulation
- [NEW] Added an option to backup file when saving an existing simulation
- [CHG] Replaced script editing component to enable code intellisense (Windows only)
- [CHG] Reworked Electrolyte Flash algorithms
- [FIX] Fixed Liquid Phase Enthalpy calculation in Activity Coefficient-based PPs
- [FIX] Fixed Gibbs reactor energy balances
- [FIX] General bug fixes and optimizations

Version 3.4 Build 5644

- [NEW] Added support for Heterogeneous Catalytic reactions
- [NEW] UNIFAC Interaction Parameter Regression Tool
- [NEW] Custom UO input and output variables
- [NEW] Mouse double-click on flowsheet objects opens specific property editor windows
- [NEW] Added three new sample flowsheets
- [CHG] New Welcome Screen
- [CHG] Human-readable error messages
- [CHG] Flowsheet UO compound mapping
- [CHG] Copy and Paste EOS interaction parameters
- [CHG] Minor usability enhancements 
- [CHG] Changed Reid Vapor Pressure calculation procedure
- [FIX] Implemented limiter for parallel conversion reactions
- [FIX] Spec Op general fixes
- [FIX] Fixed conversion and equilibrium reactor expression parsing
- [FIX] General bug fixes

Version 3.4 Build 5608

- [NEW] NIST-MODFAC Property Package (http://trc.nist.gov/TDE/Help/TDE103b/NIST-Modified-UNIFAC-AC-Model.htm)
- [NEW] Model Comparison in Binary Enevelope Utility
- [NEW] New Flowsheet Solver modes
- [NEW] Linked Spreadsheet Table
- [NEW] Activate/Deactivate Flowsheet Objects
- [NEW] Added Input and Output Variables to Custom (Script) Unit Operation
- [NEW] Restore previous successful flowsheet solutions
- [NEW] Equilibrium calculations now use Henry coefficients for non-condensables 
- [NEW] Added support for CAPE-OPEN Array Parameter type
- [NEW] Mouse double-click now opens specific windows for some unit operations
- [NEW] Added window docking menu for Mono compatibility
- [NEW] Export simulation compounds to XML user database
- [CHG] Excel UO is now Spreadsheet UO, supports ODS files and runs on Linux
- [CHG] Speed enhancements for calculations with UNIFAC-type Property Packages
- [CHG] Flash Algorithm "Fast" option is now a Property Package parameter
- [CHG] Changed Flowsheet UO subflowsheet display mode to dockable
- [CHG] Parallelized phase identification routine loop
- [CHG] Optimized Recycle code
- [CHG] Improved Lee-Kesler root finding
- [CHG] New object palette icons
- [FIX] Corrected some Hydrate calculation models
- [FIX] Fixed GPU multithreading lockups
- [FIX] Fixed Sum Rates Absorption Column solver
- [FIX] Minor bug fixes and enhancements

Version 3.3 Build 5517

- [NEW] Flowsheet Unit Operation
- [NEW] Redesigned Nested Loops VLLE flash algorithm
- [NEW] Fast/Rigorous option mode for Nested Loops VLE flash algorithm
- [NEW] Timed simulation script events
- [NEW] Updated Plugin Interface (IUtilityPlugin2), includes new generic function which can be called by scripts
- [CHG] Updated/Redesigned CoolProp interface
- [CHG] Autosave scripts when saving flowsheet
- [FIX] Fixed Heat of Vaporization calculation for ChemSep compounds
- [FIX] Fixed bugs in the PSV sizing utility
- [FIX] General bug fixes and code optimizations

Version 3.3 Build 5488

- [NEW] Excel Unit Operation
- [NEW] IronPython Script Manager
- [CHG] Flowsheet drawing optimizations
- [CHG] Added Energy Stream requirement to Vessel UO to maintain energy balance consistency
- [CHG] Added a confirmation message to add units systems from simulation files
- [CHG] Sensitivity Analysis Utility GUI adjustments
- [CHG] CAPE-OPEN Property Package adjustments
- [CHG] Added Material Stream's phase mole flow information to property grid
- [FIX] (T)xy, (P)xy binary diagram fixes
- [FIX] Fixed Command Line mode issues
- [FIX] Fixed bugs in the Compound Creator utility
- [FIX] Fixed Rigorous Column stage naming
- [FIX] Fixed proxy settings for new version check
- [FIX] Fixed isolated Material Stream not calculating with "Calculate All" command
- [FIX] Minor bug fixes

Version 3.2 Build 5398

- [CHG] Cleaned exception information in log window
- [FIX] General fixes to Parallel CPU/GPU calculations
- [FIX] General fixes and stability enhancements to Three-Phase Flash Algorithms
- [FIX] Fixed Interaction Parameter XML restoring bug
- [FIX] Fixed Recycle Op not updating mole flows
- [FIX] Fixed a report display bug in CAPE-OPEN Unit Operation

Version 3.2 Build 5390

- [NEW] Triangular LLE Phase Envelope Utility
- [NEW] User Compound Database Management Tool
- [NEW] Material Stream data copy through the PFD context menu
- [NEW] Added Enthalpy/Entropy/Cp calculation method selection to Activity Coefficient-based Property Packages.
- [NEW] Added support for hydrate calculations in equilibrium with gas phase only
- [NEW] New interpolation methods for distillation column initial estimates of temperature, vapor and liquid flows
- [CHG] Pure Component Property Viewer improvements
- [CHG] Updates to Help system
- [CHG] Updates to stability tests on three-phase flash algorithms
- [FIX] Implemented missing methods for XML saving/loading of Watch Items
- [FIX] Fixed bugs in the Electrolytes subsystem
- [FIX] Gibbs Reactor model fixes
- [FIX] Minor bug fixes and UI tweaks

Version 3.2 Build 5349

- [NEW] Implemented Pressure-Enthalpy (PH) and Pressure-Vapor Fraction (PVF) Flash algorithms for Electrolyte Property Packages
- [NEW] Added an option to restore Material Streams' default properties
- [NEW] Added "Ignore Vapor Phase" property to the Tank Unit Op
- [CHG] Changed Units System persistence to XML format
- [CHG] Updated calculation procedures in the Petroleum Cold Flow Properties utility
- [CHG] Steam Tables Property Package now works with Water compound from ChemSep database
- [FIX] Restored table property values after loading simulation from a XML file
- [FIX] Minor bug fixes

Version 3.2 Build 5309

- [NEW] Added hydrate calculation method option to the phase envelope utility
- [CHG] Enhanced hydrate models to correctly support hydrocarbons that don't form hydrates

Version 3.2 Build 5296

- [NEW] New Phase Identification Algorithm based on the work of Venkatarathnam et al (http://dx.doi.org/10.1016/j.fluid.2010.12.001)
- [NEW] Enhanced Phase Envelope Utility now shows hydrate curves, dry-basis dew points for wet natural gas mixtures and phase identification boundary for PR and SRK EOS
- [NEW] Added Motor Octane Number (MON) and Methane Number (MN) properties to the Natural Gas Properties Plugin
- [NEW] New flash calculation spec added to the Separator Vessel model
- [CHG] Enhanced NRTL/UNIQUAC interaction parameter estimation procedures, can now estimate temperature dependency
- [CHG] Enhanced flash algorithm initialization
- [CHG] All three-phase flash algorithms now calculate true tree-phase saturation points if required
- [CHG] Enhanced Hydrate Formation Utility with faster calculation times and ChemSep compound support
- [CHG] Updated UNIFAC/MODFAC Property Packages' configuration dialog with UNIFAC group information
- [FIX] Fixed reporting tool
- [FIX] Minor bug fixes

Version 3.1 Build 5265

- [FIX] Mono Runtime compatibility fixes
- [FIX] Fixed Spreadsheet bugs
- [FIX] Fixed bugs in the Adjust and Set logical operations
- [FIX] Fixed PV/TV Flash temperature search limits
- [FIX] Fix for sensitivity analysis window resizing
- [CHG] Updated quickview table
- [CHG] Fixed zoom level of the quickview table at 100%

Version 3.1 Beta Build 5259

- [NEW] Rewritten flowsheet solver for faster calculation times and recycle optimization
- [NEW] CoolProp Property Package (www.coolprop.org)
- [NEW] Simulation Configuration Wizard
- [CHG] Redesigned Online Help file
- [CHG] Activity Coefficient-based Property Packages now default to Ideal Vapor Phase modeling
- [CHG] Properties like Viscosity, Thermal Conductivity and Surface Tension are now calculated using experimental data whenever available
- [CHG] Enhanced Pure Compound Property Viewer to show all T-dependent properties
- [FIX] Restored compatibility with pre-3.0 binary format simulation files (*.dwsim)
- [FIX] Small interface updates and general bug fixes

Version 3.0 Build 5212

- [FIX] Fixed Material Stream molar/volumetric flow input through the Property Grid.

Version 3.0 Build 5209

- [FIX] Fixed ratios not being set in the Splitter Op.
- [FIX] Fixed a small bug in Gibbs PH flash calculation.

Version 3.0 Build 5206

- [NEW] New Nested-Loops Three-Phase algorithm
- [FIX] Various fixes and stability/reliability enhancements for the flash algorithms
- [FIX] Minor bug fixes.

Version 3.0 Build 5162

- [NEW] Added a new "Stream Flow Spec" operation mode for the Splitter Unit Operation.
- [CHG] Workaround for CAPE-OPEN terminate call error during flowsheet close event.
- [CHG] Updated Cudafy.NET.dll to v1.26, compiling CUDA code with CUDA SDK 5.5 and Visual C++ 2012 Express.
- [CHG] Added a trivial solution check to the Gibbs PT flash algorithm.
- [FIX] Fix for zeroing mass and mole flow through CAPE-OPEN calls.
- [FIX] Workaround for Gibbs PT-Flash algorithm reaching maximum number of iterations (happens on single phase regions).
- [FIX] Fixed XML serialization of Double and Single data types.
- [FIX] Minor bug fixes.

Version 3.0 Build 5154

- [NEW] Excel Interface: added functions to return interaction parameters stored in DWSIM for a given binary / model.
- [NEW] Excel Interface: saving and reading settings to/from INI file (stored in "My Documents\DWSIM Application Data") for improved compatibility.
- [FIX] Inside-Out Flash: fixed resulting vapor composition in three-phase PT-Flash.
- [FIX] Separator Vessel: fixed equilibrium calculation for a single compound mixture.
- [FIX] Flowsheet Solver: fixed flash calculation for single compound not following the defined specification.
- [FIX] Fixed equilibrium calculation routines to handle single-compound streams.
- [FIX] Exposing more temperature-dependent pure compound properties through Excel and CAPE-OPEN interfaces.
- [FIX] Excel Interface: Added a function to return compound constants as loaded from the database files.
- [FIX] Fixed a small bug in mixture vapor pressure calculation.
- [FIX] UNIQUAC model: fixed infinite dilution activity coefficient calculation.
- [FIX] Data Regression utility: fixed images not showing in database management buttons.

Version 3.0 Build 5145

- [CHG] Changed the XML simulation loading code, now keeps loading information even if it finds errors in the XML structure
- [CHG] Changed ChemSep database initial directory setting for Excel compatibility if it doesn't find it through registry (defaults to [dwsim]\chemsepdb\chemsep1.xml)
- [FIX] Fixed XML loading of Sensitivity Analysis and Optimization Cases

Version 3.0 Build 5142

- [NEW] User Interaction Parameter database system for NRTL and UNIQUAC models (work in progress)
- [CHG] Compounds and Property Packages can now be added with a double mouse click
- [FIX] Fixed bug #13: Steam Tables - gas-liquid equilibrium in adibatic expander (https://sourceforge.net/apps/mantisbt/dwsim/view.php?id=13)
- [FIX] Fixed bug #14: Sensitivity Analysis Definition cannot be saved (https://sourceforge.net/apps/mantisbt/dwsim/view.php?id=14)
- [FIX] Fixes to the CAPE-OPEN subsystem
- [FIX] Fixed validation of PT-flash calculations
- [FIX] Fixed a bug in the PH-Flash calculation with Gibbs 3P algorithm
- [FIX] Fixed mixing rule for liquid density
- [FIX] Fixed some bugs in the Shortcut Column model
- [FIX] Binary Envelope - y axis scale adjusted to x axis in Txy and Pxy diagrams

Version 3.0 Build 5114

- [NEW] Temperature-dependent VLE/LLE regression for NRTL/UNIQUAC interaction parameters
- [NEW] Excel Interface: created new functions that accept an initial estimate for temperature and pressure calculations
- [CHG] Users can now specify the gibbs energy delta tolerance for validation of flash calculation results
- [CHG] Welcome Screen: Button "Samples" opens a dialog with samples directory selected instead of displaying samples directory.
- [CHG] Enhanced stability of the three-phase Gibbs Minimization flash algorithm
- [FIX] Fixed enthalpy and entropy calculation through Excel interface
- [FIX] Fixed CSTR and Shortcut Column models
- [FIX] Minor bug fixes and enhancements

Version 3.0 Build 5065

- [CHG] Exposed more properties of the Distillation Column to Adjust, Set, Report and Optimization tools
- [FIX] Fixed Excel Thermo Interface
- [FIX] Fixed Material Stream cleaning through CAPE-OPEN commands
- [FIX] Fixed inverted sign on Russell's IO duties results
- [FIX] Fixed stage efficiency on Russell's IO solver
- [FIX] Fixed some Spanish translations
- [FIX] Fixed XML loading of Pipe, Shortcut Column and Master Property Table

Version 3.0 Build 5031

- [NEW] Added Aqueous Electrolyte systems simulation support with two new Property Packages: LIQUAC* and Extended UNIQUAC
- [NEW] New Unit Operations for Solid-handling simulations: Solids Separator and Continuous Cake Filter
- [NEW] Unified code base and single executable for .NET/Mono, compiled for CLR v4.0
- [NEW] New XML simulation file format for full compatibility between platforms (Windows/Linux/OS X)
- [NEW] New Parallel Calculations engine with support for multicore CPUs and CUDA/OpenCL-capable GPUs
- [NEW] 'Copy Data to Clipboard' function added to flowsheet objects
- [CHG] Stability enhancements to Rigorous Column model and Boston-Britt Inside-Out solver
- [CHG] Report tool now fully supports Excel Spreadsheet file format
- [CHG] Added more material stream composition input options: Mass/Mole Flows, Standard Liquid Volumetric Fractions, Molarity and Molality (for electrolyte simulations)
- [CHG] Mixer and Separator Vessel models now supports up to six inlet streams
- [CHG] NRTL/UNIQUAC Interaction Parameters are now temperature-dependent
- [CHG] All Activity coefficient models now include an option to model the Vapor Phase as ideal. The Data Regression utility also supports this option in order to regress data to find the interaction parameters accordingly
- [CHG] The Compound Creator was enhanced to include more Joback/UNIFAC groups, Element information and Solid Phase properties estimation, including temperature-dependent ones (Cp, Density). The regression graphs now show calculated properties for every temperature dependent property
- [CHG] Phase Envelope utilities now have cancellation support
- [CHG] The Binary Envelope Utility was enhanced to show every kind of equilibrium line supported by DWSIM (VLE, LLE, SLE and Critical)
- [CHG] The Data Regression utility now supports fixing some interaction parameters, enhanced LLE /SLE regression
- [CHG] The Pure Compound Property viewer now includes molecular properties, solid properties and property tables
- [FIX] Lots of bug fixes and minor enhancements

Version 2.1 Build 4819

- [FIX] Fixed empty property grid after adding an object to the flowsheet
- [FIX] Fixed Binary Envelope utility "index out of bounds" error
- [FIX] Fixed unit system not being updated after loading from file
- [FIX] Fixed NRTL parameter estimation in the Data Regression utility
- [FIX] Fixed vapor pressure parameters not being saved in the Compound Creator
- [FIX] Fixed NRTL/UNIQUAC parameter estimation between compounds from mixed databases
- [FIX] Fixed boiling point calculation instability in the Binary Envelope utility when two liquid phases are present
- [FIX] Fixed key compound list for instability test not being updated after adding/removing a compound from the simulation
- [FIX] Fixed object property sorting in the Sensitivity Analysis utility
- [FIX] Fixed German translation for the "Advanced" section in Thermodynamics config screen 
- [FIX] Fixed Critical Temperature calculation in the Compound Creator when Normal Boiling Point isn't selected to be calculated by UNIFAC/Joback
- [FIX] Fixed Adjust error function units
- [CHG] Phase Envelope utility is now set to show all calculated points even when an error occurs
- [CHG] Saved file name is now displayed in the window title for Regression and Compound Creator cases

Version 2.1 Build 4768

- [CHG] Various enhancements added to the Binary Envelope utility

Version 2.1 Build 4767

- [CHG] New Material Streams are now added to the flowsheet at standard conditions (25 C, 1 atm, 1 kg/s)
- [CHG] Enhanced stability of the Three-Phase Flash algorithms when there is no vapor phase
- [CHG] More stable NRTL parameter estimation through UNIFAC (fixed alpha12 at 0.3)
- [FIX] Fixed flowsheet page setup for printing
- [FIX] Fixed system of units saving and loading on German and Spanish GUI languages

Version 2.1 Build 4764

- [NEW] Added user-defined units and other minor enhancements/bug fixes to the Compound Creator
- [CHG] Added a message box to warn the user when NRTL/UNIQUAC parameter estimation with UNIFAC method fails
- [CHG] Added an error message to the Information Window when Bubble/Dew point calculation fails
- [FIX] DWSIM now throws an exception when an UNIFAC interaction parameter is not found for a binary during an activity coefficient calculation
- [FIX] Fixed Master Table property list when changing locale setting
- [FIX] Fixed Material Stream's compound mass and mole flow units when displayed on tables and reports

Version 2.1 Build 4762

- [CHG] Added UNIQUAC parameter input to the Compound Creator
- [CHG] Added data point information to the Compound Creator regression viewer
- [FIX] Fixed Compound Creator filename and checkbox persistence

Version 2.1 Build 4761

- [CHG] Enhanced Compound Creator with UNIFAC group pictures and the ability to enter equation coefficients for temperature-dependent properties.
- [CHG] Updated UNIFAC groups and interaction parameters with data from http://www.aim.env.uea.ac.uk/aim/info/UNIFACgroups.html
- [FIX] Fixed CSTR/PFR models

Version 2.1 Build 4753

- [NEW] Added support for multiple dependent variables to the Sensitivity Analysis utility
- [FIX] Fixed Rigorous Column outlet streams' compound mass fractions
- [FIX] Fixed single-compound inlet stream calculation error
- [FIX] Fixed Sensitivity Analysis flowsheet restoring

Version 2.1 Build 4715

- [NEW] Added German translation for the Compound Creator utility (by Gregor Reichert) 
- [FIX] Fixed Material Stream specification not being honored when changing mixture composition
- [FIX] Fixed Pipe Hydraulic Editor form resizing and length/diameter units
- [FIX] Fixed ChemSep database loading in CAPE-OPEN mode
- [FIX] Fixed Data Regression utility
- [FIX] Fixed Heater/Cooler outlet temperature specification bug

Version 2.1 Build 4698

- [CHG] Enhanced Element Matrix and Initial Estimates editor for the Gibbs Reactor

Version 2.1 Build 4680

- [NEW] Save selected object properties to text file
- [FIX] Fixed PFR, CSTR and Pump models
- [FIX] Fixed instability on the Nested Loop PH Flash code
- [FIX] Fixed pressure unit conversion from barg to Pa

Version 2.1 Build 4606

- [CHG] COSMO-SAC database loading is now done only on-demand instead of during startup
- [CHG] Petalas-Aziz pressure drop model now uses a native library by the authors

Version 2.1 Build 4605

- [NEW] Simultaneous Adjust Solver
- [NEW] Added the option to edit pure compound properties through the Pure Compound Properties utility
- [NEW] Added automatic calculation of PR and SRK Peneloux volume translation (shift) coefficients for pseudocomponents
- [FIX] Several fixes to the Pipe Segment (and pressure drop) model, includes Joule-Thomson cooling option
- [FIX] Fixed drawing of Adjust's line connectors

Version 2.1 Build 4602

- [NEW] Added Peneloux volume translation support for PR and SRK Property Packages (Configure Property Package > General Options > Use Peneloux Volume translation => set to 1)
- [FIX] Fixed material stream composition editing in commmand line mode
- [FIX] Fixed ambient temperature not being set on Thermal Profile Editor (Pipe Segment)

Version 2.1 Build 4589

- [CHG] Enabled calculation of material streams when there is no mass/mole flow
- [FIX] Fixed mixer calculation when some of the inlet streams have no flow
- [FIX] Fixed a bug with the mass balance in the gas-liquid separator

Version 2.1 Build 4569

- [NEW] Save/Restore simulation states
- [NEW] Capture flowsheet snapshot and send to clipboard
- [FIX] Fixed command line run mode
- [FIX] Fixed heat capacity coefficients generated by the Compound Creator
- [FIX] Minor fixes to petroleum charact. utilities

Version 2.1 Build 4534

- [FIX] More fixes and enhancements to the flash algorithms
- [FIX] Fixed a bug in the excel interface
- [NEW] Added "reset settings" functionality by pressing Shift during startup

Version 2.1 Build 4526

- [NEW] Added Lua scripting support
- [FIX] Further fixes and improvements to the flash algorithms
- [FIX] Fixed ChemSep column operation with pseudocomponents/hypotheticals
- [FIX] Fixed Zc/Vc calculation for pseudocomponents
- [FIX] Fixed some bugs in the CAPE-OPEN Thermo interfaces

Version 2.1 Build 4513

- [FIX] Enhanced stability and reliability for Pressure-Enthalpy flash calculations

Version 2.1 Build 4503

- [NEW] FPROPS Property Package (needs testing)
- [NEW] Gibbs Minimization (experimental stage!), Hybrid Nested Loops / Inside Out flash algorithms
- [NEW] Added convergence information (error value, time taken and iteration count) for all flash algorithms
- [FIX] Fixed IO PH/PS flash calculation for single compounds
- [FIX] Fixed Property Grid issues with some unit operations and Spanish language
- [FIX] Fixed a bug with energy stream connection to rigorous columns

Version 2.1 Build 4466

- [NEW] Added custom object ordering feature to the Master Table
- [NEW] Added more mole flow units of measure
- [NEW] Added the possibility of removing multiple compounds from the simulation at once
- [FIX] Fixed ChemSep database registry search
- [FIX] Minor translation fixes

Version 2.1 Build 4463

- [FIX] Fixed calculation of ideal gas heat capacity for petroleum fractions

Version 2.1 Build 4452

- [NEW] Petroleum Assay Manager (store/reload/import/export bulk and distillation characterization assay data)
- [NEW] Master Property Table object ordering by property or name ascending/descending
- [NEW] PFD Zoom All, Pan (Shift + Left mouse button)
- [NEW] Area unit conversion to the Heat Exchanger
- [FIX] Fixed PFD Select and Center Object

Version 2.1 Build 4442

- [NEW] Added the Master Property Table to display grouped properties from objects of the same type
- [NEW] Added save-to-image flowsheet feature
- [NEW] Added bubble and dew points to Material Stream's property list
- [NEW] Added the capability of editing custom system of units
- [NEW] Added a keyboard shortcut (Ctrl+E) to edit material stream compositions
- [NEW] Added the capability of showing/hiding table items from the Property Grid

Version 2.1 Build 4438

- [CHG] Changed Adjust variables' units to match the ones in the selected system of units
- [FIX] Fixed custom system of units not being restored
- [FIX] Fixed Adjust behavior
- [FIX] Fixed Flowsheet printing
- [FIX] Fixed Steam Tables Property Package PVF/TVF stream spec calculation
- [FIX] Fixed Bubble and Dew points not being shown if the stream state is single phase
- [FIX] Fixed WFP donation window always showing
- [FIX] Fixed duplicate Separator Op in the Object Palette

Version 2.1 Build 4422

- [CHG] Overall speed and usability improvements
- [NEW] Added drag-and-drop support for opening simulations/cases from Windows Explorer

Version 2.1 Build 4416

- [CHG] Includes ChemSep Lite 6.90, now with 400+ compound database and 40 compounds/300 stages Column model
- [NEW] Includes PRSV2 Property Package with Van Laar-type mixing rule (original PRSV2 w/ Margules MR is now PRSV2-M)
- [FIX] Fixed PRSV2 compresibility factor calculation

Version 2.1 Beta Build 4410

- [NEW] New Property Package: Peng-Robinson-Stryjek-Vera 2 (PRSV2)
- [CHG] Added missing data estimation feature to the Data Regression Utility
- [CHG] Enhanced Unit Set Creator to start with units from the current system

Version 2.1 Beta Build 4407

- [CHG] Added LLE support and initial estimates calculation using UNIFAC structure to the Data Regression Utility
- [CHG] Changed density input from Specific Gravity to API Gravity on the Dist. Curves Characterization Utility
- [FIX] Fixed solver not recalculating outlet streams
- [FIX] Fixed infinite volumetric flow on first Material Stream calculation through the property grid
- [FIX] Fixed Adjust and Spec Ops not showing anything on the property grid

Version 2.1 Beta Build 4404

- [NEW] Added the Compound Creator Utility and corresponding user database structure (work in progress)
- [NEW] Added a Binary Data Regression utility (work in progress)
- [NEW] Added feature request #3409646 (Name given to Material Objects)
- [CHG] Rewritten solver logic increases calculation speed by 35% (Cavett sample)
- [FIX] Fixed Lee-Kesler Cp/Cv calculation
- [FIX] Fixed Distillation Curves Petroleum Characterization Utility
- [FIX] Fixed Pipe Segment model overall HTC calculation and liquid phase volume retrieval
- [FIX] Fixed Spanish translation of the Object Palette
- [FIX] Fixed bug #3409641 (Call to Terminate missing)
- [FIX] Fixed bug #3409637 (Collection Count method called twice in a row)
- [FIX] Fixed bug #3409628 (ICapeUtilities queried twice in a row)

Version 2.0 Build 4258

- [FIX] Fixed unit conversion from lbmol/h to mol/s
- [FIX] Fixed empty compound list array when adding DWSIM Property Packages to CAPE-OPEN simulators
- [FIX] Fixed calculation of activity coefficient when requested from external components (Excel/CAPE-OPEN)
- [FIX] Fixed Energy Stream parameter list (CAPE-OPEN)

Version 2.0 Build 4252

- [FIX] Fixed Compressor and Expander models for single component simulations
- [FIX] Fixed a bug that caused DWSIM to throw an exception on startup related to user-created unit systems
- [FIX] Fixed the negative temperature input bug on Heater and Cooler models

Version 2.0 Build 4251

- [FIX] Fixed the reference state for enthalpy and entropy calculations

Version 2.0 Build 4249

- [NEW] Excel Interface for Equilibrium and Property calculators
- [FIX] Minor bug fixes

Version 2.0 Build 4235

- [FIX] Fixed Shell and Tube Heat Exchanger shell side pressure drop calculation
- [NEW] Added an option to save simulations with password protection
- [CHG] Centralized flowsheet drawing surface (PFD)

Version 2.0 Build 4225

- [FIX] Fixed Shell and Tube Heat Exchanger model
- [FIX] Fixed Units of measure in Watch Panel
- [CHG] Changed update checker to show only a non-obtrusive link on the status panel

Version 2.0 Build 4220

- [NEW] Added a set of shapes for CAPE-OPEN Unit Operations in the flowsheet
- [FIX] Fixed CAPE-OPEN compliancy of the Steam Tables Property Package 
- [FIX] Corrected CAPE-OPEN Report Window behavior

Version 2.0 Build 4207

- [NEW] Support for CAPE-OPEN Unit Operations, Property Packages (1.0/1.1) and Plugins (Flowsheet Monitoring Objects)
- [NEW] DWSIM Property Packages can now be exposed to external CAPE-OPEN compliant simulators
- [NEW] Inside-Out Three-Phase (VLLE) Flash Algorithm
- [NEW] PC-SAFT (without association term) Property Package
- [NEW] UNIFAC Property Package with Liquid-Liquid interaction parameters
- [NEW] Liquid-Liquid Extractor operation mode for the Absorption Column
- [NEW] Three-Phase separation mode for the Vessel
- [NEW] ChemSep database automatic loading
- [NEW] Watch window - allows property monitoring from different objects at the same time
- [NEW] CAPE-OPEN Unit Reports window - view output from CAPE-OPEN Unit Operations
- [NEW] Updated flowsheet drawing theme
- [NEW] 'Send Error Info' button added to the Unhandled Exception window
- [NEW] New version checking tool - informs the user when a new version becomes available and downloads the setup file
- [FIX] General bug fixes and speed improvements

Version 1.8 Build 4101

- [NEW] 'Fouling Factor' calculation mode for the Heat Exchanger Shell and Tube model
- [NEW] Added non-linear solver IPOPT to the Optimizer
- [NEW] DWSIM now reads experimental liquid density and liquid thermal conductivity data for ChemSep components (enabled by default)
- [NEW] Added multiple selection capability to the flowsheet to enable moving multiple objects at once
- [NEW] Added a 'Snap to Grid' capability to the flowsheet for better object alignment
- [CHG] Reactivated flowsheet navigation through the arrow keys
- [CHG] Reactivated the quick connect tool on the flowsheet
- [CHG] General Heat Exchanger model improvements
- [FIX] Fixed 'lbmol/h' unit conversion from SI to English

Version 1.8 Build 4080

- [NEW] Model for rating Shell and Tube Heat Exchangers
- [NEW] Scripting support for pre- and post- Unit Op calculations
- [NEW] Console Output and Calculation Queue windows
- [NEW] "Fast mode" switch for Inside-Out Flash calculations

Version 1.8 Build 3947

- [NEW] Paste from Excel function (Ctrl-V) added to the Composition Editor
- [FIX] Fixed liquid density and water content calculations in Peng-Robinson IWVT Property Package
- [FIX] Fixed Grayson-Streed fugacity calculation
- [CHG] Added ChemSep component support to the COSMO-SAC Property Package

Version 1.8 Build 3938

- [NEW] New converter in Property Grid for some units (temperature, pressure, flow rates, etc.)
- [NEW] New gauge pressure units
- [FIX] Fixed a cut/paste bug in the script editor
- [FIX] Fixed wrong molar/mass fraction values in flowsheet tables

Version 1.8 Build 3922

- [NEW] COSMO-SAC Property Package based on the JCOSMO library (http://code.google.com/p/jcosmo/)
- [CHG] Improvements to the Custom UO script editor
- [CHG] Updated Plugin Interface definition

Version 1.8 Build 3908

- [NEW] Added IronPython, IronRuby, VBScript and JScript scripting support
- [NEW] Added a new Unit Operation: Custom UO, which lets the user run scripts as an unit operation calculation routine

Version 1.7 Build 3875

- [NEW] Added German translation by Rainer Gollnitz

Version 1.7 Build 3868

- [NEW] Interface definition for external plugins
- [FIX] Updated Rigorous Column solvers (Inside-Out and Simultaneous Correction)
- [FIX] Updated Critical Point calculation

Version 1.7 Build 3850

- [NEW] Lee-Kesler-Pl�cker Property Package
- [FIX] Fixed K-value calculation call in the Sum Rates method for solving Absorption Columns
- [FIX] Fixed IO Flash calculation in single phase region
- [FIX] Fixed Critical Point calculation with PR and SRK Equations of State
- [FIX] Fixed portions of GUI language that were not being set on the first run

Version 1.7 Build 3840

- [NEW] Gibbs Reactor model (vapor phase only) with two solving methods: reaction extents and direct minimization
- [NEW] New global settings for Property Packages: Flash Algorithm and Calculate Bubble/Dew points
- [NEW] New approach for equilibrium calculation: Inside-Out by Boston and Britt
- [NEW] Added an option to adjust Rackett Parameters and Acentric Factors to match SG and NBP in Petroleum Characterization Utilities
- [NEW] New Quick Settings toolbar: Unit system and number formatting
- [NEW] New menu gives quick access to DWSIM Tools
- [NEW] Added the option to select stream component amounts as properties to show on PFD tables
- [CHG] Completely rewritten Equilibrium Reactor model
- [CHG] Updated ChemSep database loading code to support ChemSep 6.62
- [FIX] Fixed an exception when working with pump curves to calculate pump power
- [FIX] Fixed a bug in temperature calculation with the Steam Tables Property Package

Version 1.6 Build 3756

- [FIX] Fixed Conversion Reactor calculation with more than one reaction in parallel
- [FIX] Fixed Separator Vessel calculation under normal flash settings (Force PH flash option not selected)

Version 1.6 Build 3752

- [NEW] Component Separator Unit Op
- [NEW] Orifice Plate Unit Op
- [CHG] Improvements to the Optimizer, new solvers, more control options
- [CHG] Changed object insertion method from strip buttons to menu items, enhanced object palette to include new view modes
- [FIX] Fixed problems with rigorous column stream connections
- [FIX] Fixed Pump curves feature
- [FIX] Fixed Shortcut Column unhandled exception

Version 1.6 Build 3676

- [NEW] Added new rigorous column specification options
- [NEW] New rigorous column solving method: Napthali-Sandholm Simultaneous Correction (SC)
- [NEW] Added curves support to the Pump unit op
- [NEW] Added expression support to the Sensitivity Analysis Utility
- [FIX] Corrected Cooler and Heater heat exchanged sign
- [CHG] Various Flowsheet improvements (new font and color scheme, new table style, calculation indicator, improved connector drawing algorithm) 
- [CHG] General improvements to the IO Method, new configuration options
- [CHG] Added co/countercurrent flow direction selector to the Heat Exchanger
- [FIX] Fixed Pipe heat loss calculations
- [FIX] Fixed some translation errors
- [FIX] Removed degree symbol from Celsius temperature unit to improve compatibility with foreign languages
- [FIX] Fixed PH Flash setting in Thermo & Reactions configuration section not being effective
- [FIX] Optimizer/Sensitivity utilites: Fixed Material Streams' molar and volumetric flow changes not being effective
- [FIX] Fixed temperature calculations in the Steam Tables property package
- [FIX] Fixed Heat Exchanger Area calculation
- [FIX] Fixed Pipe properties not showing when the GUI language is set to Spanish

Version 1.6 Build 3618

- [FIX] Fixed control placement in the composition editor when using Spanish locale settings

Version 1.6 Build 3605

- [NEW] Added Spanish GUI translation (many thanks to Abad Lira and Gustavo Le�n!)
- [NEW] Added a Multivariate, Constrained Optimization utility
- [NEW] Added a Sensitivity Analysis utility supporting up to 2 independent variables
- [NEW] Added "command-line run mode" (read the documentation for more details)
- [NEW] Added a 'Write to the Flowsheet' capability to the Spreadsheet
- [NEW] New Property Packages: Chao-Seader, Grayson-Streed, Modified UNIFAC (Dortmund) and Peng-Robinson with support for immiscible water and Volume Translation
- [NEW] New Energy Recycle unit operation
- [NEW] Added more units for the most commom properties (temperature, pressure, etc.)
- [NEW] Added code to display a message when the flash algorithm converges to the trivial solution
- [CHG] Redesigned UNIFAC group structure to include all available groups
- [CHG] Report can now show all properties from all objects in the flowsheet
- [CHG] Removed single-phase and phase change limitations from the Heat Exchanger
- [CHG] Property Package selection interface now groups packages by type
- [CHG] Changed component selection interface
- [FIX] Fixed bugs and made minor changes to the code/interface

Version 1.5 Build 3399

- [FIX] Corrected Ideal Gas Enthalpy/Entropy calculation for CheResources and ChemSep database components
- [FIX] Corrected heat duty sign in Equilibrium Reactor
- [FIX] Corrected a unit inconsistency in Rigorous Column heat balances

Version 1.5 Build 3398

- [NEW] Added component volumetric fraction / volumetric flow information to Material Streams
- [NEW] Added a liquid density calculation mode to EOS-specific Property Packages (EOS or Rackett) - this also affects partial volume calculations
- [FIX] Fixed bug #2750848 (null reference error when adding components)
- [FIX] Fixed the 'Recalculate All' calculator feature - it wasn't recalculating input streams
- [FIX] Fixed some errors in the Equilibrium Reactor calculation routine
- [FIX] Fixed some errors when connecting product streams to Absorbers
- [FIX] Added a default value of zero for the UNIFAC groups in the Component Creator to avoid null reference errors

Version 1.5 Build 3377

- [NEW] New Calculator features: Break Calculation, Recalculate All and Clear Queue List
- [NEW] Added information about calculation time
- [FIX] Fixed positioning of rigorous column connections
- [FIX] Fixed UNIFAC Property Package configuration error
- [FIX] Fixed rigorous column 'Decalculate' routine
- [FIX] Fixed stability curve (phase envelope utility) calculation issues
- [FIX] Rigorous columns are now recalculated when editing properties in modal windows
- [CHG] Changed calculator behavior so the interface is more responsive

Version 1.5 Build 3372

- [FIX] Object selection by mouse dragging now updates the property grid correctly
- [FIX] Changed liquid viscosity mixing rule (for multicomponent systems)
- [FIX] Fixed an error in liquid mixture surface tension calculation
- [FIX] Fixed null Viscosity error in C7+ characterization tool
- [FIX] Fixed PRLK Property Package configuration error
- [FIX] Fixed a bug in rigorous column condenser connections
- [FIX] Fixed n-Butane database parameter (liquid viscosity)
- [FIX] Fixed rigorous column full reflux condenser behavior
- [FIX] Fixed vessel and tank null object reference error
- [CHG] Speed improvements in column IO method

Version 1.5 Build 3353

- [NEW] Redesigned component database system
- [NEW] Added two new Property Packages: NRTL/Peng-Robinson and UNIQUAC/Peng-Robinson
- [NEW] Added support for loading ChemSep(TM) databases
- [NEW] Added a tool to characterize petroleum fractions from ASTM/TBP distillation curves
- [NEW] Added a tool to insert user-defined components
- [NEW] Added the ability to use multiple property packages in a single simulation
- [NEW] Dockable help window with localized tips
- [NEW] New Unit Operations: Distillation Column, Absorption Column, Refluxed Absorber and Reboiled Absorber
- [NEW] Rigorous Column solving methods: Bubble-Point, Sum-Rates and Inside-Out
- [NEW] New utility for calculation of Petroleum Cold Flow Properties
- [CHG] Surface Tension is now correctly listed as a liquid phase property
- [CHG] Redesigned splash and welcome screens
- [FIX] Fixed high-pressure vapor viscosity calculation
- [FIX] Fixed liquid viscosity calculation error when supercritical components are present
- [FIX] Fixed flash calculation vapor fraction initialization error (should avoid some impossible solutions)
- [FIX] Fixed culture-specific error related to Property Package parameter storage
- [FIX] Fixed calculation of vapor phase thermal conductivity
- [FIX] Fixed a bug in internal calculation of heat capacity ratio
- [FIX] Fixed MRU filelist

Version 1.4

- [NEW] Added three new Unit Operations: Shortcut Column, Equilibrium Reactor and a basic Heat Exchanger
- [NEW] Added three new utilities: PSV/Vessel Sizing and Spreadsheet
- [CHG] New drag-and-drop feature for adding objects to flowsheet
- [CHG] Some cosmetic changes in the PFD
- [CHG] The critical point utility was modified to calculate multiple critical points when they exists. It's (still) not perfect, but it should work well in most cases.
- [CHG] Petroleum Characterization Utility: temperature-dependent properties are now calculated only when requested, greatly improving the speed of the pseudocomponent creation process.
- [FIX] Corrected the PFD object numbering bug

Version 1.3

- This version is the first to be released under the GPL v3 license
- Added support for English language and translation. Contact the developer for more information.
- Added reactions (conversion, kinetic, equilibrium) and reactors support (Conversion, PFR, CSTR)
- Removed Krypton Toolkit controls in order to mantain consistency of the GUI as a whole
- English reporting now works correctly

- Corrected many other bugs from the previous version