DWSIM Standalone Thermodynamics Library
Version 6.5.3
Copyright 2016-2021 Daniel Medeiros

The DWSIM Standalone Thermodynamics Library is a .NET/Mono managed dynamic link library (DLL) that exposes DWSIM’s thermodynamics engine to external applications using a simple programming interface, with no dependency on external components. 

DWSIM Standalone Thermodynamics Library is free for commercial and non-commercial use. Read the license.txt file for more details.

Usage

To use the library in your .NET projects, add a reference to the DWSIM.Thermodynamics.StandaloneLibrary.dll file. All calculation functions will be available in the DTL.Thermodynamics namespace, inside the 'Calculator' class.

To use the library in other languages/ecosystems through its COM interface, you must register it with Administrator privileges using RegAsm, which can be found in your .NET 4.0 installation directory (usually C:\Windows\Microsoft.NET\Framework\v4.0.30319\). After finding the tool, open a DOS console window and run the following command:

"RegAsm.exe /tlb:DWSIM.Thermodynamics.StandaloneLibrary.tlb DWSIM.Thermodynamics.StandaloneLibrary.dll"

This will create and register the type library automatically, making it callable through COM.

Methods

This library has methods to calculate:

- Single Compound Properties
- Single Phase Mixture Properties
- PT, PH, PS, PVF and TVF Equilibrium Flashes, using an algorithm of your choice (two or three phases)

Property and Equilibrium calculation functions require parameters that must be one or more values returned by GetPropPackList, GetCompoundList, GetPropList, GetCompoundConstPropList, GetCompoundTDepPropList, GetCompoundPDepPropList and GetPhaseList. They are self-explanatory, and will return values in an array of strings.

For instance, the PTFlash function requires the name of the Property Package to use, the compound names and mole fractions, temperature in K, pressure in Pa and you may optionally provide new interaction parameters that will override the ones used internally by the library. The calculation results will be returned as a (n+2) x (3) string matrix, where n is the number of compounds. First row will contain the phase names, the second will contain the phase mole fractions and the other lines will contain the compound mole fractions in the corresponding phases.

For PH, PS, TVF and PVF flash calculation functions, an additional line is returned that will contain the temperature in K or pressure in Pa in the last matrix column.