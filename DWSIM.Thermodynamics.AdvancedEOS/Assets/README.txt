Advanced EOS Library for DWSIM
Copyright 2017 Daniel Medeiros
==============================

ABOUT

The Advanced EOS Library is a bridge which connects DWSIM on Windows and Linux to a collection of open-source MATLAB/Octave routines which implement various Equations of State, including PC-SAFT with support for associating compounds and the Perturbed-Hard-Sphere-Chain (PHSC).

The original project (http://hpp.uva.es/open-source-software-eos/) consists in a set of MATLAB programs for the calculation of phase equilibrium and other thermodynamic properties using different equations of state (classical cubic equations, cubic equations with excess Gibbs energy mixing rules, group contribution equations and SAFT equations). All programs are open-source and have been designed to be easily reusable using an object-oriented programming methodology.

This library exposes these programs to DWSIM as fully-implemented Property Packages by running them in Octave (https://www.gnu.org/software/octave/) and reading the results through a C# interface (https://www.codeproject.com/Articles/342007/OctaveSharp-Running-GNU-Octave-with-Csharp).

AVAILABLE EQUATIONS OF STATE

- Perturbed-Chain Statistical Associating Fluid Theory (PC-SAFT) with associating compounds support
- Statistical Associating Fluid Theory (Original)
- Perturbed Hard-Sphere-Chain (PHSC)
- Peng-Robinson with Wong-Sandler mixing rules (PRWS)
- Valderrama-Patel-Teja (VPT)

LICENSE

This library is licensed under the terms of the GPL license Version 3.

DISCLAIMER

The data and information within DWSIM has been obtained from a wide variety of literature sources. While reasonable care has been exercised in the collection of data and testing of this software, the author and contributors of the DWSIM Project disclaims any warranty, expressed or implied, as to the accuracy or reliability of the data or calculations contained therein. The results of calculations obtained from DWSIM yield approximate results, which will not always be suitable for every application. The software is designed for use by trained professional personnel and is not a substitute for sound professional judgment. It is the sole responsibility of the user to validate the data presented by DWSIM and to determine whether the results of this program are accurate and suitable for any specific purpose. No guarantee of accuracy or fitness for any purpose is expressed or implied. The author and contributors strongly recommends that the data be checked against other sources and/or methods before use and application. DWSIM's author and its contributors shall not be held liable for any direct, indirect, consequential or incidental damages incurred through use of the data or calculations.

SOURCE

The C# source code is available at https://github.com/DanWBR/dwsim4/tree/master/DWSIM.Thermodynamics.AdvancedEOS