/*
 *                      Yeppp! library implementation
 *
 * This file is part of Yeppp! library and licensed under the New BSD license.
 * See LICENSE.txt for the full text of the license.
 */

using System.Reflection;

#if YEP_BUNDLE_LIBRARY
	[assembly: AssemblyTitle("Yeppp.CLR.Bundle")]
#else
	[assembly: AssemblyTitle("Yeppp.CLR")]
#endif
[assembly: AssemblyDescription("CLR/.Net bindings for Yeppp! library")]
#if YEP_BUNDLE_LIBRARY
	[assembly: AssemblyConfiguration("Bundle")]
#else
	[assembly: AssemblyConfiguration("Bindings")]
#endif
[assembly: AssemblyCompany("Georgia Institute of Technology")]
#if YEP_BUNDLE_LIBRARY
	[assembly: AssemblyProduct("Yeppp! library (multi-platform CLR bundle)")]
#else
	[assembly: AssemblyProduct("Yeppp! library (CLR support glue)")]
#endif
[assembly: AssemblyCopyright("© Marat Dukhan 2010-2012\r\n© Georgia Institute of Technology 2012-2014")]
[assembly: AssemblyTrademark("")]
[assembly: AssemblyCulture("")]

[assembly: System.Runtime.InteropServices.ComVisible(false)]
