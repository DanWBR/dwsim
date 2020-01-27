using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace DWSIM.UI.Desktop.Shared
{
    public class StringArrays
    {
        public static String[] renderer()
        {
            return new String[] { "Software (CPU)", "Hardware (OpenGL)" };
        }
        public static String[] proppacks()
        {
            return new String[] { "Peng-Robinson (PR)", "Soave-Redlich-Kwong (SRK)", "Raoult\'s Law", "NRTL", "UNIQUAC", "IAPWS-IF97 Steam Tables", "Modified UNIFAC (Dortmund)", "Lee-Kesler-Plöcker", "Chao-Seader", "CoolProp" };
        }
        public static String[] flashalg()
        {
            return new String[] { "Two-Phase VLE", "Three-Phase VLLE" };
        }
        public static String[] reactionphase()
        {
            return new String[] { "Mixture", "Vapor", "Liquid" };
        }
        public static String[] unitssystem()
        {
            return new String[] { "SI", "CGS", "ENG", "User" };
        }
        public static String[] mscompinputtype()
        {
            return new String[] { "Molar Fractions", "Mass Fractions", "Molar Flows", "Mass Flows" };
        }
        public static String[] flash_spec()
        {
            return new String[] { "Temperature/Pressure (TP)", "Temperature/VaporFraction (TVF)", "Pressure/VaporFraction (PVF)", "Pressure/Enthalpy (PH)", "Pressure/Entropy (PS)" };
        }
        public static String[] numberformat()
        {
            return new String[] { "G", "N", "N2", "N4", "N6", "R", "E", "E1", "E2", "E3", "E4", "E6", "#0.0#", "#0.0##", "#0.0###", "#0.0####", "#0.0#####", "#0.0######" };
        }
        public static String[] fontsize()
        {
            return new String[] { "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20" };
        }
        public static String[] envelopetype()
        {
            return new String[] { "Pressure-Temperature", "Pressure-Enthalpy", "Pressure-Entropy", "Temperature-Enthalpy", "Temperature-Entropy", "Volume-Pressure", "Volume-Temperature" };
        }
        public static String[] binaryenvelopetype()
        {
            return new String[] { "P-x/y", "T-x/y" };
        }
        public static String[] valvecalcmode()
        {
            return new String[] { "Outlet Pressure", "Pressure Drop", "Liquid Service Kv (IEC 60534)", "Gas Service Kv (IEC 60534)", "Steam Service Kv (IEC 60534)"  };
        }
        public static String[] pumpcalcmode()
        {
            return new String[] { "Outlet Pressure", "Pressure Increase", "Power", "Energy Stream", "Defined Curves" };
        }
        public static String[] condensertype()
        {
            return new String[] { "Total", "Partial" };
        }
        public static String[] heatercalcmode()
        {
            return new String[] { "Heat Added/Removed", "Outlet Temperature", "Outlet Quality" };
        }
        public static String[] comprcalcmode()
        {
            return new String[] { "Outlet Pressure", "Pressure Variation", "Power Required", "Energy Stream", "Known Head","Performance Curves" };
        }
        public static String[] expndrcalcmode()
        {
            return new String[] { "Outlet Pressure", "Pressure Variation", "Power Generated", "Known Head", "Performance Curves" };
        }
        public static String[] hxcalcmode()
        {
            return new String[] { "Calculate Hot Outlet Temperature",
            "Calculate Cold Outlet Temperature", 
            "Calculate Both Temperatures", 
            "Calculate Both Temperatures (UA)", 
            "Calculate Area",
            "Shell and Tube (Rating)",
            "Shell and Tube (Design)",
            "Pinch Point",
            "Thermal Efficiency" }; 
        }
        public static String[] hxflowdir()
        {
            return new String[] { "Parallel Flow", "Counter Flow" };
        }
        public static String[] hxspectemp()
        {
            return new String[] { "Cold Fluid", "Hot Fluid" };
        }
        public static String[] mixercalcmode()
        {
            return new String[] { "Inlet Minimum", "Inlet Average", "Inlet Maximum" };
        }
        public static String[] splittercalcmode()
        {
            return new String[] { "Split Ratios", "Stream Mass Flow", "Stream Mole Flow" };
        }
        public static String[] rctcalcmode()
        {
            return new String[] { "Adiabatic", "Isothermic", "Outlet Temperature" };
        }
        public static String[] csepspecstream()
        {
            return new String[] { "Stream 1", "Stream 2" };
        }
        public static String[] csepspectype()
        {
            return new String[] { "% Inlet Mass Flow", "% Inlet Mass Flow", "Absolute Mass Flow", "Absolute Mole Flow" };
        }
        public static String[] cspecunit()
        {
            return new String[] { "-", "%", "g/s", "kg/s", "kg/h", "kg/d", "kg/min", "lb/s", "lb/min", "lb/h", "mol/s", "lbmol/h" };
        }
        public static String[] reactionbasis()
        {
            return new String[] { "Activity", "Fugacity", "Mass Concentration", "Mass Fraction", "Molar Concentration", "Molar Fraction", "Partial Pressure" };
        }
        public static String[] insulationmaterial()
        {
            return new String[] { "Asphalt", "Concrete", "Polyurethane Foam", "PVC Foam", "Fiberglass", "Plastic", "Glass", "User-Defined" };
        }
        public static String[] external_env()
        {
            return new String[] { "Air", "Water", "Gravel", "Stones", "Dry Soil", "Moist Soil" };
        }
        public static String[] thermalprofiletype()
        {
            return new String[] { "Defined HTC", "Defined Heat Exchange", "Calculated HTC" };
        }
        public static String[] pipecalcmode()
        {
            return new String[] { "Specify Length", "Specify Outlet Pressure", "Specify Outlet Temperature" };
        }
        public static String[] columnspec()
        {
            return new String[] { "Product Molar Flow Rate", "Product Mass Flow Rate", "Reflux/Boil-Up Ratio", "Heat Load", "Compound Mass Flow", "Compound Molar Flow", "Compound Recovery", "Compound Fraction", "Temperature" };
        }
        public static String[] petroleumtype()
        {
            return new String[] { "Light", "Average", "Heavy" };
        }

        public static String AboutInformation()
        {
            return "DISCLAIMER\n\nThe data and information within DWSIM has been obtained from a wide variety of literature sources." +
    "While reasonable care has been exercised in the collection of data and testing of this software, the author and contributors of the DWSIM Project" +
    "disclaims any warranty, expressed or implied, as to the accuracy or reliability of the data or calculations contained therein. The results of" +
    "calculations obtained from DWSIM yield approximate results, which will not always be suitable for every application. The software is designed" +
    "for use by trained professional personnel and is not a substitute for sound professional judgment. It is the sole responsibility of the user" +
    "to validate the data presented by DWSIM and to determine whether the results of this program are accurate and suitable for any specific purpose." +
    "No guarantee of accuracy or fitness for any purpose is expressed or implied. The author and contributors strongly recommends that the data be checked" +
    "against other sources and/or methods before use and application. DWSIM\'s author and its contributors shall not be held liable for any direct, indirect," +
    "consequential or incidental damages incurred through use of the data or calculations.\n\nCOMPOUND DATABASE\n\nDWSIM uses the Standard Compound Database " +
    "from ChemSep LITE (http://www.chemsep.org), distributed under the Perl Artistic License v2 (https://opensource.org/licenses/Artistic-2.0)." +
    "\n\nCOOLPROP COMPOUND DATABASE\n\nFor CoolProp compound database licensing terms, please visit http://www.coolprop.org" +
    "\n\nChEDL THERMO COMPOUND DATABASE\n\nFor ChEDL Thermo compound database licensing terms, please visit https://github.com/CalebBell/thermo";
        }

        public static String Supporters()
        {
            return "Splash Screen design by Wendel Marcus.\n\nFlowsheet Object icon design by Gustavo León.";
        }


        public static String CreateTitle()
        {
            return "Create New";
        }
        public static String CreateDesc()
        {
            return "Welcome to DWSIM Simulator!\n\nAfter this tutorial is complete, you can click on the first button on the top bar to create a new simulation, erasing the contents of the current flowsheet.";
        }
        public static String LoadTitle()
        {
            return "Load Data";
        }
        public static String LoadDesc()
        {
            return " You can also click on the second button at the tob bar to load simulation data from a XML file.";
        }
        public static String SaveTitle()
        {
            return "Save Data";
        }
        public static String SaveDesc()
        {
            return " You can click on the third button at the top bar at any time to save the simulation data to a XML file.";
        }
        public static String CalcTitle()
        {
            return "Run Simulation";
        }
        public static String CalcDesc()
        {
            return "After building the simulation and setting up the flowsheet, click on the 'Play' button to solve it.";
        }
        public static String Flowsheet()
        {
            return "Process Flowsheet Diagram (PFD)";
        }
        public static String FlowsheetDesc()
        {
            return "Drag and drop objects from the OBJECTS menu to the PFD area to add them to the flowsheet.";
        }
        public static String FlowsheetOps()
        {
            return "Working with the PFD";
        }
        public static String FlowsheetOpsDesc()
        {
            return "Drag objects around to reposition them. Drag the flowsheet to reposition all objects. Pinch to zoom the entire flowsheet. Double-tap to zoom and fit all objects into the visible flowsheet area.";
        }
        public static String EditingObjects()
        {
            return "Editing Objects";
        }
        public static String EditingObjectsDesc()
        {
            return "Long-touch on any flowsheet object to setup its connections, input properties and view results.";
        }
        public static String UnlockTitle()
        {
            return "Unlock Additional Features";
        }
        public static String UnlockDesc()
        {
            return "You can buy add-ins to unlock additional features like Property Packages, Unit Operations, Reactors and Custom Systems of Units.";
        }

        public static String CompoundsTitle()
        {
            return "Compounds";
        }
        public static String CompoundsDesc()
        {
            return "Add and remove compounds to/from the simulation.";
        }
        public static String BasisTitle()
        {
            return "Simulation Basis";
        }
        public static String BasisDesc()
        {
            return "Select the Property Package that you want to use to solve phase equilibria and calculate properties.";
        }
        public static String ReportsTitle()
        {
            return "Reports";
        }
        public static String ReportsDesc()
        {
            return "After solving the flowsheet, you can save and view a report with the calculation results.";
        }

        public static String error_uo() { return "This error generally occurs because of invalid input parameters or miscalculated inlet stream(s). Try the following procedures before solving the flowsheet again:\n\n- Check input parameters\n\n- Check inlet stream(s)\n\n- Perturb one or more input parameters(change them a little bit)\n\n- Try a different Property Package"; }
     
        public static String error_flash() { return "A flash error occurs during the phase equilibrium calculation, and means that something went wrong while doing the mass and/or energy balance.Try the following procedures before solving the flowsheet again:\n\n- Check input parameters\n\n- Check inlet stream(s)\n\n- Perturb one or more input parameters(change them a little bit)\n\n- Try relaxing the flash convergence parameters at the Property Package settings panel(Basis -&gt; Configure Property Package)\n\n- Try a different Property Package"; }
        
        public static String error_divergence() { return "This error generally occurs because of invalid input parameters or miscalculated inlet stream(s). Try the following procedures before solving the flowsheet again:\n\n- Check input parameters to see if a feasible solution is possible\n\n- Check inlet stream(s)\n\n- Perturb one or more input parameters(change them a little bit)\n\n- Try a different Property Package"; }
        
        public static String error_flash_max_it() { return "A flash error occurs during the phase equilibrium calculation.If the flash calculation didn\'t converge, then there\'s a high probability that the current Property Package is unsuitable for your simulation. Try the following procedures before solving the flowsheet again:\n\n- Try relaxing the flash convergence parameters at the Property Package settings panel (Basis -&gt; Configure Property Package)\n\n- Try a different Property Package"; }
        
        public static String error_invalid_parameter() { return "This error generally occurs because of invalid input parameters.Try the following procedures before solving the flowsheet again:\n\n- Check input parameters to see if a feasible solution is possible\n\n- Check inlet stream(s)"; }

    }
}
