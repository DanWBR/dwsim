// DWSIM Advanced EOS Model Library
// Based on http://hpp.uva.es/open-source-software-eos/

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Reflection;
using System.IO;
using DWSIM.Interfaces;
using System.Threading.Tasks;
using DWSIM.Thermodynamics.PropertyPackages;

namespace DWSIM.Thermodynamics.AdvancedEOS
{

    public enum Model
    { 
        //EOS
        PRBM,   //PR-Boston-Mathias
        PRWS,   //PR-Wong-Sandler
        PT,     //Patel-Teja
        VPT,    //Valderrama-Patel-Teja

        //Predictive EOS
        PSRK,   //Predictive Soave-Redlich-Kwong
        GC,     //Group Contribution

        // Chain
        PHSC,   //Perturbed Hard Sphere Chain

        //SAFT
        SAFT,   //Statistical Associating Fluid Theory
        PC_SAFT //Perturbed Chain Statistical Associating Fluid Theory 
    }

    public enum ThermoProperty
    { 
        CompressibilityCoeff,
        FugacityCoeff,
        Density,
        Enthalpy,
        VaporPressure,
        BubblePoint,
        DewPoint,
        IsothermalFlash,
        SolidSolubility,
        GibbsMin
    }

    public abstract class AdvEOSPropertyPackageBase: PropertyPackages.PropertyPackage
    {

        public Model PropertyPackageModel = Model.PRBM;

        private Octave GetOctaveInstance()
        { 
        
            var octave = new Octave(GlobalSettings.Settings.OctavePath);

            octave.ExecuteCommand("addpath('" + Path.GetDirectoryName(Assembly.GetEntryAssembly().Location) + Path.DirectorySeparatorChar + "ECE')");
            octave.ExecuteCommand("cd '" + GlobalSettings.Settings.OctaveFileTempDir + "'");

            return octave;

        }

        protected abstract string GetModelSpecificParameters(); 

        public Object CalculateProperty(ThermoProperty prop, double[] vx, string state, Double param1, Double param2, Double param3, Double param4)
        {

            // PARAM order: T, P, H, S

            string filename = GlobalSettings.Settings.OctaveFileTempDir + Path.DirectorySeparatorChar + Path.GetRandomFileName();

            StringWriter contents = new StringWriter();

            System.Globalization.CultureInfo ci = System.Globalization.CultureInfo.InvariantCulture;

            int i = 0;
            foreach (ICompound c in CurrentMaterialStream.Phases[0].Compounds.Values)
            {
                string cname = "comp" + i.ToString();
                contents.WriteLine(cname + " = cSubstance");
                contents.WriteLine(cname + ".Name = '" + c.ConstantProperties.Name + "';");
                contents.WriteLine(cname + ".MW = " + c.ConstantProperties.Molar_Weight.ToString(ci) + ";");
                contents.WriteLine(cname + ".Tc = " + c.ConstantProperties.Critical_Temperature.ToString(ci) + ";");
                contents.WriteLine(cname + ".Pc = " + c.ConstantProperties.Critical_Pressure.ToString(ci) + ";");
                contents.WriteLine(cname + ".w = " + c.ConstantProperties.Acentric_Factor.ToString(ci) + ";");
                contents.WriteLine("");
                i += 1;
            }

            contents.WriteLine("mix = CMixture;");
            i = 1;
            foreach (ICompound c in CurrentMaterialStream.Phases[0].Compounds.Values)
            {
                string cname = "comp" + i.ToString();
                contents.WriteLine("mix.comp(" + i.ToString() + ") = " + cname + ";");
                contents.WriteLine("");
                i += 1;
            }

            contents.WriteLine("mix.x = [");
            i = 0;
            foreach (ICompound c in CurrentMaterialStream.Phases[0].Compounds.Values)
            {
                contents.Write(vx[i].ToString(ci) + " ");
            }
            contents.WriteLine("];");
            contents.WriteLine("");

            contents.WriteLine(GetModelSpecificParameters());

            switch (PropertyPackageModel)
            {
                case Model.GC:
                 contents.WriteLine("EoS = cGCEoS;");
                   break;
                case Model.PC_SAFT:
                   contents.WriteLine("EoS = cPCSAFTEoS;");
                    break;
                case Model.PHSC:
                    contents.WriteLine("EoS = cPHSCEoS;");
                    break;
                case Model.PRBM:
                    contents.WriteLine("EoS = cPRBMEoS;");
                    break;
                case Model.PRWS:
                    contents.WriteLine("EoS = cPRWSEoS;");
                    break;
                case Model.PSRK:
                    contents.WriteLine("EoS = cPSRKEoS;");
                    break;
                case Model.PT:
                    contents.WriteLine("EoS = cPTEoS;");
                    break;
                case Model.SAFT:
                    contents.WriteLine("EoS = cSAFTEoS;");
                    break;
                case Model.VPT:
                    contents.WriteLine("EoS = cVPTEoS;");
                    break;
                default:
                    break;
            }

            switch (prop)
            { 
                case ThermoProperty.CompressibilityCoeff:
                    if (state == "L")
                    {
                        contents.WriteLine("Z = compr(EoS," + param1.ToString(ci) + ", " + param2.ToString(ci)  + ",mix,'liq')");
                    }
                    else {
                        contents.WriteLine("Z = compr(EoS," + param1.ToString(ci) + ", " + param2.ToString(ci) + ",mix,'gas')");
                    }
                    break;
                case ThermoProperty.Density:
                    if (state == "L")
                    {
                        contents.WriteLine("[denMass,denMol,Z,EoS] = Density(EoS," + param1.ToString(ci) + ", " + param2.ToString(ci) + ",mix,'liq')");
                    }
                    else
                    {
                        contents.WriteLine("[denMass,denMol,Z,EoS] = Density(EoS," + param1.ToString(ci) + ", " + param2.ToString(ci) + ",mix,'gas')");
                    }
                    break;
                case ThermoProperty.Enthalpy:
                    if (state == "L")
                    {
                        contents.WriteLine("[Hres,EoS] = Enthalpy(EoS,mix," + param1.ToString(ci) + ", " + param2.ToString(ci) + ",'liq')");
                    }
                    else
                    {
                        contents.WriteLine("[Hres,EoS] = Enthalpy(EoS,mix," + param1.ToString(ci) + ", " + param2.ToString(ci) + ",'gas')");
                    }
                    break;
                case ThermoProperty.FugacityCoeff:
                    if (state == "L")
                    {
                        contents.WriteLine("f = fugF(EoS," + param1.ToString(ci) + ", " + param2.ToString(ci) + ",mix,'liq')");
                    }
                    else
                    {
                        contents.WriteLine("f = fugF(EoS," + param1.ToString(ci) + ", " + param2.ToString(ci) + ",mix,'gas')");
                    }
                    break;
                default:
                    break;
            }

            File.WriteAllText(filename, contents.ToString());

            var octave = GetOctaveInstance();
            octave.ExecuteCommand(Path.GetFileName(filename), 120000);

            switch (prop)
            {
                case ThermoProperty.CompressibilityCoeff:
                    return octave.GetScalar("Z");
                case ThermoProperty.Density:
                    return octave.GetScalar("denMass");
                case ThermoProperty.Enthalpy:
                    return octave.GetScalar("Hres") + RET_Hid(298.15, param1, vx);
                case ThermoProperty.FugacityCoeff:
                    return octave.GetVector("f");
                default:
                    return null;
            }

        }
      
        public override bool MobileCompatible
        {
            get { return false; }
        }

        public override bool SupportsComponent(Interfaces.ICompoundConstantProperties comp)
        {
            return true;
        }

        #region legacy

        public override double DW_CalcEnergyFlowMistura_ISOL(double T, double P)
        {
            throw new NotImplementedException();
        }

        public override double DW_CalcCp_ISOL(PropertyPackages.Phase Phase1, double T, double P)
        {
            throw new NotImplementedException();
        }

        public override double DW_CalcCv_ISOL(PropertyPackages.Phase Phase1, double T, double P)
        {
            throw new NotImplementedException();
        }

        public override double DW_CalcK_ISOL(PropertyPackages.Phase Phase1, double T, double P)
        {
            throw new NotImplementedException();
        }

        public override double DW_CalcMM_ISOL(PropertyPackages.Phase Phase1, double T, double P)
        {
            throw new NotImplementedException();
        }

        public override double DW_CalcMassaEspecifica_ISOL(PropertyPackages.Phase Phase1, double T, double P, double Pvp = 0)
        {
            throw new NotImplementedException();
        }

        public override double DW_CalcPVAP_ISOL(double T)
        {
            throw new NotImplementedException();
        }

        public override double DW_CalcTensaoSuperficial_ISOL(PropertyPackages.Phase Phase1, double T, double P)
        {
            throw new NotImplementedException();
        }

        public override double DW_CalcViscosidadeDinamica_ISOL(PropertyPackages.Phase Phase1, double T, double P)
        {
            throw new NotImplementedException();
        }


#endregion

        public override double AUX_VAPDENS(double T, double P)
        {
            return (double)CalculateProperty(ThermoProperty.Density, RET_VMOL(PropertyPackages.Phase.Vapor), "V", T, P, 0, 0);
        }

        public override double[] DW_CalcFugCoeff(Array Vx, double T, double P, PropertyPackages.State st)
        {
            double[] vz = Vx.Cast<double>().ToArray();
            switch (st)
            {
                case PropertyPackages.State.Liquid:
                    return (double[])CalculateProperty(ThermoProperty.FugacityCoeff, vz, "L", T, P, 0, 0);
                case PropertyPackages.State.Vapor:
                    return (double[])CalculateProperty(ThermoProperty.FugacityCoeff, vz, "V", T, P, 0, 0);
                default:
                    return null;
            }
        }

        public override double DW_CalcEnthalpy(Array Vx, double T, double P, PropertyPackages.State st)
        {
            double[] vz = Vx.Cast<double>().ToArray();
            switch (st)
            {
                case PropertyPackages.State.Liquid:
                    return (double)CalculateProperty(ThermoProperty.Enthalpy, vz, "L", T, P, 0, 0);
                case PropertyPackages.State.Vapor:
                    return (double)CalculateProperty(ThermoProperty.Enthalpy, vz, "V", T, P, 0, 0);
                default:
                    return 0f;
            }
        }

        public override double DW_CalcEnthalpyDeparture(Array Vx, double T, double P, PropertyPackages.State st)
        {
            throw new NotImplementedException();
        }

        public override double DW_CalcEntropy(Array Vx, double T, double P, PropertyPackages.State st)
        {
            double[] vz = Vx.Cast<double>().ToArray();
            switch (st)
            {
                case PropertyPackages.State.Liquid:
                    return (double)CalculateProperty(ThermoProperty.Enthalpy, vz, "L", T, P, 0, 0)/T;
                case PropertyPackages.State.Vapor:
                    return (double)CalculateProperty(ThermoProperty.Enthalpy, vz, "V", T, P, 0, 0)/T;
                default:
                    return 0f;
            }
        }

        public override double DW_CalcEntropyDeparture(Array Vx, double T, double P, PropertyPackages.State st)
        {
            throw new NotImplementedException();
        }

        public override void DW_CalcPhaseProps(PropertyPackages.Phase Phase)
        {
            double result = 0f;
            Phase dwpl = default(Phase);

            double T = 0f;
            double P = 0f;
            double phasemolarfrac = 0f;
            double overallmolarflow = 0f;

            int phaseID = 0;
            T = this.CurrentMaterialStream.Phases[0].Properties.temperature.GetValueOrDefault();
            P = this.CurrentMaterialStream.Phases[0].Properties.pressure.GetValueOrDefault();

            switch (Phase)
            {
                case PropertyPackages.Phase.Mixture:
                    phaseID = 0;
                    dwpl = PropertyPackages.Phase.Mixture;
                    break;
                case PropertyPackages.Phase.Vapor:
                    phaseID = 2;
                    dwpl = PropertyPackages.Phase.Vapor;
                    break;
                case PropertyPackages.Phase.Liquid1:
                    phaseID = 3;
                    dwpl = PropertyPackages.Phase.Liquid1;
                    break;
                case PropertyPackages.Phase.Liquid2:
                    phaseID = 4;
                    dwpl = PropertyPackages.Phase.Liquid2;
                    break;
                case PropertyPackages.Phase.Liquid3:
                    phaseID = 5;
                    dwpl = PropertyPackages.Phase.Liquid3;
                    break;
                case PropertyPackages.Phase.Liquid:
                    phaseID = 1;
                    dwpl = PropertyPackages.Phase.Liquid;
                    break;
                case PropertyPackages.Phase.Aqueous:
                    phaseID = 6;
                    dwpl = PropertyPackages.Phase.Aqueous;
                    break;
                case PropertyPackages.Phase.Solid:
                    phaseID = 7;
                    dwpl = PropertyPackages.Phase.Solid;
                    break;
            }

            if (phaseID > 0)
            {
                overallmolarflow = this.CurrentMaterialStream.Phases[0].Properties.molarflow.GetValueOrDefault();
                phasemolarfrac = this.CurrentMaterialStream.Phases[phaseID].Properties.molarfraction.GetValueOrDefault();
                result = overallmolarflow * phasemolarfrac;
                this.CurrentMaterialStream.Phases[phaseID].Properties.molarflow = result;
                result = result * this.AUX_MMM(Phase) / 1000;
                this.CurrentMaterialStream.Phases[phaseID].Properties.massflow = result;
                if (this.CurrentMaterialStream.Phases[0].Properties.massflow.GetValueOrDefault() > 0)
                {
                    result = phasemolarfrac * overallmolarflow * this.AUX_MMM(Phase) / 1000 / this.CurrentMaterialStream.Phases[0].Properties.massflow.GetValueOrDefault();
                }
                else
                {
                    result = 0;
                }
                this.CurrentMaterialStream.Phases[phaseID].Properties.massfraction = result;
                this.DW_CalcCompVolFlow(phaseID);
                this.DW_CalcCompFugCoeff(Phase);
            }


            if (phaseID == 3 | phaseID == 4 | phaseID == 5 | phaseID == 6)
            {
                
                result = this.AUX_LIQDENS(T, P, 0.0, phaseID, false);

                this.CurrentMaterialStream.Phases[phaseID].Properties.density = result;
                
                this.CurrentMaterialStream.Phases[phaseID].Properties.enthalpy = this.DW_CalcEnthalpy(RET_VMOL(dwpl), T, P, State.Liquid);
                
                this.CurrentMaterialStream.Phases[phaseID].Properties.entropy = this.DW_CalcEntropy(RET_VMOL(dwpl), T, P, State.Liquid);

                result = (double)CalculateProperty(ThermoProperty.CompressibilityCoeff, RET_VMOL(dwpl), "L", T, P, 0, 0);
                this.CurrentMaterialStream.Phases[phaseID].Properties.compressibilityFactor = result;
                
                //Ideal
                result = this.AUX_LIQCPm(T, phaseID);
                this.CurrentMaterialStream.Phases[phaseID].Properties.heatCapacityCp = result;
                this.CurrentMaterialStream.Phases[phaseID].Properties.heatCapacityCv = result;
                   
                result = this.AUX_MMM(Phase);
                this.CurrentMaterialStream.Phases[phaseID].Properties.molecularWeight = result;
                
                result = this.CurrentMaterialStream.Phases[phaseID].Properties.enthalpy.GetValueOrDefault() * this.CurrentMaterialStream.Phases[phaseID].Properties.molecularWeight.GetValueOrDefault();
                this.CurrentMaterialStream.Phases[phaseID].Properties.molar_enthalpy = result;
                
                result = this.CurrentMaterialStream.Phases[phaseID].Properties.entropy.GetValueOrDefault() * this.CurrentMaterialStream.Phases[phaseID].Properties.molecularWeight.GetValueOrDefault();
                this.CurrentMaterialStream.Phases[phaseID].Properties.molar_entropy = result;
                
                result = this.AUX_CONDTL(T);
                this.CurrentMaterialStream.Phases[phaseID].Properties.thermalConductivity = result;
                
                result = this.AUX_LIQVISCm(T);
                this.CurrentMaterialStream.Phases[phaseID].Properties.viscosity = result;
                this.CurrentMaterialStream.Phases[phaseID].Properties.kinematic_viscosity = result / this.CurrentMaterialStream.Phases[phaseID].Properties.density.Value;
                
            }
            else if (phaseID == 2)
            {

                result = this.AUX_VAPDENS(T, P);
                this.CurrentMaterialStream.Phases[phaseID].Properties.density = result;
                this.CurrentMaterialStream.Phases[phaseID].Properties.enthalpy = this.DW_CalcEnthalpy(RET_VMOL(dwpl), T, P, State.Vapor);
                this.CurrentMaterialStream.Phases[phaseID].Properties.entropy = this.DW_CalcEntropy(RET_VMOL(dwpl), T, P, State.Vapor);

                result = (double)CalculateProperty(ThermoProperty.CompressibilityCoeff, RET_VMOL(dwpl), "V", T, P, 0, 0);
                this.CurrentMaterialStream.Phases[phaseID].Properties.compressibilityFactor = result;
                
                result = this.AUX_CPm(PropertyPackages.Phase.Vapor, T);
                
                this.CurrentMaterialStream.Phases[phaseID].Properties.heatCapacityCp = AUX_CPm(PropertyPackages.Phase.Vapor, T);
                this.CurrentMaterialStream.Phases[phaseID].Properties.heatCapacityCv = AUX_CPm(PropertyPackages.Phase.Vapor, T);

                result = this.AUX_MMM(Phase);
                this.CurrentMaterialStream.Phases[phaseID].Properties.molecularWeight = result;
                
                result = this.CurrentMaterialStream.Phases[phaseID].Properties.enthalpy.GetValueOrDefault() * this.CurrentMaterialStream.Phases[phaseID].Properties.molecularWeight.GetValueOrDefault();
                this.CurrentMaterialStream.Phases[phaseID].Properties.molar_enthalpy = result;
                
                result = this.CurrentMaterialStream.Phases[phaseID].Properties.entropy.GetValueOrDefault() * this.CurrentMaterialStream.Phases[phaseID].Properties.molecularWeight.GetValueOrDefault();
                this.CurrentMaterialStream.Phases[phaseID].Properties.molar_entropy = result;
                
                result = this.AUX_CONDTG(T, P);
                this.CurrentMaterialStream.Phases[phaseID].Properties.thermalConductivity = result;
                
                result = this.AUX_VAPVISCm(T, this.CurrentMaterialStream.Phases[phaseID].Properties.density.GetValueOrDefault(), this.AUX_MMM(Phase));
                this.CurrentMaterialStream.Phases[phaseID].Properties.viscosity = result;
                this.CurrentMaterialStream.Phases[phaseID].Properties.kinematic_viscosity = result / this.CurrentMaterialStream.Phases[phaseID].Properties.density.Value;

            }
            else if (phaseID == 7)
            {
                result = this.AUX_SOLIDDENS();
                this.CurrentMaterialStream.Phases[phaseID].Properties.density = result;
                List<Interfaces.ICompoundConstantProperties> constprops = new List<Interfaces.ICompoundConstantProperties>();
                foreach (Interfaces.ICompound su in this.CurrentMaterialStream.Phases[0].Compounds.Values)
                {
                    constprops.Add(su.ConstantProperties);
                }
                this.CurrentMaterialStream.Phases[phaseID].Properties.enthalpy = this.DW_CalcEnthalpy(RET_VMOL(dwpl), T, P, State.Solid);
                this.CurrentMaterialStream.Phases[phaseID].Properties.entropy = this.DW_CalcEntropy(RET_VMOL(dwpl), T, P, State.Solid);
                this.CurrentMaterialStream.Phases[phaseID].Properties.compressibilityFactor = 0.0;
                //result
                result = this.DW_CalcSolidHeatCapacityCp(T, RET_VMOL(PropertyPackages.Phase.Solid), constprops);
                this.CurrentMaterialStream.Phases[phaseID].Properties.heatCapacityCp = result;
                this.CurrentMaterialStream.Phases[phaseID].Properties.heatCapacityCv = result;
                result = this.AUX_MMM(Phase);
                this.CurrentMaterialStream.Phases[phaseID].Properties.molecularWeight = result;
                result = this.CurrentMaterialStream.Phases[phaseID].Properties.enthalpy.GetValueOrDefault() * this.CurrentMaterialStream.Phases[phaseID].Properties.molecularWeight.GetValueOrDefault();
                this.CurrentMaterialStream.Phases[phaseID].Properties.molar_enthalpy = result;
                result = this.CurrentMaterialStream.Phases[phaseID].Properties.entropy.GetValueOrDefault() * this.CurrentMaterialStream.Phases[phaseID].Properties.molecularWeight.GetValueOrDefault();
                this.CurrentMaterialStream.Phases[phaseID].Properties.molar_entropy = result;
                result = this.AUX_CONDTG(T, P);
                this.CurrentMaterialStream.Phases[phaseID].Properties.thermalConductivity = 0.0;
                //result
                this.CurrentMaterialStream.Phases[phaseID].Properties.viscosity = 1E+20;
                this.CurrentMaterialStream.Phases[phaseID].Properties.kinematic_viscosity = 1E+20;

            }
            else if (phaseID == 1)
            {
                DW_CalcLiqMixtureProps();


            }
            else
            {
                DW_CalcOverallProps();


            }

            if (phaseID > 0)
            {
                if (this.CurrentMaterialStream.Phases[phaseID].Properties.density.GetValueOrDefault() > 0 & overallmolarflow > 0)
                {
                    result = overallmolarflow * phasemolarfrac * this.AUX_MMM(Phase) / 1000 / this.CurrentMaterialStream.Phases[phaseID].Properties.density.GetValueOrDefault();
                }
                else
                {
                    result = 0;
                }
                this.CurrentMaterialStream.Phases[phaseID].Properties.volumetric_flow = result;
            }
        }

        public override void DW_CalcProp(string property, PropertyPackages.Phase phase)
        {

            double result = 0.0;
            int phaseID = -1;
            string state = "";
            State pstate = default(State);

            double T = 0;
            double P = 0;
            T = this.CurrentMaterialStream.Phases[0].Properties.temperature.GetValueOrDefault();
            P = this.CurrentMaterialStream.Phases[0].Properties.pressure.GetValueOrDefault();

            switch (phase)
            {
                case Phase.Vapor:
                    state = "V";
                    pstate = PropertyPackages.State.Vapor;
                    break;
                case Phase.Liquid:
                case Phase.Liquid1:
                case Phase.Liquid2:
                case Phase.Liquid3:
                case Phase.Aqueous:
                    state = "L";
                    pstate = PropertyPackages.State.Liquid;
                    break;
                case Phase.Solid:
                    state = "S";
                    pstate = PropertyPackages.State.Solid;
                    break;
            }

            switch (phase)
            {
                case PropertyPackages.Phase.Mixture:
                    phaseID = 0;
                    break;
                case PropertyPackages.Phase.Vapor:
                    phaseID = 2;
                    break;
                case PropertyPackages.Phase.Liquid1:
                    phaseID = 3;
                    break;
                case PropertyPackages.Phase.Liquid2:
                    phaseID = 4;
                    break;
                case PropertyPackages.Phase.Liquid3:
                    phaseID = 5;
                    break;
                case PropertyPackages.Phase.Liquid:
                    phaseID = 1;
                    break;
                case PropertyPackages.Phase.Aqueous:
                    phaseID = 6;
                    break;
                case PropertyPackages.Phase.Solid:
                    phaseID = 7;
                    break;
            }

            this.CurrentMaterialStream.Phases[phaseID].Properties.molecularWeight = this.AUX_MMM(phase);

            switch (property.ToLower())
            {
                case "compressibilityfactor":
                    result = (double)CalculateProperty(ThermoProperty.CompressibilityCoeff, RET_VMOL(phase), state, T, P, 0, 0);
                    this.CurrentMaterialStream.Phases[phaseID].Properties.compressibilityFactor = result;
                    break;
                case "heatcapacity":
                case "heatcapacitycp":
                    if (state == "V")
                    {
                        result = this.AUX_CPm(phase, T);
                    }
                    else
                    {
                        //Ideal
                        result = this.AUX_LIQCPm(T, phaseID);
                        break;
                    }
                    this.CurrentMaterialStream.Phases[phaseID].Properties.heatCapacityCp = result;
                    break;
                case "heatcapacitycv":
                    if (state == "V")
                    {
                        result = this.AUX_CPm(phase, T);
                    }
                    else
                    {
                        //Ideal
                        result = this.AUX_LIQCPm(T, phaseID);
                        break;
                    }
                    this.CurrentMaterialStream.Phases[phaseID].Properties.heatCapacityCp = result;
                    break;
                case "enthalpy":
                case "enthalpynf":
                    result = this.DW_CalcEnthalpy(RET_VMOL(phase), T, P, pstate);
                    this.CurrentMaterialStream.Phases[phaseID].Properties.enthalpy = result;
                    result = this.CurrentMaterialStream.Phases[phaseID].Properties.enthalpy.GetValueOrDefault() * this.CurrentMaterialStream.Phases[phaseID].Properties.molecularWeight.GetValueOrDefault();
                    this.CurrentMaterialStream.Phases[phaseID].Properties.molar_enthalpy = result;
                    break;
                case "entropy":
                case "entropynf":
                    result = this.DW_CalcEntropy(RET_VMOL(phase), T, P, pstate);
                    this.CurrentMaterialStream.Phases[phaseID].Properties.entropy = result;
                    result = this.CurrentMaterialStream.Phases[phaseID].Properties.entropy.GetValueOrDefault() * this.CurrentMaterialStream.Phases[phaseID].Properties.molecularWeight.GetValueOrDefault();
                    this.CurrentMaterialStream.Phases[phaseID].Properties.molar_entropy = result;
                    break;
                case "excessenthalpy":
                    result = this.DW_CalcEnthalpyDeparture(RET_VMOL(phase), T, P, pstate);
                    this.CurrentMaterialStream.Phases[phaseID].Properties.excessEnthalpy = result;
                    break;
                case "excessentropy":
                    result = this.DW_CalcEntropyDeparture(RET_VMOL(phase), T, P, pstate);
                    this.CurrentMaterialStream.Phases[phaseID].Properties.excessEntropy = result;
                    break;
                case "enthalpyf":
                    double entF = this.AUX_HFm25(phase);
                    result = this.DW_CalcEnthalpy(RET_VMOL(phase), T, P, pstate);
                    this.CurrentMaterialStream.Phases[phaseID].Properties.enthalpyF = result + entF;
                    result = this.CurrentMaterialStream.Phases[phaseID].Properties.enthalpyF.GetValueOrDefault() * this.CurrentMaterialStream.Phases[phaseID].Properties.molecularWeight.GetValueOrDefault();
                    this.CurrentMaterialStream.Phases[phaseID].Properties.molar_enthalpyF = result;
                    break;
                case "entropyf":
                    entF = this.AUX_SFm25(phase);
                    result = this.DW_CalcEntropy(RET_VMOL(phase), T, P, pstate);
                    this.CurrentMaterialStream.Phases[phaseID].Properties.entropyF = result + entF;
                    result = this.CurrentMaterialStream.Phases[phaseID].Properties.entropyF.GetValueOrDefault() * this.CurrentMaterialStream.Phases[phaseID].Properties.molecularWeight.GetValueOrDefault();
                    this.CurrentMaterialStream.Phases[phaseID].Properties.molar_entropyF = result;
                    break;
                case "viscosity":
                    if (state == "L")
                    {
                        result = this.AUX_LIQVISCm(T);
                    }
                    else
                    {
                        result = this.AUX_VAPVISCm(T, this.CurrentMaterialStream.Phases[phaseID].Properties.density.GetValueOrDefault(), this.AUX_MMM(phase));
                    }
                    this.CurrentMaterialStream.Phases[phaseID].Properties.viscosity = result;
                    break;
                case "thermalconductivity":
                    if (state == "L")
                    {
                        result = this.AUX_CONDTL(T);
                    }
                    else
                    {
                        result = this.AUX_CONDTG(T, P);
                    }
                    this.CurrentMaterialStream.Phases[phaseID].Properties.thermalConductivity = result;
                    break;
                case "fugacity":
                case "fugacitycoefficient":
                case "logfugacitycoefficient":
                case "activity":
                case "activitycoefficient":
                    this.DW_CalcCompFugCoeff(phase);
                    break;
                case "volume":
                case "density":
                    if (state == "L")
                    {
                        result = this.AUX_LIQDENS(T, P, 0.0, phaseID, false);
                    }
                    else
                    {
                        result = this.AUX_VAPDENS(T, P);
                    }
                    this.CurrentMaterialStream.Phases[phaseID].Properties.density = result;
                    break;
                case "surfacetension":
                    this.CurrentMaterialStream.Phases[0].Properties.surfaceTension = this.AUX_SURFTM(T);
                    break;
                default:
                    throw new CapeOpen.CapeThrmPropertyNotAvailableException();
            }

        }
        
        public override void DW_CalcCompPartialVolume(PropertyPackages.Phase phase, double T, double P)
        {
            //do nothing
        }
    }
}
