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

    public abstract class AdvEOSPropertyPackageBase : PropertyPackages.PropertyPackage
    {

        Random random = new Random();

        public Model PropertyPackageModel = Model.PRBM;

        PropertyPackages.Auxiliary.LeeKesler lk = new PropertyPackages.Auxiliary.LeeKesler();

        private bool useLeeKeslerEnthalpy = true;

        public bool UseLeeKeslerEnthalpy
        {
            get
            {
                return useLeeKeslerEnthalpy;
            }

            set
            {
                useLeeKeslerEnthalpy = value;
            }
        }

        public override object ReturnInstance(string typename)
        {
            Type t = Type.GetType(typename, false);
            return Activator.CreateInstance(t);
        }

        private Octave GetOctaveInstance()
        {

            if (GlobalSettings.Settings.OctavePath == "" && Environment.OSVersion.Platform == PlatformID.Win32NT) {
                if (GlobalSettings.Settings.CAPEOPENMode)
                {
                    throw new Exception("Octave binaries path not set. Open the Property Package editor, set the Octave binaries path and try again.");
                }
                else
                {
                    throw new Exception("Octave binaries path not set (go to 'Edit' > 'General Settings' > 'Other' > 'DWSIM/Octave bridge settings').");
                }
            }

            var octave = new Octave(GlobalSettings.Settings.OctavePath);

            octave.ExecuteCommand("addpath('" + Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location) + Path.DirectorySeparatorChar + "ECE')");
            octave.ExecuteCommand("cd '" + Path.GetTempPath() + Path.DirectorySeparatorChar + "'");

            return octave;

        }

        protected abstract string GetModelSpecificParameters();

        protected abstract string GetModelInteractionParameters();

        public Object CalculateProperty(ThermoProperty prop, double[] vx, string state, Double param1, Double param2, Double param3, Double param4)
        {

            // PARAM order: T, P, H, S

            string model = "", propname = "";

            if (param1 <= 0 || param2 <= 0 || vx.Sum() == 0f)
            {
                switch (prop)
                {
                    case ThermoProperty.CompressibilityCoeff:
                    case ThermoProperty.Density:
                    case ThermoProperty.Enthalpy:
                        return 0d;
                    case ThermoProperty.FugacityCoeff:
                        return RET_NullVector();
                    default:
                        return 0d;
                }
            }
            
            var chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
            var stringChars = new char[8];

            for (int j = 0; j < stringChars.Length; j++)
            {
                stringChars[j] = chars[random.Next(chars.Length)];
            }

            var finalString = new String(stringChars);

            string filename = Path.GetTempPath() + Path.DirectorySeparatorChar + finalString + ".m";

            StringWriter contents = new StringWriter();

            System.Globalization.CultureInfo ci = System.Globalization.CultureInfo.InvariantCulture;

            int i = 0;
            foreach (ICompound c in CurrentMaterialStream.Phases[0].Compounds.Values)
            {
                string cname = "comp" + i.ToString();
                contents.WriteLine(cname + " = cSubstance;");
                contents.WriteLine(cname + ".name = '" + c.ConstantProperties.Name + "';");
                contents.WriteLine(cname + ".MW = " + c.ConstantProperties.Molar_Weight.ToString(ci) + ";");
                contents.WriteLine(cname + ".Tc = " + c.ConstantProperties.Critical_Temperature.ToString(ci) + ";");
                contents.WriteLine(cname + ".Pc = " + c.ConstantProperties.Critical_Pressure.ToString(ci) + ";");
                contents.WriteLine(cname + ".w = " + c.ConstantProperties.Acentric_Factor.ToString(ci) + ";");
                contents.WriteLine("");
                i += 1;
            }

            contents.WriteLine(GetModelSpecificParameters());

            contents.WriteLine("mix = cMixture;");
            i = 1;
            foreach (ICompound c in CurrentMaterialStream.Phases[0].Compounds.Values)
            {
                string cname = "comp" + (i - 1).ToString();
                contents.WriteLine("mix.comp(" + i.ToString() + ") = " + cname + ";");
                i += 1;
            }

            contents.Write("mix.x = [");
            i = 0;
            foreach (ICompound c in CurrentMaterialStream.Phases[0].Compounds.Values)
            {
                contents.Write(vx[i].ToString(ci) + " ");
                i += 1;
            }
            contents.WriteLine("];");
            contents.WriteLine("");

            contents.WriteLine(GetModelInteractionParameters());

            switch (PropertyPackageModel)
            {
                case Model.PC_SAFT:
                    model = "PC-SAFT";
                    if (GlobalSettings.Settings.CAPEOPENMode)
                    {
                        if (!GlobalSettings.Settings.ExcelMode) ((CapeOpen.ICapeDiagnostic)this._pme).LogMessage("PC-SAFT calculations may take longer than usual, please be patient...");
                    }
                    else
                    {
                        if (!GlobalSettings.Settings.ExcelMode) Flowsheet.ShowMessage("PC-SAFT calculations may take longer than usual, please be patient...", IFlowsheet.MessageType.Tip);
                    }
                    if (GlobalSettings.Settings.ExcelMode) Console.WriteLine("PC-SAFT calculations may take longer than usual, please be patient...");
                    contents.WriteLine("EoS = cPCSAFTEoS;");
                    break;
                case Model.PHSC:
                    model = "PHSC";
                    if (GlobalSettings.Settings.CAPEOPENMode)
                    {
                        if (!GlobalSettings.Settings.ExcelMode) ((CapeOpen.ICapeDiagnostic)this._pme).LogMessage("PHSC calculations may take longer than usual, please be patient...");
                    }
                    else
                    {
                        if (!GlobalSettings.Settings.ExcelMode) Flowsheet.ShowMessage("PHSC calculations may take longer than usual, please be patient...", IFlowsheet.MessageType.Tip);
                    }
                    if (GlobalSettings.Settings.ExcelMode) Console.WriteLine("PHSC calculations may take longer than usual, please be patient...");
                    contents.WriteLine("EoS = cPHSCEoS;");
                    break;
                case Model.PRBM:
                    model = "PR Boston Mathias";
                    contents.WriteLine("EoS = cPRBMEoS;");
                    break;
                case Model.PRWS:
                    model = "PR Wong-Sandler";
                    contents.WriteLine("EoS = cPRWSEoS;");
                    break;
                case Model.PT:
                    model = "Patel-Teja";
                    contents.WriteLine("EoS = cPTEoS;");
                    break;
                case Model.SAFT:
                    model = "SAFT";
                    if (GlobalSettings.Settings.CAPEOPENMode)
                    {
                        if (!GlobalSettings.Settings.ExcelMode) ((CapeOpen.ICapeDiagnostic)this._pme).LogMessage("SAFT calculations may take longer than usual, please be patient...");
                    }
                    else
                    {
                        if (!GlobalSettings.Settings.ExcelMode) Flowsheet.ShowMessage("SAFT calculations may take longer than usual, please be patient...", IFlowsheet.MessageType.Tip);
                    }
                    if (GlobalSettings.Settings.ExcelMode) Console.WriteLine("SAFT calculations may take longer than usual, please be patient...");
                    contents.WriteLine("EoS = cSAFTEoS;");
                    break;
                case Model.VPT:
                    model = "VPT";
                    contents.WriteLine("EoS = cVPTEoS;");
                    break;
                default:
                    break;
            }
            contents.WriteLine("");

            switch (prop)
            {
                case ThermoProperty.CompressibilityCoeff:
                    propname = "Compressibility Factor";
                    if (state == "L")
                    {
                        contents.WriteLine("Z = compr(EoS," + param1.ToString(ci) + "," + param2.ToString(ci) + ",mix,'liq')");
                    }
                    else
                    {
                        contents.WriteLine("Z = compr(EoS," + param1.ToString(ci) + "," + param2.ToString(ci) + ",mix,'gas')");
                    }
                    break;
                case ThermoProperty.Density:
                    propname = "Density";
                    if (state == "L")
                    {
                        contents.WriteLine("[denMass,denMol,Z,EoS] = Density(EoS,mix," + param1.ToString(ci) + "," + param2.ToString(ci) + ",'liq')");
                    }
                    else
                    {
                        contents.WriteLine("[denMass,denMol,Z,EoS] = Density(EoS,mix," + param1.ToString(ci) + "," + param2.ToString(ci) + ",'gas')");
                    }
                    break;
                case ThermoProperty.Enthalpy:
                    propname = "Enthalpy";
                    if (state == "L")
                    {
                        contents.WriteLine("[Hres,EoS] = Enthalpy(EoS,mix," + param1.ToString(ci) + "," + param2.ToString(ci) + ",'liq')");
                    }
                    else
                    {
                        contents.WriteLine("[Hres,EoS] = Enthalpy(EoS,mix," + param1.ToString(ci) + "," + param2.ToString(ci) + ",'gas')");
                    }
                    break;
                case ThermoProperty.FugacityCoeff:
                    propname = "Fugacity Coefficient";
                    if (state == "L")
                    {
                        contents.WriteLine("f = fugF(EoS," + param1.ToString(ci) + "," + param2.ToString(ci) + ",mix,'liq')");
                    }
                    else
                    {
                        contents.WriteLine("f = fugF(EoS," + param1.ToString(ci) + "," + param2.ToString(ci) + ",mix,'gas')");
                    }
                    break;
                default:
                    break;
            }

            File.WriteAllText(filename, contents.ToString());

            var octave = GetOctaveInstance();

            Object result = null;

            try
            {
                if (GlobalSettings.Settings.CAPEOPENMode)
                {
                    if (!GlobalSettings.Settings.ExcelMode) ((CapeOpen.ICapeDiagnostic)this._pme).LogMessage("Running file '" + Path.GetFileName(filename) + "' on Octave (octave-cli) to calculate property '" + propname + "' with model '" + model + "', [PID: " + octave.OctaveProcess.Id + "]");
                }
                else
                {
                   if (!GlobalSettings.Settings.ExcelMode) CurrentMaterialStream.Flowsheet.ShowMessage("Running file '" + Path.GetFileName(filename) + "' on Octave (octave-cli) to calculate property '" + propname + "' with model '" + model + "', [PID: " + octave.OctaveProcess.Id + "]", IFlowsheet.MessageType.Information);
                }
                if (GlobalSettings.Settings.ExcelMode) Console.WriteLine("Running file '" + Path.GetFileName(filename) + "' on Octave (octave-cli) to calculate property '" + propname + "' with model '" + model + "', [PID: " + octave.OctaveProcess.Id + "]");
                octave.ExecuteCommand(Path.GetFileNameWithoutExtension(filename), (int)(GlobalSettings.Settings.OctaveTimeoutInMinutes * 60 * 1000));
                if (GlobalSettings.Settings.CAPEOPENMode)
                {
                    if (!GlobalSettings.Settings.ExcelMode) ((CapeOpen.ICapeDiagnostic)this._pme).LogMessage("Octave instance with PID " + octave.OctaveProcess.Id + " finished successfully. Time taken: " + (DateTime.Now - octave.OctaveProcess.StartTime).TotalSeconds + "s");
                }
                else
                {
                    if (!GlobalSettings.Settings.ExcelMode) CurrentMaterialStream.Flowsheet.ShowMessage("Octave instance with PID " + octave.OctaveProcess.Id + " finished successfully. Time taken: " + (DateTime.Now - octave.OctaveProcess.StartTime).TotalSeconds + "s", IFlowsheet.MessageType.Information);
                }
                if (GlobalSettings.Settings.ExcelMode) Console.WriteLine("Octave instance with PID " + octave.OctaveProcess.Id + " finished successfully. Time taken: " + (DateTime.Now - octave.OctaveProcess.StartTime).TotalSeconds + "s");
                switch (prop)
                {
                    case ThermoProperty.CompressibilityCoeff:
                        result = octave.GetScalar("Z");
                        break;
                    case ThermoProperty.Density:
                        result = octave.GetScalar("denMass");
                        break;
                    case ThermoProperty.Enthalpy:
                        result = RET_Hid(298.15, param1, vx) - octave.GetScalar("Hres") / AUX_MMM(vx) ;
                        break;
                    case ThermoProperty.FugacityCoeff:
                        if (CurrentMaterialStream.Phases[0].Compounds.Count == 1)
                        {
                            result = new double[]{octave.GetScalar("f")};
                        }
                        else
                        {
                            result = octave.GetVector("f");
                        }
                        break;
                    default:
                        break;
                }
            }
            catch (Exception ex)
            {
                if (!GlobalSettings.Settings.ExcelMode && !GlobalSettings.Settings.CAPEOPENMode) CurrentMaterialStream.Flowsheet.ShowMessage("Octave process with ID " + octave.OctaveProcess.Id + " finished with errors.", IFlowsheet.MessageType.GeneralError);
                octave.OctaveProcess.Kill();
                octave = null;
                File.Delete(filename);
                throw ex;
            }
            finally
            {
                octave.OctaveProcess.Kill();
                octave = null;
                File.Delete(filename);
            }

            return result;

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

        public double CalcCp(double[] Vx, double T, double P, State st)
        {

            double h1, h2;
            h1 = DW_CalcEnthalpy(Vx, T, P, st);
            h2 = DW_CalcEnthalpy(Vx, T + 0.01, P, st);

            return (h2 - h1) / 0.01;

        }

        public override double AUX_VAPDENS(double T, double P)
        {
            return (double)CalculateProperty(ThermoProperty.Density, RET_VMOL(PropertyPackages.Phase.Vapor), "V", T, P, 0, 0);
        }

        public override double AUX_LIQDENS(double T, Array Vx, double P = 0, double Pvp = 0, bool FORCE_EOS = false)
        {
            return (double)CalculateProperty(ThermoProperty.Density, Vx.Cast<double>().ToArray(), "L", T, P, 0, 0);
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
                    return RET_NullVector();
            }
        }

        public override double DW_CalcEnthalpy(Array Vx, double T, double P, PropertyPackages.State st)
        {
            double[] vz = Vx.Cast<double>().ToArray();
            if (UseLeeKeslerEnthalpy)
            {
                switch (st)
                {
                    case PropertyPackages.State.Liquid:
                        return lk.H_LK_MIX("L", T, P, vz, RET_VKij(), RET_VTC(), RET_VPC(), RET_VW(), RET_VMM(), RET_Hid(298.15, T, vz));
                    case PropertyPackages.State.Vapor:
                        return lk.H_LK_MIX("V", T, P, vz, RET_VKij(), RET_VTC(), RET_VPC(), RET_VW(), RET_VMM(), RET_Hid(298.15, T, vz));
                    case PropertyPackages.State.Solid:
                        return lk.H_LK_MIX("L", T, P, vz, RET_VKij(), RET_VTC(), RET_VPC(), RET_VW(), RET_VMM(), RET_Hid(298.15, T, vz)) - RET_HFUSM(AUX_CONVERT_MOL_TO_MASS(vz), T);
                    default:
                        return 0d;
                }
            }
            else
            {
                switch (st)
                {
                    case PropertyPackages.State.Liquid:
                        return (double)CalculateProperty(ThermoProperty.Enthalpy, vz, "L", T, P, 0, 0);
                    case PropertyPackages.State.Vapor:
                        return (double)CalculateProperty(ThermoProperty.Enthalpy, vz, "V", T, P, 0, 0);
                    case PropertyPackages.State.Solid:
                        return (double)CalculateProperty(ThermoProperty.Enthalpy, vz, "L", T, P, 0, 0) - RET_HFUSM(AUX_CONVERT_MOL_TO_MASS(vz), T);
                    default:
                        return 0d;
                }
            }
        }

        public override double DW_CalcEnthalpyDeparture(Array Vx, double T, double P, PropertyPackages.State st)
        {
            throw new NotImplementedException();
        }

        public override double DW_CalcEntropy(Array Vx, double T, double P, PropertyPackages.State st)
        {
            double[] vz = Vx.Cast<double>().ToArray();
            if (UseLeeKeslerEnthalpy)
            {
                switch (st)
                {
                    case PropertyPackages.State.Liquid:
                        return lk.S_LK_MIX("L", T, P, vz, RET_VKij(), RET_VTC(), RET_VPC(), RET_VW(), RET_VMM(), RET_Sid(298.15, T, P, vz));
                    case PropertyPackages.State.Vapor:
                        return lk.S_LK_MIX("V", T, P, vz, RET_VKij(), RET_VTC(), RET_VPC(), RET_VW(), RET_VMM(), RET_Sid(298.15, T, P, vz));
                    case PropertyPackages.State.Solid:
                        return lk.S_LK_MIX("L", T, P, vz, RET_VKij(), RET_VTC(), RET_VPC(), RET_VW(), RET_VMM(), RET_Sid(298.15, T, P, vz)) - RET_HFUSM(AUX_CONVERT_MOL_TO_MASS(vz), T) / T;
                    default:
                        return 0d;
                }
            }
            else
            {
                switch (st)
                {
                    case PropertyPackages.State.Liquid:
                        return (double)CalculateProperty(ThermoProperty.Enthalpy, vz, "L", T, P, 0, 0) / T;
                    case PropertyPackages.State.Vapor:
                        return (double)CalculateProperty(ThermoProperty.Enthalpy, vz, "V", T, P, 0, 0) / T;
                    case PropertyPackages.State.Solid:
                        return (double)CalculateProperty(ThermoProperty.Enthalpy, vz, "L", T, P, 0, 0) / T - RET_HFUSM(AUX_CONVERT_MOL_TO_MASS(vz), T) / T;
                    default:
                        return 0d;
                }
            }
        }

        public override double DW_CalcEntropyDeparture(Array Vx, double T, double P, PropertyPackages.State st)
        {
            throw new NotImplementedException();
        }

        public override void DW_CalcPhaseProps(PropertyPackages.Phase Phase)
        {
            double result = 0d;
            Phase dwpl = default(Phase);

            double T = 0d;
            double P = 0d;
            double phasemolarfrac = 0d;
            double overallmolarflow = 0d;

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

                if (!GlobalSettings.Settings.EnableParallelProcessing)
                {
                    result = this.AUX_LIQDENS(T, RET_VMOL(dwpl), P);
                    this.CurrentMaterialStream.Phases[phaseID].Properties.density = result;
                    this.CurrentMaterialStream.Phases[phaseID].Properties.compressibilityFactor = P / result * AUX_MMM(dwpl) / 1000 / 8.314 / T;

                    this.CurrentMaterialStream.Phases[phaseID].Properties.enthalpy = this.DW_CalcEnthalpy(RET_VMOL(dwpl), T, P, State.Liquid);
                    this.CurrentMaterialStream.Phases[phaseID].Properties.entropy = this.CurrentMaterialStream.Phases[phaseID].Properties.enthalpy.GetValueOrDefault() / T;

                    result = this.CalcCp(RET_VMOL(dwpl), T, P, State.Liquid);
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
                else
                {

                    result = this.AUX_MMM(Phase);
                    this.CurrentMaterialStream.Phases[phaseID].Properties.molecularWeight = result;

                    Task t1, t2, t3, t4;
                    t1 = Task.Factory.StartNew(() =>
                    {
                        var res = this.AUX_LIQDENS(T, P, 0.0, phaseID, false);
                        this.CurrentMaterialStream.Phases[phaseID].Properties.density = res;
                        this.CurrentMaterialStream.Phases[phaseID].Properties.compressibilityFactor = P / res * AUX_MMM(dwpl) / 1000 / 8.314 / T;
                        res = this.AUX_LIQVISCm(T);
                        this.CurrentMaterialStream.Phases[phaseID].Properties.viscosity = res;
                        this.CurrentMaterialStream.Phases[phaseID].Properties.kinematic_viscosity = res / this.CurrentMaterialStream.Phases[phaseID].Properties.density.GetValueOrDefault();
                    });
                    t2 = Task.Factory.StartNew(() =>
                    {
                        this.CurrentMaterialStream.Phases[phaseID].Properties.enthalpy = this.DW_CalcEnthalpy(RET_VMOL(dwpl), T, P, State.Liquid);
                        this.CurrentMaterialStream.Phases[phaseID].Properties.entropy = this.CurrentMaterialStream.Phases[phaseID].Properties.enthalpy.GetValueOrDefault() / T;
                        var res = this.CurrentMaterialStream.Phases[phaseID].Properties.enthalpy.GetValueOrDefault() * this.CurrentMaterialStream.Phases[phaseID].Properties.molecularWeight.GetValueOrDefault();
                        this.CurrentMaterialStream.Phases[phaseID].Properties.molar_enthalpy = res;
                        res = this.CurrentMaterialStream.Phases[phaseID].Properties.entropy.GetValueOrDefault() * this.CurrentMaterialStream.Phases[phaseID].Properties.molecularWeight.GetValueOrDefault();
                        this.CurrentMaterialStream.Phases[phaseID].Properties.molar_entropy = res;
                    });
                    t3 = Task.Factory.StartNew(() =>
                    {
                        var res = this.CalcCp(RET_VMOL(dwpl), T, P, State.Liquid);
                        this.CurrentMaterialStream.Phases[phaseID].Properties.heatCapacityCp = res;
                        this.CurrentMaterialStream.Phases[phaseID].Properties.heatCapacityCv = res;
                    });
                    t4 = Task.Factory.StartNew(() =>
                    {
                        var res = this.AUX_CONDTL(T);
                        this.CurrentMaterialStream.Phases[phaseID].Properties.thermalConductivity = res;
                    });
                    Task.WaitAll(t1, t2, t3, t4);

                }


            }
            else if (phaseID == 2)
            {

                if (!GlobalSettings.Settings.EnableParallelProcessing)
                {
                    result = this.AUX_VAPDENS(T, P);
                    this.CurrentMaterialStream.Phases[phaseID].Properties.density = result;
                    this.CurrentMaterialStream.Phases[phaseID].Properties.enthalpy = this.DW_CalcEnthalpy(RET_VMOL(dwpl), T, P, State.Vapor);
                    this.CurrentMaterialStream.Phases[phaseID].Properties.entropy = this.CurrentMaterialStream.Phases[phaseID].Properties.enthalpy.GetValueOrDefault() / T;
                    this.CurrentMaterialStream.Phases[phaseID].Properties.compressibilityFactor = P / result * AUX_MMM(dwpl) / 1000 / 8.314 / T;
                    result = this.CalcCp(RET_VMOL(dwpl), T, P, State.Vapor);
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
                else
                {
                    result = this.AUX_MMM(Phase);
                    this.CurrentMaterialStream.Phases[phaseID].Properties.molecularWeight = result;

                    Task t1, t2, t3, t4;
                    t1 = Task.Factory.StartNew(() =>
                    {
                        var res = this.AUX_VAPDENS(T, P);
                        this.CurrentMaterialStream.Phases[phaseID].Properties.density = res;
                        this.CurrentMaterialStream.Phases[phaseID].Properties.compressibilityFactor = P / res * AUX_MMM(dwpl) / 1000 / 8.314 / T;
                        res = this.AUX_VAPVISCm(T, this.CurrentMaterialStream.Phases[phaseID].Properties.density.GetValueOrDefault(), this.AUX_MMM(Phase));
                        this.CurrentMaterialStream.Phases[phaseID].Properties.viscosity = res;
                        this.CurrentMaterialStream.Phases[phaseID].Properties.kinematic_viscosity = res / this.CurrentMaterialStream.Phases[phaseID].Properties.density.Value;
                    });
                    t2 = Task.Factory.StartNew(() =>
                    {
                        this.CurrentMaterialStream.Phases[phaseID].Properties.enthalpy = this.DW_CalcEnthalpy(RET_VMOL(dwpl), T, P, State.Vapor);
                        this.CurrentMaterialStream.Phases[phaseID].Properties.entropy = this.CurrentMaterialStream.Phases[phaseID].Properties.enthalpy.GetValueOrDefault() / T;
                        var res = this.CurrentMaterialStream.Phases[phaseID].Properties.enthalpy.GetValueOrDefault() * this.CurrentMaterialStream.Phases[phaseID].Properties.molecularWeight.GetValueOrDefault();
                        this.CurrentMaterialStream.Phases[phaseID].Properties.molar_enthalpy = res;
                        res = this.CurrentMaterialStream.Phases[phaseID].Properties.entropy.GetValueOrDefault() * this.CurrentMaterialStream.Phases[phaseID].Properties.molecularWeight.GetValueOrDefault();
                        this.CurrentMaterialStream.Phases[phaseID].Properties.molar_entropy = res;
                    });
                    t3 = Task.Factory.StartNew(() =>
                    {
                        var res = this.CalcCp(RET_VMOL(dwpl), T, P, State.Vapor);
                        this.CurrentMaterialStream.Phases[phaseID].Properties.heatCapacityCp = res;
                        this.CurrentMaterialStream.Phases[phaseID].Properties.heatCapacityCv = res;
                    });
                    t4 = Task.Factory.StartNew(() =>
                    {
                        var res = this.AUX_CONDTG(T, P);
                        this.CurrentMaterialStream.Phases[phaseID].Properties.thermalConductivity = res;
                    });
                    Task.WaitAll(t1, t2, t3, t4, t4);
                }

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

            double result = 0.0d;
            int phaseID = -1;
            string state = "";
            State pstate = default(State);

            double T = 0d;
            double P = 0d;
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
                case "heatcapacitycv":
                    if (state == "V")
                    {
                        result = this.CalcCp(RET_VMOL(phase), T, P, State.Vapor);
                    }
                    else
                    {
                        result = this.CalcCp(RET_VMOL(phase), T, P, State.Liquid);
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

        public override bool LoadData(List<System.Xml.Linq.XElement> data)
        {
            return base.LoadData(data);
        }

        public override List<System.Xml.Linq.XElement> SaveData()
        {
            return base.SaveData();
        }

    }
}
