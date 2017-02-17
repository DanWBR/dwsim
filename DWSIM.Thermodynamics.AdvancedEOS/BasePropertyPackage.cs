// DWSIM Advanced EOS Model Library
// Based on http://hpp.uva.es/open-source-software-eos/

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Reflection;
using System.IO;

namespace DWSIM.Thermodynamics.AdvancedEOS
{

    public enum Model
    { 
        //EOS
        PRBM, //PR-Boston-Mathias
        PRWS, //PR-Wong-Sandler
        PT, //Patel-Teja
        VPT, //Valderrama-Patel-Teja

        //Predictive EOS
        PSRK,
        GC,

        // Chain
        PHSC,

        //SAFT
        SAFT,
        PC_SAFT  
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

        public Octave GetOctaveInstance()
        { 
        
            var octave = new Octave(GlobalSettings.Settings.OctavePath);

            octave.ExecuteCommand("addpath('" + Path.GetDirectoryName(Assembly.GetEntryAssembly().Location) + Path.DirectorySeparatorChar + "ECE')");
            octave.ExecuteCommand("cd '" + GlobalSettings.Settings.OctaveFileTempDir + "'");

            return octave;

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

        public override double[] DW_CalcFugCoeff(Array Vx, double T, double P, PropertyPackages.State st)
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

    }
}
