using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace DWSIM.Thermodynamics.AdvancedEOS
{
    class AdvEOSPropertyPackageBase: PropertyPackages.PropertyPackage
    {

        void test()
        { 
        
            var octave = new Octave(@"C:\Users\ptc0\Documents\Octave-4.0.3\bin\");

            //octave.ExecuteCommand("addpath('ECE')");
            //octave.ExecuteCommand("ECE/Examples'");
            //octave.ExecuteCommand("PCSAFTExample");
            //double[] K = octave.GetVector("K");

        }
        
        public override double AUX_VAPDENS(double T, double P)
        {
            throw new NotImplementedException();
        }

        public override void DW_CalcCompPartialVolume(PropertyPackages.Phase phase, double T, double P)
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

        public override double DW_CalcEnergyFlowMistura_ISOL(double T, double P)
        {
            throw new NotImplementedException();
        }

        public override double DW_CalcEnthalpy(Array Vx, double T, double P, PropertyPackages.State st)
        {
            throw new NotImplementedException();
        }

        public override double DW_CalcEnthalpyDeparture(Array Vx, double T, double P, PropertyPackages.State st)
        {
            throw new NotImplementedException();
        }

        public override double DW_CalcEntropy(Array Vx, double T, double P, PropertyPackages.State st)
        {
            throw new NotImplementedException();
        }

        public override double DW_CalcEntropyDeparture(Array Vx, double T, double P, PropertyPackages.State st)
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

        public override void DW_CalcPhaseProps(PropertyPackages.Phase Phase)
        {
            throw new NotImplementedException();
        }

        public override void DW_CalcProp(string property, PropertyPackages.Phase phase)
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

        public override bool MobileCompatible
        {
            get { throw new NotImplementedException(); }
        }

        public override bool SupportsComponent(Interfaces.ICompoundConstantProperties comp)
        {
            throw new NotImplementedException();
        }

    }
}
