using System.Collections.Generic;
using DWSIM.Thermodynamics.PropertyPackages.Auxiliary;
using System.IO;
using System.Reflection;
using FileHelpers;
using DWSIM.Thermodynamics.AdvancedEOS.EditingForms;

namespace DWSIM.Thermodynamics.AdvancedEOS
{
    public class PCSAFTPropertyPackage : BaseSAFTPropertyPackage
    {

        public PCSAFTPropertyPackage()
        {
            PropertyPackageModel = Model.PC_SAFT;

            ComponentName = "PC-SAFT (with Association Support)";
            ComponentDescription = "The Perturbed Chain SAFT model is a state-of-the-art, engineering-like equation of state. It is designed for modelling mixtures of all types of substances: gases, solvents and polymers.";

            IsConfigurable = true;

            ReadParameters();

        }

    }
}

