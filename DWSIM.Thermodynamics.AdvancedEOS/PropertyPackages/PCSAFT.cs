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
            ComponentDescription = "";

            IsConfigurable = true;

            ReadParameters();

        }

    }
}

