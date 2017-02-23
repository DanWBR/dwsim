using System.Collections.Generic;
using DWSIM.Thermodynamics.PropertyPackages.Auxiliary;

namespace DWSIM.Thermodynamics.AdvancedEOS
{
    public class SAFTPropertyPackage: BaseSAFTPropertyPackage
    {

        public SAFTPropertyPackage()
        {
            PropertyPackageModel = Model.SAFT;
            ComponentName = "Statistical Associating Fluid Theory (SAFT)";
            ComponentDescription = "";
            IsConfigurable = true;

            ReadParameters();

        }

    }
}
