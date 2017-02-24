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
            ComponentDescription = "Based on the Statistical Associating Fluid Theory (SAFT), an equation-of-state model has been developed for predicting phase equilibria. The agreement with molecular simulation data has been found to be excellent for associating spheres, mixtures of associating spheres, and non-associating chains.";
            IsConfigurable = true;

            ReadParameters();

        }

    }
}
