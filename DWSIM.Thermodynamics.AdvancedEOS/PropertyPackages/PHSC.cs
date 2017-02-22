namespace DWSIM.Thermodynamics.AdvancedEOS
{
    public class PHSCPropertyPackage : AdvEOSPropertyPackageBase
    {
        public PHSCPropertyPackage()
        {
            PropertyPackageModel = Model.PHSC;
            ComponentName = "Perturbed Hard Sphere Chain (PHSC)";
            ComponentDescription = "";
            IsConfigurable = true;

        }

        protected override string GetModelSpecificParameters()
        {
            return "";
        }

        protected override string GetModelInteractionParameters()
        {
 	        return "";
        }
    }
}

