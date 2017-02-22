namespace DWSIM.Thermodynamics.AdvancedEOS
{
    public class SAFTPropertyPackage: AdvEOSPropertyPackageBase
    {
        public SAFTPropertyPackage()
        {
            PropertyPackageModel = Model.SAFT;
            ComponentName = "Statistical Associating Fluid Theory (SAFT)";
            ComponentDescription = "";
            IsConfigurable = true;
        }
        
        protected override string GetModelSpecificParameters()
        {
            return "";
        }
    }
}
