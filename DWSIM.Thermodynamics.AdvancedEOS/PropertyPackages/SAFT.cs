namespace DWSIM.Thermodynamics.AdvancedEOS
{
    public class SAFTPropertyPackage: AdvEOSPropertyPackageBase
    {
        public SAFTPropertyPackage()
        {
            PropertyPackageModel = Model.SAFT;
        }
        
        protected override string GetModelSpecificParameters()
        {
            return "";
        }
    }
}
