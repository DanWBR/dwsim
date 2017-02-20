namespace DWSIM.Thermodynamics.AdvancedEOS
{
    public class VPTPropertyPackage : AdvEOSPropertyPackageBase
    {
        public VPTPropertyPackage()
        {
            PropertyPackageModel = Model.VPT;
        }

        protected override string GetModelSpecificParameters()
        {
            return "";
        }
    }
}
