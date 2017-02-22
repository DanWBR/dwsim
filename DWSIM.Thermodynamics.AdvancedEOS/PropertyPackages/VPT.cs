namespace DWSIM.Thermodynamics.AdvancedEOS
{
    public class VPTPropertyPackage : AdvEOSPropertyPackageBase
    {
        public VPTPropertyPackage()
        {
            PropertyPackageModel = Model.VPT;
            ComponentName = "Valderrama-Patel-Teja EOS (VPT)";
            ComponentDescription = "";
            IsConfigurable = true;
        }

        protected override string GetModelSpecificParameters()
        {
            return "";
        }
    }
}
