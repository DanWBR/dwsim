using System;

namespace DWSIM.UI.Desktop
{
    public class Flowsheet: FlowsheetBase.FlowsheetBase
    {

        public override void DisplayForm(object form)
        {
            throw new NotImplementedException();
        }

        public override Interfaces.IFlowsheet GetNewInstance()
        {
            return new Flowsheet();
        }

        public override void ShowDebugInfo(string text, int level)
        {
            Console.WriteLine(text);
        }

        public override void ShowMessage(string text, Interfaces.IFlowsheet.MessageType mtype)
        {
            throw new NotImplementedException();
        }

        public override void UpdateOpenEditForms()
        {
            throw new NotImplementedException();
        }
    }
}
