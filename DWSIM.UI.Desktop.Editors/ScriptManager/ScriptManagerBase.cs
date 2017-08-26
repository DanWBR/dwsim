using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Eto.Forms;

namespace DWSIM.UI.Desktop.Editors
{
    public abstract class ScriptManagerBase : TableLayout
    {

        public ScriptManagerBase()
            : base()
        { 
        
        }

        public abstract void UpdateList();

        public abstract void UpdateScripts();

    }
}
