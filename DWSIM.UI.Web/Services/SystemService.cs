using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;

namespace DWSIM.UI.Web.Services
{
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ComVisible(true)]
    public class SystemService
    {
        public void OpenUrlInLocalBrowser(string url)
        {
            Process.Start(url);
        }
    }
}
