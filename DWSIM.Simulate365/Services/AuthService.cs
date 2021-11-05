using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;

namespace DWSIM.Simulate365.Services
{
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ComVisible(true)]
    public  class AuthService
    {

        public string GetUserToken()
        {
            var token = UserService.GetInstance().GetUserToken();
            return token;
        }

    }
}
