using System;
using System.Collections.Generic;
using System.Text;

namespace DotNumerics.Optimization.LBFGSB
{

    public enum BFGSTask
    {
        None,
        RESTART,
        WARNING,
        START,
        FG,
        FG_LNSRCH, 
        FG_ST,
        FG_START,
        CPU,
        NEW_X,
        CONV,
        ABNO,
        ERROR,
        STOP
    }

    public enum BFGSWord
    {
        con,
        bnd,
        tnt,
        aaa
    }


}
