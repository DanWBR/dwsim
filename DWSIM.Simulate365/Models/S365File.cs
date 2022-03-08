using DWSIM.Interfaces;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace DWSIM.Simulate365.Models
{
   public class S365File : IFile
    {
        // we are using Drive file Ids, there are also list Ids on sharepoint
        public string FileId { get; set; }
        public string DriveId { get; set; }
        public string Filename { get; set; }
        public string FilePath { get; set; }
        public string SimulatePath { get; set; }

        #region FilePickerService

        public string ReadAllText()
        {
            return System.IO.File.ReadAllText(FilePath);
        }

        #endregion
    }
}
