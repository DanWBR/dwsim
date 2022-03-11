using DWSIM.Interfaces;
using DWSIM.Simulate365.Services;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace DWSIM.Simulate365.Models
{
   public class S365File : IVirtualFile
    {
        public string ParentDriveId { get; set; }

        // we are using Drive file Ids, there are also list Ids on sharepoint
        public string FileId { get; set; }

        public string DriveId { get; set; }

        public string Filename { get; set; }

        // Local file path
        public string FilePath { get; set; }

        /// <summary>
        /// Directory path on Simulate 365, doesn't contain file name
        /// </summary>
        public string SimulatePath { get; set; }

        #region FilePickerService

        public string ReadAllText()
        {
            return System.IO.File.ReadAllText(FilePath);
        }

        public void Write(string localFilePath)
        {
            FileUploaderService.UploadFile(DriveId, ParentDriveId, localFilePath, Filename, SimulatePath);
        }

        public void Write(System.IO.Stream stream)
        {
            FileUploaderService.UploadFile(DriveId, ParentDriveId, stream, Filename, SimulatePath);
        }

        #endregion
    }
}
