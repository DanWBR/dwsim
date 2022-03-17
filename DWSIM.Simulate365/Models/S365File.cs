using DWSIM.Interfaces;
using DWSIM.Simulate365.Services;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace DWSIM.Simulate365.Models
{
   public class S365File : IVirtualFile
    {
        private string _localTmpFile;

        public string ParentDriveId { get; set; }

        // we are using Drive file Ids, there are also list Ids on sharepoint
        public string FileId { get; set; }

        public string DriveId { get; set; }

        public string Filename { get; set; }
        
        /// <summary>
        /// Directory path on Simulate 365, doesn't contain file name
        /// </summary>
        public string FullPath { get; set; }

        public S365File(string localTmpFile)
        {
            _localTmpFile = localTmpFile;
        }

        public void Delete()
        {
            throw new NotImplementedException();
        }

        public bool Exists()
        {
            throw new NotImplementedException();
        }

        public string GetExtension()
        {
            return Path.GetExtension(Filename);
        }

        public Stream OpenRead()
        {
            return File.OpenRead(_localTmpFile);
        }

        public string ReadAllText()
        {
            return System.IO.File.ReadAllText(_localTmpFile);
        }

        public void Write(string localFilePath)
        {
            var file = FileUploaderService.UploadFile(DriveId, ParentDriveId, localFilePath, Filename, FullPath);
            FileId = file.FileId;
        }

        public void Write(System.IO.Stream stream)
        {
            var file = FileUploaderService.UploadFile(DriveId, ParentDriveId, stream, Filename, FullPath);
            FileId = file.FileId;
        }
    }
}
