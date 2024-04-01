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

        public string ParentUniqueIdentifier { get; set; }

        public string FileUniqueIdentifier { get; set; }

        public string Filename { get; set; }

        /// <summary>
        /// Directory path on Simulate 365, doesn't contain file name
        /// </summary>
        public string FullPath { get; set; }
        public UploadConflictAction? ConflictAction { get; set; }

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
            var file = FileUploaderService.UploadFile(FileUniqueIdentifier, ParentUniqueIdentifier, localFilePath, Filename, FullPath, ConflictAction ?? UploadConflictAction.Overwrite);
            FileUniqueIdentifier = file.FileUniqueIdentifier;
            Filename = file.Filename;
            FullPath = file.FullPath;
            FileManagementService.GetInstance().FileSavedToDashboard();
        }

        public void Write(System.IO.Stream stream)
        {
            var file = FileUploaderService.UploadFile(FileUniqueIdentifier, ParentUniqueIdentifier, stream, Filename, FullPath, ConflictAction ?? UploadConflictAction.Overwrite);
            FileUniqueIdentifier = file.FileUniqueIdentifier;
            Filename = file.Filename;
            FullPath = file.FullPath;
            FileManagementService.GetInstance().FileSavedToDashboard();
        }


    }
}
