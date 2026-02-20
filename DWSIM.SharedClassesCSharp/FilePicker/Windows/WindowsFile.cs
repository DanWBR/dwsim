using DWSIM.Interfaces;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace DWSIM.SharedClassesCSharp.FilePicker.Windows
{
    public class WindowsFile : IVirtualFile
    {
        private string _filePath;

        public string Filename { get => Path.GetFileName(_filePath); }

        public string FullPath => _filePath;

        public string ParentUniqueIdentifier => throw new NotImplementedException();

        public WindowsFile(string filePath)
        {
            _filePath = filePath;
        }
        
        public string ReadAllText()
        {
            return File.ReadAllText(_filePath);
        }

        public Stream OpenRead()
        {
            return File.OpenRead(_filePath);
        }

        public void Write(Stream stream)
        {
            stream.Seek(0, SeekOrigin.Begin);

            using (var fileStream = new FileStream(_filePath, FileMode.Create, FileAccess.Write))
            {
                stream.CopyTo(fileStream);
            }
        }

        public void Write(string localFile)
        {
            File.Copy(localFile, _filePath);
        }

        public string GetExtension()
        {
            return Path.GetExtension(_filePath);
        }        

        public void Delete()
        {
            File.Delete(_filePath);
        }

        public bool Exists()
        {
            return File.Exists(_filePath);
        }

        
    }
}
