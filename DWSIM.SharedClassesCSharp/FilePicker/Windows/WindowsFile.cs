using DWSIM.Interfaces;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace DWSIM.SharedClassesCSharp.FilePicker.Windows
{
    public class WindowsFile : IFile
    {
        private string _filePath;

        public WindowsFile(string filePath)
        {
            _filePath = filePath;
        }

        public string ReadAllText()
        {
            return File.ReadAllText(_filePath);
        }
    }
}
