using DWSIM.Interfaces;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace DWSIM.SharedClassesCSharp.FilePicker
{
    public class FilePickerAllowedType : IFilePickerAllowedType
    {
        public string Name { get; private set; }

        public IEnumerable<string> AllowedExtensions { get; private set; }

        public FilePickerAllowedType(string name, string[] extensions)
        {
            Name = name;
            AllowedExtensions = extensions;
        }

        public FilePickerAllowedType(string name, string extension)
        {
            Name = name;
            AllowedExtensions = new[] { extension };
        }
    }
}
