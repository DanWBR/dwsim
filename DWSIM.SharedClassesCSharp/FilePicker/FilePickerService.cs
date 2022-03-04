using DWSIM.Interfaces;
using DWSIM.SharedClassesCSharp.FilePicker.Windows;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace DWSIM.SharedClassesCSharp.FilePicker
{
    public class FilePickerService : IFilePickerService
    {
        private static FilePickerService _instance;
        public static IFilePickerService GetInstance()
        {
            if (_instance == null)
                _instance = new FilePickerService();

            return _instance;
        }

        private FilePickerService()
        {

        }

        public IFilePicker GetFilePicker()
        {
            return new WindowsFilePicker();
        }
    }
}
