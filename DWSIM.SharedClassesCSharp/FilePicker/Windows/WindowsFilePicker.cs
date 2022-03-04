using DWSIM.Interfaces;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace DWSIM.SharedClassesCSharp.FilePicker.Windows
{
    public class WindowsFilePicker : IFilePicker
    {
        public IFile ShowOpenDialog(IEnumerable<IFilePickerAllowedType> allowedTypes)
        {
            var openFileDialog = new OpenFileDialog();

            if (allowedTypes != null && allowedTypes.Count() > 0)
            {
                var list = allowedTypes.Select(t => t.Name + "|" + String.Join(";", t.AllowedExtensions));
                openFileDialog.Filter = String.Join("|", list);
            }

            // Show the dialog
            DialogResult result = openFileDialog.ShowDialog();

            if (result == DialogResult.OK) // Test result.
            {
                var file = new WindowsFile(openFileDialog.FileName);
                return file;
            }
            else
                return null;
        }

        //public IEnumerable<IFile> ShowOpenDialog()
        //{
        //    var openFileDialog = new OpenFileDialog();
        //    DialogResult result = openFileDialog.ShowDialog(); // Show the dialog.

        //    if (result == DialogResult.OK) // Test result.
        //    {
        //        var files = openFileDialog.FileNames.Select(f => new WindowsFile(f));
        //        return files;
        //    }
        //    else
        //        return null;
        //}

        public void ShowSaveDialog()
        {
            throw new NotImplementedException();
        }
    }

    
}
