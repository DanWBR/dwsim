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
        public IVirtualFile ShowOpenDialog(IEnumerable<IFilePickerAllowedType> allowedTypes)
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

        public IVirtualFile ShowSaveDialog(IEnumerable<IFilePickerAllowedType> allowedTypes)
        {
            var saveFileDialog = new SaveFileDialog();

            if (allowedTypes != null && allowedTypes.Count() > 0)
            {
                var list = allowedTypes.Select(t => t.Name + "|" + String.Join(";", t.AllowedExtensions));
                saveFileDialog.Filter = String.Join("|", list);
            }

            // Show the dialog
            DialogResult result = saveFileDialog.ShowDialog();

            if (result == DialogResult.OK) // Test result.
            {
                var file = new WindowsFile(saveFileDialog.FileName);
                return file;
            }
            else
                return null;
        }
    }    
}
