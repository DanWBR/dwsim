
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace DWSIM.Simulate365.Services
{
    public class FileManagementService
    {
        private static FileManagementService _singletonInstance;
        #region Public events    
        public event EventHandler OnSaveFileToDashboard;
        public event EventHandler OnFileSavedToDashboard;

        #endregion

        public static FileManagementService GetInstance()
        {
            if (_singletonInstance == null)
                _singletonInstance = new FileManagementService();

            return _singletonInstance;
        }
        public void SaveFileToDashboard()
        {
            this.OnSaveFileToDashboard?.Invoke(this, EventArgs.Empty);
        }
        public void FileSavedToDashboard()
        {
            this.OnFileSavedToDashboard?.Invoke(this, EventArgs.Empty);
        }
    }
}
