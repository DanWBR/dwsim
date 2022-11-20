using DWSIM.Simulate365.Models;
using Microsoft.Graph;
using System;
using System.Collections.Generic;
using System.IO;
using System.Runtime.InteropServices;
using System.Threading.Tasks;

namespace DWSIM.Simulate365.Services
{
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ComVisible(true)]
    public class FilePickerService
    {
        public event EventHandler S3365DashboardFileOpenStarted;
        public event EventHandler<S365File> S3365DashboardFileOpened;
        public event EventHandler<S365DashboardSaveFile> S365DashboardSaveFileClicked;
        public event EventHandler S365DashboardFolderCreated;

        public S365DashboardSaveFile SelectedSaveFile { get; private set; } = null;
        public S365File SelectedOpenFile { get; private set; } = null;

        public void OpenFile(string driveItemId, string flowsheetsDriveId, string fullPath)
        {
            try
            {
                S3365DashboardFileOpenStarted?.Invoke(this, new EventArgs());

                var token = UserService.GetInstance().GetUserToken();
                var client = GraphClientFactory.CreateClient(token);

                var item = Task.Run(async () => await client.Drives[flowsheetsDriveId].Items[driveItemId].Request().GetAsync()).Result;

                // Get drive item
                var stream = Task.Run(async () => await client.Drives[flowsheetsDriveId].Items[driveItemId].Content.Request().GetAsync()).Result;

                var extension = System.IO.Path.GetExtension(item.Name);
                var tmpFilePath = Path.Combine(Path.GetTempPath(), $"{Guid.NewGuid().ToString()}{extension}");

                Task.Run(async () =>
                {
                    using (var destStream = System.IO.File.OpenWrite(tmpFilePath))
                        await stream.CopyToAsync(destStream);
                }).Wait();

                this.SelectedOpenFile = new S365File(tmpFilePath) 
                { 
                    Filename = item.Name, 
                    DriveId = flowsheetsDriveId, 
                    ParentDriveId=item.ParentReference.Id,
                    FileId = driveItemId, 
                    FullPath = fullPath 
                };

                S3365DashboardFileOpened?.Invoke(this, this.SelectedOpenFile);
            }
            catch (Exception ex)
            {
                this.SelectedOpenFile = null;
                throw new Exception("An error occurred while opening file from S365 Dashboard.", ex);
            }
        }
        public void SaveFile(string filename, string flowsheetsDriveId, string parentDriveId, string fullPath)
        {
            try
            {
                this.SelectedSaveFile = new S365DashboardSaveFile
                {
                    Filename = filename,
                    FlowsheetsDriveId = flowsheetsDriveId,
                    ParentDriveId = parentDriveId,
                    SimulatePath= fullPath
                };

                this.S365DashboardSaveFileClicked?.Invoke(this, this.SelectedSaveFile);
            }
            catch (Exception ex)
            {
                this.SelectedSaveFile = null;
                throw new Exception("An error occurred while saving file to S365 Dashboard.", ex);
            }
        }

        public void CreateFolder(string folderName, string flowsheetsDriveId, string parentDriveId)
        {
            try
            {
                var token = UserService.GetInstance().GetUserToken();
                var client = GraphClientFactory.CreateClient(token);
                var driveItem = new DriveItem
                {
                    Name = folderName,
                    Folder = new Folder
                    {
                    },
                    AdditionalData = new Dictionary<string, object>()
                                             {
                                                 {"@microsoft.graph.conflictBehavior", "rename"}
                                             }
                };

                Task.Run(async () => { await client.Drives[flowsheetsDriveId].Items[parentDriveId].Children.Request().AddAsync(driveItem); }).Wait();

                S365DashboardFolderCreated?.Invoke(this, new EventArgs());
            }
            catch (Exception ex)
            {

                throw new Exception("An error occurred while creating folder on S365 Dashboard.", ex);
            }
        }

    }

    public class S365DashboardSaveFile
    {
        public string Filename { get; set; }
        public string FlowsheetsDriveId { get; set; }
        public string ParentDriveId { get; set; }
        public string SimulatePath { get; set; }
    }
}
