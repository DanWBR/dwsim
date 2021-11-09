using Microsoft.Graph;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;

namespace DWSIM.Simulate365.Services
{
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ComVisible(true)]
    public class FilePickerService
    {
        public static event EventHandler S3365DashboardFileOpenStarted;
        public static event EventHandler<S365OpenFileEventArgs> S3365DashboardFileOpened;
        public static event EventHandler<S365SaveFileEventArgs> S365DashboardSaveFileClicked;
        public static event EventHandler S365DashboardFolderCreated;

        public async void OpenFile(string driveItemId, string flowsheetsDriveId)
        {
            try
            {
                S3365DashboardFileOpenStarted?.Invoke(this, new EventArgs());
                var token = UserService.GetInstance().GetUserToken();
                var client = GraphClientFactory.CreateClient(token);

                var item = await client.Drives[flowsheetsDriveId].Items[driveItemId].Request().GetAsync();
                // Get drive item
                var stream = await client.Drives[flowsheetsDriveId].Items[driveItemId].Content.Request().GetAsync();


                var extension = System.IO.Path.GetExtension(item.Name);
                var filePath = Path.Combine(Path.GetTempPath(), $"{Guid.NewGuid().ToString()}{extension}");

                using (var destStream = System.IO.File.OpenWrite(filePath))
                    await stream.CopyToAsync(destStream);

                S3365DashboardFileOpened?.Invoke(this, new S365OpenFileEventArgs { FilePath = filePath, FlowsheetsDriveId = flowsheetsDriveId, DriveItemId = driveItemId });

            }
            catch (Exception ex)
            {

                throw new Exception("An error occurred while opening file from S365 Dashboard.", ex);
            }
        }
        public async void SaveFile(string filename, string extension, string flowsheetsDriveId, string parentDriveId)
        {
            try
            {
                S365DashboardSaveFileClicked?.Invoke(this, new S365SaveFileEventArgs
                {
                    Filename = $"{filename}.{extension}",
                    FlowsheetsDriveId = flowsheetsDriveId,
                    ParentDriveId = parentDriveId
                });
            }
            catch (Exception ex)
            {

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

                Task.Run( async ()=> { await client.Drives[flowsheetsDriveId].Items[parentDriveId].Children.Request().AddAsync(driveItem); }).Wait();
                
                    S365DashboardFolderCreated?.Invoke(this, new EventArgs());
            }
            catch (Exception ex)
            {

                throw new Exception("An error occurred while creating folder on S365 Dashboard.", ex);
            }
        }

    }

    public class S365SaveFileEventArgs
    {
        public string Filename { get; set; }
        public string FlowsheetsDriveId { get; set; }
        public string ParentDriveId { get; set; }
    }

    public class S365OpenFileEventArgs
    {
        public string FilePath { get; set; }
        //drive id of Flowsheets folder in simulate 365
        public string FlowsheetsDriveId { get; set; }
        public string DriveItemId { get; set; }
    }
}
