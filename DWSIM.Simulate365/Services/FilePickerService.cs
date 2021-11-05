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
        public static event EventHandler<string> S3365DashboardFileOpened;

        public async  void OpenFile(string siteId, string driveItemId, string driveId)
        {
            try
            {
                S3365DashboardFileOpenStarted?.Invoke(this, new EventArgs());
                var token = UserService.GetInstance().GetUserToken();
                var client =  GraphClientFactory.CreateClient(token);

                var item = await client.Drives[driveId].Items[driveItemId].Request().GetAsync();
                // Get drive item
                var stream = await client.Drives[driveId].Items[driveItemId].Content.Request().GetAsync();

                
                var extension = System.IO.Path.GetExtension(item.Name);
                var filePath = Path.Combine(Path.GetTempPath(), $"{Guid.NewGuid().ToString()}{extension}");

                using (var destStream = File.OpenWrite(filePath))
                  await   stream.CopyToAsync(destStream);

                S3365DashboardFileOpened?.Invoke(this, filePath);

            }
            catch (Exception ex)
            {

                throw new Exception("An error occurred while opening file from S365 Dashboard.",ex);
            }
        }
    }
}
