using DWSIM.Simulate365.FormFactories;
using DWSIM.Simulate365.Models;
using DWSIM.UI.Web.Settings;
using Microsoft.Graph;
using Newtonsoft.Json;
using System;
using System.Collections.Generic;
using System.Data;
using System.IO;
using System.Linq;
using System.Net.Http;
using System.Runtime.InteropServices;
using System.Text;
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

        public void OpenFile(string fileUniqueIdentifier)
        {
            try
            {
                S3365DashboardFileOpenStarted?.Invoke(this, new EventArgs());

                var token = UserService.GetInstance().GetUserToken();
                var client = GetDashboardClient(token);

                var result = Task.Run(async () => await client.GetAsync($"/api/files/{fileUniqueIdentifier}/single?includeBreadcrumbs=true")).Result;
                var resultContent = Task.Run(async () => await result.Content.ReadAsStringAsync()).Result;
                var itemWithBreadcrumbs = JsonConvert.DeserializeObject<FilesWithBreadcrumbsResponseModel>(resultContent);

                var item = itemWithBreadcrumbs.File;
                // Get drive item
                var stream = Task.Run(async () => await client.GetStreamAsync($"/api/files/{fileUniqueIdentifier}/download")).Result;

                var extension = System.IO.Path.GetExtension(item.Name);
                var tmpFilePath = Path.Combine(Path.GetTempPath(), $"{Guid.NewGuid().ToString()}{extension}");

                Task.Run(async () =>
                {
                    using (var destStream = System.IO.File.OpenWrite(tmpFilePath))
                        await stream.CopyToAsync(destStream);
                }).Wait();

                var parentFolderBreadcrumb = itemWithBreadcrumbs.BreadcrumbItems.LastOrDefault();

                this.SelectedOpenFile = new S365File(tmpFilePath)
                {
                    Filename = item.Name,
                    ParentUniqueIdentifier = parentFolderBreadcrumb?.UniqueIdentifier.ToString() ?? string.Empty,
                    FileUniqueIdentifier = item.UniqueIdentifier.ToString(),
                    FullPath = GetFullPath(itemWithBreadcrumbs.BreadcrumbItems,item)
                };

                S3365DashboardFileOpened?.Invoke(this, this.SelectedOpenFile);
            }
            catch (Exception ex)
            {
                this.SelectedOpenFile = null;
                throw new Exception("An error occurred while opening file from S365 Dashboard.", ex);
            }
        }
        private string GetFullPath(List<BreadcrumbItem> breadcrumbItems, FileModel file)
        {
            var filePath = "//Simulate 365 Dashboard";

            if (breadcrumbItems?.Any() ?? false)
            {
                var breadcrumbPath = string.Join("/", breadcrumbItems.Select(x => x.Name));
                filePath = $"{filePath}/{breadcrumbPath}/{file.Name}";
            }
            else
            {
                filePath = $"{filePath}/{file.Name}";
            }
            return filePath;
        }

        public void SaveFile(string filename, string parentDirectoryUniqueId, string fullPath, int? conflictAction)
        {
            try
            {
                this.SelectedSaveFile = new S365DashboardSaveFile
                {
                    Filename = filename,
                    ParentUniqueIdentifier = parentDirectoryUniqueId,
                    SimulatePath = fullPath,
                    ConflictAction = conflictAction.HasValue ? (UploadConflictAction)conflictAction.Value : (UploadConflictAction?)null
                };

                this.S365DashboardSaveFileClicked?.Invoke(this, this.SelectedSaveFile);
            }
            catch (Exception ex)
            {
                this.SelectedSaveFile = null;
                throw new Exception("An error occurred while saving file to S365 Dashboard.", ex);
            }
        }

        public void CreateFolder(string folderName, string parentDirectoryUniqueId)
        {
            try
            {
                var token = UserService.GetInstance().GetUserToken();
                var client = GetDashboardClient(token);

                var model = new
                {
                    ParentDirectoryUniqueId = !string.IsNullOrWhiteSpace(parentDirectoryUniqueId) ? Guid.Parse(parentDirectoryUniqueId) : (Guid?)null,
                    DirectoryName = folderName,
                    // 0 = Overwrite folder, 1= Keep both
                    ConflictAction = 0
                };

                var content = new StringContent(JsonConvert.SerializeObject(model), Encoding.UTF8, "application/json");

                Task.Run(async () => { await client.PostAsync($"/api/files", content); }).Wait();

                S365DashboardFolderCreated?.Invoke(this, new EventArgs());
            }
            catch (Exception ex)
            {

                throw new Exception("An error occurred while creating folder on S365 Dashboard.", ex);
            }
        }

        public void ShowLoginForm()
        {
            var userService = UserService.GetInstance();
            userService.ShowLogin();
        }

        private HttpClient GetDashboardClient(string token)
        {
            var client = new HttpClient();
            client.BaseAddress = new Uri(DashboardSettings.DashboardServiceUrl);
            client.DefaultRequestHeaders.Add("Authorization", $"Bearer {token}");

            return client;

        }
    }

    public class S365DashboardSaveFile
    {
        public string Filename { get; set; }
        public string ParentUniqueIdentifier { get; set; }
        public string SimulatePath { get; set; }
        public UploadConflictAction? ConflictAction { get; set; }
    }
}
