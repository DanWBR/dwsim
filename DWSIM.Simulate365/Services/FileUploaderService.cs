using Microsoft.Graph;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace DWSIM.Simulate365.Services
{

    public static class ConflictBehaviour
    {
        public const string Fail = "fail";
        public const string Replace = "replace";
        public const string Rename = "rename";
    }

    public class FileUploaderService
    {
        public static async Task UploadFile(string flowsheetsDriveId, string parentDriveId, string filePath, string filename)
        {
            try
            {
                var token = UserService.GetInstance().GetUserToken();
                var client = GraphClientFactory.CreateClient(token);               

                var driveItemRequestBuilder = client.Drives[flowsheetsDriveId].Items[parentDriveId];
                await UploadDocumentAsync(driveItemRequestBuilder, filename, filePath, ConflictBehaviour.Replace);
            }
            catch (Exception ex)
            {

                throw new Exception("An error occurred while saving file to Simulate 365 Dashboard.", ex);
            }
        }

        public static async Task<DriveItem> UploadDocumentAsync(IDriveItemRequestBuilder driveItemRequestBuilder, string filename, string filePath, string conflictBehaviour = ConflictBehaviour.Rename)
        {
            FileInfo info = new FileInfo(filePath);

            UploadSession uploadSession = null;

            try
            {
                var uploadProps = new DriveItemUploadableProperties
                {
                    ODataType = null,
                    AdditionalData = new Dictionary<string, object>
                    {
                        ["@microsoft.graph.conflictBehavior"] = conflictBehaviour
                    }
                };

                uploadSession = await driveItemRequestBuilder
                                        .ItemWithPath(filename)
                                        .CreateUploadSession(uploadProps)
                                        .Request()
                                        .PostAsync();
            }
            catch (ServiceException ex)
            {
                throw new Exception("An error occured while creating upload session.", ex);
            }

            if (uploadSession == null)
                throw new Exception("Upload session is null.");

            try
            {
                using (var fileStream = System.IO.File.OpenRead(filePath))
                {
                    // Performs upload, slice by slice
                    int maxSliceSize = 5 * 1024 * 1024; //5MB
                    var fileUploadTask = new LargeFileUploadTask<DriveItem>(uploadSession, fileStream, maxSliceSize);
                    var uploadResult = await fileUploadTask.UploadAsync();

                    if (!uploadResult.UploadSucceeded)
                    {
                        throw new Exception("File upload failed!");
                    }
                    else
                    {
                        return uploadResult.ItemResponse;
                    }
                }
            }
            catch (Exception ex)
            {
                throw new Exception("An error occurred while trying to upload document.", ex);
            }
        }

    }
}
