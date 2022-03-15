using DWSIM.Simulate365.Models;
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
        public static S365File UploadFile(string flowsheetsDriveId, string parentDriveId, string filePath, string filename, string simulatePath)
        {
            using (var fileStream = System.IO.File.OpenRead(filePath))
                return UploadFile(flowsheetsDriveId, parentDriveId, fileStream, filename, simulatePath);
        }

        public static S365File UploadFile(string flowsheetsDriveId, string parentDriveId, Stream fileStream, string filename, string simulatePath)
        {
            try
            {
                fileStream.Seek(0, SeekOrigin.Begin);

                var token = UserService.GetInstance().GetUserToken();
                var client = GraphClientFactory.CreateClient(token);

                var driveItemRequestBuilder = client.Drives[flowsheetsDriveId].Items[parentDriveId];
                var item = Task.Run(async () => await UploadDocumentAsync(driveItemRequestBuilder, filename, fileStream, ConflictBehaviour.Replace)).Result;

                return new S365File(filename) { FileId = item.Id, DriveId = parentDriveId, Filename = item.Name, FullPath = simulatePath };

            }
            catch (Exception ex)
            {

                throw new Exception("An error occurred while saving file to Simulate 365 Dashboard.", ex);
            }
        }

        public static async Task<DriveItem> UploadDocumentAsync(IDriveItemRequestBuilder driveItemRequestBuilder, string filename, Stream fileStream, string conflictBehaviour = ConflictBehaviour.Rename)
        {
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
            catch (Exception ex)
            {
                throw new Exception("An error occurred while trying to upload document.", ex);
            }
        }

    }
}
