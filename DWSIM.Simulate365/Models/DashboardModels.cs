using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace DWSIM.Simulate365.Models
{
    /// <summary>
    /// These models are from Dashboard v2 project, used to have typed response from HttpClient
    /// </summary>
    public class FilesWithBreadcrumbsResponseModel
    {
        public FileModel File { get; set; }

        public List<BreadcrumbItem> BreadcrumbItems { get; set; }
    }

    public class FileModel
    {
        public long Id { get; set; }

        public string Name { get; set; } = String.Empty;
        public Guid UniqueIdentifier { get; set; } = Guid.Empty;

        public long CurrentVersionNumber { get; set; }

        public DateTime CreatedAt { get; set; }
        public string Comment { get; set; } = string.Empty;
        public bool Stared { get; set; } = false;
        public long ParentDirectoryId { get; set; }

        public DateTime LastModifiedAt { get; set; }

        public FileSystemEntityType Type { get; set; }
        public List<TagResponseModel> Tags { get; set; }
    }

    public class BreadcrumbItem
    {
        public string Name { get; set; }
        public Guid UniqueIdentifier { get; set; }
    }
    public enum FileSystemEntityType
    {
        File,
        Directory
    }
    public class TagResponseModel
    {
        public long Id { get; set; }
        public string Name { get; set; } = string.Empty;
    }

    public class UploadFileResponseModel
    {
        public string Filename { get; set; }
        public string OriginalFilename { get; set; }
        public Guid? ParentDirectoryUniqueIdentifier { get; set; }
        public Guid FileUniqueIdentifier { get; set; }
        public string SimulatePath { get; set; }
    }

}
