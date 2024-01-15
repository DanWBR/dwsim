using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using DWSIM.Interfaces;
using LiteDB;

namespace DWSIM.FileStorage
{
    /// <summary>
    /// File Database Provider for DWSIM, based on LiteDB.
    /// </summary>
    public class FileDatabaseProvider : IFileDatabaseProvider
    {

        private MemoryStream DBMem;
        private LiteDatabase DB;

        private static MemoryStream ReadStream(string path)
        {
            using (var temp = new MemoryStream(File.ReadAllBytes(path)))
            {
                var ms = new MemoryStream();
                temp.CopyTo(ms);
                return ms;
            }
        }

        private bool IsDBLoaded = false;

        /// <summary>
        /// Checks if the database is loaded.
        /// </summary>
        public bool IsDatabaseLoaded { get { return IsDBLoaded; } }

        /// <summary>
        /// Checks if the given file exists in the database.
        /// </summary>
        /// <param name="filename">Filename to search.</param>
        /// <returns>True if the file exists.</returns>
        public bool CheckIfExists(string filename)
        {
            var file = DB.FileStorage.FindById(Path.GetFileName(filename));
            return file != null;
        }

        /// <summary>
        /// Exports the embedded database to a file.
        /// </summary>
        /// <param name="filepath">Filename to export the database to.</param>
        public void ExportDatabase(string filepath)
        {
            DB.Checkpoint();
            File.WriteAllBytes(filepath, DBMem.ToArray());
        }

        /// <summary>
        /// Exports a file/item from the database to a file in the filesystem.
        /// </summary>
        /// <param name="filename">file item to export.</param>
        /// <param name="exportpath">Path with filename to save the item to.</param>
        public void ExportFile(string filename, string exportpath)
        {
            var file = DB.FileStorage.FindById(Path.GetFileName(filename));
            if (file != null)
            {
                if (!File.Exists(exportpath)) file.SaveAs(exportpath);
            }
            else
            {
                throw new Exception("File not found");
            }
        }

        /// <summary>
        /// Returns a stored image file as a System.Drawing.Image object.
        /// </summary>
        /// <param name="filename">Image file name in the database.</param>
        /// <returns>A System.Drawing.Image object.</returns>
        public System.Drawing.Image GetFileAsImage(string filename)
        {
            var fname = Path.GetFileName(filename);
            var file = DB.FileStorage.FindById(fname);
            if (file != null)
            {
                using (var ms = new MemoryStream())
                {
                    file.CopyTo(ms);
                    ms.Position = 0;
                    var image = System.Drawing.Image.FromStream(ms);
                    return image;
                }
            }
            else
            {
                return null;
            }
        }

        /// <summary>
        /// Returns a stored image file as an Eto.Drawing.Bitmap object.
        /// </summary>
        /// <param name="filename">Image file name in the database.</param>
        /// <returns>An Eto.Drawing.Bitmap object.</returns>
        public Eto.Drawing.Bitmap GetFileAsEtoBitmap(string filename)
        {
            var fname = Path.GetFileName(filename);
            var file = DB.FileStorage.FindById(fname);
            if (file != null)
            {
                using (var ms = new MemoryStream())
                {
                    file.CopyTo(ms);
                    ms.Position = 0;
                    var image = new Eto.Drawing.Bitmap(ms);
                    return image;
                }
            }
            else
            {
                return null;
            }
        }

        /// <summary>
        /// Returns the file contents as plain text.
        /// </summary>
        /// <param name="filename">File/Item name in the database.</param>
        /// <returns>The file contents as plain text.</returns>
        public string GetFileAsText(string filename)
        {
            var file = DB.FileStorage.FindById(Path.GetFileName(filename));
            if (file != null)
            {
                var tmpfile = DWSIM.SharedClasses.Utility.GetTempFileName();
                file.SaveAs(tmpfile);
                var text = File.ReadAllText(tmpfile);
                File.Delete(tmpfile);
                return text;
            }
            else
            {
                return null;
            }
        }

        /// <summary>
        /// Gets a list of all files in the database.
        /// </summary>
        /// <returns></returns>
        public List<string> GetFiles()
        {
            var files = DB.FileStorage.FindAll();
            return files.Select(f => f.Filename).ToList();
        }

        /// <summary>
        /// Gets the contents of a file as a memory stream.
        /// </summary>
        /// <param name="filename">File/Item name</param>
        /// <returns>A MemoryStream object containing the item.</returns>
        public MemoryStream GetFileStream(string filename)
        {
            var file = DB.FileStorage.FindById(Path.GetFileName(filename));
            if (file != null)
            {
                var ms = new MemoryStream();
                file.CopyTo(ms);
                ms.Position = 0;
                return ms;
            }
            else
            {
                return null;
            }
        }

        /// <summary>
        /// For internal use only.
        /// </summary>
        /// <param name="dbpath"></param>
        public void LoadDatabase(string dbpath)
        {
            ReleaseDatabase();
            DBMem = ReadStream(dbpath);
            DBMem.Position = 0;
            DB = new LiteDatabase(DBMem);
            IsDBLoaded = true;
        }

        /// <summary>
        /// Stores a file in the database.
        /// </summary>
        /// <param name="filepath">Path of the file to import.</param>
        public void PutFile(string filepath)
        {
            DB.FileStorage.Upload(Path.GetFileName(filepath), filepath);
            DB.Checkpoint();
        }

        /// <summary>
        /// Stores a file in the database.
        /// </summary>
        /// <param name="filepath">Path of the file to import.</param>
        /// <param name="internalfilename">Internal name of the stored file.</param>
        public void PutFile(string filepath, string internalfilename)
        {
            DB.FileStorage.Upload(internalfilename, filepath);
            DB.Checkpoint();
        }

        /// <summary>
        /// Stores a file in the database.
        /// </summary>
        /// <param name="stream"></param>
        /// <param name="filename"></param>
        public void PutFile(Stream stream, string filename)
        {
            DB.FileStorage.Upload(filename, filename, stream);
            DB.Checkpoint();
        }

        /// <summary>
        /// For internal use only.
        /// </summary>
        public void ReleaseDatabase()
        {
            if (DB != null) DB?.Dispose();
            if (DBMem != null) DBMem?.Dispose();
            IsDBLoaded = false;
        }

        /// <summary>
        /// Removes a file from the database.
        /// </summary>
        /// <param name="filename">File/Item name.</param>
        public void DeleteFile(string filename)
        {
            var file = DB.FileStorage.FindById(Path.GetFileName(filename));
            if (file != null)
            {
                DB.FileStorage.Delete(file.Id);
                DB.Rebuild();
                DB.Checkpoint();
            }
            else
            {
                throw new Exception("File not found");
            }
        }

        /// <summary>
        /// For internal use only.
        /// </summary>
        public void CreateDatabase()
        {
            ReleaseDatabase();
            DBMem = new MemoryStream();
            DB = new LiteDatabase(DBMem);
            IsDBLoaded = true;
        }

        /// <summary>
        /// Gets the database size in kilobytes.
        /// </summary>
        /// <returns>DB size in KB.</returns>
        public int GetSizeinKB()
        {
            return (int)DBMem.Length / 1024;
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="filename"></param>
        /// <param name="stream"></param>
        public void ExportFile(string filename, MemoryStream stream)
        {
            var file = DB.FileStorage.FindById(Path.GetFileName(filename));
            if (file != null)
            {
                file.CopyTo(stream);
                stream.Position = 0;
            }
            else
            {
                throw new Exception("File not found");
            }
        }
    }
}
