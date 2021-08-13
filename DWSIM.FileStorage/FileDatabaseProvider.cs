using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using DWSIM.Interfaces;
using LiteDB;

namespace DWSIM.FileStorage
{
    public class FileDatabaseProvider : IFileDatabaseProvider
    {

        private string tmpdb = "";
        private LiteDatabase DB;

        private bool IsDBLoaded = false;

        public bool IsDatabaseLoaded { get { return IsDBLoaded; } }

        public bool CheckIfExists(string filename)
        {
            var file = DB.FileStorage.FindById(Path.GetFileName(filename));
            return file != null;
        }

        public void ExportDatabase(string filepath)
        {
            DB.Checkpoint();
            File.Copy(tmpdb, filepath, true);
        }

        public void ExportFile(string filename, string exportpath)
        {
            var file = DB.FileStorage.FindById(Path.GetFileName(filename));
            if (file != null)
            {
                file.SaveAs(exportpath);
            }
            else
            {
                throw new Exception("File not found");
            }
        }

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

        public string GetFileAsText(string filename)
        {
            var file = DB.FileStorage.FindById(Path.GetFileName(filename));
            if (file != null)
            {
                var tmpfile = Path.GetTempFileName();
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

        public List<string> GetFiles()
        {
            var files = DB.FileStorage.FindAll();
            return files.Select(f => f.Filename).ToList();
        }

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

        public void LoadDatabase(string dbpath)
        {
            ReleaseDatabase();
            tmpdb = Path.GetTempFileName();
            File.Copy(dbpath, tmpdb, true);
            DB = new LiteDatabase(tmpdb);
            IsDBLoaded = true;
        }

        public void PutFile(string filepath)
        {
            DB.FileStorage.Upload(Path.GetFileName(filepath), filepath);
            DB.Checkpoint();
        }

        public void ReleaseDatabase()
        {
            if (DB != null)
            {
                DB.Dispose();
                if (File.Exists(tmpdb)) File.Delete(tmpdb);
            }
            IsDBLoaded = false;
        }

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

        public void CreateDatabase()
        {
            ReleaseDatabase();
            tmpdb = Path.GetTempFileName();
            DB = new LiteDatabase(tmpdb);
            IsDBLoaded = true;
        }

        public int GetSizeinKB()
        {
            return (int)new FileInfo(tmpdb).Length / 1024;
        }
    }
}
