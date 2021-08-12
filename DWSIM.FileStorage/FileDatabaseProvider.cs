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

        public bool IsDatabaseLoaded { get { return IsDBLoaded; } }

        public bool CheckIfExists(string filename)
        {
            var file = DB.FileStorage.FindById(Path.GetFileName(filename));
            return file != null;
        }

        public void ExportDatabase(string filepath)
        {
            DB.Commit();
            File.WriteAllBytes(filepath, DBMem.ToArray());
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
            var file = DB.FileStorage.FindById(Path.GetFileName(filename));
            if (file != null)
            {
                var ms = new MemoryStream();
                file.CopyTo(ms);
                ms.Position = 0;
                var image = System.Drawing.Image.FromStream(ms);
                return image;
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
            else {
                return null;            
            }
        }

        public void LoadDatabase(string dbpath)
        {
            ReleaseDatabase();
            DBMem = ReadStream(dbpath);
            DB = new LiteDatabase(DBMem);
            IsDBLoaded = true;
        }

        public void PutFile(string filepath)
        {
            DB.FileStorage.Upload(Path.GetFileName(filepath), filepath);
            DB.Commit();
        }

        public void ReleaseDatabase()
        {
            DB?.Dispose();
            DBMem?.Dispose();
            IsDBLoaded = false;
        }

        public void DeleteFile(string filename)
        {
            var file = DB.FileStorage.FindById(Path.GetFileName(filename));
            if (file != null)
            {
                DB.FileStorage.Delete(file.Id);
                DB.Commit();
                DB.Rebuild();
                DB.Commit();
            }
            else
            {
                throw new Exception("File not found");
            }
        }

        public void CreateDatabase()
        {
            ReleaseDatabase();
            DBMem = new MemoryStream();
            DB = new LiteDatabase(DBMem);
            IsDBLoaded = true;
        }

        public int GetSizeinKB()
        {
            DB.Commit();
            return (int)DBMem.Length / 1024;
        }
    }
}
