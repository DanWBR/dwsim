using System;
using System.IO;
using System.Reflection;

namespace DWSIM.macOS.ClassicUILoader
{
    public class Loader
    {

        public static void Load()
        {
            var asm = Assembly.LoadFile(Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), "DWSIM.exe"));
            var instance = asm.CreateInstance("DWSIM.FormMain");
            try
            {
                instance.GetType().GetMethod("Form1_Load").Invoke(instance, new object[] { null, new System.EventArgs() });
            }
            catch (Exception ex)
            {
                Console.WriteLine("Load failed. " + ex.ToString());
            }
            try
            {
                instance.GetType().GetMethod("Show", new Type[] { }).Invoke(instance, null);
            }
            catch (Exception ex)
            {
                Console.WriteLine("Show failed. " + ex.ToString());
            }
        }

    }
}
