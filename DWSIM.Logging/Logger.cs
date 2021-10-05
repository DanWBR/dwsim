using DWSIM.GlobalSettings;
using NLog;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace DWSIM.Logging
{
    public class Logger
    {

        private static NLog.Logger logger;

        public static void Initialize()
        {

            var logfiledir = "";
            if (GlobalSettings.Settings.RunningPlatform() == Settings.Platform.Mac)
            {
                logfiledir = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.Personal), "Documents", "DWSIM Application Data");
            }
            else
            {
                logfiledir = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments), "DWSIM Application Data");
            }
            if (!Directory.Exists(logfiledir)) Directory.CreateDirectory(logfiledir);

            var config = new NLog.Config.LoggingConfiguration();

            // Targets where to log to: File and Console
            var logfile = new NLog.Targets.FileTarget("logfile") { FileName = Path.Combine(logfiledir, "errors.log") };

            // Rules for mapping loggers to targets            
            config.AddRule(LogLevel.Trace, LogLevel.Fatal, logfile);

            // Apply config           
            NLog.LogManager.Configuration = config;

            logger = NLog.LogManager.GetCurrentClassLogger();

        }

        public static void LogError(string message, Exception ex)
        {
            logger.Error(ex, message);
        }

        public static void LogUnhandled(string message, Exception ex)
        {
            logger.Fatal(ex, message);
        }

        public static void LogWarning(string message, Exception ex)
        {
            logger.Warn(ex, message);
        }


    }
}
