using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Diagnostics;
using System.Threading;
using System.IO;
using System.Globalization;

namespace DWSIM.Libraries.PythonLink
{
    public class Python
    {

        public Process PythonProcess { get; set; }
        private string PythonEchoString { get; set; }
        public Python(string PathToPythonBinaries)
        {
            StartPython(PathToPythonBinaries, false);
        }

        public Python(string PathToPythonBinaries, bool CreateWindow)
        {
            StartPython(PathToPythonBinaries, CreateWindow);
        }

        string ptob;
        bool cw;
        private void StartPython(string PathToPythonBinaries, bool CreateWindow)
        {
            ptob = PathToPythonBinaries;
            cw = CreateWindow;
            this.PythonEchoString = Guid.NewGuid().ToString();
            PythonProcess = new Process();
            ProcessStartInfo pi = new ProcessStartInfo();
            if (System.Environment.OSVersion.Platform == PlatformID.Unix)
            {
                pi.FileName = "python";
            }
            else
            {
                if (PathToPythonBinaries[PathToPythonBinaries.Length - 1] != '\\')
                    PathToPythonBinaries = PathToPythonBinaries + Path.DirectorySeparatorChar;
                pi.FileName = PathToPythonBinaries + "python.exe";
            }
            pi.RedirectStandardInput = true;
            pi.RedirectStandardOutput = true;
            pi.RedirectStandardError = true;
            pi.UseShellExecute = false;
            pi.CreateNoWindow = !CreateWindow;
            pi.Verb = "open";
            pi.Arguments = "-i";
            //
            pi.WorkingDirectory = ".";
            PythonProcess.StartInfo = pi;
            PythonProcess.Start();
            PythonProcess.OutputDataReceived += new DataReceivedEventHandler(PythonProcess_OutputDataReceived);
            PythonProcess.BeginOutputReadLine();
            PythonEntryText = ExecuteCommand("echo = '" + PythonEchoString + "'", true);
        }

        public double GetScalar(string command)
        {
            string rasp = ExecuteCommand(command, false);
            return double.Parse(rasp, CultureInfo.InvariantCulture);
        }

        public double[] GetVector(string command)
        {
            string rasp = ExecuteCommand(command, false);
            return rasp.TrimStart(new char[] { '(' }).TrimEnd(new char[] { ')' }).Trim().Split(new char[] { ',' }).Select((item) => Double.Parse(item, CultureInfo.InvariantCulture)).ToArray();
        }

        StringBuilder SharedBuilder = new StringBuilder();

        ManualResetEvent PythonDoneEvent = new ManualResetEvent(false);

        public string PythonEntryText { get; internal set; }

        public void WorkThread(object o)
        {
            var command = ((object[])o)[0];
            SharedBuilder.Clear();
            PythonDoneEvent.Reset();
            if (command != null)
            {
                if (command is string)
                {
                    PythonProcess.StandardInput.WriteLine((string)command);
                }
                else {
                    foreach (string s in (string[])((object[])o)[0])
                    {
                        PythonProcess.StandardInput.WriteLine(s);
                    }
                }
            }
            if ((bool)((object[])o)[1]) 
                PythonProcess.StandardInput.WriteLine("echo");
            else
                PythonProcess.StandardInput.WriteLine("sys.stdout.flush()");
            PythonDoneEvent.WaitOne();
        }

        public string ExecuteCommand(string command, bool writeecho)
        {
            Thread tmp = new Thread(new ParameterizedThreadStart(WorkThread));
            tmp.Start(new object[] { command, writeecho });

            tmp.Join();

            return SharedBuilder.ToString();
        }

        public string ExecuteMultilineCommand(string[] command, bool writeecho)
        {
            Thread tmp = new Thread(new ParameterizedThreadStart(WorkThread));
            tmp.Start(new object[] { command, writeecho });

            tmp.Join();

            return SharedBuilder.ToString();
        }

        string errorMessage = null;
        void PythonProcess_OutputDataReceived(object sender, DataReceivedEventArgs e)
        {
            if (e.Data == null)
            {
                SharedBuilder.Clear();
                errorMessage = PythonProcess.StandardError.ReadToEnd();
                SharedBuilder.Append("Python has exited with the following error message: \r\n" + errorMessage);
                PythonDoneEvent.Set();
                return;
            }
            if (e.Data.Trim() == "'" + PythonEchoString + "'")
                PythonDoneEvent.Set();
            else
            {
                SharedBuilder.Append(e.Data);
                PythonDoneEvent.Set();
            }

        }
        public delegate void PythonRestartedEventHandler(object sender, EventArgs e);
    }
}
