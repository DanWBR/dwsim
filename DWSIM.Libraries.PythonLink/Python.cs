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
                pi.FileName = PathToPythonBinaries + "py.exe";
            }
            pi.RedirectStandardInput = true;
            pi.RedirectStandardOutput = true;
            pi.RedirectStandardError = true;
            pi.UseShellExecute = false;
            pi.CreateNoWindow = !CreateWindow;
            pi.Verb = "open";
            //
            pi.WorkingDirectory = ".";
            PythonProcess.StartInfo = pi;
            PythonProcess.Start();
            PythonProcess.OutputDataReceived += new DataReceivedEventHandler(PythonProcess_OutputDataReceived);
            PythonProcess.BeginOutputReadLine();
            PythonEntryText = ExecuteCommand(null);
        }

        public double GetScalar(string scalar)
        {
            string rasp = ExecuteCommand(scalar, 30000);
            string val = rasp.Replace(scalar + " = ", "").Substring(rasp.LastIndexOf("\\") + 1).Trim();
            return double.Parse(val, CultureInfo.InvariantCulture);
        }

        public double[] GetVector(string vector)
        {
            string rasp = ExecuteCommand(vector, 30000);
            string[] lines = rasp.Split(new char[] { '\r', '\n' }, StringSplitOptions.RemoveEmptyEntries);
            int i = 0;
            //catam urmatorul entry
            List<double> data = new List<double>();
            while (i < lines.Length - 1)
            {
                i++;
                string line = lines[i];
                string[] dataS = line.Split(new char[] { ' ', '\t' }, StringSplitOptions.RemoveEmptyEntries);
                for (int k = 0; k < dataS.Length; k++)
                {
                    data.Add(double.Parse(dataS[k], CultureInfo.InvariantCulture));
                }
            }
            //caz special in care a pus toate rezultatele pe o singura linie
            if (data.Count == 0)
            {
                string[] dataS = lines[lines.Length - 1].Split(new char[] { ' ', '\t' }, StringSplitOptions.RemoveEmptyEntries);
                if (dataS.Length != 0)
                    for (int k = 0; k < dataS.Length; k++)
                    {
                        data.Add(double.Parse(dataS[k], CultureInfo.InvariantCulture));
                    }
            }
            return data.ToArray();
        }

        public double[][] GetMatrix(string matrix)
        {
            //string rasp = ExecuteCommand(matrix);
            //aflam numarul de randuri
            string rasp = ExecuteCommand(matrix + "(:,1)", 30000);
            string[] lines = rasp.Split(new char[] { '\r', '\n' }, StringSplitOptions.RemoveEmptyEntries);
            double[][] mat = new double[lines.Length - 1][];
            for (int i = 0; i < mat.Length; i++)
            {
                mat[i] = GetVector(matrix + "(" + (i + 1) + ",:)");
            }
            return mat;
        }

        StringBuilder SharedBuilder = new StringBuilder();

        ManualResetEvent PythonDoneEvent = new ManualResetEvent(false);
      
        public string PythonEntryText { get; internal set; }

        public void WorkThread(object o)
        {
            string command = (string)o;
            SharedBuilder.Clear();
            PythonDoneEvent.Reset();
            if (command != null)
            {
                PythonProcess.StandardInput.WriteLine(command);
            }
            //ca sa avem referinta pentru output
            PythonProcess.StandardInput.WriteLine("\"" + PythonEchoString + "\"");
            PythonDoneEvent.WaitOne();
        }
        public string ExecuteCommand(string command, int timeout)
        {
            if (PythonProcess.HasExited)
            {
                StartPython(ptob, cw);
                if (PythonRestarted != null) PythonRestarted(this, EventArgs.Empty);
            }
            exitError = false;

            Thread tmp = new Thread(new ParameterizedThreadStart(WorkThread));
            tmp.Start(command);

            if (!tmp.Join(timeout))
            {
                tmp.Abort();
                throw new Exception("Python timeout");
            }
            if (exitError)
            {
                throw new Exception(errorMessage);
            }
            return SharedBuilder.ToString();
        }
        public string ExecuteCommand(string command)
        {
            Thread tmp = new Thread(new ParameterizedThreadStart(WorkThread));
            tmp.Start(command);

            tmp.Join();

            return SharedBuilder.ToString();
        }
        bool exitError = false;
        string errorMessage = null;
        void PythonProcess_OutputDataReceived(object sender, DataReceivedEventArgs e)
        {
            if (e.Data == null)
            {
                SharedBuilder.Clear();
                errorMessage = PythonProcess.StandardError.ReadToEnd();
                SharedBuilder.Append("Python has exited with the following error message: \r\n" + errorMessage);
                exitError = true;
                PythonDoneEvent.Set();
                return;
            }
            if (e.Data.Trim() == "ans = " + PythonEchoString)
                PythonDoneEvent.Set();
            else
                SharedBuilder.Append(e.Data + "\r\n");
        }
        public event PythonRestartedEventHandler PythonRestarted;
        public delegate void PythonRestartedEventHandler(object sender, EventArgs e);
    }
}
