//**************************************************************//
// Class FileDownloader v1.0.2 - April 2009                     //
// By De Dauw Jeroen - jeroendedauw@gmail.com                   //
//**************************************************************//
// Copyright 2009 - BN+ Discussions                             //
// http://code.bn2vs.com                                        //
//**************************************************************//

// This code is avaible at
// > BN+ Discussions: http://code.bn2vs.com/viewtopic.php?t=153
// > The Code Project: http://www.codeproject.com/KB/cs/BackgroundFileDownloader.aspx

// VB.Net implementation avaible at
// > BN+ Discussions: http://code.bn2vs.com/viewtopic.php?t=150
// > The Code Project: http://www.codeproject.com/KB/vb/FileDownloader.aspx

// Dutch support can be found here: http://www.helpmij.nl/forum/showthread.php?t=416568

using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.IO;
using System.Linq;
using System.Net;
using System.Text;

namespace FileDownloaderApp
{
    #region FileDownloader
    /// <summary>Class for downloading files in the background that supports info about their progress, the total progress, cancellation, pausing, and resuming. The downloads will run on a separate thread so you don't have to worry about multihreading yourself. </summary>
    /// <remarks>Class FileDownloader v1.0.2, by De Dauw Jeroen - April 2009</remarks>
    class FileDownloader : System.Object, IDisposable
    {

        #region Nested Types
        /// <summary>Simple struct for managing file info</summary>
        public struct FileInfo
        {
            /// <summary>The complete path of the file (directory + filename)</summary>
            public string Path;
            /// <summary>The name of the file</summary>
            public string Name;

            /// <summary>Create a new instance of FileInfo</summary>
            /// <param name="path">The complete path of the file (directory + filename)</param>
            public FileInfo(String path)
            {
                this.Path = path;
                this.Name = this.Path.Split("/"[0])[this.Path.Split("/"[0]).Length - 1];
            }

        }

        /// <summary>Holder for events that are triggered in the background worker but need to be fired in the main thread</summary>
        private enum Event
        {
            CalculationFileSizesStarted,

            FileSizesCalculationComplete,
            DeletingFilesAfterCancel,

            FileDownloadAttempting,
            FileDownloadStarted,
            FileDownloadStopped,
            FileDownloadSucceeded,

            ProgressChanged
        };

        /// <summary>Holder for the action that needs to be invoked</summary>
        private enum InvokeType
        {
            EventRaiser,
            FileDownloadFailedRaiser,
            CalculatingFileNrRaiser
        };
        #endregion

        #region Events
        /// <summary>Occurs when the file downloading has started</summary>
        public event EventHandler Started;
        /// <summary>Occurs when the file downloading has been paused</summary>
        public event EventHandler Paused;
        /// <summary>Occurs when the file downloading has been resumed</summary>
        public event EventHandler Resumed;
        /// <summary>Occurs when the user has requested to cancel the downloads</summary>
        public event EventHandler CancelRequested;
        /// <summary>Occurs when the user has requested to cancel the downloads and the cleanup of the downloaded files has started</summary>
        public event EventHandler DeletingFilesAfterCancel;
        /// <summary>Occurs when the file downloading has been canceled by the user</summary>
        public event EventHandler Canceled;
        /// <summary>Occurs when the file downloading has been completed (without canceling it)</summary>
        public event EventHandler Completed;
        /// <summary>Occurs when the file downloading has been stopped by either cancellation or completion</summary>
        public event EventHandler Stopped;

        /// <summary>Occurs when the busy state of the FileDownloader has changed</summary>
        public event EventHandler IsBusyChanged;
        /// <summary>Occurs when the pause state of the FileDownloader has changed</summary>
        public event EventHandler IsPausedChanged;
        /// <summary>Occurs when the either the busy or pause state of the FileDownloader have changed</summary>
        public event EventHandler StateChanged;

        /// <summary>Occurs when the calculation of the file sizes has started</summary>
        public event EventHandler CalculationFileSizesStarted;
        /// <summary>Occurs when the calculation of the file sizes has started</summary>
        public event CalculatingFileSizeEventHandler CalculatingFileSize;
        /// <summary>Occurs when the calculation of the file sizes has been completed</summary>
        public event EventHandler FileSizesCalculationComplete;

        /// <summary>Occurs when the FileDownloader attempts to get a web response to download the file</summary>
        public event EventHandler FileDownloadAttempting;
        /// <summary>Occurs when a file download has started</summary>
        public event EventHandler FileDownloadStarted;
        /// <summary>Occurs when a file download has stopped</summary>
        public event EventHandler FileDownloadStopped;
        /// <summary>Occurs when a file download has been completed successfully</summary>
        public event EventHandler FileDownloadSucceeded;
        /// <summary>Occurs when a file download has been completed unsuccessfully</summary>
        public event FailEventHandler FileDownloadFailed;

        /// <summary>Occurs every time a block of data has been downloaded</summary>
        public event EventHandler ProgressChanged;
        #endregion
       
        #region Fields
        // Default amount of decimals
        private const Int32 default_decimals = 2;

        // Delegates
        public delegate void FailEventHandler(object sender, Exception ex);
        public delegate void CalculatingFileSizeEventHandler(object sender, Int32 fileNr);
            
        // The download worker
        private BackgroundWorker bgwDownloader = new BackgroundWorker();

        // Preferences
        private Boolean m_supportsProgress, m_deleteCompletedFiles;
        private Int32 m_packageSize, m_stopWatchCycles;

        // State
        private Boolean m_disposed = false;
        private Boolean m_busy, m_paused, m_canceled;
        private Int64 m_currentFileProgress, m_totalProgress, m_currentFileSize;
        private Int32 m_currentSpeed, m_fileNr;

        // Data
        private String m_localDirectory;
        private List<FileInfo> m_files = new List<FileInfo>();
        private Int64 m_totalSize;

        #endregion

        #region Constructors
                /// <summary>Create a new instance of a FileDownloader</summary>
        public FileDownloader()
        {
            this.initizalize(false);
        }

        /// <summary>Create a new instance of a FileDownloader</summary>
        /// <param name="supportsProgress">Optional. Boolean. Should the FileDownloader support total progress statistics?</param>
        public FileDownloader(Boolean supportsProgress)
        {
            this.initizalize(supportsProgress);
        }

        private void initizalize(Boolean supportsProgress)
        {
            // Set the bgw properties
            bgwDownloader.WorkerReportsProgress = true;
            bgwDownloader.WorkerSupportsCancellation = true;
            bgwDownloader.DoWork += new DoWorkEventHandler(bgwDownloader_DoWork);
            bgwDownloader.RunWorkerCompleted += new RunWorkerCompletedEventHandler(bgwDownloader_RunWorkerCompleted);
            bgwDownloader.ProgressChanged += new ProgressChangedEventHandler(bgwDownloader_ProgressChanged);

            // Set the default class preferences
            this.SupportsProgress = supportsProgress;
            this.PackageSize = 4096;
            this.StopWatchCyclesAmount = 5;
            this.DeleteCompletedFilesAfterCancel = true;
        }
        #endregion

        #region Public methods
        public void Start() { this.IsBusy = true; }

        public void Pause() { this.IsPaused = true; }

        public void Resume() { this.IsPaused = false; }

        public void Stop() { this.IsBusy = false; }
        public void Stop(Boolean deleteCompletedFiles)
        {
            this.DeleteCompletedFilesAfterCancel = deleteCompletedFiles;
            this.Stop(); 
        }

        public void Dispose()
        {
            Dispose(true);
            GC.SuppressFinalize(this);
        }

        #region Size formatting functions
        /// <summary>Format an amount of bytes to a more readible notation with binary notation symbols</summary>
        /// <param name="size">Required. Int64. The raw amount of bytes</param>
        public static string FormatSizeBinary(Int64 size)
        {
            return FileDownloader.FormatSizeBinary(size, default_decimals);
        }

        /// <summary>Format an amount of bytes to a more readible notation with binary notation symbols</summary>
        /// <param name="size">Required. Int64. The raw amount of bytes</param>
        /// <param name="decimals">Optional. Int32. The amount of decimals you want to have displayed in the notation</param>
        public static string FormatSizeBinary(Int64 size, Int32 decimals)
        {
            // By De Dauw Jeroen - April 2009 - jeroen_dedauw@yahoo.com
            String[] sizes = { "B", "KiB", "MiB", "GiB", "TiB", "PiB", "EiB", "ZiB", "YiB" };
            Double formattedSize = size;
            Int32 sizeIndex = 0;
            while (formattedSize >= 1024 && sizeIndex < sizes.Length)
            {
                formattedSize /= 1024;
                sizeIndex += 1;
            }
            return Math.Round(formattedSize, decimals) + sizes[sizeIndex];
        }

        /// <summary>Format an amount of bytes to a more readible notation with decimal notation symbols</summary>
        /// <param name="size">Required. Int64. The raw amount of bytes</param>
        public static string FormatSizeDecimal(Int64 size)
        {
            return FileDownloader.FormatSizeDecimal(size, default_decimals);
        }

        /// <summary>Format an amount of bytes to a more readible notation with decimal notation symbols</summary>
        /// <param name="size">Required. Int64. The raw amount of bytes</param>
        /// <param name="decimals">Optional. Int32. The amount of decimals you want to have displayed in the notation</param>
        public static string FormatSizeDecimal(Int64 size, Int32 decimals)
        {
            // By De Dauw Jeroen - April 2009 - jeroen_dedauw@yahoo.com
            String[] sizes = { "B", "kB", "MB", "GB", "TB", "PB", "EB", "ZB", "YB" };
            Double formattedSize = size;
            Int32 sizeIndex = 0;
            while (formattedSize >= 1000 && sizeIndex < sizes.Length)
            {
                formattedSize /= 1000;
                sizeIndex += 1;
            }
            return Math.Round(formattedSize, decimals) + sizes[sizeIndex];
        }
        #endregion

        #endregion

        #region Protected methods
        protected virtual void Dispose(Boolean disposing)
        {
            if (!m_disposed)
            {
                if (disposing)
                {
                    // Free other state (managed objects)
                    bgwDownloader.Dispose();
                }
                // Free your own state (unmanaged objects)
                // Set large fields to null
                this.Files = null;
            }
        }
        #endregion

        #region Private methods
        private void bgwDownloader_DoWork(object sender, DoWorkEventArgs e)
        {
            Int32 fileNr = 0;

            if (this.SupportsProgress) { calculateFilesSize(); }

            if (!Directory.Exists(this.LocalDirectory)) { Directory.CreateDirectory(this.LocalDirectory); }

            while (fileNr < this.Files.Count && !bgwDownloader.CancellationPending)
            {
                m_fileNr = fileNr;
                downloadFile(fileNr);

                if (bgwDownloader.CancellationPending)
                {
                    fireEventFromBgw(Event.DeletingFilesAfterCancel);
                    cleanUpFiles(this.DeleteCompletedFilesAfterCancel ? 0 : m_fileNr, this.DeleteCompletedFilesAfterCancel ? m_fileNr + 1 : 1);
                }
                else
                {
                    fileNr += 1;
                }
            }
        }

        private void calculateFilesSize()
        {
            fireEventFromBgw(Event.CalculationFileSizesStarted);
            m_totalSize = 0;

            for (Int32 fileNr = 0; fileNr < this.Files.Count; fileNr++)
            {
                bgwDownloader.ReportProgress((Int32)InvokeType.CalculatingFileNrRaiser, fileNr + 1);
                try
                {
                    HttpWebRequest webReq = (HttpWebRequest)WebRequest.Create(this.Files[fileNr].Path);
                    HttpWebResponse webResp = (HttpWebResponse)webReq.GetResponse();
                    m_totalSize += webResp.ContentLength;
                    webResp.Close();
                }
                catch (Exception) { }
            }
            fireEventFromBgw(Event.FileSizesCalculationComplete);
        }

        private void fireEventFromBgw(Event eventName)
        {
            bgwDownloader.ReportProgress((int)InvokeType.EventRaiser, eventName);
        }

        private void downloadFile(Int32 fileNr)
        {
            m_currentFileSize = 0;
            fireEventFromBgw(Event.FileDownloadAttempting);

            FileInfo file = this.Files[fileNr];
            Int64 size = 0;

            Byte[] readBytes = new Byte[this.PackageSize];
            Int32 currentPackageSize;
            System.Diagnostics.Stopwatch speedTimer = new System.Diagnostics.Stopwatch();
            Int32 readings = 0;
            Exception exc = null;
            
            FileStream writer = new FileStream(this.LocalDirectory + "\\" + file.Name, System.IO.FileMode.Create);

            HttpWebRequest webReq;
            HttpWebResponse webResp = null;

            try
            {
                webReq = (HttpWebRequest)System.Net.WebRequest.Create(this.Files[fileNr].Path);
                webResp = (HttpWebResponse)webReq.GetResponse();

                size = webResp.ContentLength;
            }
            catch (Exception ex) { exc = ex; }

            m_currentFileSize = size;
            fireEventFromBgw(Event.FileDownloadStarted);

            if (exc != null)
            {
                bgwDownloader.ReportProgress((Int32)InvokeType.FileDownloadFailedRaiser, exc);
            }
            else
            {
                m_currentFileProgress = 0;
                while (m_currentFileProgress < size && !bgwDownloader.CancellationPending)
                {
                    while (this.IsPaused) { System.Threading.Thread.Sleep(100); }

                    speedTimer.Start();

                    currentPackageSize = webResp.GetResponseStream().Read(readBytes, 0, this.PackageSize);

                    m_currentFileProgress += currentPackageSize;
                    m_totalProgress += currentPackageSize;
                    fireEventFromBgw(Event.ProgressChanged);

                    writer.Write(readBytes, 0, currentPackageSize);
                    readings += 1;

                    if (readings >= this.StopWatchCyclesAmount)
                    {
                        m_currentSpeed = (Int32)(this.PackageSize * StopWatchCyclesAmount * 1000 / (speedTimer.ElapsedMilliseconds + 1));
                        speedTimer.Reset();
                        readings = 0;
                    }
                }

                speedTimer.Stop();
                writer.Close();
                webResp.Close();
                if (!bgwDownloader.CancellationPending) { fireEventFromBgw(Event.FileDownloadSucceeded); }
            }
            fireEventFromBgw(Event.FileDownloadStopped);
        }

        private void cleanUpFiles(Int32 start, Int32 length)
        {
            Int32 last = length < 0 ? this.Files.Count -1 : start + length - 1;

            for (Int32 fileNr = start; fileNr <= last; fileNr++)
            {
                String fullPath = this.LocalDirectory + "\\" + this.Files[fileNr].Name;
                if (System.IO.File.Exists(fullPath)) { System.IO.File.Delete(fullPath); }
            }
        }

        private void bgwDownloader_RunWorkerCompleted(object sender, RunWorkerCompletedEventArgs e)
        {
            m_paused = false;
            m_busy = false;

            if (this.HasBeenCanceled) 
            {
                if (Canceled != null) { this.Canceled(this, new EventArgs()); }  
            } 
            else
            {
                if (Completed != null) { this.Completed(this, new EventArgs()); }
            }

            if (Stopped != null) { this.Stopped(this, new EventArgs()); }
            if (IsBusyChanged != null) { this.IsBusyChanged(this, new EventArgs()); }
            if (StateChanged != null) { this.StateChanged(this, new EventArgs()); }
        }

        private void bgwDownloader_ProgressChanged(object sender, ProgressChangedEventArgs e)
        {
            switch ((InvokeType)e.ProgressPercentage)
            {
                case InvokeType.EventRaiser:
                    switch ((Event)e.UserState)
                    {
                        case Event.CalculationFileSizesStarted:
                            if (CalculationFileSizesStarted != null) { this.CalculationFileSizesStarted(this, new EventArgs()); }
                            break;
                        case Event.FileSizesCalculationComplete:
                            if (FileSizesCalculationComplete != null) { this.FileSizesCalculationComplete(this, new EventArgs()); }
                            break;
                        case Event.DeletingFilesAfterCancel:
                            if (DeletingFilesAfterCancel != null) { this.DeletingFilesAfterCancel(this, new EventArgs()); }
                            break;

                        case Event.FileDownloadAttempting:
                            if (FileDownloadAttempting != null) { this.FileDownloadAttempting(this, new EventArgs()); }
                            break;
                        case Event.FileDownloadStarted:
                            if (FileDownloadStarted != null) { this.FileDownloadStarted(this, new EventArgs()); }
                            break;
                        case Event.FileDownloadStopped:
                            if (FileDownloadStopped != null) { this.FileDownloadStopped(this, new EventArgs()); }
                            break;
                        case Event.FileDownloadSucceeded:
                            if (FileDownloadSucceeded != null) { this.FileDownloadSucceeded(this, new EventArgs()); }
                            break;
                        case Event.ProgressChanged:
                            if (ProgressChanged != null) { this.ProgressChanged(this, new EventArgs()); }
                            break;
                    }
                    break;
                case InvokeType.FileDownloadFailedRaiser:
                    if (FileDownloadFailed != null) { this.FileDownloadFailed(this, (Exception)e.UserState); }
                    break;
                case InvokeType.CalculatingFileNrRaiser:
                    if (CalculatingFileSize != null) { this.CalculatingFileSize(this, (Int32)e.UserState); }
                    break;
            }
        }
        #endregion

        #region Properties
        /// <summary>Gets or sets the list of files to download</summary>
        public List<FileInfo> Files
        {
            get { return m_files; }
            set
            {
                if (this.IsBusy)
                {
                    throw new InvalidOperationException("You can not change the file list during the download");
                }
                else
                {
                    if (this.Files != null) m_files = value;
                }
            }
        }

        /// <summary>Gets or sets the local directory in which files will be stored</summary>
        public String LocalDirectory
        {
            get { return m_localDirectory; }
            set
            {
                if (this.LocalDirectory != value) { m_localDirectory = value; }
            }
        }

        /// <summary>Gets or sets if the FileDownloader should support total progress statistics. Note that when enabled, the FileDownloader will have to get the size of each file before starting to download them, which can delay the operation.</summary>
        public Boolean SupportsProgress
        {
            get { return m_supportsProgress; }
            set
            {
                if (this.IsBusy)
                {
                    throw new InvalidOperationException("You can not change the SupportsProgress property during the download");
                }
                else
                {
                    m_supportsProgress = value;
                }
            }
        }

        /// <summary>Gets or sets if when the download process is cancelled the complete downloads should be deleted</summary>
        public Boolean DeleteCompletedFilesAfterCancel
        {
            get { return m_deleteCompletedFiles; }
            set { m_deleteCompletedFiles = value; }
        }

        /// <summary>Gets or sets the size of the blocks that will be downloaded</summary>
        public Int32 PackageSize
        {
            get { return m_packageSize;  }
            set 
            {
                if (value > 0)
                {
                    m_packageSize = value;
                }
                else
                {
                    throw new InvalidOperationException("The PackageSize needs to be greather then 0");
                }
            }
        }

        /// <summary>Gets or sets the amount of blocks that need to be downloaded before the progress speed is re-calculated. Note: setting this to a low value might decrease the accuracy</summary>
        public Int32 StopWatchCyclesAmount
        {
            get { return m_stopWatchCycles; }
            set 
            {
                if (value > 0)
                {
                    m_stopWatchCycles = value;
                }
                else
                {
                    throw new InvalidOperationException("The StopWatchCyclesAmount needs to be greather then 0");
                }
            }
        }

        /// <summary>Gets or sets the busy state of the FileDownloader</summary>
        public Boolean IsBusy 
        {
            get { return m_busy; }
            set
            {
                if (this.IsBusy != value)
                {
                    m_busy = value;
                    m_canceled = !value;
                    if (this.IsBusy)
                    {
                        m_totalProgress = 0;
                        bgwDownloader.RunWorkerAsync();

                        if (Started != null) { this.Started(this, new EventArgs()); }
                        if (IsBusyChanged != null) { this.IsBusyChanged(this, new EventArgs()); }
                        if (StateChanged != null) { this.StateChanged(this, new EventArgs()); }
                    }
                    else
                    {
                        m_paused = false;
                        bgwDownloader.CancelAsync();
                        if (CancelRequested != null) { this.CancelRequested(this, new EventArgs()); }
                        if (StateChanged != null) { this.StateChanged(this, new EventArgs()); }
                    }
                }
            }
        }

        /// <summary>Gets or sets the pause state of the FileDownloader</summary>
        public Boolean IsPaused
        {
            get { return m_paused; }
            set
            {
                if (this.IsBusy)
                {
                    if (this.IsPaused != value) 
                    {
                        m_paused = value;
                        if (this.IsPaused) 
                        {
                            if (Paused != null) { this.Paused(this, new EventArgs()); }
                        } 
                        else 
                        {
                            if (Resumed != null) { this.Resumed(this, new EventArgs()); }
                        }
                        if (IsPausedChanged != null) { this.IsPausedChanged(this, new EventArgs()); }
                        if (StateChanged != null) { this.StateChanged(this, new EventArgs()); }
                    }
                }
                else
                {
                    throw new InvalidOperationException("You can not change the IsPaused property when the FileDownloader is not busy");
                }
            }
        }

         /// <summary>Gets if the FileDownloader can start</summary>
        public Boolean CanStart
        {
            get { return !this.IsBusy; }
        }

        /// <summary>Gets if the FileDownloader can pause</summary>
        public Boolean CanPause
        {
            get { return this.IsBusy && !this.IsPaused && !bgwDownloader.CancellationPending; }
        }

        /// <summary>Gets if the FileDownloader can resume</summary>
        public Boolean CanResume
        {
            get { return this.IsBusy && this.IsPaused && !bgwDownloader.CancellationPending; }
        }

        /// <summary>Gets if the FileDownloader can stop</summary>
        public Boolean CanStop
        {
            get { return this.IsBusy && !bgwDownloader.CancellationPending; }
        }

        /// <summary>Gets the total size of all files together. Only avaible when the FileDownloader suports progress</summary>
        public Int64 TotalSize
        {
            get
            {
                if (this.SupportsProgress)
                {
                    return m_totalSize;
                }
                else
                {
                    throw new InvalidOperationException("This FileDownloader that it doesn't support progress. Modify SupportsProgress to state that it does support progress to get the total size.");
                }
            }
        }

        /// <summary>Gets the total amount of bytes downloaded</summary>
        public Int64 TotalProgress
        {
            get { return m_totalProgress; }
        }

        /// <summary>Gets the amount of bytes downloaded of the current file</summary>
        public Int64 CurrentFileProgress
        {
            get { return m_currentFileProgress; }
        }

        /// <summary>Gets the total download percentage. Only avaible when the FileDownloader suports progress</summary>
        public Double TotalPercentage()
        {
            return this.TotalPercentage(default_decimals);
        }

        /// <summary>Gets the total download percentage. Only avaible when the FileDownloader suports progress</summary>
        public Double TotalPercentage(Int32 decimals)
        {
            if (this.SupportsProgress)
            {
                return Math.Round((Double)this.TotalProgress / this.TotalSize * 100, decimals);
            }
            else
            {
                throw new InvalidOperationException("This FileDownloader that it doesn't support progress. Modify SupportsProgress to state that it does support progress.");
            }
        }

        /// <summary>Gets the percentage of the current file progress</summary>
        public Double CurrentFilePercentage()
        {
            return this.CurrentFilePercentage(default_decimals);
        }

        /// <summary>Gets the percentage of the current file progress</summary>
        public Double CurrentFilePercentage(Int32 decimals)
        {
            return Math.Round((Double)this.CurrentFileProgress / this.CurrentFileSize * 100, decimals);
        }

        /// <summary>Gets the current download speed in bytes</summary>
        public Int32 DownloadSpeed
        {
            get { return m_currentSpeed; }
        }

        /// <summary>Gets the FileInfo object representing the current file</summary>
        public FileInfo CurrentFile
        {
            get { return this.Files[m_fileNr]; }
        }

        /// <summary>Gets the size of the current file in bytes</summary>
        public Int64 CurrentFileSize
        {
            get { return m_currentFileSize; }
        }

        /// <summary>Gets if the last download was canceled by the user</summary>
        public Boolean HasBeenCanceled
        {
            get { return m_canceled; }
        }
        #endregion

    }
    #endregion
}
