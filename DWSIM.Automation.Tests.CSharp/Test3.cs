using DWSIM.Interfaces.Enums.GraphicObjects;
using DWSIM.Thermodynamics.Streams;
using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading;
using System.Threading.Tasks;

namespace DWSIM.Automation.Tests.CSharp
{
    public class HangTest
    {
        private static Task _agentTask = null;
        private static SingleThreadTaskScheduler taskScheduler;

        [STAThread]
        static void Main()
        {
            CancellationTokenSource tokenSource = new CancellationTokenSource();

            taskScheduler = new SingleThreadTaskScheduler(tokenSource.Token);
            taskScheduler.Start();

            //DWSIM.GlobalSettings.Settings.AppTaskScheduler = TaskScheduler.Default;
            //DWSIM.GlobalSettings.Settings.TaskScheduler = -1;

            _agentTask = RunAgentAsync();

            Console.WriteLine("Running in background.");
            Console.ReadLine();
            tokenSource.Cancel();
            Console.WriteLine("Canceled.");
        }

        static async Task RunAgentAsync()
        {
            await taskScheduler.Schedule(() =>
            {
                var dwsimDir = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location);
                System.IO.Directory.SetCurrentDirectory(dwsimDir);

                var interf = new DWSIM.Automation.Automation2();

                var sourceFile = Path.Combine(dwsimDir, "samples/Cavett's Problem.dwxml");
                var flowsheet = interf.LoadFlowsheet(sourceFile);

                interf.CalculateFlowsheet2(flowsheet);

                Console.WriteLine("DWSIM done.");

                return 1;
            });
        }
    }

    // Copied from https://stackoverflow.com/a/30726903/336305
    public sealed class SingleThreadTaskScheduler : TaskScheduler
    {
        [ThreadStatic]
        private static bool isExecuting;
        private readonly CancellationToken cancellationToken;

        private readonly BlockingCollection<Task> taskQueue;

        public SingleThreadTaskScheduler(CancellationToken cancellationToken)
        {
            this.cancellationToken = cancellationToken;
            this.taskQueue = new BlockingCollection<Task>();
        }

        public void Start()
        {
            var thread = new Thread(RunOnCurrentThread) { Name = "STTS Thread" };
            thread.SetApartmentState(ApartmentState.STA);
            thread.Start();
        }

        // Just a helper for the sample code
        public Task<TResult> Schedule<TResult>(Func<TResult> action)
        {
            return
                Task.Factory.StartNew<TResult>
                    (
                        action,
                        CancellationToken.None,
                        TaskCreationOptions.None,
                        this
                    );
        }

        // You can have this public if you want - just make sure to hide it
        private void RunOnCurrentThread()
        {
            isExecuting = true;

            try
            {
                foreach (var task in taskQueue.GetConsumingEnumerable(cancellationToken))
                {
                    TryExecuteTask(task);
                }
            }
            catch (OperationCanceledException)
            { }
            finally
            {
                isExecuting = false;
            }
        }

        // Signaling this allows the task scheduler to finish after all tasks complete
        public void Complete() { taskQueue.CompleteAdding(); }
        protected override IEnumerable<Task> GetScheduledTasks() { return null; }

        protected override void QueueTask(Task task)
        {
            try
            {
                taskQueue.Add(task, cancellationToken);
            }
            catch (OperationCanceledException)
            { }
        }

        protected override bool TryExecuteTaskInline(Task task, bool taskWasPreviouslyQueued)
        {
            // We'd need to remove the task from queue if it was already queued. 
            // That would be too hard.
            if (taskWasPreviouslyQueued) return false;

            return isExecuting && TryExecuteTask(task);
        }
    }
}
