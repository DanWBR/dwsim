Imports System.Collections.Concurrent
Imports System.Collections.Generic
Imports System.Linq
Imports System.Threading
Imports System.Threading.Tasks

Namespace TaskSchedulers

    ''' <summary>Provides a scheduler that uses STA threads.</summary> 
    Public NotInheritable Class StaTaskScheduler

        Inherits TaskScheduler

        Implements IDisposable

        ''' <summary>Stores the queued tasks to be executed by our pool of STA threads.</summary> 
        Private _tasks As BlockingCollection(Of Task)

        ''' <summary>The STA threads used by the scheduler.</summary> 
        Private ReadOnly _threads As List(Of Thread)

        ''' <summary>Initializes a new instance of the StaTaskScheduler class with the specified concurrency level.</summary> 
        ''' <param name="numberOfThreads">The number of threads that should be created and used by this scheduler.</param> 
        Public Sub New(numberOfThreads As Integer)
            ' Validate arguments 
            If numberOfThreads < 1 Then
                Throw New ArgumentOutOfRangeException("concurrencyLevel")
            End If

            ' Initialize the tasks collection 
            _tasks = New BlockingCollection(Of Task)()

            ' Create the threads to be used by this scheduler 
            ' Continually get the next task and try to execute it. 
            ' This will continue until the scheduler is disposed and no more tasks remain. 
            _threads = Enumerable.Range(0, numberOfThreads).[Select](Function(i)
                                                                         Dim thread = New Thread(Sub()
                                                                                                     For Each t In _tasks.GetConsumingEnumerable()
                                                                                                         TryExecuteTask(t)
                                                                                                     Next
                                                                                                 End Sub)
                                                                         thread.IsBackground = True
                                                                         thread.SetApartmentState(ApartmentState.STA)
                                                                         Return thread

                                                                     End Function).ToList()

            ' Start all of the threads 
            _threads.ForEach(Sub(t) t.Start())
        End Sub

        ''' <summary>Queues a Task to be executed by this scheduler.</summary> 
        ''' <param name="task">The task to be executed.</param> 
        Protected Overrides Sub QueueTask(task As Task)
            ' Push it into the blocking collection of tasks 
            _tasks.Add(task)
        End Sub

        ''' <summary>Provides a list of the scheduled tasks for the debugger to consume.</summary> 
        ''' <returns>An enumerable of all tasks currently scheduled.</returns> 
        Protected Overrides Function GetScheduledTasks() As IEnumerable(Of Task)
            ' Serialize the contents of the blocking collection of tasks for the debugger 
            Return _tasks.ToArray()
        End Function

        ''' <summary>Determines whether a Task may be inlined.</summary> 
        ''' <param name="task">The task to be executed.</param> 
        ''' <param name="taskWasPreviouslyQueued">Whether the task was previously queued.</param> 
        ''' <returns>true if the task was successfully inlined; otherwise, false.</returns> 
        Protected Overrides Function TryExecuteTaskInline(task As Task, taskWasPreviouslyQueued As Boolean) As Boolean
            ' Try to inline if the current thread is STA 
            Return Thread.CurrentThread.GetApartmentState() = ApartmentState.STA AndAlso TryExecuteTask(task)
        End Function

        ''' <summary>Gets the maximum concurrency level supported by this scheduler.</summary> 
        Public Overrides ReadOnly Property MaximumConcurrencyLevel() As Integer
            Get
                Return _threads.Count
            End Get
        End Property

        ''' <summary> 
        ''' Cleans up the scheduler by indicating that no more tasks will be queued. 
        ''' This method blocks until all threads successfully shutdown. 
        ''' </summary> 
        Public Sub Dispose() Implements IDisposable.Dispose
            If _tasks IsNot Nothing Then
                ' Indicate that no new tasks will be coming in 
                _tasks.CompleteAdding()

                ' Wait for all threads to finish processing tasks 
                For Each thread In _threads
                    thread.Join()
                Next

                ' Cleanup 
                _tasks.Dispose()
                _tasks = Nothing
            End If
        End Sub

    End Class

End Namespace
