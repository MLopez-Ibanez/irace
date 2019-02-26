/*
 * Copyright (C) 2010 Olivier ROUSSEL
 *
 * This file is part of runsolver.
 *
 * runsolver is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * runsolver is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with runsolver.  If not, see <http://www.gnu.org/licenses/>.
 */
#ifndef _Watcher_hh_
#define _Watcher_hh_

#include <thread>
#include <mutex>

#include "ProcessTree.hh"
#include "ProcessHistory.hh"
#include "TimeStamper.hh"

using namespace std;

class Watcher
{
private:
  bool hasChildReaper=false; // did we set prctl(PR_SET_CHILD_SUBREAPER)?
  
  thread watcherThread;
  mutex cout_mutex;
  
  // when set, save the most relevant information in an easy to parse
  // format consisting of VAR=VALUE lines
  char *varOutputFilename;

  // when set, save the most relevant information in a binary format file
  char *binVarOutputFilename;

  // when the solver uses less than cpuUsageThreshold % of the CPU,
  // try to identify process of other users which use more than
  // heavyProcessThreshold % of the CPU
  static constexpr float cpuUsageThreshold=0.8; // % of CPU
  static constexpr float heavyProcessThreshold=0.1; // % of CPU

  pid_t childpid=0; // pid of the process we're watching
  bool solverIsRunning;
  bool abortChild; // request to kill the child when runsolver is killed

  float limitCPUTime; // CPU time (in seconds) given to solver before we stop it
  float limitWallClockTime; // Wall clock time (in seconds)  given to solver before we stop it
  long limitVSize; // VSize given to solver before we stop it
  long limitMemory; // memory given to solver before we stop it


  // used in timerThread
  float maxDisplayPeriod; // max. period between two samplings

  struct timeval starttv,stoptv;
  float elapsed; // elapsed time in seconds since the start of the
                 // watched process. Updated by timerThread
  float lastDisplayedElapsedTime; // last elapsed time at which a
				   // process tree was displayed

  bool stopSolverRequested=false; 
  const char *stopReason=nullptr; // reason for stopping the solver
  int stopSolverState=0; // steps during the solver stop

  ProcessTree *procTree,*lastProcTree; // to gather data about a process and its children
  ProcessHistory procHistory; // history of the last procTree

  // list of all tasks created by the solver. Only updated when
  // cleanupSolverOwnIPCQueues is set
  set<pid_t> listAllProcesses;

  // time in seconds between a SIGTERM and a SIGKILL sent to children
  int delayBeforeKill;

  // maximum cumulated vsize/memory of all children
  long maxVSize,maxMemory;

  // current CPU time of the watched process
  float currentCPUTime,currentSystemTime,currentUserTime; 

  // the time of children whose termination was reported directly to
  // runsolver (because their parent died and they were adopted by
  // runsolver)
  float completedCPUTime,completedUserTime,completedSystemTime;
  // pid of the processes whose termination was reported to runsolver
  vector<pid_t> completedChildrenList;
  
  long currentVSize; // current VSize of the watched process
  long lastVSize; // last VSize of the watched process

  long currentMemory; // current memory of the watched process
  long lastMemory; // last memory of the watched process

  bool cleanupSolverOwnIPCQueues; // delete IPC queues that the solver may have created?
  bool cleanupAllIPCQueues; // delete all IPC queues that are owned by the user on exit

  TimeStamper *timeStamper=nullptr;
public:
  Watcher(): procHistory(10)
  {
    solverIsRunning=false;
    abortChild=false;

    maxDisplayPeriod=60;

    limitCPUTime=0; // no CPU limit by default
    limitWallClockTime=0; // no CPU limit by default
    limitVSize=0; // no vsize limit by default
    limitMemory=0; // no memory limit by default

    delayBeforeKill=2;

    maxVSize=0;
    maxMemory=0;

    currentCPUTime=0;
    currentUserTime=0;
    currentSystemTime=0;

    completedCPUTime=0;
    completedUserTime=0;
    completedSystemTime=0;

    currentVSize=0;
    lastVSize=0;

    currentMemory=0;
    lastMemory=0;

    varOutputFilename=NULL;
    binVarOutputFilename=NULL;

    cleanupAllIPCQueues=false;
    cleanupSolverOwnIPCQueues=false;

    lastProcTree=NULL;
    procTree=new ProcessTree();
    procHistory.push(procTree);
  }

  ~Watcher()
  {
    // procTree and lastProcTree are deleted by ~ProcessHistory()
  }

  /**
   * associate a timestamper. The timeStamper will be called
   * periodically to communicate the current CPU time.
   */
  void setTimeStamper(TimeStamper &t)
  {
    timeStamper=&t;
  }

  /**
   * indicate if prctl(PR_SET_CHILD_SUBREAPER,...) was used
   */
  void setChildReaper(bool cr)
  {
    hasChildReaper=cr;
  }
  
  /**
   * delete IPC queues that the solver may have created
   */
  void setCleanupSolverOwnIPCQueues(bool cleanup)
  {
    cleanupSolverOwnIPCQueues=cleanup;
  }

  /**
   * delete all IPC queues that were created by this user
   */
  void setCleanupAllIPCQueues(bool cleanup)
  {
    cleanupAllIPCQueues=cleanup;
  }

  void setCPULimit(int sec)
  {
    limitCPUTime=sec;
  }

  void setWallClockLimit(int sec)
  {
    limitWallClockTime=sec;
  }

  /**
   * limits are expressed in kilobytes
   *
   * soft limit= limit (calls stopSolver())
   * hard limit= limit+reserve (causes immediate SIGKILL)
   */
  void setVSizeLimit(long limit)
  {
    limitVSize=limit;
  }

  /**
   * limits are expressed in kilobytes
   *
   * soft limit= limit (calls stopSolver())
   */
  void setMemoryLimit(long limit)
  {
    limitMemory=limit;
  }

  /**
   * save the main statistics to a given file (text format)
   */
  void setVarOutputFile(char *filename)
  {
    varOutputFilename=filename;
  }

  /**
   * save the main statistics to a given file (binary format)
   */
  void setBinVarOutputFile(char *filename)
  {
    binVarOutputFilename=filename;
  }
 
  /**
   * set the time we should wait between sending a SIGTERM and a
   * SIGKILL to a solver we want to stop
   */
  void setDelayBeforeKill(int seconds)
  {
    delayBeforeKill=seconds;
  }


  void watchPID(pid_t pid)
  {
    cout << "solver pid=" << pid << ", runsolver pid=" << getpid() << endl;

    childpid=pid;
    solverIsRunning=true;

    gettimeofday(&starttv,NULL);

    procTree->setRunsolverPID(getpid());

    procTree->setElapsedTime(0);

    // it looks like we can't get reliable information on the process
    // immediately
    // procTree->readProcesses();

    watcherThread=thread(ref(*this));

    int childstatus;
    struct rusage childrusage;
    int wait4result;

    while(true)
    {
      wait4result=waitpid(-1,&childstatus,0);

      if(wait4result<0 && errno==ECHILD) // nothing to wait for
        break;
      
      if(wait4result<0)
      {
        perror("FATAL: waitpid() failed");
        break;
      }

      float user,syst;
	
      getrusage(RUSAGE_CHILDREN,&childrusage);
      
      user=childrusage.ru_utime.tv_sec+childrusage.ru_utime.tv_usec*1E-6;
      syst=childrusage.ru_stime.tv_sec+childrusage.ru_stime.tv_usec*1E-6;
	
      completedCPUTime=user+syst;
      completedUserTime=user;
      completedSystemTime=syst;

      completedChildrenList.push_back(wait4result);
      
#warning print the last history?
      struct timeval tv;

      gettimeofday(&tv,NULL);
      float elapsed=tv.tv_sec+tv.tv_usec/1E6-starttv.tv_sec-starttv.tv_usec/1E6;

      cout_mutex.lock();
      cout << "\n[startup+" << elapsed << " s]\n"
	   << "# the end of solver process " << wait4result
	   << " was just reported to runsolver\n"
	   << "# cumulated CPU time of all completed processes:  user=" << user << " s, system=" << syst << " s" << endl;
      if(wait4result!=childpid)
	cout << "# this solver process was not waited by its parent and was adopted by runsolver\n";
      cout_mutex.unlock();
    }
    
    solverIsRunning=false;
    gettimeofday(&stoptv,NULL);

    cout << endl << "Solver just ended." << endl;

    watcherThread.join();

    cout << "Dumping a history of the last processes samples" 
	 << endl;

    procHistory.dumpHistory(cout,lastDisplayedElapsedTime);
    cout << endl;

    if (WIFEXITED(childstatus))
      cout << "Child status: " << WEXITSTATUS(childstatus) << endl;
    else
      if (WIFSIGNALED(childstatus))
      {
	int sig=WTERMSIG(childstatus);
	
	cout << "Child ended because it received signal " 
	     << sig  << " (" << getSignalName(sig) << ")" << endl;
	
#ifdef WCOREDUMP
	if (WCOREDUMP(childstatus))
	  cout << "Child dumped core" << endl; 
#endif

      }
      else
	if (WIFSTOPPED(childstatus))
	{
	  cout << "Child was stopped by signal "
	       << getSignalName(WSTOPSIG(childstatus))
	       << endl;
	}
	else
	{
	  cout << "Child ended for unknown reason !!" << endl;
	}

    float wcTime; // Elapsed real seconds
    float solverCPUTime; // Elapsed solver (CPU) seconds
    float solverUserTime; // Elapsed solver (User CPU) seconds
    float solverSystemTime; // Elapsed solver (System CPU) seconds

    wcTime=
      stoptv.tv_sec+stoptv.tv_usec*1E-6
      -starttv.tv_sec-starttv.tv_usec*1E-6;

    // get the CPU time of our children
    getrusage(RUSAGE_CHILDREN,&childrusage);

    solverUserTime=
      childrusage.ru_utime.tv_sec+childrusage.ru_utime.tv_usec*1E-6;

    solverSystemTime=
      childrusage.ru_stime.tv_sec+childrusage.ru_stime.tv_usec*1E-6;

    // solverCPUTime already contains completedCPUTime
    solverCPUTime=solverUserTime+solverSystemTime;

    cout << "Real time (s): " << wcTime << endl;
    cout << "CPU time (s): " << solverCPUTime << endl;
    cout << "CPU user time (s): " << solverUserTime << endl;
    cout << "CPU system time (s): " << solverSystemTime << endl;
    cout << "CPU usage (%): " << ((wcTime!=0)?100*solverCPUTime/wcTime:100)
	 << endl;
    cout << "Max. virtual memory (cumulated for all children) (KiB): " 
	 << maxVSize << endl;
    cout << "Max. memory (cumulated for all children) (KiB): " 
	 << maxMemory << endl;

    if (cleanupAllIPCQueues || cleanupSolverOwnIPCQueues)
      cleanupIPCMsgQueues();

    struct rusage r;
    getrusage(RUSAGE_CHILDREN,&r);

    cout << endl;
    cout << "getrusage(RUSAGE_CHILDREN,...) data:" << endl;
    cout << "user time used= " 
    << r.ru_utime.tv_sec+r.ru_utime.tv_usec*1E-6 << endl;
    cout << "system time used= " 
	 << r.ru_stime.tv_sec+r.ru_stime.tv_usec*1E-6 << endl;	
    cout << "maximum resident set size= " << r.ru_maxrss << endl;
    cout << "integral shared memory size= " << r.ru_ixrss << endl;
    cout << "integral unshared data size= " << r.ru_idrss << endl;         
    cout << "integral unshared stack size= " << r.ru_isrss << endl;
    cout << "page reclaims= " << r.ru_minflt << endl;        
    cout << "page faults= " << r.ru_majflt << endl;        
    cout << "swaps= " << r.ru_nswap << endl;         
    cout << "block input operations= " << r.ru_inblock << endl;       
    cout << "block output operations= " << r.ru_oublock << endl;       
    cout << "messages sent= " << r.ru_msgsnd << endl;        
    cout << "messages received= " << r.ru_msgrcv << endl;        
    cout << "signals received= " << r.ru_nsignals << endl;      
    cout << "voluntary context switches= " << r.ru_nvcsw << endl;         
    cout << "involuntary context switches= " << r.ru_nivcsw << endl;
    cout << endl;
  
    if(completedCPUTime!=0)
    {
	cout << endl;
	cout << "# summary of solver processes directly reported to runsolver:\n";
	if(!completedChildrenList.empty())
	{
	  cout << "#   pid: " << completedChildrenList[0];
	  for(size_t i=1;i<completedChildrenList.size();++i)
	    cout << ',' << completedChildrenList[i];
	  cout << endl;
	}
	cout << "#   total CPU time (s): " << completedCPUTime << endl;
	cout << "#   total CPU user time (s): " << completedUserTime << endl;
	cout << "#   total CPU system time (s): " << completedSystemTime << endl
	     << endl;
    }    

    ExecutionSummary execSummary;

    strncpy(execSummary.runsolverVersion,VERSION,sizeof(execSummary.runsolverVersion));
    execSummary.wcTime=wcTime;
    execSummary.cpuTime=solverCPUTime;
    execSummary.userTime=solverUserTime;
    execSummary.systemTime=solverSystemTime;
    execSummary.cpuUsage=(wcTime!=0)?100*solverCPUTime/wcTime:100;
    execSummary.maxVM=maxVSize;
    execSummary.timeOut=(limitCPUTime && solverCPUTime>limitCPUTime) ||
      (limitWallClockTime && (wcTime>limitWallClockTime));
    execSummary.memOut=limitVSize && maxVSize>limitVSize;
    execSummary.maxMem=maxMemory;
 
    if(varOutputFilename)
    {
      ofstream var(varOutputFilename);
 
      if(!var.good())
        cout << "Unable to open file " << varOutputFilename
             << " to save the main statistics in text format"
             << " (option --var): " << strerror(errno) << "\n" <<endl;
      else
      {          
        var << "# WCTIME: wall clock time in seconds\n"
            << "WCTIME=" <<  execSummary.wcTime << endl;
        
        var << "# CPUTIME: CPU time in seconds (USERTIME+SYSTEMTIME)\n"
            << "CPUTIME=" << execSummary.cpuTime << endl;
        
        var << "# USERTIME: CPU time spent in user mode in seconds\n"
            << "USERTIME=" << execSummary.userTime << endl;
 
        var << "# SYSTEMTIME: CPU time spent in system mode in seconds\n"
            << "SYSTEMTIME=" << execSummary.systemTime << endl;
 
        var << "# CPUUSAGE: CPUTIME/WCTIME in percent\n"
            << "CPUUSAGE=" << execSummary.cpuUsage << endl;
 
        var << "# MAXVM: maximum virtual memory used in KiB\n" 
            << "MAXVM=" << execSummary.maxVM << endl;
 
        var << "# TIMEOUT: did the solver exceed the time limit?\n"
            << "TIMEOUT=" << boolalpha << execSummary.timeOut << endl;
      
        var << "# MEMOUT: did the solver exceed the memory limit?\n"
            << "MEMOUT=" << boolalpha << execSummary.memOut << endl;
      
        if(!var.good())
          cout << "failed to save the main statistics in text format\n";
      }
    }
 
    if(binVarOutputFilename)
    {
      int fd=open(binVarOutputFilename,O_RDWR|O_CREAT|O_TRUNC,0644);
      if(fd<0)
        cout << "Unable to open file " << binVarOutputFilename
             << " to save the main statistics in binary format"
             << " (option --bin-var): " << strerror(errno) << "\n" <<endl;
      else
      {
        if(ftruncate(fd,sizeof(ExecutionSummary))<0)
          perror("resizing the binary statistics file failed: ");
      
        void *p=mmap(NULL,sizeof(ExecutionSummary),
                     PROT_READ|PROT_WRITE,MAP_SHARED,
                     fd,0);
      
        if(p==MAP_FAILED)
          perror("failed to map binary statistics file: ");
        else
        {
          memcpy(p,&execSummary,sizeof(execSummary));
          if(munmap(p,sizeof(ExecutionSummary))!=0)
            perror("failed to copy binary statistics file: ");
          close(fd);
        }
      }
    } // if(binVarOutputFilename)
 
  }

  /**
   * properly stop a solver
   *
   * to be used when the caller cannot wait (for example from a
   * system call callback).
   */
  void stopSolver(const char *msg)
  {
    stopSolverRequested=true;
    stopReason=msg;
  }

  /**
   * procedure run by the thread in charge of gathering data about the
   * watched process
   */
  void operator() ()
  {
    struct timespec d,delay={0,100000000}; // 0.1 s
    struct timeval tv;
    float displayPeriod,nextDisplayTime=0;
    int count=0,subcount=1; // number of seconds and tenths of second

    gettimeofday(&tv,NULL);
    displayPeriod=0.1;

    while(solverIsRunning)
    {
      if(abortChild)
	stopSolver("Received SIGTERM or SIGINT, killing child");

      d=delay;

      // try to compensate possible delays
      d.tv_nsec-=((tv.tv_usec-starttv.tv_usec+1000000)%100000)*1000;

      // wait (use a loop in case of an interrupt)
      while(nanosleep(&d,&d)==-1 && errno==EINTR);

      // get currenttime
      gettimeofday(&tv,NULL);
      elapsed=
	tv.tv_sec+tv.tv_usec/1E6
	-starttv.tv_sec-starttv.tv_usec/1E6;

      // at the beginning or every second, get a fresh list of processes
      if (subcount==10 || (count==0 && subcount<=5))
      {
	// identify new children
	if (readProcessData(true))
	{
	  solverIsRunning=false;
	  break;
	}
	
	if(subcount==10)
	{
	  subcount=1;
	  ++count;
	}
	else
	  ++subcount;
      }
      else
      { 
	// simple update
	if (readProcessData(false))
	{
	  solverIsRunning=false;
	  break;
	}
	++subcount;
      }
      //procTree->dumpProcessTree(cout); //???
      if(elapsed>=nextDisplayTime)
      {
	displayProcessData();

	nextDisplayTime+=displayPeriod;
	displayPeriod=std::min(2*displayPeriod,maxDisplayPeriod);
	//cout << "displayPeriod=" << displayPeriod << endl;
	//cout << "nextDisplayTime=" << nextDisplayTime << endl;
      }


      if(stopSolverRequested)
      {
	/*The solver will first receive a SIGTERM to give it a chance to
	  output the best solution it found so far (in the case of an
	  optimizing solver). A few seconds later, the program will receive a
	  SIGKILL signal from the controlling program to terminate the
	  solver.*/

	if(stopSolverState==0)
	{
	  // we've just been notified
	  cout << "\n\n\n" << stopReason << endl;

	  // give some evidence to the user
	  displayProcessData();

	  // send SIGTERM
	  sendSIGTERM();

	  cout << "Sleeping " << delayBeforeKill << " seconds" << endl;
	}
	else if(stopSolverState>=delayBeforeKill*10)
	{
	  sendSIGKILL();
	  solverIsRunning=false; // we can't do any better anyway
	}

	++stopSolverState;
      } // if(stopSolverRequested)
    }

    // just to be sure, make sure we don't have any child any more
    d={0,10000000};
    nanosleep(&d,nullptr); // wait 10 ms
    ProcessTree pt;
    pt.setRunsolverPID(getpid());
    pt.readProcesses(); // get a fresh list of processes
    if(!pt.solverEnded())
    {
      cout << "WARNING: unexpectedly, some solver processes are still present and will be killed:" << endl;
      pt.dumpProcessTree(cout);
      pt.sendSignalNow(SIGKILL);
    }

    cout << "??? end of watcher thread" << endl;

#ifdef debug
    struct rusage r;
    getrusage(RUSAGE_THREAD,&r);
    cout << "the watcher thread used "
	 << r.ru_utime.tv_sec+r.ru_utime.tv_usec*1E-6
	 << " second user time and " 
	 << r.ru_stime.tv_sec+r.ru_stime.tv_usec*1E-6 
	 << " second system time\n" 
	 << endl;
#endif
  }

protected:

  /**
   * gather data about the watched processes
   *
   * if updateChildrenList is false, only update process informations
   * but don't try to identify new processes (faster), if
   * updateChildrenList is true, identify all processes which are a
   * children of a watched process.
   *
   * @return true iff the main process was terminated
   */
  bool readProcessData(bool updateChildrenList=true)
  {
    if (!childpid)
      return false; // trying to collect data before parent got the pid

#ifdef debug
    cout << "Reading process data (updateChildrenList=" << updateChildrenList << ")" << endl;
#endif

    lastProcTree=procTree;
    procTree=new ProcessTree(*procTree);

    procTree->setElapsedTime(elapsed);
    procTree->setCompletedCPUTime(completedCPUTime);

    if (updateChildrenList)
    {
      procTree->readProcesses();
      if (cleanupSolverOwnIPCQueues)
	procTree->listProcesses(listAllProcesses);
    }
    else
      procTree->updateProcessesData();

    if (procTree->solverEnded() && !solverIsRunning) // ???
    {
      delete procTree;
      procTree=lastProcTree;
      return true; // don't go any further
    }

    lastVSize=currentVSize;
    lastMemory=currentMemory;

    procTree->currentCPUTime(currentUserTime,currentSystemTime);
    currentCPUTime=currentUserTime+currentSystemTime;

    currentVSize=procTree->currentVSize();
    currentMemory=procTree->currentMemory();

    maxVSize=max(maxVSize,currentVSize);
    maxMemory=max(maxMemory,currentMemory);

    procHistory.push(procTree);

    if(timeStamper)
      timeStamper->setCPUtimeFromAnotherThread(currentCPUTime+completedCPUTime);

#warning do we need a mutex or volatile or something?
    if (limitCPUTime && currentCPUTime+completedCPUTime>=limitCPUTime)
      stopSolver("Maximum CPU time exceeded: sending SIGTERM then SIGKILL");

    if (limitWallClockTime && elapsed>=limitWallClockTime)
      stopSolver("Maximum wall clock time exceeded: sending SIGTERM then SIGKILL");

    if (limitVSize && currentVSize>=limitVSize)
      stopSolver("Maximum VSize exceeded: sending SIGTERM then SIGKILL");

    if (limitMemory && currentMemory>=limitMemory)
      stopSolver("Maximum memory exceeded: sending SIGTERM then SIGKILL");

    return false;
  }

  /**
   * display the data we have about all watched processes
   *
   */
  void displayProcessData()
  {
    cout_mutex.lock();

    lastDisplayedElapsedTime=procTree->getElapsedTime();
    procTree->dumpProcessTree(cout);
    procTree->dumpCPUTimeAndVSize(cout,currentCPUTime,currentVSize,currentMemory);

    if (elapsed>2 && currentCPUTime<cpuUsageThreshold*elapsed)
    {
      // its looks like we're not using all CPU. Maybe there's another
      // heavy process running. Try to identify it.
      procTree->dumpHeavyProcesses(cout,heavyProcessThreshold);
    }

    cout_mutex.unlock();	
  }

  void sendSIGTERM()
  {
    cout << "\nSending SIGTERM to process tree (bottom up)" << endl;
    procTree->sendSignalBottomUp(SIGTERM);
  }

  void sendSIGKILL()
  {
    cout << "\nSending SIGKILL to process tree (bottom up)" << endl;
    procTree->sendSignalBottomUp(SIGKILL);
  }

  /**
   * delete IPC queues that may have been created by the solver
   */
  void cleanupIPCMsgQueues()
  {
#ifndef __linux__
#error This code is linux specific
#endif

    struct msginfo msginfo;
    struct msqid_ds msgqueue;
    int maxid,msqid;
    uid_t myUid=geteuid();

    cout << "\n";

    maxid=msgctl(0,MSG_INFO,reinterpret_cast<struct msqid_ds *>(&msginfo));
    if (maxid<0) 
      return;

    for (int id=0;id<=maxid;++id) 
    {
      msqid=msgctl(id,MSG_STAT,&msgqueue);

      if (msqid<0)
	continue;

      if (msgqueue.msg_perm.cuid==myUid &&
	  (cleanupAllIPCQueues || 
	   listAllProcesses.find(msgqueue.msg_lspid)!=listAllProcesses.end() ||
	   listAllProcesses.find(msgqueue.msg_lrpid)!=listAllProcesses.end()))
      {
	cout << "deleting IPC queue " << msqid;
	if (msgctl(msqid,IPC_RMID,&msgqueue))
	  cout << " (failed: " << strerror(errno) << ")";
	cout << "\n";
      }
    }
  }
};

// Local Variables:
// mode: C++
// End:
#endif
