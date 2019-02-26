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

#define _GNU_SOURCE


// if TIMESTAMPSEPARATESTDOUTANDERR is defined, when time stamping the
// solver output, stdout and stderr are transmitted separately to
// runsolver. This lets us print whether the line comes from stdout or
// stderr. Unfortunately, when streams are separated, we have no
// guarantee to get the lines in the order they were sent (try with
// testtimestamper.cc). For this reason, this flag should be
// undefined. When undefined, stdout and stderr are combined together
// in the solver and are transmitted through one single pipe to
// runsolver. Anyway, this is the usual way to pipe a program output
// to another program input
#undef TIMESTAMPSEPARATESTDOUTANDERR

#define SENDSIGNALBOTTOMUP

/*
 * TODO
 *
 * - create a class Watcher that handles the procTree and the timer thread
 *
 * - arrange so that display occurs each n*period seconds (and not n*(period+epsilon))
 * - print the command line of new processes (??)
 *
 * - use /proc/%pid/status
 *
 * - man pthreads : le comportement de la mesure du temps pour les
 *   threads depend de la version du noyau
 *
 */

#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <stdexcept>

#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <sys/wait.h>
#include <sys/times.h>
#include <sys/mman.h>
#include <unistd.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <pthread.h>
#include <errno.h>
#include <sys/prctl.h>
#include <sys/utsname.h>

#include <sys/ipc.h>
#include <sys/msg.h>
#include <sys/utsname.h>

#ifdef WITH_NUMA
#include <numa.h>
#endif

#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#include <getopt.h>

#include "Cores.hh"
#include "SignalNames.hh"
#include "ProcessList.hh"
#include "CircularBufferFilter.hh"
#include "ExecutionSummary.hh"

#include "Watcher.hh"
#include "TimeStamper.hh"

using namespace std;

const char *version=VERSION;

/**
 * use only one instance
 *
 */
class RunSolver
{
private:
  class Limit
  {
  private:
    int resource;
    rlim_t limit;
  protected:
    const char *name,*unit; // name and unit of this limit
    int scale; // call setrlimit() with limit<<scale. Useful to
               // maintain a limit in KiB while setrlimit() needs a
               // limit in bytes (set scale=10)
  public:
    /**
     * resource==-1 means that this is a fake limit (which is not
     * enforced via setrlimit())
     */
    Limit(int resource) : resource(resource) 
    {
      scale=0;
    }

    void setLimit(rlim_t lim) {limit=lim;}

    void output(ostream &out)
    {
      out << "Enforcing " << name << ": " 
	  << limit << " " << unit << endl;
    }

    void outputEnforcedLimit(ostream &out)
    {
      out << "Current " << name << ": " 
	  << getEnforcedLimit() << " " << unit << endl;
    }

    void enforceLimit() 
    {
      if (resource<0)
	return; // this is a fake limit, don't enforce anything

      struct rlimit lim={limit,limit};

      if (getrlimit(resource,&lim))
      {
	perror("getrlimit failed");
	return;
      }

      lim.rlim_cur=limit<<scale;
      if (setrlimit(resource,&lim))
	perror("setrlimit failed");
    }

    rlim_t getEnforcedLimit() 
    {
      if (resource<0)
	return limit; // this is a fake limit, don't ask the system

      struct rlimit lim;

      if (getrlimit(resource,&lim))
      {
	perror("getrlimit failed");
	return (rlim_t)-1; // good value to return ???
      }

      return lim.rlim_cur>>scale;
    }
  };

public:
  /**
   * This is a fake limit. It doesn't enforce anything by its own.
   */
  class SoftVSIZELimit : public Limit
  {
  public:
    SoftVSIZELimit(rlim_t size) : Limit(-1)
    {
      name="VSIZE limit (soft limit, will send SIGTERM then SIGKILL)";
      unit="KiB";
      scale=10;
      setLimit(size);
    }
  };

  class HardVSIZELimit : public Limit
  {
  public:
    HardVSIZELimit(rlim_t size) : Limit(RLIMIT_AS)
    {
      name="VSIZE limit (hard limit, stack expansion will fail with SIGSEGV, brk() and mmap() will return ENOMEM)";
      unit="KiB";
      scale=10;
      setLimit(size);
    }
  };

  /**
   * This is a fake limit. It doesn't enforce anything by its own.
   */
  class MemoryLimit : public Limit
  {
  public:
    MemoryLimit(rlim_t size) : Limit(-1)
    {
      name="RSS+Swap limit (soft limit, will send SIGTERM then SIGKILL)";
      unit="KiB";
      scale=10;
      setLimit(size);
    }
  };

  /**
   * This is a fake limit. It doesn't enforce anything by its own.
   */
  class SoftCPULimit : public Limit
  {
  public:
    SoftCPULimit(rlim_t cpuTime) : Limit(-1) 
    {
      name="CPUTime limit (soft limit, will send SIGTERM then SIGKILL)";
      unit="seconds";
      setLimit(cpuTime);
    }
  };
  
  /**
   * This is a fake limit. It doesn't enforce anything by its own.
   */
  class WallClockLimit : public Limit
  {
  public:
    WallClockLimit(rlim_t wallClockTime) : Limit(-1) 
    {
      name="wall clock limit (soft limit, will send SIGTERM then SIGKILL)";
      unit="seconds";
      setLimit(wallClockTime);
    }
  };
  
  class HardCPULimit : public Limit
  {
  public:
    HardCPULimit(rlim_t cpuTime) : Limit(RLIMIT_CPU) 
    {
      name="CPUTime limit (hard limit, will send SIGXCPU)";
      unit="seconds";
      setLimit(cpuTime);
    }
  };
  
  class FileSizeLimit : public Limit
  {
  public:
    FileSizeLimit(rlim_t fileSize) : Limit(RLIMIT_FSIZE) 
    {
      name="FSIZE limit";
      unit="KiB";
      scale=10;
      setLimit(fileSize);
    }
  };
  
  class NumberOfFilesLimit : public Limit
  {
  public:
    NumberOfFilesLimit(rlim_t nbFiles) : Limit(RLIMIT_NOFILE) 
    {
      name="NbFILES limit";
      unit="files";
      setLimit(nbFiles);
    }
  };
  
  class StackSizeLimit : public Limit
  {
  public:
    /**
     * won't enforce limit
     */
    StackSizeLimit() : Limit(RLIMIT_STACK) 
    {
      name="StackSize limit";
      unit="KiB";
      scale=10;
    }

    /**
     * will enforce limit
     */
    StackSizeLimit(rlim_t size) : Limit(RLIMIT_STACK) 
    {
      name="Stack size limit";
      unit="KiB";
      scale=10;
      setLimit(size);
    }
  };
  

private:
  // this is the object in charge of monitoring the solver and its children
  Watcher watcher;

  static Watcher *instanceWatcher; // instance of Watcher for signal handlers

  /**
   * signal handler to gather data about the solver
   *
   * may be called for SIGINT, SIGTERM or SIGALRM
   */
  static void watcherSigHandler(int signum, siginfo_t *siginfo, void *ucontext)
  {
#ifdef debug
    cout << "signal handler called for " << getSignalName(signum) << endl;
#endif

#ifdef disabled
    // if we don't use the SA_NOCLDSTOP flag for sigaction() when
    // registering the handler for SIGCHLD, we get a SIGCHLD each time
    // the traced process is stopped by a sigtrap. These must be
    // ignored. The code below ignores all SIGCHLD including the one
    // which let us know that the process we're watching has
    // terminated (despite the fact that this is something we want to
    // know). For this reason, this code is disabled and we use
    // SA_NOCLDSTOP

    if (signum==SIGCHLD)
      return;
#endif

    if (signum==SIGTERM || signum==SIGINT)
    {
      // ask the timer thread to kill the child
      instanceWatcher->stopSolver("Received SIGTERM or SIGINT, killing child");
      return;
    }
  }

  vector<Limit *> limits;

  ofstream out;

  // when set, redirect the standard input of the child to this file 
  char *inputRedirectionFilename=nullptr;

  // when set, redirect the standard output of the child to this file 
  char *outputRedirectionFilename=nullptr;

  // a class to timestamp each line of some output streams
  static TimeStamper tStamp;

  bool timeStamping=false; // true iff the solver output must be timestamped
  int stdoutRedirFD[2]; // pipe for timestamping the solver stdout
#ifdef TIMESTAMPSEPARATESTDOUTANDERR
  int stderrRedirFD[2]; // pipe for timestamping the solver stderr
#endif
  bool usePTY=false; // use a PTY to collect output from the solver ? (to
                     // force the solver to line-buffer its output)
  int ptymaster; // PTY master

  pthread_t timeStamperTID; // tid of the thread which does the timestamping

  // limits imposed on the size of the solver output (0 if unlimited)
  unsigned long long int limitedOutputActivateSize=0,limitedOutputMaxSize=0;

  CircularBufferFilter limitedOutputSizeFilter; // a filter to limit
						// the solver output
						// size

  streambuf *coutSaveBuf=nullptr; // backup of the cout buf (in case of a redirection)

public:
  RunSolver()
  {
  }

  ~RunSolver()
  {
    // delete limits
    for(Limit *l: limits)
      delete l;
    
    // cancel redirection before we leave
    if (coutSaveBuf)
      cout.rdbuf(coutSaveBuf);
  }


  /**
   * use a PTY to collect the solver output
   */
  void setUsePTY(bool usePTY)
  {
    this->usePTY=usePTY;
  }

  /**
   * send the output of the watching process to a given file
   */
  void setWatcherOutputFile(char *filename)
  {
    out.open(filename);
    coutSaveBuf=cout.rdbuf(out.rdbuf());
  }

  /**
   * redirect the standard input of the solver to a given file
   */
  void setInputRedirection(char *filename)
  {
    inputRedirectionFilename=filename;
  }

  /**
   * redirect the standard output of the solver to a given file
   */
  void setOutputRedirection(char *filename)
  {
    outputRedirectionFilename=filename;
  }

  /**
   * save the main statistics to a given file (text format)
   */
  void setVarOutputFile(char *filename)
  {
    watcher.setVarOutputFile(filename);
  }

  /**
   * save the main statistics to a given file (binary format)
   */
  void setBinVarOutputFile(char *filename)
  {
    watcher.setBinVarOutputFile(filename);
  }

  /**
   * decide if we should timestamp the solver output or not
   */
  void setTimeStamping(bool val)
  {
    timeStamping=val;
  }

  /**
   * decide if we should add an EOF line to the solver output or not
   */
  void setTimeStampingAddEOF(bool val)
  {
    if (!timeStamping)
      throw runtime_error("EOF line can only be added when timestamping is on");

    tStamp.addEOF(val);
  }

  /**
   * limit the size of the solver output
   */
  void setSolverOutputLimits(unsigned long long int activateSize,
			     unsigned long long int maxSize)
  {
    if (!timeStamping)
      throw runtime_error("limit on the output size can only be enforced when timestamping is on");

    limitedOutputActivateSize=activateSize;
    limitedOutputMaxSize=maxSize;
  }

  /**
   * set the time we should wait between sending a SIGTERM and a
   * SIGKILL to a solver we want to stop
   */
  void setDelayBeforeKill(int seconds)
  {
    watcher.setDelayBeforeKill(seconds);
  }

  void setCPULimit(int sec)
  {
    watcher.setCPULimit(sec);

    // SoftCPULimit doesn't enforce anything by its own
    addLimit(new SoftCPULimit(sec)); 

    addLimit(new HardCPULimit(sec+30)); 
    // add an extra delay because we want to stop the solver by
    // stopSolver() instead of SIGXCPU
  }

  void setWallClockLimit(int sec)
  {
    watcher.setWallClockLimit(sec);

    // WallClockLimit doesn't enforce anything by its own
    addLimit(new WallClockLimit(sec)); 
  }

  /**
   * limits are expressed in kilobytes
   *
   * soft limit= limit (calls stopSolver())
   * hard limit= limit+reserve (causes immediate SIGKILL)
   */
  void setVSizeLimit(long limit, long reserve)
  {
    watcher.setVSizeLimit(limit);

    // SoftVSIZELimit doesn't enforce anything by its own
    addLimit(new SoftVSIZELimit((limit)));

    // add an extra amount of memory because we want to stop the solver by
    // stopSolver() instead of SIGKILL
    addLimit(new HardVSIZELimit((limit+reserve)));
  }

  /**
   * limits are expressed in kilobytes
   *
   * soft limit= limit (calls stopSolver())
   */
  void setMemoryLimit(long limit)
  {
    watcher.setMemoryLimit(limit);

    addLimit(new MemoryLimit((limit)));
  }

  void setSignal(const char *signalName)
  {
    throw runtime_error("unimplemented");
  }

  /**
   * delete IPC queues that the solver may have created
   */
  void setCleanupSolverOwnIPCQueues(bool cleanup)
  {
    watcher.setCleanupSolverOwnIPCQueues(cleanup);
  }

  /**
   * delete all IPC queues that were created by this user
   */
  void setCleanupAllIPCQueues(bool cleanup)
  {
    watcher.setCleanupAllIPCQueues(cleanup);
  }

  /**
   * add a limit to respect
   *
   * @parm limit: must be dynamically allocated
   */
  void addLimit(Limit *limit) 
  {
    limits.push_back(limit);
  }

  void printLimits(ostream &s)
  {
    for(size_t i=0;i<limits.size();++i)
      limits[i]->output(s);

    if (limitedOutputMaxSize)
    {
      if (timeStamping)
	cout << "Solver output will be limited to a maximum of "
	     << limitedOutputMaxSize << " bytes. The first "
	     << limitedOutputActivateSize << " bytes and the last "
	     << limitedOutputMaxSize-limitedOutputActivateSize
	     << " bytes will be preserved" << endl;
      else
	cout << "Solver output limit is ignored (currently only "
	        "available with timestamping)" << endl;
    }

    if (timeStamping && usePTY)
      cout << "Using a pseudo terminal to collect output from the solver" 
	   << endl;
  }

  /**
   * select cores that will be available to the process we watch
   *
   * if physicalView is true, desc contains the id of the cores to be
   * used as they are known on the system. In this case, cores 0 and
   * 1 (for example) may belong to different processors.
   *
   * if physicalView is false, desc contains the index of the cores to
   * be used in a list of available cores sorted by the processor to
   * which they belong. In this case, cores 0 and 1 (for example) will
   * necessarily belong to the same processor (unless it has only one
   * core!)
   */
  void selectCores(const string &desc, bool physicalView)  
  {
    vector<unsigned short int> availableCores,selectedCores;

    getExistingCores(availableCores,physicalView);

    istringstream d(desc);
    size_t a,b;

    while(true)
    {
      if(!(d>>a))
      {
	printCoresListSyntax();
	exit(1);
      }

      while(isspace(d.peek()))
	d.get();

      if(d.peek()=='-')
      {
	d.get();
	if(d>>b)
	{
	  //cout << "read " << a << "-" << b << endl;
	  if(b<a)
	    swap(a,b);

	  for(size_t i=a;i<=b;++i)
	    if(i>=0 && i<availableCores.size())
	      selectedCores.push_back(availableCores[i]);
	}
	else
	{
	  printCoresListSyntax();
	  exit(1);
	}
      }
      else
      {
	//cout << "read " << a << endl;
	selectedCores.push_back(availableCores[a]);
      }

      if(d.peek()==',')
	d.get();
      else
	if(d.peek()==EOF)
	  break;
	else
	{
	  printCoresListSyntax();
	  exit(1);
	}
    }

    cpu_set_t mask=affinityMask(selectedCores);
    if(sched_setaffinity(0,sizeof(cpu_set_t),&mask)!=0)
      perror("sched_setaffinity failed: ");
  }

  void printCoresListSyntax()
  {
    cout << "Syntax of a core list:\n"
	 << "  range first-last or individual numbers separated by commas\n" 
	 << "  examples: 0-1,5,7 or 0,1,5,7 or 0-7\n"
	 << endl;
  }


  /**
   * run a solver
   */
  void run(char **cmd)
  {
    instanceWatcher=&watcher;

    struct utsname unameData;
    uname(&unameData);
    cout << unameData.sysname << " " << unameData.release << endl;

    {
      int res=prctl(PR_SET_CHILD_SUBREAPER,1,0,0,0);
      if(res!=0)
      {
        cout << "prctl(PR_SET_CHILD_SUBREAPER,...) unavailable" << endl;
	watcher.setChildReaper(false);

	cout << "This version of runsolver requires a kernel version >= 3.4 in order to use prctl(PR_SET_CHILD_SUBREAPER,...). Aborting.\n";
	exit(1);
      }
      else
	watcher.setChildReaper(true);
    }
    
    struct sigaction handler;

    handler.sa_sigaction=watcherSigHandler;
    sigemptyset(&handler.sa_mask);
    handler.sa_flags=SA_SIGINFO|SA_NOCLDSTOP|SA_RESTART;
    /*
      The SA_RESTART flag tells that system calls which are
      interrupted by a signal should be automatically restarted. This
      way, we don't have to encapsulate system calls in a loop which
      would restart them when they return with
      errno=EINTR. Alternatively, we could have used
      siginterrupt(). 

      The SA_NOCLDSTOP prevent us from getting a SIGCHLD each time a 
      process is stopped for tracing.
    */

    sigaction(SIGALRM,&handler,NULL);
    sigaction(SIGINT,&handler,NULL);
    sigaction(SIGTERM,&handler,NULL);

    if (timeStamping)
    {
      int fd=STDOUT_FILENO;

      if (outputRedirectionFilename)
      {
	fd=open(outputRedirectionFilename,O_WRONLY|O_CREAT|O_TRUNC,0644);
	if (fd<0)
	  throw runtime_error(string("open failed during output redirection: ")
			      +strerror(errno));
      }

      int outputFromSolverFD=0;

      if (usePTY)
      {
	ptymaster=posix_openpt(O_RDWR);
	if (ptymaster<0)
	{
	  perror("Failed to create pseudo-terminal");
	  exit(1);
	}

	outputFromSolverFD=ptymaster;

	if (grantpt(ptymaster)!=0)
	{
	  perror("Failed to grant the pseudo-terminal");
	  exit(1);
	}

	if (unlockpt(ptymaster)!=0)
	{
	  perror("Failed to unlock the pseudo-terminal");
	  exit(1);
	}
      }
      else
      {
	pipe(stdoutRedirFD);
#ifdef TIMESTAMPSEPARATESTDOUTANDERR
	pipe(stderrRedirFD);
#endif
      }

#ifdef TIMESTAMPSEPARATESTDOUTANDERR
      tStamp.watch(stdoutRedirFD[0],'o',fd); // 'o' as output
      tStamp.watch(stderrRedirFD[0],'e',fd); // 'e' as error
#else

      if (usePTY)
	outputFromSolverFD=ptymaster;
      else
	outputFromSolverFD=stdoutRedirFD[0];

      if (limitedOutputMaxSize)
      {
	limitedOutputSizeFilter.setup(fd,limitedOutputActivateSize,
				      limitedOutputMaxSize);
	tStamp.watch(outputFromSolverFD,&limitedOutputSizeFilter,0);
      }
      else
	tStamp.watch(outputFromSolverFD,0,fd);
#endif
      tStamp.resetTimeStamp();

      int err=pthread_create(&timeStamperTID,NULL,timeStampThread,NULL);
      if (err)
	cout << "Failed to create a thread to timestamp the solver output" 
	     << endl;
    }

    pid_t childpid=fork();
    if (childpid<0)
    {
      perror("fork failed");
      exit(127);
    }
    else
      if (childpid==0)
      {
	// child

	// enforce limits
	for(size_t i=0;i<limits.size();++i)
	  limits[i]->enforceLimit();

	StackSizeLimit stackLimit;
	stackLimit.outputEnforcedLimit(cout);
	cout << endl;

	// create a new process group (for several reasons, see for
	// example ProcessTree::rootProcessEnded())
	setpgid(0,0); 

	if (inputRedirectionFilename)
	{
	  int err;
	  int fd;

	  fd=open(inputRedirectionFilename,O_RDONLY);
	  if (fd<0)
	    throw runtime_error(string("open failed during input redirection: ")
				+strerror(errno));

	  err=dup2(fd,STDIN_FILENO);
	  if (err<0)
	    throw runtime_error(string("dup2 failed during input redirection: ")
				+strerror(errno));
	  
	  close(fd);
	}

	if (outputRedirectionFilename && !timeStamping)
	{
	  int err;
	  int fd;

	  fd=open(outputRedirectionFilename,O_WRONLY|O_CREAT|O_TRUNC,0644);
	  if (fd<0)
	    throw runtime_error(string("open failed during output redirection: ")
				+strerror(errno));

	  err=dup2(fd,STDOUT_FILENO);
	  if (err<0)
	    throw runtime_error(string("dup2 failed during output redirection: ")
				+strerror(errno));

	  err=dup2(fd,STDERR_FILENO);
	  if (err<0)
	    throw runtime_error(string("dup2 failed during output redirection: ")
				+strerror(errno));
	  
	  close(fd);
	}

	if (timeStamping)
	{
	  if (usePTY)
	  {
	    int err;
	    int fd;

	    char *pts=ptsname(ptymaster);
	      
	    if (pts==NULL)
	      throw runtime_error(string("Failed to get pty slave name:")
				  +strerror(errno));
	      
	    fd=open(pts,O_RDWR);
	    if (fd<0)
	      throw runtime_error(string("open of pty slave failed: ")
				  +strerror(errno));

	    err=dup2(fd,STDOUT_FILENO);
	    if (err<0)
	      throw runtime_error(string("dup2 failed during output redirection: ")
				  +strerror(errno));

	    err=dup2(fd,STDERR_FILENO);
	    if (err<0)
	      throw runtime_error(string("dup2 failed during output redirection: ")
				  +strerror(errno));
	  
	    close(fd);
	  }
	  else
	  {
	    // plain tube

	    // redirecting stdout and stderr to the write side of the
	    // pipes to runsolver; close the read side of the pipe which
	    // belongs to our father

	    close(stdoutRedirFD[0]);
	    dup2(stdoutRedirFD[1],STDOUT_FILENO);
	    close(stdoutRedirFD[1]);

#ifdef TIMESTAMPSEPARATESTDOUTANDERR
	    close(stderrRedirFD[0]);
	    dup2(stderrRedirFD[1],STDERR_FILENO);
	    close(stderrRedirFD[1]);
#else
	    dup2(STDOUT_FILENO,STDERR_FILENO);
#endif
	  }
	}

	execvp(cmd[0],cmd);
	// only returns when it failed
	perror("exec failed");

	int i=0;
	cerr << "Solver command line was: ";
	while (cmd[i])
	  cerr << cmd[i++] << ' ';

	cerr << '\n' << endl;

	exit(127);
      }
      else
      {
	// parent
#ifdef debug
	cout << "child has pid " << childpid << endl;
#endif
	// We don't care about stdin. In case someone writes 'echo
	// data | runsolver program', runsolver just closes its stdin
	// and the child will be the only one to access it.
	close(STDIN_FILENO);

	// let runsolver run on the last allocated core
	vector<unsigned short int> cores;

	getAllocatedCoresByProcessorOrder(cores);
	if(cores.size()>1)
	{
	  int tmp=cores.back();
	  cores.clear();
	  cores.push_back(tmp);
	}

	cpu_set_t mask=affinityMask(cores);
	if(sched_setaffinity(0,sizeof(cpu_set_t),&mask)!=0)
	  perror("sched_setaffinity failed: ");

	if (timeStamping && !usePTY)
	{
	  // the write side of the pipe belongs to the child
	  close(stdoutRedirFD[1]);
#ifdef TIMESTAMPSEPARATESTDOUTANDERR
	  close(stderrRedirFD[1]);
#endif
	}

	if(timeStamping)
	  watcher.setTimeStamper(tStamp);

	// start monitoring the solver (will create a thread, and wait
	// for the solver end)
	watcher.watchPID(childpid);

	if (timeStamping)
	{
	  // wait for the time stamper thread to output the last lines
	  cout << "??? join timestamper begins" << endl;
	  pthread_join(timeStamperTID,NULL);
	  cout << "??? join timestamper ends" << endl;
	}

	cout << "??? end of timestamper thread" << endl; //???

	// print the resources used by runsolver itself
	struct rusage r;
	getrusage(RUSAGE_SELF,&r);
	cout << "runsolver used "
	     << r.ru_utime.tv_sec+r.ru_utime.tv_usec*1E-6
	     << " second user time and " 
	     << r.ru_stime.tv_sec+r.ru_stime.tv_usec*1E-6 
	     << " second system time\n" 
	     << endl;	

	cout << "The end" << endl;
      }
  }


  /**
   * procedure run by the thread in charge of timestamping the solver
   * output stream
   */
  static void *timeStampThread(void *)
  {
    tStamp.timeStampLines(); // enless loop

    return NULL;
  }

};


#ifdef WITH_NUMA
void numaInfo()
{
  if(numa_available()==-1)
    return;
  
  int nbNodes=numa_num_configured_nodes();
  long mem,memFree;
  
  cout << "NUMA information:\n";
  cout << "  number of nodes: " << nbNodes << endl;
  for(int i=0;i<nbNodes;++i)
  {
    mem=numa_node_size(i,&memFree);
    mem/=1024*1024;
    memFree/=1024*1024;
    cout << "  memory of node " << i << ": " << mem << " MiB (" << memFree << " MiB free)\n";
  }


  cout << "  node distances:\n";
  for(int l=0;l<nbNodes;++l)
  {
    cout << "   ";
    for(int c=0;c<nbNodes;++c)
    {
      cout << setw(4) << numa_distance(l,c);
    }
    cout << endl;
  }
  
  cout << endl;
}
#endif

// static members
Watcher *RunSolver::instanceWatcher=nullptr;
TimeStamper RunSolver::tStamp;

static struct option longopts[] =
{
  {"cpu-limit", required_argument, NULL, 'C'},
  {"wall-clock-limit", required_argument, NULL, 'W'},
  {"mem-limit", required_argument, NULL, 'M'},
  {"vsize-limit", required_argument, NULL, 'V'},
  {"rss-swap-limit", required_argument, NULL, 'R'},
  {"stack-limit", required_argument, NULL, 'S'},
  {"output-limit", required_argument, NULL, 'O'},
  {"input", required_argument, NULL, 'i'},
  {"delay", required_argument, NULL, 'd'},
  {"help", no_argument, NULL, 'h'},
  {"watcher-data", required_argument, NULL, 'w'},
  {"var", required_argument, NULL, 'v'},
  {"solver-data", required_argument, NULL, 'o'},
  {"timestamp", no_argument, NULL, 1000},
  {"use-pty", no_argument, NULL, 1002},
  {"cleanup-own-ipc-queues", no_argument, NULL, 1003},
  {"cleanup-all-ipc-queues", no_argument, NULL, 1004},
  {"cores", required_argument, NULL, 1005},
  {"phys-cores", required_argument, NULL, 1006},
  {"add-eof", no_argument, NULL, 1007},
  {"sig", required_argument, NULL, 1008},
  {"bin-var", required_argument, NULL, 1009},
  {"version", no_argument, NULL, 1010},
  {"watchdog", required_argument, NULL, 1011},
  {NULL, no_argument, NULL, 0}
};


void usage(char *prgname)
{
  cout << "Usage: " << prgname << endl
       << "       [--version]\n"
       << "       [-w file | --watcher-data file]\n"
       << "       [-v file | --var file]\n"
       << "       [-o file | --solver-data file]\n"
       << "       [--cores first-last]\n"
       << "       [-C cpu-limit | --cpu-limit cpu-limit]\n"
       << "       [-W time-limit | --wall-clock-limit time-limit]\n"
       << "       [-M vsize-limit | --mem-limit vsize-limit]  (deprecated, use -V or --vsize-limit)\n"
       << "       [-V vsize-limit | --vsize-limit vsize-limit]\n"
       << "       [-R rss+swap-limit | --rss-swap-limit rss+swap-limit]\n"
       << "       [-S stack-limit | --stack-limit stack-limit]\n"
       << "       [-d delay | --delay d]\n"
       << "       [--signal signalName]\n"
       << "       [--input filename]\n"
       << "       [--timestamp]\n"
       << "       [-O start,max | --output-limit start,max]\n"
       << "       [--use-pty]\n"
       << "       [--cleanup-own-ipc-queues | --cleanup-all-ipc-queues]\n"
       << "       [--bin-var filename]\n"
       << "       [--watchdog delay]\n"
       << "       command args...\n" 
       << endl;

  cout << "Vsize and rss+swap limits must be expressed in mega-bytes." << endl;
  cout << "The stack-limit must be expressed in mega-bytes." << endl;
  cout << "The cpu-limit must be expressed in seconds (CPU time)." << endl;
  cout << "The time-limit must be expressed in seconds (wall clock time)." << endl;
  cout << "When the time or memory limit is exceeded, the watching "
       << "process will try to send a SIGTERM and after <delay> "
       << "seconds will send a SIGKILL to the watched process" << endl;
  cout << "--version\n"
       << "  print the version and exit\n";
  cout << "-w filename or --watcher-data filename\n"
       << "  sends the watcher informations to filename" << endl;
  cout << "-v filename or --var filename\n"
       << "  save the most relevant information (times,...) in an easy to parse VAR=VALUE file" << endl;
  cout << "-o filename or --solver-data filename\n"
       << "  redirects the solver output (both stdout and stderr) to filename\n"
       << "--signal signalName\n"
       << "  send signal <signalName> instead of SIGTERM\n" 
       << "--input filename\n"
       << "  redirects the standard input of the runned program to filename\n" 
       << "--timestamp\n"
       << "  instructs to timestamp each line of the solver standard output and\n"
       << "  error files (which are then redirected to stdout)\n"
       << "--add-eof\n"
       << "  when timestamps are used, request to add an 'EOF' line at the end of the solver output\n"
       << "--output-limit start,max or -O start,max:\n"
       << "  limits the size of the solver output.\n"
       << "  Currently implies --timestamp. The solver output will be limited\n"
       << "  to a maximum of <max> MiB. The first <start> MiB will be\n"
       << "  preserved as well as the last <max-start> MiB.\n" 
       << "--phys-cores list\n"
       << "  allocate a subset of the cores to the solver. The list contains\n"
       << "  core numbers separated by commas, or ranges first-last. This list\n"
       << "  must contain core identifiers as they are known by the system in\n"
       << "  /proc/cpuinfo.\n"
       << "--cores list\n"
       << "  allocate a subset of the cores to the solver. The list contains\n"
       << "  core numbers separated by commas, or ranges first-last. This list\n"
       << "  contains the index of the selected cores in a list of core identifiers\n"
       << "  sorted by the processor they belong to. For example, if processor 0\n"
       << "  contains cores 0, 2, 4, 6 and processor 1 contains cores 1, 3, 5, 7,\n"
       << "  the sorted list is 0, 2, 4, 6, 1, 3, 5, 7 and the argument 0-3 will select\n"
       << "  the 4 cores of processor 0 (with physical id 0, 2, 4, 6). This option\n"
       << "  allows to ignore the details of the core numbering scheme used by the kernel.\n"
       << "--use-pty\n"
       << "  use a pseudo-terminal to collect the solver output. Currently only\n"
       << "  available when lines are timestamped. Some I/O libraries (including\n"
       << "  the C library) automatically flushes the output after each line when\n"
       << "  the standard output is a terminal. There's no automatic flush when\n"
       << "  the standard output is a pipe or a plain file. See setlinebuf() for\n"
       << "  some details. This option instructs runsolver to use a\n"
       << "  pseudo-terminal instead of a pipe/file to collect the solver\n"
       << "  output. This fools the solver which will line-buffer its output.\n"
       << "--cleanup-own-ipc-queues\n"
       << "  on exit, delete IPC queues that the user owns and to which the solver\n"
       << "  was the last process to read/write [may fail to delete some queues]\n" 
       << "--cleanup-all-ipc-queues\n"
       << "  on exit, delete all IPC queues that the user created [will also delete\n"
       << "  queues that don't belong to the solver]\n" 
       << "--bin-var filename\n"
       << "  save the most relevant information (times,...) about the solver\n"
       << "  execution in a binary format file named filename. The format of\n"
       << "  the file is given by the internal struct ExecutionSummary (see\n"
       << "  ExecutionSummary.hh). As this file is accessed through a shared\n"
       << "  memory mapping, this option is essentially useful to\n"
       << "  efficiently report the results to a calling program. This\n"
       << "  avoids to use the --var option and parse the resulting text\n"
       << "  file later.\n"
       << "--watchdog delay\n"
       << "  send an ABRT signal to runsolver after delay seconds\n"
       << endl;

  exit(1);
}

void printVersion()
{
  cout << "runsolver version " << version << " (svn: " << SVNVERSION
       << ") Copyright (C) 2010-2013 Olivier ROUSSEL\n\n"
       << "This program is distributed in the hope that it will be useful,\n"
       << "but WITHOUT ANY WARRANTY; without even the implied warranty of\n"
       << "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n"
       << "GNU General Public License for more details.\n"
       << endl;
}

void watchdog(int watchdogDelay)
{
  cout << "Starting watchdog thread" << endl;
  sleep(watchdogDelay);
  cout << "watchdog killed runsolver after " << watchdogDelay << " seconds." << endl;
  kill(getpid(),SIGABRT);
}

int main(int argc, char **argv)
{
  RunSolver solver;
  int optc;

  // memLimit in KiB
  long vsizeLimit=0,memLimit=0;
  // difference between the 'hard' and the 'soft' limit (in KiB)
  int vsizeSoftToHardLimit=50*1024; 

  int watchdogDelay=0;

  string cmdline;
  for(int i=0;i<argc;++i)
  {
    cmdline+=argv[i];
    cmdline+=' ';
  }

  try
  {
    ios_base::sync_with_stdio();

    while ((optc = getopt_long (argc, argv, "+o:w:v:C:W:M:S:O:d:h", longopts, NULL))
	   != EOF)
    {
      switch (optc)
      {
      case 'o':
	solver.setOutputRedirection(optarg);
	break;
      case 'i':
	solver.setInputRedirection(optarg);
	break;
      case 'w':
	solver.setWatcherOutputFile(optarg);
	break;
      case 'v':
	solver.setVarOutputFile(optarg);
	break;
      case 'M':
      case 'V':
	vsizeLimit=atol(optarg)*1024;
	break;
      case 'R':
        memLimit=atol(optarg)*1024;
	break;
      case 'C':
	solver.setCPULimit(atoi(optarg));
	break;
      case 'W':
	solver.setWallClockLimit(atoi(optarg));
	break;
      case 'S':
	solver.addLimit(new RunSolver::StackSizeLimit(atoi(optarg)*1024));
	break;
      case 'd':
	solver.setDelayBeforeKill(atoi(optarg));
	break;
      case 'O':
	int activate,max;
	if (sscanf(optarg,"%d,%d",&activate,&max)!=2 ||
	    activate>=max)
	{
	  cout << "Syntax: --output-limit A,M with A<M" << endl;
	  exit(1);
	}
	solver.setTimeStamping(true);
	solver.setSolverOutputLimits(activate*1024*1024,max*1024*1024);
	break;
      case 1000:
	solver.setTimeStamping(true);
	break;
      case 1002:
	solver.setUsePTY(true);
	break;
      case 1003:
	solver.setCleanupSolverOwnIPCQueues(true);;
	break;
      case 1004:
	solver.setCleanupAllIPCQueues(true);;
	break;
      case 1005:
	solver.selectCores(optarg,false);
	break;
      case 1006:
	solver.selectCores(optarg,true);
	break;
      case 1007:
	solver.setTimeStampingAddEOF(true);
      case 1008:
	solver.setSignal(optarg);
	break;
      case 1009:
	solver.setBinVarOutputFile(optarg);
	break;
      case 1010:
        printVersion();
        exit(0);
	break;
      case 1011:
	watchdogDelay=atoi(optarg);
	break;
      default:
	usage (argv[0]);
      }
    }

    // this must be output AFTER the command line has been parsed
    // (i.e. after the possible redirection have been set up)
    printVersion();

#ifdef WITH_NUMA
    numaInfo();
#endif
    
    if (optind == argc)
      usage (argv[0]);

    cout << "command line: " << cmdline << endl
	 << endl;
    
    vector<unsigned short int> cores;

    getAllocatedCoresByProcessorOrder(cores);

    cout << "running on " << cores.size() << " cores: ";
    printAllocatedCores(cout,cores);
    cout << "\n\n";

    if (vsizeLimit)
      solver.setVSizeLimit(vsizeLimit,vsizeSoftToHardLimit);	

    if (memLimit)
      solver.setMemoryLimit(memLimit);	

    solver.printLimits(cout);

    thread watchdogThread;
    if(watchdogDelay)
    {
      watchdogThread=move(thread(watchdog,watchdogDelay));
      watchdogThread.detach();
    }

    solver.run(&argv[optind]);
  }
  catch (exception &e)
  {
    cout.flush();
    cerr << "\n\tUnexpected exception in runsolver:\n";
    cerr << "\t" << e.what() << endl;
    exit(1);
  }
}

/*
setitimer(ITIMER_PROF,...) to receive SIGPROF regularly

alarm to receive SIGALRM at the timeout ???
*/

// Local Variables:
// mode: C++
// End:

