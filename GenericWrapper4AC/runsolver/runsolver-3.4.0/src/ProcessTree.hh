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



#ifndef _ProcessTree_hh_
#define _ProcessTree_hh_

#include <sys/types.h>
#include <dirent.h>
#include <signal.h>
#include <errno.h>

#include <cassert>
#include <iostream>
#include <cctype>
#include <map>
#include <stdexcept>

#include "ProcessData.hh"

using namespace std;

class ProcessTree
{
private:
  typedef map<pid_t,ProcessData *> ProcMap;
  
  ProcMap tree;
  vector<pid_t> roots; // root processes of the solver

  string loadavgLine;
  long memTotal,memFree,swapTotal,swapFree; // data from /proc/meminfo

  float elapsed; // number of seconds elapsed since the start of the program

  float uptime; // up time of the host (in seconds)

  float completedCPUTime=0; // CPU time of completed processes
  
  bool treeHasAllProcesses=false;

  pid_t runsolverPID=-1;
public:
  ProcessTree()
  {
  }

  ProcessTree(const ProcessTree &pt): roots(pt.roots), completedCPUTime(pt.completedCPUTime), runsolverPID(pt.runsolverPID)
  {
    treeHasAllProcesses=pt.treeHasAllProcesses;
    
    elapsed=pt.elapsed;
    memTotal=pt.memTotal;
    memFree=pt.memFree;
    swapTotal=pt.swapTotal;
    swapFree=pt.swapFree;
    loadavgLine=pt.loadavgLine;

    clone(pt);
  }

  ~ProcessTree()
  {
    clear();
  }

  void clear()
  {
    for(ProcMap::iterator it=tree.begin();it!=tree.end();++it)
      delete (*it).second;

    tree.clear();
    roots.clear();
  }

  void setRunsolverPID(pid_t pid)
  {
    runsolverPID=pid;
  }


  void setElapsedTime(float timeSinceStartOfProgram)
  {
    elapsed=timeSinceStartOfProgram;
  }

  void setCompletedCPUTime(float cpuTime)
  {
    completedCPUTime=cpuTime;
  }

  float getElapsedTime() const
  {
    return elapsed;
  }

  /**
   * return true when the main process has ended
   */
  bool solverEnded()
  {
    return roots.empty();
  }

  /**
   * gather informations on all processes (to determine all children
   * of the watched process)
   *
   */
  void readProcesses()
  {
    DIR *procfs=opendir("/proc");
    struct dirent *dirEntry;
    pid_t pid;

    //cout << "??? runsolverPID=" << runsolverPID << endl;
    
    clear();

    readGlobalData();

    if (!procfs)
      throw runtime_error("unable to read /proc filesystem");

    while((dirEntry=readdir(procfs)))
    {
      // we only care about process ID
      if (!isdigit(*dirEntry->d_name))
	continue;

      //cout << "process " << dirEntry->d_name << endl;

      pid=atoi(dirEntry->d_name);
      ProcessData *pd=new ProcessData(pid);

      if(pd->isValid())
      {
	//cout << "pid=" << pid << " ppid=" << pd->getppid() << endl;
	
        tree[pid]=pd;
	if(pd->getppid()==runsolverPID)
	  roots.push_back(pid);
      }
      else
        delete pd;
    }

    closedir(procfs);

    treeHasAllProcesses=true;

    identifyChildren();

    for(pid_t root: roots)
      readTasksRec(root);
  }
  
  /**
   * update informations on processes which are a child of runsolver.
   *
   * doesn't attempt to identify new children processes
   */
  void updateProcessesData()
  {
    bool allRootsOK=true;
    
    for(pid_t root: roots)
      allRootsOK=allRootsOK && updateProcessesDataRec(root);

    if(!allRootsOK) // lost some roots?
      readProcesses(); // find the new roots
  }

  /**
   *
   * list heavy processes running on the system. must be called right
   * after readProcesses. We only consider processes which are run
   * by another user.
   *
   * threshold is the percentage of CPU (between 0 and 1) above which
   * a process is considered as heavy
   */
  void dumpHeavyProcesses(ostream &s, float threshold)
  {
    uid_t myUid=getuid();

    // we need all processes in the tree. Reread if necessary.
    if (!treeHasAllProcesses)
      readProcesses();

    cout << "heavy processes:\n";

    for(ProcMap::iterator it=tree.begin();it!=tree.end();++it)
    {
      ProcessData *data=(*it).second;

      if (!data || !data->isValid())
      {
	cout << "Ooops ! No data available on process " << (*it).first << endl;
	continue;
      }

      float pcpu=data->percentageCPU(uptime);

      // is this someone else process which uses a significant
      // proportion of the CPU?
      if (data->getUid()!=myUid && pcpu>threshold)
      {
	pid_t pid=(*it).first;

	s << "  %CPU=" << static_cast<int>(pcpu*100) 
	  << " pid=" << pid 
	  << " uid=" << data->getUid() 
	  << " cmd=";

	dumpCmdLine(s,pid);

	s << endl;
      }
    }
  }

  float currentCPUTime()
  {
    float userTime=0,systemTime=0;

    for(pid_t root: roots)
      currentCPUTimeRec(root,userTime,systemTime);

    return userTime+systemTime;
  }

  void currentCPUTime(float &userTime, float &systemTime)
  {
    userTime=0;
    systemTime=0;
    for(pid_t root: roots)
      currentCPUTimeRec(root,userTime,systemTime);
  }

  long currentVSize()
  {
    long sum=0;
    for(pid_t root: roots)
      sum+=currentVSizeRec(root);
    
    return sum;
  }

  long currentMemory()
  {
    long sum=0;
    for(pid_t root: roots)
      sum+=currentMemoryRec(root);
    return sum;
  }

  /**
   * add the pid of each solver task to "list"
   */
  void listProcesses(set<pid_t> &list)
  {
    for(pid_t root: roots)
      listProcessesRec(list,root);
  }

  void dumpProcessTree(ostream &out)
  {
    cout << "\n[startup+" << elapsed << " s]";
    if(treeHasAllProcesses)
      cout << '*';
    cout << endl;

    //cout << "nbRoots=" << roots.size() << endl; // ???
    dumpGlobalData(out);

    for(pid_t root: roots)
      dumpProcessTreeRec(out,root);
  }

  void dumpCPUTimeAndVSize(ostream &out)
  {
    float userTime,systemTime;
    long VSize,mem;
    VSize=currentVSize();
    mem=currentMemory();
    currentCPUTime(userTime,systemTime);
    dumpCPUTimeAndVSize(out,userTime+systemTime,VSize,mem);
  }

  void dumpCPUTimeAndVSize(ostream &out, 
			   float currentCPUTime, long currentVSize, long currentMemory)
  {
    if(completedCPUTime!=0)
      out << "Current cumulated CPU time of completed processes: "
	  << completedCPUTime << " s\n";

    out << "Current children cumulated CPU time: " 
	<< currentCPUTime << " s\n";

    out << "Current children cumulated vsize: " 
	 << currentVSize << " KiB\n";

    out << "Current children cumulated memory: " 
	<< currentMemory << " KiB" << endl;
  }

  /**
   * send a signal to the whole process tree without delay
   */
  void sendSignalNow(int sig)
  {
    for(pid_t root: roots)
      sendSignalNowRec(root,sig);
  }

  void sendSignalBottomUp(int sig)
  {
    for(pid_t root: roots)
      sendSignalBottomUpRec(root,sig);
  }

  void sendSignalBottomUp(pid_t pid, int sig)
  {
    sendSignalBottomUpRec(pid,sig);
  }

protected:
  void readGlobalData()
  {
    ifstream in;
    string key;
    long value;
    
    in.open("/proc/loadavg");
    if(in.good())
      getline(in,loadavgLine);
    in.close();

    in.open("/proc/meminfo");
    int fieldsToRead=4;
    while(fieldsToRead>0)
    {
      in >> key >> value;
      if(in.fail())
	break;

      if(key=="MemTotal:")
      {
	memTotal=value;
	--fieldsToRead;
      }
      else if(key=="MemFree:")
      {
	memFree=value;
	--fieldsToRead;
      }
      else if(key=="SwapTotal:")
      {
	swapTotal=value;
	--fieldsToRead;
      }
      else if(key=="SwapFree:")
      {
	swapFree=value;
	--fieldsToRead;
      }
      
      getline(in,key);
    }
    in.close();

    in.open("/proc/uptime");
    if(in.good())
      in >> uptime;
    in.close();
  }

  void identifyChildren()
  {
    // get links from fathers to children
    for(ProcMap::iterator it=tree.begin();it!=tree.end();++it)
    {
      ProcessData *data=(*it).second;

      if (!data || !data->isValid())
      {
	cout << "Ooops ! No data available on process " << (*it).first << endl;
	continue;
      }

      pid_t parent=data->getppid();

      if (parent==-1)
	continue; // we just have no data on this process

      ProcMap::iterator itParent=tree.find(parent);
      if (itParent!=tree.end())
	(*itParent).second->addChild((*it).first);
#ifdef debug
      else
	if ((*it).first!=1) // init has no father
	{
	  cout << "Ooops! Can't find parent pid " << parent 
	       << " of child pid " <<  (*it).first << endl;
	  dumpProcessTree(cout);
	}
#endif
    }
  }


  /**
   * returns false if the process <root> does not exist
   */
  bool updateProcessesDataRec(pid_t root)
  {
    ProcessData *data=tree[root];

    if (!data) // no data on this process
      return false;

    if (!data->update())
    {
      // this process doesn't exist any more
      tree.erase(root);
      return false;
    }

    for(pid_t childpid: data->getChildren())
      updateProcessesDataRec(childpid);

    return true;
  }

  void dumpGlobalData(ostream &out)
  {
    out << "/proc/loadavg: " << loadavgLine << "\n";
    out << "/proc/meminfo: memFree=" << memFree << "/" << memTotal
	<< " swapFree=" << swapFree << "/" << swapTotal << endl;
  }

  void currentCPUTimeRec(pid_t pid, float &userTime, float &systemTime)
  {
    ProcessData *data=tree[pid];

    if (!data || !data->isValid()) // no data on this process
      return;

    userTime+=data->getOverallUserTime();
    systemTime+=data->getOverallSystemTime();

    for(pid_t childpid: data->getChildren())
      if (tree[childpid] && !tree[childpid]->isTask())
	currentCPUTimeRec(childpid,userTime,systemTime);
  }

  long currentVSizeRec(pid_t pid)
  {
    ProcessData *data=tree[pid];

    if (!data || !data->isValid()) // no data on this process
      return 0;

    long size=data->getVSize();

    for(pid_t childpid: data->getChildren())
      if (tree[childpid] && !tree[childpid]->isTask())
	size+=currentVSizeRec(childpid);

    return size;
  }

  long currentMemoryRec(pid_t pid)
  {
    ProcessData *data=tree[pid];

    if (!data || !data->isValid()) // no data on this process
      return 0;

    long mem=data->getMemory();

    for(pid_t childpid: data->getChildren())
      if (tree[childpid] && !tree[childpid]->isTask())
	mem+=currentMemoryRec(childpid);

    return mem;
  }

  void sendSignalNowRec(pid_t pid, int sig)
  {
    ProcessData *data=tree[pid];

    if (!data || !data->isValid()) // no data on this process
      return;
    
    if (data->getNbChildren()!=0)
    {
      for(pid_t childpid: data->getChildren())
	if (tree[childpid] && !tree[childpid]->isTask())
	  sendSignalNowRec(childpid,sig);
    }

    cout << "??? kill " << pid << " sig " << sig << endl;
    kill(pid,sig);
  }

  void sendSignalBottomUpRec(pid_t pid, int sig)
  {
    ProcessData *data=tree[pid];

    if (!data || !data->isValid()) // no data on this process
      return;

    if (data->getNbChildren()!=0)
    {
      for(pid_t childpid: data->getChildren())
	if (tree[childpid] && !tree[childpid]->isTask())
	  sendSignalBottomUpRec(childpid,sig);

      // give some time to the father to wait for its children
      struct timespec delay={0,020000000}; // 20 ms
      
      // use a loop in case of an interrupt
      while(nanosleep(&delay,&delay)==-1 && errno==EINTR);
    }

    kill(pid,sig);
  }

  void readTasksRec(pid_t pid)
  {
    ProcessData *data=tree[pid];

    if (!data || !data->isValid()) // no data on this process
      return;

    readProcessTasks(pid);

    for(pid_t childpid: data->getChildren())
      if (tree[childpid] && !tree[childpid]->isTask())
	readTasksRec(childpid);
  }

  void readProcessTasks(pid_t pid)
  {
    char processdir[64]; // ???
    DIR *procfs;
    struct dirent *dirEntry;
    pid_t tid;
    ProcessData *data;

    data=tree[pid];

    if (!data || !data->isValid())
      return;

    snprintf(processdir,sizeof(processdir),"/proc/%d/task",pid);

    procfs=opendir(processdir);
    if (!procfs)
    {
      if (errno==ENOENT)
      {
	// process "pid" is probably gone. Don't make a fuss about it
	return;
      }

      cout << "!!! unable to read " << processdir << " filesystem (" 
	   << strerror(errno) << ") !!!" << endl;
      return;
    }

    while((dirEntry=readdir(procfs)))
    {
      // we only care about process ID
      if (!isdigit(*dirEntry->d_name))
	continue;

      tid=atoi(dirEntry->d_name);
      if (tid==pid)
	continue;

      //cout << "task " << dirEntry->d_name 
      //     << " (pid=" << pid << ")" << endl;

      ProcessData *pd=new ProcessData(pid,tid);

      if(pd->isValid())
      {
        tree[tid]=pd;

        // add a link from the father to the task
        data->addChild(tid);
      }
      else
        delete pd;
    }

    closedir(procfs);
  }

  void listProcessesRec(set<pid_t> &list,pid_t pid)
  {
    ProcessData *data=tree[pid];
    
    if (!data || !data->isValid())
      return;

    list.insert(pid);

    for(pid_t childpid: data->getChildren())
      listProcessesRec(list,childpid);
  }

  void dumpProcessTreeRec(ostream &out,pid_t pid)
  {
    ProcessData *data=tree[pid];
    
    if (!data || !data->isValid())
      return;

    out << *data;
    for(pid_t childpid: data->getChildren())
      dumpProcessTreeRec(out,childpid);
  }

  void clone(const ProcessTree &pt)
  {
    treeHasAllProcesses=false; // we only copy the solver processes

    for(pid_t root: pt.roots)
      cloneRec(pt,root);
  }
  
  void cloneRec(const ProcessTree &pt, pid_t pid)
  {
    ProcMap::const_iterator it=pt.tree.find(pid);
    if (it==pt.tree.end())
      return;

    ProcessData *data=(*it).second;
    
    if (!data || !data->isValid())
      return;

    tree[pid]=new ProcessData(*data);

    for(pid_t childpid: data->getChildren())
      cloneRec(pt,childpid);
  }

  void dumpCmdLine(ostream &s, pid_t pid)
  {
    char buffer[128];
    char fileName[64]; // ???
    int fd;

    snprintf(fileName,sizeof(fileName),"/proc/%d/cmdline",pid);
    
    fd=open(fileName,O_RDONLY);

    if(fd>0)
    {
      unsigned int size=0,r;

      while(size<sizeof(buffer) && 
	    (r=read(fd,buffer+size,sizeof(buffer)-size))>0)
	size+=r;

      for(unsigned int i=0;i<size;++i)
	if(buffer[i])
	  s << buffer[i];
	else
	  s << ' ';

      close(fd);
    }
  }

};

// Local Variables:
// mode: C++
// End:
#endif
