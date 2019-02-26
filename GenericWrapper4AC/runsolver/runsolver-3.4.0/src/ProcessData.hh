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



#ifndef _ProcessData_hh_
#define _ProcessData_hh_

#include <vector>
using namespace std;

/**
 * The information we keep on a process
 */
class ProcessData
{
private:
  bool valid; // did we collect meaningful data?

  static const unsigned long int clockTicksPerSecond;

  uid_t uid;
  gid_t gid;

  pid_t pid,tid,ppid,pgrp;
  unsigned long int utime,stime,cutime,cstime;
  unsigned long int starttime;
  unsigned long int vsize;

  unsigned long int rss,swap; // in kB, see /proc/[pid]/status

  vector<pid_t> children;

  char statLine[1024]; // ???
  char statmLine[1024]; // ???

  vector<unsigned short int> allocatedCores;

  bool selected; // a flag to select/unselect processes

  inline void init()
  {
    valid=false;
    pid=-1;
    tid=-1;
    ppid=-1;
    utime=stime=cutime=cstime=0;
    vsize=0;
  }

public:
  ProcessData() {init();}

  ProcessData(pid_t pid, pid_t tid=0)
  {
    init();
    read(pid,tid);
  }

  ProcessData(const ProcessData &pd)
  {
    valid=false;

    uid=pd.uid;
    gid=pd.gid;

    pid=pd.pid;
    tid=pd.tid;
    ppid=pd.ppid;
    pgrp=pd.pgrp;
    utime=pd.utime;
    stime=pd.stime;
    cutime=pd.cutime;
    cstime=pd.cstime;
    starttime=pd.starttime;
    vsize=pd.vsize;

    statLine[0]=0;
    statmLine[0]=0;

    for(size_t i=0;i<pd.children.size();++i)
      children.push_back(pd.children[i]);
  }

  ~ProcessData()
  {
  }

  /*
   * return false iff the process doesn't exit any more or we failed
   * to read the data
   */
  bool read(pid_t pid, pid_t tid=0)
  {
    char fileName[64]; // ???
    FILE *file;
    int nbFields;

    this->pid=pid;
    this->tid=tid;

    valid=false;

    if (tid)
      snprintf(fileName,sizeof(fileName),"/proc/%d/task/%d/stat",pid,tid);
    else
      snprintf(fileName,sizeof(fileName),"/proc/%d/stat",pid);
    
    if ((file=fopen(fileName,"r"))!=NULL)
    {
      struct stat info;

      fstat(fileno(file),&info);

      uid=info.st_uid;
      gid=info.st_gid;

      if (fgets(statLine,sizeof(statLine),file)==NULL)
      {
#ifdef debug
	perror("failed to read stat file");
#endif

	strcpy(statLine,"-- couldn't read stat file --");
	fclose(file);

	return valid=false;
      }
      
      fclose(file);
    }
    else
    {
#ifdef debug
      perror("failed to open stat file");
#endif

      strcpy(statLine,"-- couldn't open stat file --");

      return valid=false;
    }

    nbFields=sscanf(statLine,
#if WSIZE==32
		    "%*d "
		    "%*s "
		    "%*c "
		    "%d %d %*d %*d %*d "
		    "%*u %*u %*u %*u %*u " // lu lu lu lu lu
		    "%Lu %Lu %Lu %Lu "  /* utime stime cutime cstime */
		    "%*d %*d " // ld ld
		    "%*d "
		    "%*d " // ld
		    "%Lu "  /* start_time */
		    "%lu ",
#else
		    "%*d "
		    "%*s "
		    "%*c "
		    "%d %d %*d %*d %*d "
		    "%*u %*u %*u %*u %*u " // lu lu lu lu lu
		    "%lu %lu %lu %lu "  /* utime stime cutime cstime */
		    "%*d %*d " // ld ld
		    "%*d "
		    "%*d " // ld
		    "%lu "  /* start_time */
		    "%lu ",
#endif
		    &ppid,&pgrp,
		    &utime, &stime, &cutime, &cstime,
		    &starttime,
		    &vsize
		    );

    if(nbFields!=8)
    {
#ifdef debug
      cout << "FAILED TO READ EACH FIELD (got " << nbFields << " fields)\n";
#endif

      return valid=false;      
    }
    
    if (!tid)
    {
      snprintf(fileName,sizeof(fileName),"/proc/%d/statm",pid);

      if ((file=fopen(fileName,"r"))!=NULL)
      {
	if (fgets(statmLine,sizeof(statmLine),file)==NULL)
	{
#ifdef debug
	  perror("failed to read statm file");
#endif

	  strcpy(statmLine,"-- couldn't read statm file --");
	  fclose(file);

	  return valid=false;
	}

	fclose(file);
      }
      else
      {
#ifdef debug
	perror("failed to open statm file");
#endif

	strcpy(statmLine,"-- couldn't open statm file --");
      }
    }

    if (!tid)
    {
      // read /proc/%d/status
      snprintf(fileName,sizeof(fileName),"/proc/%d/status",pid);
      ifstream in(fileName);
      string tmp;
      int nbFieldsToRead=2;
      
      while(in.good() && nbFieldsToRead>0)
      {
        in >> tmp;
        if(tmp=="VmRSS:")
        {
          in >> rss;
          --nbFieldsToRead;
        }
        else
          if(tmp=="VmSwap:")
          {
            in >> swap;
            --nbFieldsToRead;
          }
        getline(in,tmp);
      }
      
      if(nbFieldsToRead!=0)
      {
#ifdef debug
        cout << "FAILED TO READ EACH FIELD in /proc/" << pid << "/status\n";
#endif

        return valid=false;      
      }
    }

    getAllocatedCores();

    return valid=true;
  }

  /**
   * update data on this process
   *
   * return false iff the process doesn't exit any more
   */ 
  bool update()
  {
    return read(pid,tid);
  }

  /**
   * do we have valid data?
   */
  bool isValid() const {return valid;}
  
  /**
   * return the % of CPU used by this process (and its children when
   * withChildren is true). The result is between 0 and 1
   */
  float percentageCPU(float uptime, bool withChildren=false) const
  {
    float cputime=stime+utime;

    if (withChildren)
      cputime+=cstime+cutime;

    cputime/=clockTicksPerSecond;

    float wctime=uptime-static_cast<float>(starttime)/clockTicksPerSecond;

    if (wctime==0)
      return 0;
    else
      return cputime/wctime;
  }


  bool isTask() const
  {
    return tid!=0;
  }


  const vector<pid_t> &getChildren() const
  {
    return children;
  }
  
  void addChild(pid_t child)
  {
    children.push_back(child);
  }

  int getNbChildren() const
  {
    return children.size();
  }

  pid_t getPIDChild(int i) const
  {
    return children[i];
  }

  pid_t getppid() const
  {
    return ppid;
  }

  pid_t getProcessGroupId() const
  {
    return pgrp;
  }

  uid_t getUid() const
  {
    return uid;
  }

  void select()
  {
    selected=true;
  }

  void unselect()
  {
    selected=false;
  }

  bool isSelected() const
  {
    return selected;
  }

  float getOverallCPUTime() const
  {
    // note that cstime and cutime (child system and user time) are
    // only updated by the wait call in the parent (this is to say,
    // only once the child has terminated). Therefore, we can't rely
    // on these variables to limit the global cpu use of a process
    // and its children

    return (stime+(float)utime
	    +cstime+(float)cutime
	    )/clockTicksPerSecond;
  }

  float getOverallUserTime() const
  {
    // note that cstime and cutime (child system and user time) are
    // only updated by the wait call in the parent (this is to say,
    // only once the child has terminated). Therefore, we can't rely
    // on these variables to limit the global cpu use of a process
    // and its children

    return (utime+(float)cutime)/clockTicksPerSecond;
  }

  float getOverallSystemTime() const
  {
    // note that cstime and cutime (child system and user time) are
    // only updated by the wait call in the parent (this is to say,
    // only once the child has terminated). Therefore, we can't rely
    // on these variables to limit the global cpu use of a process
    // and its children

    return (stime+(float)cstime)/clockTicksPerSecond;
  }

  /**
   * return the current vsize in kB
   */
  long getVSize() const
  {
    return vsize/1024;
  }

  /**
   * return the current memory consumption in kB
   */
  long getMemory() const
  {
    return rss+swap;
  }

  /**
   * get the list of cores allocated to this process
   */
  void getAllocatedCores()
  {
    //return; // ???
    
    cpu_set_t mask;
    allocatedCores.clear();
  
    sched_getaffinity(pid,sizeof(cpu_set_t),&mask);

#warning "don't watse time converting to a vector, just keep the cpu_set_t"
    for(int i=0;i<CPU_SETSIZE;++i)
      if(CPU_ISSET(i,&mask))
	allocatedCores.push_back(i);
  }

  /**
   * print the list of cores allocated to this process
   * (getAllocatedCores must be called first).
   */
  void printAllocatedCores(ostream &s) const
  {
    //return; // ???
    size_t end;

    for(size_t beg=0;beg<allocatedCores.size();beg=end)
    {
      end=beg+1;
      while(end<allocatedCores.size() && 
	    allocatedCores[end]==allocatedCores[end-1]+1)
	++end;

      if(beg!=0)
	cout << ',';

      if(end==beg+1)
	s << allocatedCores[beg];
      else
	s << allocatedCores[beg] << '-' << allocatedCores[end-1];
    }
  }


  friend ostream &operator <<(ostream &out, const ProcessData &data);
};

ostream &operator <<(ostream &out, const ProcessData &data)
{
  out << "[pid=" << data.pid ;
  if (data.tid)
    out << "/tid=" << data.tid;

  out << "] ppid=" << data.ppid 
      << " vsize=" << data.vsize/1024 
      << " memory=" << data.getMemory() 
      << " CPUtime=" << data.getOverallCPUTime()
      << " cores=";

  data.printAllocatedCores(out);

  out << endl;

  if (data.tid)
    out << "/proc/" << data.pid << "/task/" 
	<< data.tid << "/stat : " << data.statLine;
  else
    out << "/proc/" << data.pid << "/stat : " << data.statLine;

  if (!data.tid)
    out << "/proc/" << data.pid << "/statm: " << data.statmLine;

  out << flush;
  return out;
}

const unsigned long int ProcessData::clockTicksPerSecond=sysconf(_SC_CLK_TCK);

// Local Variables:
// mode: C++
// End:
#endif
