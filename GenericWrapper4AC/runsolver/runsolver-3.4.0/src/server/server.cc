/*
 https://lwn.net/Articles/230975/ saved as ELC: How much memory are applications really using? [LWN.net].html
http://eqware.net/Articles/CapturingProcessMemoryUsageUnderLinux/index.html saved as Capturing Process Memory Usage Under Linux.html
https://stackoverflow.com/questions/561245/virtual-memory-usage-from-java-under-linux-too-much-memory-used/561450#561450 saved as Virtual Memory Usage from Java under Linux, too much memory used - Stack Overflow.html
https://www.selenic.com/smem/
pmap
 */
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <iostream>
#include <fstream>
#include <sstream>
#include <cctype>
#include <sys/utsname.h>
#include <boost/algorithm/string.hpp>
#include <set>
using namespace std;

/**
 * /proc/xx/maps [beg,end(
 *
 */

class MemoryUsage
{
protected:
  //enum PageMapType

  uint64_t unmappedPages,ramPages,swapPages,sharedPages;
  
public:
  MemoryUsage()
  {
    struct utsname unam;

    uname(&unam);
    
    vector<string> ver;
    boost::split(ver,unam.release, boost::is_any_of("."));

    cout << "rel=" << ver[0] << "." << ver[1] << endl;
  }
  
  bool readMaps(const vector<pid_t> &pidList)
  {
#warning must add support for hugepages
    uint64_t lastUnmappedPages,lastRamPages,lastSwapPages,lastSharedPages;
    uint64_t pagesize=sysconf(_SC_PAGESIZE);

    set<uint64_t> pagesList;
    pair<set<uint64_t>::iterator,bool> ret;
    
    unmappedPages=ramPages=swapPages=sharedPages=0;
    
    for(pid_t pid: pidList)
    {
      ostringstream mapsfname,pagemapfname;
      mapsfname << "/proc/" << pid << "/maps";
      pagemapfname << "/proc/" << pid << "/pagemap";
  
      ifstream maps(mapsfname.str());
      if(!maps.good())
        return false;
      
      int pagemap=open(pagemapfname.str().c_str(),O_RDONLY);
      if(pagemap<0)
        cout << "access to " << pagemapfname.str() << " denied\n";

      unsigned long beg,end,inode;
      string perm,path,tmp;
      char c;
      
      while(true)
      {
        path.clear();
        maps >> hex >> beg >> c >> end >> dec >> perm >> tmp >> tmp >> inode;
        while((c=maps.peek()) && isspace(c) && c!='\n')
          maps.get();
        getline(maps,path);
        if(maps.fail() && path.empty())
          break;


        cout << (end-beg)/pagesize << "\t" << hex << beg << "-" << end << dec << "  " << perm << " path=" << path << endl;

        lastUnmappedPages=unmappedPages;
        lastRamPages=ramPages;
        lastSwapPages=swapPages;
        lastSharedPages=sharedPages;
 
        if(pagemap>=0)
        {
          uint64_t desc;
	  off_t pos=sizeof(desc)*(beg/pagesize);
          off_t res=lseek(pagemap,pos,SEEK_SET);
          if(res<0 || res!=pos)
            cout << "lseek failed:" << strerror(errno) << endl;

          //cout << "res=" << hex << res << dec << endl;
          
          for(unsigned long p=beg;p<end;p+=pagesize)
          {
#warning EOF sur les derniers accès
            int result=read(pagemap,&desc,sizeof(desc));
            if(result==0)
            {
              ramPages+=(end-p)/pagesize; // ???
              break;
            }
            
            if(result!=sizeof(desc))
              cout << "error reading page description (res=" << result << "): " << strerror(errno) << "\n";
            
            uint64_t pfn=desc&((1UL<<55)-1); // page frame number
            bool present=desc&(1UL<<63);
            bool swap=desc&(1UL<<62);
            bool map=desc&(1UL<<61);
            bool excl=desc&(1UL<<56);
            bool softDirty=desc&(1UL<<55);

            cout << hex << p << " -> "
                 << (present?"Present ":"")
                 << (swap?"Swp ":"")
                 << (map?"Map ":"")
                 << (excl?"Excl ":"")
                 << (softDirty?"SoftD ":"")
                 << " " 
                 << ((desc>>57)&0xF)
                 << " " << (desc&((1UL<<55)-1)) << dec << endl;

            if(pfn)
	    {
	      ret=pagesList.insert(pfn);
	      if(!ret.second)
		++sharedPages;
	      else
		if(swap)
		  ++swapPages;
		else
		  ++ramPages;
	    }
	    else
              ++unmappedPages;
          }
        } //if(pagemap>=0

        cout << "=> total=" << (end-beg)/1024 << "KB"
             << " unmapped=" << (unmappedPages-lastUnmappedPages)*(pagesize/1024) << "KB"
             << " ram=" << (ramPages-lastRamPages)*(pagesize/1024) << "KB"
             << " swap=" << (swapPages-lastSwapPages)*(pagesize/1024) << "KB"
             << " shared=" << (sharedPages-lastSharedPages)*(pagesize/1024) << "KB" << endl;


      }//while(true)

      close(pagemap);
    }//for(pid_t pid...


    cout << "pagesList (size=" << pagesList.size() << "):";
    //for(uint64_t p: pagesList)
    //  cout << " " << hex << p << dec;
    cout << endl;
    
    cout << "unmapped=" << unmappedPages*(pagesize/1024) << "KB\n"
	 << "ram=" << ramPages*(pagesize/1024) << "KB\n"
	 << "swap=" << swapPages*(pagesize/1024) << "KB\n"
	 << "shared=" << sharedPages*(pagesize/1024) << "KB\n"
	 << "VSS=all=" << (ramPages+unmappedPages+swapPages+sharedPages)*(pagesize/1024) << "KB\n"
	 << "RSS=ram+shared=" << (ramPages+sharedPages)*(pagesize/1024) << "KB\n";
      
    return true;
  }
};

int main()
{
  MemoryUsage mu;

  
  mu.readMaps({getpid()});

  sleep(1);
  cout << "pid=" << getpid() << endl;
  ostringstream cmd;

  for(const char *fname: {"status","stat","statm","smaps","maps"})
  {
    cmd.str("");
    cmd << "/bin/cat /proc/" << getpid() << "/" << fname;
    cout << "\n\n#######################################################################\n"
         << cmd.str() << endl;
    system(cmd.str().c_str());
  }
  //sleep(30);

  /*
    ./server>/tmp/res
    awk '/^Rss:/ {s=s+$2} END {print s}' /tmp/res
  
    status ne semble pas donner la bonne valeur de RSS (incohérent avec la somme des RSS de smaps)

    il y a quelques petites différences avec les valeurs données par smaps
   */
}
