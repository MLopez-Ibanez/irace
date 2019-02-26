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



#ifndef _ExecutionSummary_hh_
#define _ExecutionSummary_hh_

struct ExecutionSummary
{
  int byteOrder; // used to detect byte ordering if needed (=0x01020304)
  int version; // version number of this format
  char runsolverVersion[16]; // version of runsolver
  float wcTime; // wall-clock time in seconds
  float cpuTime; // CPU time in seconds (userTime+systemTime)
  float userTime; // user CPU time in seconds
  float systemTime; // system CPU time in seconds
  float cpuUsage; // cpuTime/wcTime (in percent)
  long maxVM; // maximum virtual memory used in KiB
  long maxMem; // maximum memory used in KiB
  bool timeOut; // did the solver exceed the time limit?
  bool memOut; // did the solver exceed the memory limit?

  
  ExecutionSummary()
  {
    set();
  }

  void set()
  {
    version=3;
    byteOrder=0x01020304;
  }

  void reset()
  {
    byteOrder=0;
  }
  
  bool ok() const
  {
    return byteOrder==0x01020304 && version==3;
  }
};

#endif
