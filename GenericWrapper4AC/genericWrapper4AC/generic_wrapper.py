#!/usr/bin/env python3.5
# encoding: utf-8

'''
genericWrapper -- template for an AClib target algorithm wrapper
abstract methods for generation of callstring and parsing of solver output 
@author:     Marius Lindauer  
@copyright:  2018 ML4AAD. All rights reserved.
@license:    BSD
'''

import signal
import os
import sys
import time
import random
import shutil
import json
import logging
import re
import traceback
import tempfile

from subprocess import Popen, PIPE
from tempfile import NamedTemporaryFile

from genericWrapper4AC.argparser.parse import parse, get_parser, get_extended_parser

__version__ = "2.0.0"

if sys.version_info < (3, 5):
    raise ValueError("GenericWrapper requires Python 3.5 or newer.")


def signalHandler(signum, frame):
    sys.exit(2)


class AbstractWrapper(object):
    '''
        abstract algorithm wrapper
    '''

    def __init__(self):
        '''
            Constructor
        '''

        root = logging.getLogger()
        ch = logging.StreamHandler(sys.stdout)
        formatter = logging.Formatter('[%(name)s][%(levelname)s] %(message)s')
        ch.setFormatter(formatter)
        root.handlers = [ch]
        self.logger = logging.getLogger("GenericWrapper")

        self.RESULT_MAPPING = {"SAT": "SUCCESS",
                               "UNSAT": "SUCCESS"}
        self._watcher_file = None
        self._solver_file = None

        self._exit_code = None

        self._crashed_if_non_zero_status = True
        self._use_tmpdir = False

        self._subprocesses = []

        self._DEBUG = True
        if self._DEBUG:
            self.logger.setLevel(logging.DEBUG)

        self.data = None

        self._DELAY2KILL = 1

        self.parser = get_parser()
        self.args = None

    def main(self, exit:bool=True):
        '''
            main method of the generic wrapper
            1. parses cmd arguments; 
            2. calls target algorithm wrapped by runsolver
            3. parses outputs
            4. terminates
            
            Arguments
            ---------
            exit: bool
                exit with sys.exit at the end 
            
        '''

        signal.signal(signal.SIGTERM, signalHandler)
        signal.signal(signal.SIGQUIT, signalHandler)
        signal.signal(signal.SIGINT, signalHandler)

        # returns genericWrapper4AC.data.data.Data
        self.data, self.args = parse(cmd_arguments=sys.argv, parser=self.parser)
        self.data.tmp_dir, algo_temp_dir = self.set_tmpdir(tmp_dir=self.data.tmp_dir)

        # because of legacy reasons,
        # we still pass a dictionary to get_command_line_args
        runargs = {
            "instance": self.data.instance,
            "specifics": self.data.specifics,
            "cutoff": self.data.cutoff,
            "runlength": self.data.runlength,
            "seed": self.data.seed,
            "tmp": algo_temp_dir.name
        }

        try:
            target_cmd = self.get_command_line_args(
                runargs=runargs, config=self.data.config)

            start_time = time.time()
            self.call_target(target_cmd)
            self.data.time = time.time() - start_time
            self.logger.debug("Measured wallclock time: %f" %
                              (self.data.time))
            self.read_runsolver_output()
            self.logger.debug("Measured time by runsolver: %f" %
                              (self.data.time))

            resultMap = self.process_results(
                self._solver_file, {"exit_code": self.data.exit_code, "instance": self.data.instance})

            if 'status' in resultMap:
                self.data.status = self.RESULT_MAPPING.get(
                    resultMap['status'], resultMap['status'])
            if 'runtime' in resultMap:
                self.data.time = resultMap['runtime']
            if 'quality' in resultMap:
                self.data.cost = resultMap['quality']
            if 'cost' in resultMap: # overrides quality
                self.data.cost = resultMap['cost']
            elif 'misc' in resultMap:
                self.data.additional += "; " + resultMap['misc']

            # if quality is still set to 2**32 - 1 and we use the new format,
            # overwrite quality with runtime, since irace only looks at the
            # cost field
            if self.data.new_format and self.data.cost == 2**32 - 1:
                self.data.cost = self.data.time

            sys.exit()

        except (KeyboardInterrupt, SystemExit):
            self.cleanup()
            self.print_result_string()
            if exit:
                if self.data.exit_code:
                    sys.exit(self.data.exit_code)
                else:
                    sys.exit(0)

    def set_tmpdir(self, tmp_dir):
        '''
            set temporary directory for log files;
            if not set, try to use $TMPDIR,
            otherwise use "."
        '''

        if tmp_dir is None:
            if "TMPDIR" in os.environ:
                self._use_tmpdir = True
                tmp_dir = os.environ["TMPDIR"]
            else:
                tmp_dir = "."
        else:
            self._use_tmpdir = True
            tmp_dir = tmp_dir

        if not os.path.isdir(tmp_dir):
            self.data.status = "ABORT"
            self.data.additional = "temp directory is missing - should have been at %s." % (args.tmp_dir)
            self.data.exit_code = 1
            sys.exit(1)

        # create tmp dir for target algorithm files
        algo_tmp_dir = tempfile.TemporaryDirectory(dir=tmp_dir)

        return tmp_dir, algo_tmp_dir

    def call_target(self, target_cmd: str):
        '''
            extends the target algorithm command line call with the runsolver
            and executes it

            Arguments
            --------
            target_cmd: str
                target cmd (from get_command_line_args)
                
        '''
        random_id = random.randint(0, 1000000)
        self._watcher_file = NamedTemporaryFile(
            suffix=".log", prefix="watcher-%d-" % (random_id), dir=self.data.tmp_dir, delete=False)
        self._solver_file = NamedTemporaryFile(
            suffix=".log", prefix="solver-%d-" % (random_id), dir=self.data.tmp_dir, delete=False)

        runsolver_cmd = [self.data.runsolver, "-M", self.data.mem_limit, "-C", self.data.cutoff,
                         "-w", "\"%s\"" % (self._watcher_file.name),
                         "-o",  "\"%s\"" % (self._solver_file.name)]

        runsolver_cmd = " ".join(map(str, runsolver_cmd)) + " " + target_cmd
        # for debugging
        self.logger.debug("Calling runsolver. Command-line:")
        self.logger.debug(runsolver_cmd)

        # run
        try:
            io = Popen(runsolver_cmd, shell=True,
                       preexec_fn=os.setpgrp, universal_newlines=True)
            self._subprocesses.append(io)
            io.wait()
            self._subprocesses.remove(io)
            if io.stdout:
                io.stdout.flush()
        except OSError:
            self.data.status = "ABORT"
            self.data.additional = "execution failed: %s" % (
                " ".join(map(str, runsolver_cmd)))
            self._exit_code = 1
            sys.exit(1)
        self._solver_file.seek(0)
        self._watcher_file.seek(0)

    def float_regex(self):
        return '[+-]?\d+(?:\.\d+)?(?:[eE][+-]\d+)?'

    def read_runsolver_output(self):
        '''
            reads self._watcher_file, 
            extracts runtime
            and returns if memout or timeout found
        '''
        self.logger.debug("Reading runsolver output from %s" %
                          (self._watcher_file.name))
        try:
            data = str(self._watcher_file.read().decode("utf8"))
        except:
            # due to the common, rare runsolver bug,
            # the watcher file can be corrupted and can failed to be read
            self.data.exit_code = 0
            self.logger.warn(
                "Failed to read runsolver's watcher file---trust own wc-time measurment")
            return

        if (re.search('runsolver_max_cpu_time_exceeded', data) or re.search('Maximum CPU time exceeded', data)):
            self.data.status = "TIMEOUT"

        if (re.search('runsolver_max_memory_limit_exceeded', data) or re.search('Maximum VSize exceeded', data)):
            self.data.status = "TIMEOUT"
            self.data.additional += " memory limit was exceeded"

        cpu_pattern1 = re.compile('^runsolver_cputime: (%s)' % (
            self.float_regex()), re.MULTILINE)
        cpu_match1 = re.search(cpu_pattern1, data)

        cpu_pattern2 = re.compile('^CPU time \\(s\\): (%s)' % (
            self.float_regex()), re.MULTILINE)
        cpu_match2 = re.search(cpu_pattern2, data)

        if (cpu_match1):
            self.data.time = float(cpu_match1.group(1))
        if (cpu_match2):
            self.data.time = float(cpu_match2.group(1))

        exitcode_pattern = re.compile('Child status: ([0-9]+)')
        exitcode_match = re.search(exitcode_pattern, data)

        if (exitcode_match):
            self.data.exit_code = int(exitcode_match.group(1))

    def print_result_string(self):
        '''
            print result in old ParamILS format
            and also in new AClib format 
              if it new call string format was used
        '''

        # ensure a minimal runtime of 0.0005
        self.data.time = max(0.0005, self.data.time)

        if self.data.new_format:
            aclib2_out_dict = {"status": str(self.data.status),
                               "cost": float(self.data.cost),
                               "runtime": float(self.data.time),
                               "misc": str(self.data.additional)
                               }
            print("Result of this algorithm run: %s" %
                  (json.dumps(aclib2_out_dict)))

        sys.stdout.write("Result for ParamILS: %s, %.4f, %s, %s, %s" % (
            self.data.status, self.data.time,
            str(self.data.runlength),
            str(self.data.cost),
            str(self.data.seed)))

        if self.data.additional != "":
            sys.stdout.write(", %s" % (self.data.additional))
        sys.stdout.write("\n")
        sys.stdout.flush()

    def cleanup(self):
        '''
            cleanup if error occurred or external signal handled
        '''
        if (len(self._subprocesses) > 0):
            print("killing the target run!")
            try:
                for sub in self._subprocesses:
                    # sub.terminate()
                    Popen(["pkill", "-TERM", "-P", str(sub.pid)],
                          universal_newlines=True)
                    self.logger.debug("Wait %d seconds ..." %
                                      (self._DELAY2KILL))
                    time.sleep(self._DELAY2KILL)
                    if sub.returncode is None:  # still running
                        sub.kill()

                self.logger.debug(
                    "done... If anything in the subprocess tree fork'd a new process group, we may not have caught everything...")
                self.data.additional += "; forced to exit by signal or keyboard interrupt."
                self.data.time = self.data.cutoff
            except (OSError, KeyboardInterrupt, SystemExit):
                self.data.additional += "; forced to exit by multiple signals/interrupts."
                self.data.time = self.data.cutoff

        if (self.data.status is "ABORT" or self.data.status is "CRASHED"):
            if self.data.exit_code:
                self.data.additional += '; Problem with run. Exit code was %d.' % (
                    self.data.exit_code)
            else:
                self.data.additional += '; Problem with run. Exit code was N/A.'

            if (self._watcher_file and self._solver_file):
                self.data.additional += '; Preserving runsolver output at %s - preserving target algorithm output at %s' % (
                    self._watcher_file.name or "<none>", self._solver_file.name or "<none>")

        try:
            if self._watcher_file:
                self._watcher_file.close()
            if self._solver_file:
                self._solver_file.close()

            if self.data.status not in ["ABORT", "CRASHED"]:
                os.remove(self._watcher_file.name)
                os.remove(self._solver_file.name)
            elif self._use_tmpdir:
                shutil.copy(self._watcher_file.name, ".")
                shutil.copy(self._solver_file.name, ".")
                os.remove(self._watcher_file.name)
                os.remove(self._solver_file.name)

        except (OSError, KeyboardInterrupt, SystemExit):
            self.data.additional = "problems removing temporary cd files during cleanup."
        except AttributeError:
            pass  # in internal mode, these files are not generated

    def get_command_line_args(self, runargs, config):
        '''
        Returns the command call list containing arguments to execute the implementing subclass' solver.
        The default implementation delegates to get_command_line_args_ext. If this is not implemented, a
        NotImplementedError will be raised.

        Args:
            runargs: a map of any non-configuration arguments required for the execution of the solver.
            config: a mapping from parameter name (with prefix) to parameter value.
        Returns:
            A command call list to execute a target algorithm.
        '''
        raise NotImplementedError()

    def process_results(self, filepointer, out_args):
        '''
        Parse a results file to extract the run's status (SUCCESS/CRASHED/etc) and other optional results.

        Args:
            filepointer: a pointer to the file containing the solver execution standard out.
            exit_code : exit code of target algorithm
        Returns:
            A map containing the standard AClib run results. The current standard result map as of AClib 2.06 is:
            {
                "status" : <"SAT"/"UNSAT"/"TIMEOUT"/"CRASHED"/"ABORT">,
                "runtime" : <runtime of target algrithm>,
                "quality" : <a domain specific measure of the quality of the solution [optional]>,
                "misc" : <a (comma-less) string that will be associated with the run [optional]>
            }
            ATTENTION: The return values will overwrite the measured results of the runsolver (if runsolver was used). 
        '''
        raise NotImplementedError()
