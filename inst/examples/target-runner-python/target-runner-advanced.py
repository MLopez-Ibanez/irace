#!/usr/bin/env python
##############################################################################
#                                                                            #
# With this target-runner, you can:                                          #
#                                                                            #
# - set environment variables                                                #
# - run your jobs read the standard output / standard err, and check the     #
#   return code                                                              #
# - you can choose between 5 different ways of running your job and reading  #
#   the standard out (you might have to try several ones until you find one  #
#   that is robust enough for your experiments); among the 5 different ways  #
#   there are various combinations of spawining subprocesses, using pipes or #
#   temporary files, and different methods to set a timeout after which the  #
#   job you are running is killed                                            #
# - you can choose how many times a job should be run before giving up (if   #
#   you have some heisenbugs, irace will not crash! :) but it will give the  #
#   configuration further chances)                                           #
# - if the job fails for several times in a row, it writes in your execution #
#   directory a c${configuration}.stdout and stderr as the normal            #
#   target-runner                                                            #
# - set a debug level, and have a detailed output to track what happens (one #
#   file per target-runner, use sparely)                                     #
#                                                                            #
##############################################################################

import os
import sys
import time
import socket
import logging
import tempfile
import subprocess
import threading
import re

# ---------------------------- DO NOT CHANGE HERE ----------------------------
#                     (unless you know what you are doing)
# ---------------------- Search for CHANGE BELOW! ----------------------------
class Runner(object):

    def __init__(self, executable, candidate, instanceid, seed, parameters,
                 parse_output, max_tests, maximize = False, log_level = logging.ERROR):
        self.executable = os.path.expanduser(executable)
        self.instanceid = instanceid
        self.seed = seed
        self.parse_output = parse_output
        self.candidate = candidate
        self.parameters = parameters
        self.max_tests = max_tests
        self.filename_prefix = 'c' + str(candidate) + '-' + str(instanceid) + '-' + str(seed) 

        # default exec function
        self.execute = self.execute1

        self.maximize = maximize
        
        # logging (by default only errors are logged)
        filename = self.filename_prefix + '.' + socket.gethostname() + '_' + str(os.getpid())
        self.logger = logging.getLogger('target-runner')
        try :
            hdlr = logging.FileHandler(filename, delay=True)
        except OSError as e:
            print("Current working dir : %s" % os.getenv('PWD'))
            raise
        formatter = logging.Formatter('%(asctime)s %(levelname)s %(message)s')
        hdlr.setFormatter(formatter)
        self.logger.addHandler(hdlr)
        self.logger.setLevel(log_level)
    
    # changes the way the child process is executed
    def exec_mode(self, mode, max_time = 3600):
        self.execute = mode
        # self.max_time is used only in execute_timeout functions
        self.max_time = max_time


    # executes a process, waits until it finishes
    # returns the exit code, as well as the stdout and stderr
    # this version uses no intermediate file, it reads directly from pipes, it
    # is less robust though, if the child gets oddly killed it seems to hang
    # even if it should not
    def execute1(self, command):
        self.logger.debug('job started')
        self.logger.debug('PATH=' + os.environ['PATH'])
        self.logger.debug(command)
        process = subprocess.Popen(command, stdout=subprocess.PIPE,
                                            stderr=subprocess.PIPE,
                                            env=os.environ)
        (out, err) = process.communicate()
        status = process.wait()
        self.logger.debug(out)
        self.logger.debug(err)
        self.logger.debug('job finished')
        return (status, out, err)


    # executes a process, waits until it finishes
    # returns the exit code, as well as the stdout and stderr
    def execute2(self, command):
        fout = tempfile.NamedTemporaryFile(delete=False)
        fout.close()
        ferr = tempfile.NamedTemporaryFile(delete=False)
        ferr.close()
        str_cmd = ' '.join(command) + ' >' + fout.name + ' 2>' + ferr.name
        try:
            self.logger.debug('job started')
            self.logger.debug('PATH=' + os.environ['PATH'])
            self.logger.debug(str_cmd)
            process = subprocess.check_call(str_cmd, shell=True)
            status = 0
        except subprocess.CalledProcessError as e:
            status = e.returncode
            self.logger.warning('Exit status: ' + str(status))
        out = self.read(fout.name)
        err = self.read(ferr.name)
        os.unlink(fout.name)
        os.unlink(ferr.name)
        self.logger.debug(out)
        self.logger.debug(err)
        self.logger.debug('job finished')
        return (status, out, err)


    # executes a process, waits until it finishes
    # returns the exit code, as well as the stdout and stderr
    # if the process does not finish before the timeout it gets killed
    def execute_timeout1(self, command):
        fout = tempfile.SpooledTemporaryFile()
        ferr = tempfile.SpooledTemporaryFile()
        start_time = time.time()
        self.logger.debug('job started')
        self.logger.debug('PATH=' + os.environ['PATH'])
        self.logger.debug(command)
        process = subprocess.Popen(command, stdout=fout, stderr=ferr,
                                   env=os.environ)
        elapsed_time = time.time() - start_time
        while process.poll() is None and elapsed_time <= self.max_time:
            time.sleep(1)
            elapsed_time = time.time() - start_time
            self.logger.debug('elapsed time: ' + str(elapsed_time))
        if elapsed_time > self.max_time:
            process.kill()
            self.logger.warning('job killed, ' + str(self.max_time) + \
                                ' seconds of wall-clock time elapsed')
            status = 1
        status = process.poll()
        fout.seek(0)
        ferr.seek(0)
        out = str(fout.read())
        err = str(ferr.read())
        fout.close()
        ferr.close()
        self.logger.debug(out)
        self.logger.debug(err)
        self.logger.debug('job finished')
        return (status, out, err)


    # executes a process, waits until it finishes
    # returns the exit code, as well as the stdout and stderr
    # this version is like execute2 but requires python3.3 to set the timeouts
    # for the Popen.communicate() and Popen.wait()
    # if the process does not finish before the timeout it gets killed
    def execute_timeout2(self, command):
        self.logger.debug('job started')
        self.logger.debug('PATH=' + os.environ['PATH'])
        self.logger.debug(command)
        process = subprocess.Popen(command, stdout=subprocess.PIPE,
                                            stderr=subprocess.PIPE,
                                            env=os.environ)
        try:
            (out, err) = process.communicate(timeout=self.max_time)
            status = process.wait(timeout=self.max_time)
        except subprocess.TimeoutExpired:
            process.kill()
            (out, err) = process.communicate()
            status = 1
            self.logger.warning('ob killed, ' + str(self.max_time) + \
                                ' seconds of CPU time elapsed')
        self.logger.debug(out)
        self.logger.debug(err)
        self.logger.debug('job finished')
        return (status, out, err)


    # executes a process, waits until it finishes (most robust one)
    # returns the exit code, as well as the stdout and stderr
    def execute_threaded_timeout(self, command):

        class ThreadedJob(threading.Thread):

            def __init__(self, command, logger):
                threading.Thread.__init__(self)
                self.shell_command = ' '.join(command)
                self.process = None
                self.out = None
                self.err = None
                self.logger = logger

            def run(self):
                self.logger.debug('job started')
                self.logger.debug('PATH=' + os.environ['PATH'])
                self.logger.debug(self.shell_command)
                self.process = subprocess.Popen(self.shell_command,
                                                stdout=subprocess.PIPE,
                                                stderr=subprocess.PIPE,
                                                shell=True,
                                                env=os.environ)
                self.out, self.err = self.process.communicate()
                self.logger.debug(self.out)
                self.logger.debug(self.err)
                self.logger.debug('job finished')

        self.logger.debug('creating thread')
        thread = ThreadedJob(command, self.logger)
        # make daemon so that when the main program exits all (eventually
        # hanging) threads are killed
        thread.setDaemon(True)
        thread.start()
        thread.join(timeout=self.max_time)
        self.logger.debug('joined (timeout ' + str(self.max_time) + ')')
        if thread.is_alive():
            # this part here is delicate we keep it in a try catch block
            try:
                self.logger.debug('thread is still alive, we timed out')
                # send a SIGTERM
                self.logger.debug('sending SIGTERM signal')
                thread.process.terminate()
                thread.join(timeout=5.0)
                self.logger.debug('done.')
                # send a SIGKILL to be very sure
                self.logger.debug('sending SIGKILL signal')
                thread.process.kill()
                thread.join(timeout=5.0)
                self.logger.debug('done')
            except Exception as e:
                self.logger.warning('exception when killing job: \n' + \
                                    str(e))
        status = thread.process.returncode
        out = str(thread.out)
        err = str(thread.err)
        # to be very sure
        del thread
        self.logger.debug('thread deleted, returning results')
        return (status, out, err)


    # reads and exports the environment variables
    def source_env(self, filename):
        self.logger.debug('setting environment variables')
        command = '"source ' + filename + ' >/dev/null 2>&1 && env"'
        (_, out, _) =  self.execute(['bash', '-c', command])
        lines = out.split('\n')
        for line in lines:
            (key, _, value) = line.partition('=')
            os.environ[key] = value
            self.logger.debug('setting ' + key + '=' + value)
        self.logger.debug('now PATH is: ' + os.environ['PATH'])


    # reads data into a string
    def read(self, filename):
        f = open(filename)
        content = str(f.read())
        f.close()
        return content


    # writes a string to file
    def save(self, filename, content):
        f = open(filename, 'w')
        f.write(str(content))
        f.close()


    # executing the program
    def run(self):
        test = 0
        cost = ""
        while test < self.max_tests:
            command_list = [self.executable] + self.parameters
            (status, out, err) = self.execute(command_list)
            if status != 0:
                test += 1
                self.logger.warning('non-zero exit status *RETRYING* ' + \
                                    str(test) + ' of ' + str(self.max_tests))
                continue
            # parsing the output
            try: 
                cost = self.parse_output(out)
            except: 
                test += 1
                self.logger.warning('something failed in parse_output: *RETRYING* ' + \
                                    str(test) + ' of ' + str(self.max_tests))
                continue
            
            if cost is None:
                cost = ""
            
            try:
                check = float(cost)
            except:
                test += 1
                self.logger.warning('return value of parse_output was not a number: *RETRYING* ' + \
                                    str(test) + ' of ' + str(self.max_tests))
                continue

            # If maximizing, simulate multiply by -1 if maximizing
            if self.maximize:
                if cost[0] == '-':
                    cost = cost[1:]
                else:
                    cost = '-' + cost
                    
            break

        # printing the result
        if test < self.max_tests:
            self.logger.debug('returning cost: ' + cost)
            sys.stdout.write(cost + '\n')
            # force to exit all possible threads except the main one (those
            # launched with execute_threaded_timeout) are run as daemons so
            # they should be terminated automatically when exiting, but just
            # to be extra sure
            sys.exit(0)
        else:
            # in case somehting goes wrong we write stdout and stderr files
            self.logger.error('something went wrong after ' + \
                              str(self.max_tests) + ' runs')
            self.logger.error('saving candidate stdout to ' + \
                              self.filename_prefix + '.stdout')
            self.save(self.filename_prefix + '.stdout', out)
            self.save(self.filename_prefix + '.stderr', err)
            self.logger.error('return value of parse_output: ' + cost)
            self.logger.error('exit status is ' + str(status))
            sys.stdout.write('something went wrong for candidate ' + \
                             self.candidate + '\n')
            if status != 0:
                sys.stdout.write('exit status: ' + str(status) + '\n')
                sys.exit(status)
            else:
                sys.stdout.write('could not cast to float the return value of parse_output: "' + \
                                 cost + '"\n')
                sys.exit(1)

def is_exe(fpath):
    fpath = os.path.expanduser(fpath)
    return os.path.isfile(fpath) and os.access(fpath, os.X_OK) \
        and os.path.getsize(fpath) > 0

def get_execdir():
    return os.path.dirname(os.path.realpath(__file__))


# ------------------------------- CHANGE BELOW! ------------------------------ #

## This example is for the ACOTSP software. Compare it with
## examples/acotsp/target-runner

# Parse here directly the stdout of your job (the 'out' parameter).  Or you can
# ignore the out parameter and read other files produced by your job. This
# function must return a string containing a floating-point number. Everything
# else will generate an error.
def parse_output(out):
    match = re.search(r'Best ([-+0-9.eE]+)', out.strip())
    if match:
        return match.group(1);
    else:
        return "No match"
        
if __name__=='__main__':

    if len(sys.argv) < 5:
        print("\nUsage: " + __file__ + " <candidate_id> <instance_id> <seed> <instance_path_name> <list of parameters>\n")
        sys.exit(1)
    
    bindir = get_execdir()
    # Path to the target-algorithm executable
    executable = '~/bin/acotsp'
    fixed_params = ' --tries 1 --time 1 --quiet '
   
    # reading parameters and setting problem specific stuff
    ## FIXME: Convert this to a class that takes sys.argv and sets the correct
    ## variables.
    candidate_id = sys.argv[1]
    instance_id = sys.argv[2]
    seed = sys.argv[3]
    instance = sys.argv[4]
    parameters = sys.argv[5:]

    # maximum timeout in case the target algorithm does not terminate on its own.
    timeout = 180
    # maximum number of trials before giving up with the configuration
    max_tests = 5

    # Extra whitespace around options is important!
    parameters = [' -i ' + instance + ' --seed ' + seed + fixed_params ] + parameters

    runner = Runner(executable, candidate_id, instance_id, seed,
                    parameters, parse_output, max_tests,
                    #log_level = logging.DEBUG,
                    maximize = False)

    ## FIXME: Convert this to flags with meaningful names like log_level
    # execute through pipes (this is the default)
    # runner.exec_mode(runner.execute1)

    ## FIXME: Convert this to flags with meaningful names
    # execute through temporary files (slightly more robust)
    # runner.exec_mode(runner.execute2)

    ## FIXME: Convert this to flags with meaningful names
    # execute through temporary files with timeout
    # after 5 minutes of *wallclock time* if we do not get the results we kill
    # the subprocess and try another time...
    # runner.exec_mode(runner.execute_timeout1, 300)

    ## FIXME: Convert this to flags with meaningful names
    # python3 execute through pipes with timeout (slightly less robust)
    # runner.exec_mode(runner.execute_timeout2, 300)

    ## FIXME: Convert this to flags with meaningful names
    # execute through temporary files with timeout
    # after 2 minutes of *CPU time* if we do not get the results we kill
    # the subprocess and try another time...
    runner.exec_mode(runner.execute_threaded_timeout, timeout)

    # Convert this to a parameter of the constructor.
    # environment variables that should be set for testing each configuration
    # runner.source_env(bindir + 'configuration')

    # run the target-runner
    runner.run()
