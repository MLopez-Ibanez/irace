#!/usr/bin/env python3
# encoding: utf-8

'''
DummyWrapper -- Dummy wrapper for unit testing

This requires the installation of https://github.com/automl/GenericWrapper4AC

@author:     Manuel López-Ibáñez
@copyright:  2018
@license:    BSD
@contact:    manuel.lopez-ibanez@manchester.ac.uk
'''

import json
import traceback

from genericWrapper4AC.generic_wrapper import AbstractWrapper

class DummyWrapper(AbstractWrapper):
    '''
        Dummy wrapper for unit testing
    '''
    
    def __init__(self):
        AbstractWrapper.__init__(self)
        self._return_value = None
    
    def get_command_line_args(self, runargs, config):
        '''
        '''
        runargs.update(config)
        return "echo '" + json.dumps(runargs) + "'"
    
    def process_results(self, filepointer, exit_code):
        '''
        Parse a results file to extract the run's status (SUCCESS/CRASHED/etc) and other optional results.
    
        Args:
            filepointer: a pointer to the file containing the solver execution standard out.
            exit_code : exit code of target algorithm
        '''

        statuses = ['SUCCESS', 'TIMEOUT', 'CRASHED', 'ABORT']
        
        # If something fails, we a serious problem
        output = dict(status='ABORT')
        for line in filepointer:
            try:
                argmap = json.loads(str(line.decode('UTF-8')).replace("\n",""))
                ins = argmap["instance"]
                seed = argmap["seed"]
                cost = float(argmap["-cost"])
                runtime = float(argmap["-runtime"])
                status = statuses[seed % len(statuses)]
                if ins == "cost":
                    output = dict(status = status, cost = cost)
                elif ins == "time":
                    cutoff = float(argmap["cutoff"])
                    output = dict(status=status, runtime=min(cutoff,runtime))
                elif ins == "cost+time":
                    cutoff = float(argmap["cutoff"])
                    output = dict(status=status, cost=cost, runtime=min(cutoff,runtime))
            except ValueError:
                traceback.print_exc()
                pass

        return output

        
if __name__ == "__main__":
    wrapper = DummyWrapper()
    wrapper.main()    
