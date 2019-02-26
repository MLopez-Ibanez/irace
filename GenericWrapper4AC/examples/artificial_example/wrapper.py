#!/usr/bin/env python2.7
# encoding: utf-8

'''
SGDWrapper -- Simple wrapper around an external script for SGD

@author:     Marius Lindauer
@copyright:  2018 ML4AAD. All rights reserved.
@license:    BSD
@contact:    lindauer@informatik.uni-freiburg.de
'''

import sys
import re
import math
import logging

from genericWrapper4AC.generic_wrapper import AbstractWrapper

class ArtWrapper(AbstractWrapper):
    '''
        Simple wrapper for an external artificial script
    '''
    
    def __init__(self):
        logging.basicConfig()
        AbstractWrapper.__init__(self)
        
        self._return_value = None
    
    def get_command_line_args(self, runargs, config):
        '''
        Returns the command line call string to execute the target algorithm (here: Spear).
        Args:
            runargs: a map of several optional arguments for the execution of the target algorithm.
                    {
                      "instance": <instance>,
                      "specifics" : <extra data associated with the instance>,
                      "cutoff" : <runtime cutoff>,
                      "runlength" : <runlength cutoff>,
                      "seed" : <seed>
                    }
            config: a mapping from parameter name to parameter value
        Returns:
            A command call list to execute the target algorithm.
        '''
        cmd = "python examples/artificial_example/target_algorithm.py %s %d " %(runargs["instance"], runargs["seed"])
        cmd += " ".join(["%s %s" %(name[1:], value) for name, value in config.items()]) 
        
        return cmd 
    
    def process_results(self, filepointer, exit_code):
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
        
        self.logger.debug("reading solver results from %s" % (filepointer.name))

        # If solver result file is empty, we also assume a crash
        resultMap = {'status': 'CRASHED',
                     'quality': 1  # assumption minimization
                     }
        for line in filepointer:
            try:
                out_ = str(line.decode('UTF-8')).replace("\n","")
                return_value = float(out_)
                resultMap = {'status' : 'SUCCESS',
                 'quality' : return_value
                 }
                return resultMap
            except ValueError:
                pass

        return resultMap

        
if __name__ == "__main__":
    wrapper = ArtWrapper()
    wrapper.main()    
