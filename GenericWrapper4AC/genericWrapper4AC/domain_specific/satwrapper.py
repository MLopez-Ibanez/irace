#!/usr/bin/env python2.7
# encoding: utf-8

'''
SATCSSCWrapper -- target algorithm wrapper for SAT solvers

@author:     Marius Lindauer
@copyright:  2016 Ml4AAD. All rights reserved.
@license:    BSD
@contact:    lindauer@informatik.uni-freiburg.de
'''

import sys
import re
import os
import imp
import logging
from subprocess import Popen, PIPE

from genericWrapper4AC.generic_wrapper import AbstractWrapper

class SatWrapper(AbstractWrapper):
    '''
        General wrapper for a SAT solver
    '''
    
    def __init__(self):
        '''
            Constructor
        '''
        logging.basicConfig()
        AbstractWrapper.__init__(self)
        
        self.parser.add_argument("--sol-file", dest="solubility_file", default=None, help="File with \"<instance> {SATISFIABLE|UNSATISFIABLE|UNKNOWN}\" ")
        self.parser.add_argument("--sat-checker", dest="sat_checker", default=None, help="binary of SAT checker")

        self._instance = ""
        self._instance = ""
        
        self.inst_specific = None
        
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
        data = filepointer.read()
        try:
            data = str(data.decode("utf8"))
        except AttributeError:
            pass    
        
        resultMap = {}
        
        resultMap['misc'] = ""
        
        if re.search('UNSATISFIABLE', data):
            resultMap['status'] = 'UNSAT'
            
            # verify UNSAT via external knowledge
            if self.data.specifics in ["SAT", "10", "SATISFIABLE"]:
                resultMap['status'] = 'CRASHED'
                resultMap['misc'] = "according to instance specific, instance is SAT but UNSAT was reported"
            elif not self.args.solubility_file:
                resultMap['misc'] += "; solubility file was not given; could not verify UNSAT" 
            elif not os.path.isfile(self.args.solubility_file):
                resultMap['misc'] += "; have not found %s; could not verify UNSAT" % (self.args.solubility_file)
            elif not self._verify_via_solubility_file(sol="UNSAT"): # use solubility file
                resultMap['status'] = 'CRASHED'
                resultMap['misc'] += "; according to solubility file, instance is SAT but UNSAT was reported"
                
        elif re.search('SATISFIABLE', data):
            resultMap['status'] = 'SAT'
               
            # look for model
            model = None
            for line in data.split("\n"):
                if line.startswith("v "):
                    model = map(int, line.split(" ")[1:-1])
                    break

            # verify SAT
            if self.data.specifics in ["UNSAT", "20", "UNSATISFIABLE"] :
                resultMap['status'] = 'CRASHED'
                resultMap['misc'] = "; according to instance specific, instance is UNSAT but SAT was reported"
            elif not self.args.solubility_file:
                resultMap['misc'] += "; solubility file was not given; could not verify SAT" 
            elif not os.path.isfile(self.args.solubility_file):
                resultMap['misc'] += "; have not found %s; could not verify SAT" % (self.args.solubility_file)
            elif not self._verify_via_solubility_file(sol="SAT"): # use solubility file
                resultMap['status'] = 'CRASHED'
                resultMap['misc'] += "; according to solubility file, instance is UNSAT but SAT was reported"
                
                
            if not self.args.sat_checker:
                resultMap['misc'] += "; SAT checker was not given; could not verify SAT"
            elif not os.path.isfile(self.args.sat_checker):
                resultMap['misc'] += "; have not found %s; could not verify SAT" % (self.args.sat_checker)
            elif model is None:
                resultMap['misc'] += "; print of solution was probably incomplete because of runsolver SIGTERM/SIGKILL"
                resultMap['status'] = 'TIMEOUT'
            elif not self._verify_SAT(model, filepointer):
                # fix: race condition between SIGTERM of runsolver and print of solution
                if self.data.status == "TIMEOUT":
                    resultMap['status'] = 'TIMEOUT'
                    resultMap['misc'] = 'print of solution was probably incomplete because of runsolver SIGTERM/SIGKILL'
                else:
                    resultMap['status'] = 'CRASHED'
                    resultMap['misc'] = "SOLVER BUG: solver returned a wrong model"
                    # save command line call
         
        elif re.search('s UNKNOWN', data):
            resultMap['status'] = 'TIMEOUT'
            resultMap['misc'] = "Found s UNKNOWN line - interpreting as TIMEOUT"
        elif re.search('INDETERMINATE', data):
            resultMap['status'] = 'TIMEOUT'
            resultMap['misc'] = "Found INDETERMINATE line - interpreting as TIMEOUT"
        
        return resultMap
    
    def _verify_SAT(self, model, solver_output):
        '''
            verifies the model for self._instance 
            Args:
                model : list with literals
                solver_output: filepointer to solver output
            Returns:
                True if model was correct
                False if model was not correct
        '''
        cmd = [self.args.sat_checker, self._instance, solver_output.name]
        io = Popen(cmd, stdout=PIPE, universal_newlines=True)
        out_, err_ = io.communicate()
        for line in out_.split("\n"):
            if "Solution verified" in line:
                self.logger.debug("Solution verified")
                return True
            elif "Wrong solution" in line:
                return False
        return True  # should never happen

    def _verify_via_solubility_file(self, sol):
        '''
            looks in <self.args.solubility_file> whether it is already known that the instance is UNSAT
            
            Args:
                sol: ["SAT", "UNSAT"]
                claimed status
            
            Returns:
                False if the instance is known as SAT
                True otherwise
        '''
        
        with open(self.args.solubility_file) as fp:
            for line in fp:
                if line.startswith(self.data.instance):
                    line = line.strip("\n")
                    status = line.split(" ")[1]
                    if status in ["SAT", "SATISFIABLE"] and sol=="UNSAT":
                        return False
                    elif status in ["UNSAT", "UNSATISFIABLE"] and sol=="SAT":
                        return False
                    else:
                        self.logger.debug("Verified by solubility file")
                        return True
        self.logger.debug("Could not find instance in solubility file")      
        return True
        

if __name__ == "__main__":
    wrapper = SatCSSCWrapper()
    wrapper.main()    
