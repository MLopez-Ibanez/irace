import unittest
import io
import os
import tempfile

from genericWrapper4AC.domain_specific.satwrapper import SatWrapper
from genericWrapper4AC.argparser.parse import parse


class TestChecker(unittest.TestCase):
    def setUp(self):
        self.runsolver = os.path.join(
            os.path.dirname(os.path.dirname(__file__)),
            "test_binaries", "runsolver")

        self.res_template = """
        WARNING: for repeatability, setting FPU to use double precision
============================[ Problem Statistics ]=============================
|                                                                             |
|  Number of variables:         10001                                         |
|  Number of clauses:           20601                                         |
|  Parse time:                   0.00 s                                       |
|                                                                             |
============================[ Search Statistics ]==============================
| Conflicts |          ORIGINAL         |          LEARNT          | Progress |
|           |    Vars  Clauses Literals |    Limit  Clauses Lit/Cl |          |
===============================================================================
===============================================================================
restarts              : 1
conflicts             : 26             (3250 /sec)
decisions             : 13738          (0.00 %% random) (1717250 /sec)
propagations          : 52754          (6594250 /sec)
conflict literals     : 71             (0.00 %% deleted)
Memory used           : 22.00 MB
CPU time              : 0.008 s

%s
"""
        self.call = "python examples/MiniSAT/SATCSSCWrapper.py --script " \
                    "examples/MiniSAT/MiniSATWrapper.py --instance examples" \
                    "/MiniSAT/gzip_vc1071.cnf --cutoff 10 --seed 42 --config " \
                    "-rnd-freq 0 -var-decay 0.001 -cla-decay 0.001 " \
                    "-gc-frac 0.000001 -rfirst 1000"
        self.call = self.call.split(" ")

        # Add a solubility file
        self.sat_sol_file = tempfile.NamedTemporaryFile(mode="w+")
        self.sat_sol_file.write(
            "examples/MiniSAT/gzip_vc1071.cnf SATISFIABLE")
        self.sat_sol_file.flush()

        self.unsat_sol_file = tempfile.NamedTemporaryFile(mode="w+")
        self.unsat_sol_file.write(
            "examples/MiniSAT/gzip_vc1071.cnf UNSATISFIABLE")
        self.unsat_sol_file.flush()

        self.unknown_sol_file = tempfile.NamedTemporaryFile(mode="w+")
        self.unknown_sol_file.write(
            "examples/MiniSAT/gzip_vc1071.cnf UNKNOWN")
        self.unknown_sol_file.flush()

    def parse_results(self, solver, specific, sol_file):
        wrapper = SatWrapper()
        wrapper.data, wrapper.args = parse(cmd_arguments=self.call,
                                           parser=wrapper.parser)
        if sol_file == "SATISFIABLE":
            wrapper.args.solubility_file = self.sat_sol_file.name
        elif sol_file == "UNSATISFIABLE":
            wrapper.args.solubility_file = self.unsat_sol_file.name
        else:
            wrapper.args.solubility_file = self.unknown_sol_file.name

        res_file = io.StringIO(self.res_template % solver)
        res_file.name = "FAKE"
        wrapper.data.specifics = specific

        res_map = wrapper.process_results(res_file, exit_code=0)
        return res_map

    def test_all_agree(self):
        # RES SPECIFIC SOLFILE
        # SAT SAT SAT
        res_map = self.parse_results(solver="SATISFIABLE",
                                     specific="SATISFIABLE",
                                     sol_file="SATISFIABLE")
        self.assertEqual(res_map["status"], "SAT")

        # UNSAT UNSAT UNSAT
        res_map = self.parse_results(solver="UNSATISFIABLE",
                                     specific="UNSATISFIABLE",
                                     sol_file="UNSATISFIABLE")
        self.assertEqual(res_map["status"], "UNSAT")

        # SAT None None
        res_map = self.parse_results(solver="SATISFIABLE",
                                     specific="UNKNOWN",
                                     sol_file="UNKNOWN")
        self.assertEqual(res_map["status"], "SAT")

        # SAT something else
        res_map = self.parse_results(solver="SATISFIABLE",
                                     specific="SOMETHING",
                                     sol_file="ELSE")
        self.assertEqual(res_map["status"], "SAT")

        # bla something else
        res_map = self.parse_results(solver="BLA",
                                     specific="SOMETHING",
                                     sol_file="ELSE")
        self.assertFalse("status" in res_map)

    def test_SAT_but_UNSAT(self):
        # RES SPECIFIC SOLFILE
        # SAT None UNSAT
        res_map = self.parse_results(solver="SATISFIABLE",
                                     specific="UNKNOWN",
                                     sol_file="UNSATISFIABLE")
        self.assertEqual(res_map["status"], "CRASHED", res_map["misc"])

        # SAT UNSAT None
        res_map = self.parse_results(solver="SATISFIABLE",
                                     specific="UNSATISFIABLE",
                                     sol_file="UNKNOWN")
        self.assertEqual(res_map["status"], "CRASHED", res_map["misc"])

    def test_UNSAT_but_SAT(self):
        # RES SPECIFIC SOLFILE
        # UNSAT None SAT
        res_map = self.parse_results(solver="UNSATISFIABLE",
                                     specific="UNKNOWN",
                                     sol_file="SATISFIABLE")
        self.assertEqual(res_map["status"], "CRASHED", res_map["misc"])

        # UNSAT SAT None
        res_map = self.parse_results(solver="UNSATISFIABLE",
                                     specific="SATISFIABLE",
                                     sol_file="UNKNOWN")
        self.assertEqual(res_map["status"], "CRASHED", res_map["misc"])