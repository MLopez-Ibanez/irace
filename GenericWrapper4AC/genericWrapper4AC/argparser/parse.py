'''
@author:     Marius Lindauer  
@copyright:  2018 ML4AAD. All rights reserved.
@license:    BSD
'''


import os
import typing
import genericWrapper4AC

from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter, SUPPRESS

from genericWrapper4AC.data.data import Data


def get_parser():
    '''
        Creates an ArgumentParser object 
        and adds the general arguments, such as runsolver-path, temp-file-dir, mem-limit, max_quality
    '''

    parser = ArgumentParser(
        formatter_class=ArgumentDefaultsHelpFormatter, allow_abbrev=False,
        add_help=False)

    parser.add_argument("--runsolver-path", dest="runsolver", default=os.path.join(genericWrapper4AC.__path__[
                        0], "binaries", "runsolver"), help="path to runsolver binary (if None, the runsolver is deactivated)")
    parser.add_argument("--temp-file-dir", dest="tmp_dir", default=None,
                        help="directory for temporary files of runsolver (relative to -exec-dir in SMAC scenario)")
    parser.add_argument("--mem-limit", dest="mem_limit",
                        default=3072, type=int, help="memory limit in MB")
    parser.add_argument("--max_quality", dest="max_quality", default=None,
                        help="maximal quality of unsuccessful runs with timeouts or crashes")
    # deactive -h such that 'h' can be a parameter of the target algorithm
    parser.add_argument('--help', action='help', default=SUPPRESS,
                        help='Show this help message and exit.')

    return parser


def get_extended_parser(parser:ArgumentParser):
    '''
        Extends the parser created in get_parser by optional arguments supported 
        in the new aclib format
        
        Arguments
        parser: Argumentparser
    '''

    parser.add_argument("--instance", dest="instance",
                        default=None, help="path to instance")
    parser.add_argument("--cutoff", dest="cutoff", default=None,
                        type=float, help="running time cutoff")
    parser.add_argument("--seed", dest="seed", default=None,
                        type=int, help="random seed")

    return parser

def parse(cmd_arguments: typing.List[str], parser:ArgumentParser):
    '''
        use parser from get_*parser() to parse <cmd_arguments>

        Arguments
        ---------
        cmd_arguments: typing.List[str]
            command line arguments
        parser: ArgumentParser
            parser

        Returns
        -------
        d: ~genericWrapper4AC.data.data.Data
        main_args: Namespace
            required for backward compatibility
    '''

    new_format = False
    if "--config" in cmd_arguments:
        new_format = True
        parser = get_extended_parser(parser=parser)

    main_args, target_args = parser.parse_known_args(cmd_arguments[1:])

    if new_format:
        d = parse_config_new(main_args=main_args, target_args=target_args)
    else:
        d = parse_config_old(main_args=main_args, target_args=target_args)

    d.runsolver = main_args.runsolver
    d.tmp_dir = main_args.tmp_dir
    d.mem_limit = main_args.mem_limit
    if main_args.max_quality is not None:
        d.max_quality = main_args.max_quality

    d.new_format = new_format

    return d, main_args


def parse_config_old(main_args, target_args: typing.List[str]):
    '''
        creates Data() object and fills it with parsed command line arguments
        using the old call format

        Arguments
        --------
        main_args: Namespace 
            arguments parsed by argparse
        target_args: typing.List[str]
            arguments not parsed so far

        Returns
        -------
        d: ~genericWrapper4AC.data.data.Data
    '''

    d = Data()
    
    d.instance = target_args[0]
    d.specifics = target_args[1]
    # runsolver only rounds down to integer
    d.cutoff = int(float(target_args[2]) + 1 - 1e-10)
    d.cutoff = min(d.cutoff, 2**31 - 1)  # at most 32bit integer supported
    d.time = d.cutoff
    d.runlength = int(target_args[3])
    d.seed = int(target_args[4])
    d.config = target_args[5:]
    d.config = dict((name, value.strip("'"))
                    for name, value in zip(d.config[::2], d.config[1::2]))

    return d


def parse_config_new(main_args, target_args: typing.List[str]):
    '''
        creates Data() object and fills it with parsed command line arguments
        using the new ACLib2 call format

        Arguments
        --------
        main_args: Namespace 
            arguments parsed by argparse
        target_args: typing.List[str]
            arguments not parsed so far

        Returns
        -------
        d: ~genericWrapper4AC.data.data.Data
    '''
    
    d = Data()

    d.instance = main_args.instance
    # runsolver only rounds down to integer
    if main_args.cutoff is not None:
        d.cutoff = int(main_args.cutoff + 1 - 1e-10)
        d.cutoff = min(d.cutoff, 2**31 - 1)  # at most 32bit integer supported
        d.time = d.cutoff
    d.seed = main_args.seed

    d.config = target_args[target_args.index("--config") + 1:]
    d.config = dict((name, value.strip("'"))
                    for name, value in zip(d.config[::2], d.config[1::2]))

    return d
