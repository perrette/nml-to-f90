#!/usr/bin/env python2.7
"""Generates a namelist file from fortran source code

This script will parse some fortran source code and write a namelist file, 
provided a list of namelist groups and their mapping as type name.
"""
from __future__ import print_function
import os
import json
import argparse
import warnings
from itertools import groupby
from .namelist import Namelist, Param
from . import ioparams
from .ioparams import Group, Variable
from .parsef90 import parse_type

def _get_type_name(group_name, type_map, type_prefix, type_suffix):
    " build type name from group name "
    if group_name in type_map:
        type_name = type_map[group_name]
    else:
        type_name = type_prefix+group_name+type_suffix
    return type_name


def main():

    # Generate command line arguments
    parser = argparse.ArgumentParser(description=__doc__, prog="nml2f90")
            # formatter_class=argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument("src", nargs="+",default=[],help="source code to be parsed, from which to retrieve param information")
    parser.add_argument("nml", help="output namelist")

    group = parser.add_argument_group("map namelist blocks to type name")
    group.add_argument("--groups", nargs="+",default=[],help="groups to be included in the namelist")
    group.add_argument("--type-suffix", default='_t', help="suffix")
    group.add_argument("--type-prefix", default='', help="prefix")
    group.add_argument("--type-map", type=json.loads, default={}, help='dict {group:type} in json format (override suffix and prefix above if key present)')

    args = parser.parse_args()
    # print(args)

    print("Generate",args.nml)

    # read source code and accumulate into a single block
    code = ""
    for nm in args.src:
        with open(nm) as f:
            code += f.read()

    nml = Namelist()

    # include additional groups that are not in the namelist, but for which
    # command-line features are desired.
    for g in args.groups:
        type_name = _get_type_name(g, args.type_map, args.type_prefix, args.type_suffix)
        group = parse_type(code, type_name=type_name, group_name=g)
        nml.extend(group.to_nml())

    # Write to file
    nml.write(args.nml)

    print("done.")

if __name__ == "__main__":
    main()
