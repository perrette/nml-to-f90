#!/usr/bin/env python2.7
"""Generates a fortran module from a namelist.

This script will read the namelist.nml input file and output ioparams.f90, that 
contains corresponding parameter types and I/O, setter/getter routines.
"""
from __future__ import print_function
import os
import argparse
from itertools import groupby
from .namelist import Namelist
from .ioparams import (Module, Group, Variable,
                               MODULE, CHAR_LEN, REAL_KIND, INTEGER_KIND)

def main():

    # Generate command line arguments
    parser = argparse.ArgumentParser(description=__doc__)
            # formatter_class=argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument("namelist", help="input namelist")
    parser.add_argument("module", default=MODULE, nargs="?",help="module name to be created")
    parser.add_argument("--char-len", type=int, default=CHAR_LEN, help="character length (default: %(default)s)")
    parser.add_argument("--real-kind", type=int, default=REAL_KIND, help="floating point precision (default: %(default)s)")
    parser.add_argument("--int-kind", type=int, default=INTEGER_KIND, help="integer precision (default: %(default)s)")
    parser.add_argument("-v","--verbose", action="store_true", help="Make ioparams.f90 more verbose")
    parser.add_argument("--type-suffix", default='_t', help="suffix for mapping from group_name to type name")
    parser.add_argument("--type-prefix", default='', help="prefix for mapping from group_name to type name")
    # group = parser.add_argument_group("user-defined correspondance between python and fortran")
    # group.add_argument("--map-group", type=json.loads, help='user-defined group-level mapping, in json, e.g. {"group_name":"group1", "type_name":"mytype"}')
    # group.add_argument("--map-param", type=json.loads, help='user-defined param-level mapping, in json')
    # group.add_argument("--json", type=lambda s: json.loads(open(s)), help='read spec from json file instead of namelist')
    group = parser.add_argument_group("fortran features to be provided in the generated module:")
    group.add_argument("--all", action="store_true", help="include all features")
    group.add_argument("--io-nml", action="store_true", help="read_nml, write_nml")
    group.add_argument("--command-line", action="store_true", help="parse_command_argument, print_help")
    group.add_argument("--set-get-param", action="store_true", help="get_param, set_param")

    args = parser.parse_args()
    # print(args)

    # module name and source code file name
    io_mod = args.module
    if io_mod.endswith(".f90"):
        io_file = io_mod  
        io_mod = os.path.basename(io_file[:-4])
    else: 
        io_file = io_mod + ".f90"

    print("Generate",io_file,"with", io_mod, "module from", args.namelist)

    # read namelist
    params = Namelist.read(args.namelist)

    # Create module
    mod = Module(name=io_mod, verbose=args.verbose, char_len=args.char_len, int_kind=args.int_kind, real_kind=args.real_kind)

    for g, grouped_params in groupby(params, lambda x: x.group):

        # construct a group of variables
        group = Group(name=g, type_name=args.type_prefix+g+args.type_suffix, mod_name=None)
        for p in grouped_params:
            v = Variable(name=p.name, value=p.value, group=p.group, help=p.help, units=p.units)
            group.append_variable(v)

        # append the groups to the module
        mod.append_group(group)

    if args.all:
        # args.io_mod = args.command_line = args.set_param = True
        args.io_nml = args.command_line = args.set_get_param = True

    # Add features to the group
    if args.io_nml:
        mod.append_feature("io_nml")
    if args.command_line:
        mod.append_feature("command_line")
    if args.set_get_param:
        mod.append_feature("set_get_param")

    print("...detected namelist groups and corresponding types were generated:")
    for group in mod.groups:
        indent = "  "
        # print(indent, group.name,":",group.type_name," defined in "+(group.mod_name or io_mod))
        if group.mod_name is not None:
            print(indent, group.name,":",group.type_name," imported from "+group.mod_name)
        else:
            print(indent, group.name,":",group.type_name)

    print("...included features (see --help):")
    print("   --io-nml:",args.io_nml)
    print("   --command-line:",args.command_line)
    print("   --set-get-param:",args.set_get_param)

    code = mod.format()

    with open(io_file, 'w') as f:
        f.write(code)

    print("done.")

if __name__ == "__main__":
    main()
