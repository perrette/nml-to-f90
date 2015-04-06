#!/usr/bin/env python2.7
"""Generates a fortran module from a namelist.

This script will read the namelist.nml input file and output ioparams.f90, that 
contains corresponding parameter types and I/O, setter/getter routines.
"""
from __future__ import print_function
import os
import json
import argparse
import warnings
from itertools import groupby
from .namelist import Namelist
from .ioparams import (Module, Group, Variable,
                               MODULE, CHAR_LEN, REAL_KIND, INTEGER_KIND)
from .parsef90 import parse_type

def main():

    # Generate command line arguments
    parser = argparse.ArgumentParser(description=__doc__, prog="nml2f90")
            # formatter_class=argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument("namelist", help="input namelist")
    parser.add_argument("module", default=MODULE, nargs="?",help="module name to be created")
    parser.add_argument("--char-len", type=int, default=CHAR_LEN, help="character length (default: %(default)s)")
    parser.add_argument("--real-kind", type=int, default=REAL_KIND, help="floating point precision (default: %(default)s)")
    parser.add_argument("--int-kind", type=int, default=INTEGER_KIND, help="integer precision (default: %(default)s)")
    parser.add_argument("-v","--verbose", action="store_true", help="Make ioparams.f90 more verbose")

    group = parser.add_argument_group("map namelist blocks to type name")
    group.add_argument("--src", nargs="*",default=[],help="source code to be parsed, from which to import types instead of creating them")
    group.add_argument("--include-groups", nargs="*",default=[],help="include additional groups that are not in the namelist (requires --src ...)")
    group.add_argument("--exclude-groups", nargs="*",default=[],help="exclude groups even though they are in the namelist")
    group.add_argument("--type-suffix", default='_t', help="suffix")
    group.add_argument("--type-prefix", default='', help="prefix")
    group.add_argument("--type-map", type=json.loads, default={}, help='dict {group:type} in json format (override suffix and prefix above if key present)')

    group = parser.add_argument_group("fortran features to be provided in the generated module (all groups by default, but also accepts specific group names):")
    group.add_argument("--all", action="store_true", help="include all features")
    subgroup = group.add_mutually_exclusive_group()
    subgroup.add_argument("--io-nml", nargs="*", help="read_nml, write_nml")
    subgroup.add_argument("--lib-nml", nargs='*', help="read_nml: alternative to --io-nml using external lib nml.f90 (https://github.com/alex-robinson/nml)")
    group.add_argument("--command-line", nargs='*', help="parse_command_argument, print_help")
    group.add_argument("--set-get-param", nargs='*', help="get_param, set_param")

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

    # read source code and accumulate into a single block
    code = ""
    if len(args.src) > 0:
        for nm in args.src:
            with open(nm) as f:
                code += f.read()

    def _get_type_name(group_name):
        " build type name from group name "
        if group_name in args.type_map:
            type_name = args.type_map[group_name]
        else:
            type_name = args.type_prefix+group_name+args.type_suffix
        return type_name

    # Create module
    mod = Module(name=io_mod, verbose=args.verbose, char_len=args.char_len, int_kind=args.int_kind, real_kind=args.real_kind)

    for g, grouped_params in groupby(params, lambda x: x.group):

        if g in args.exclude_groups:
            continue

        # map type name
        type_name = _get_type_name(g)

        # construct a group of variables
        group = Group(name=g, type_name=type_name, mod_name=None)

        for p in grouped_params:
            v = Variable(name=p.name, value=p.value, group=p.group, help=p.help, units=p.units)
            group.append_variable(v)

        # check wether the type is defined in the source code
        if len(args.src) > 0:
            try:
                # search for type and parse it
                group_orig = parse_type(code, type_name=group.type_name, mod_name=group.mod_name, group_name=group.name)
                # update group properties
                group.update(group_orig)
            except Exception as error:
                warnings.warn(error.message)

        # append the groups to the module
        mod.append_group(group)

    # include additional groups that are not in the namelist, but for which
    # command-line features are desired.
    included_groups = [group.name for group in mod.groups]
    for g in args.include_groups:
        if len(args.src) == 0:
            # warnings.warn("No source files have been provided. See --src options.")
            raise Exception("Source files must be provided via --src *f90 if --include-groups is passed. See --help options.")
        if g not in included_groups:
            type_name = _get_type_name(g)
            group = parse_type(code, type_name=type_name, group_name=g)
            mod.append_group(group)

    if args.all:
        # args.io_mod = args.command_line = args.set_param = True
        args.io_nml = args.command_line = args.set_get_param = True

    # Add features to the group
    if args.io_nml is not None:
        mod.append_feature("io_nml", args.io_nml or None)
    if args.lib_nml is not None:
        mod.append_feature("lib_nml", args.lib_nml or None)
    if args.command_line is not None:
        mod.append_feature("command_line", args.command_line or None)
    if args.set_get_param is not None:
        mod.append_feature("set_get_param", args.set_get_param or None)

    print("...detected namelist groups and corresponding types were generated:")
    for group in mod.groups:
        indent = "  "
        # print(indent, group.name,":",group.type_name," defined in "+(group.mod_name or io_mod))
        if group.mod_name is not None:
            print(indent, group.name,":",group.type_name," imported from "+group.mod_name)
        else:
            print(indent, group.name,":",group.type_name)

    if len(mod.features) == 0:
        print("...no features included (see --help)")
    else:
        print("...included features (see --help):")
        for feature in mod.features:
            print("   --"+feature.name,feature.group_names)

    code = mod.format()

    with open(io_file, 'w') as f:
        f.write(code)

    print("done.")

if __name__ == "__main__":
    main()
