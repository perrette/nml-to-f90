#!/usr/bin/env python2.7
"""Generates a fortran module from a namelist.

This script will read the namelist.nml input file and output ioparams.f90, that 
contains corresponding parameter types and I/O, setter/getter routines.
"""
from __future__ import print_function
import sys, os, json
from collections import OrderedDict as odict
from namelist import Namelist
import nml2f90_templates
import warnings
import textwrap
import argparse

__version__ = nml2f90_templates.__version__

try:
    from docopt import docopt
    hasdocopt = True
except ImportError:
    warnings.warn("install 'docopt' to use all command-line arguments")
    hasdocopt = True

# +++++++++++++++++++++++++++++++++++++++++
# load templates
# +++++++++++++++++++++++++++++++++++++++++
_dir = os.path.abspath(os.path.dirname(nml2f90_templates.__file__))
template_module = open(os.path.join(_dir, "module_ioparams.f90")).read()
template_io = open(os.path.join(_dir, "subroutine_io.f90")).read()
template_cmd = open(os.path.join(_dir, "subroutine_cmd.f90")).read()
template_setget = open(os.path.join(_dir, "subroutine_setget.f90")).read()
code_typeconversion = open(os.path.join(_dir, "subroutine_typeconversion.f90")).read()


# +++++++++++++++++++++++++++++++++++++++++
# Variable map
# +++++++++++++++++++++++++++++++++++++++++
# For each namelist variable create dict with all relevant info
# it can be seen as a map between python and fortran source code.

# basic types
_map_type = {
    "float" : "real",
    "int" : "integer",
    "str" : "character",
    "bool" : "logical",
    "NoneType" : None, # convenience  to that value does not have to be passed to make_map_variable
}

def make_map_variable(value=None, name=None, units="", doc="", precision=None, dtype=None, block=None):
    """ return a dict of attributes to map a python variable to fortran
    """
    # basic data type
    if dtype is None:
        if type(value) is list:
            dtype = _map_type[type(value[0]).__name__]
        else:
            dtype = _map_type[type(value).__name__]

    # precision?
    if precision is None:
        if dtype == "real":
            precision = ("kind","dp")  # e.g. kind=8 ; assuming that dp is defined in module
        elif dtype == "character":
            precision = ("len","clen") # e.g. len=4 ; same, assume that clen is defined is module

    # array?
    if type(value) is list:
        array = True
        size = len(value)
    else:
        array = False
        size = None

    map_v = {
        "name": name,
        "units": units,
        "doc": doc,
        "dtype": dtype,
        "precision": precision,
        "array": array,
        "size": size,
        "value": value,
        "block": block,  # block to which this variable belongs
    }

    return map_v

def _update_variable_type_info(map_v):
    """ add fortran variable type and so on
    e.g. from read, dp, array, and size fields
    returns real(dp), dimension(size)
    """
    map_v["type"] = _make_def_variable(map_v)
    map_v["type_dyn"] = _make_def_variable(map_v, dynamic=True)
    map_v["type_interface"] = map_v["dtype"] + map_v["array"]*"_arr"

def _make_def_variable(v_map, dynamic=False):
    """ make fortran type from variable map
    """
    dtype = v_map["dtype"]

    precision = v_map["precision"]
    if precision is not None:
        if dtype == "character" and dynamic: 
            precision = ("len","*")
        dtype = dtype+"({}={})".format(*precision)

    if v_map["array"]:
        if dynamic:
            size = ":"
        else:
            size = v_map["size"]
        assert size is not None, "array has no attached size: only possible when dynamic is True"
        dtype = dtype + ", dimension({})".format(size)
    return dtype

# +++++++++++++++++++++++++++++++++++++++++
# Namelist's block map
# +++++++++++++++++++++++++++++++++++++++++
# map betweem whole namelist blocks and the fortran type and modules

def make_map_block(name, suffix="_t", prefix="", type_name=None, mod_name=None, members=None, io_nml=True, command_line=True, setget=False):
    """ return a map for one namelist block, to match block name, module and type names

    Parameters
    ---------
    name : namelist block
    prefix, suffix : short-way to built type_name, if the latter is not provided
    type_name : corresponding type name
    mod_name : corresponding module name, if the type is already defined in a module
        Note that if mod_name is not provided, the type will be defined in ioparams.f90
    members : a list of variable maps (returned by make_map_variable)

    Additionally, a few parameters indicate the functionality to add to that block
    io_nml : read_nml, write_nml
        namelist I/O, true by default
    command_line : parse_command_argument, print_help
        command-line functionality, true by default
    setget : set_param, get_param
        generic setter/getter for the type, false by default

    Returns
    -------
    a dict with the keys defined above
    """
    if type_name is None:
        type_name = prefix+name+suffix

    map_b = {
        "block_name": name, 
        "mod_name": mod_name, # if None, will be written in ioparams
        "type_name": type_name,
        "members" : members,
        "io_nml": io_nml, 
        "command_line":command_line,
        "setget":setget,
    }

    # make sure the block is right
    if members is not None:
        for m in members:
            m["block"] = name

    return map_b

def _make_def_block(block_map):
    """ build type definition
    """
    type_def = ["type {type_name}".format(**block_map)]
    for v_map in block_map["members"]:
        type_def.append("        {type} :: {name}".format(**v_map))
    type_def.append("    end type")
    return "\n".join(type_def)

# +++++++++++++++++++++++++++++++++++++++++
# Modules' format
# +++++++++++++++++++++++++++++++++++++++++
# get formatting information at the module level

def get_format_typedef(block_maps):
    """ Fill-in variable names for derived type definition

    block_maps : list of namelist block maps (see make_map_block and make_map_variable)
    """
    # groups = params.keys()
    type_definitions = []
    list_of_types = []
    type_includes = []

    # loop over derived types
    for block_map in block_maps:
        if block_map["mod_name"] is None:
            type_definitions.append( _make_def_block(block_map) )
        else:
            type_inclues.append("use {type_mod}; only: {type_name}")
        list_of_types.append( block_map["type_name"] )

    module_map = {
        "type_includes" : " \n".join(type_includes),
        "type_definitions" : "\n    ".join(type_definitions),
        "list_of_types" : " &\n".join(textwrap.wrap(", ".join(list_of_types))),
    }
    return module_map


def get_format_io(block_maps):
    """ Create I/O source code from a namelist template

    params : dict of dict (params[block_name][param_name])
    """

    io_routines = []
    read_nml_interface = []
    write_nml_interface = []

    # Namelist I/O for whole groups
    for block_map in block_maps:

        if not block_map["io_nml"]:
            continue

        # For each parameter type, build a map specific to this routine
        variable_definitions = []
        list_of_variables = []
        list_of_init = []
        list_of_assign = []
        for v_map in block_map["members"]:
            variable_definitions.append("{type} :: {name}".format(**v_map))
            list_of_variables.append("{name}".format(**v_map))
            list_of_init.append("{name} = params%{name}".format(**v_map))
            list_of_assign.append("params%{name} = {name}".format(**v_map))

        routines_map = block_map.copy()
        routines_map.update(dict(
            block_name=block_map["block_name"], # TODO: remove, and replace with name= in the templates
            variable_definitions="\n    ".join(variable_definitions),
            list_of_variables = " &\n".join(textwrap.wrap(", ".join(list_of_variables))),
            list_of_init = "\n    ".join(list_of_init),
            list_of_assign = "\n    ".join(list_of_assign),
        ))

        io_routines.append( template_io.format(**routines_map) )
        read_nml_interface.append("module procedure :: read_nml_{block_name}".format(**block_map)) 
        write_nml_interface.append("module procedure :: write_nml_{block_name}".format(**block_map)) 

    # source_code = template_module.format(types = ", ".join(types), 

    io_map = {
        "io_routines" : "\n\n".join(io_routines),
        "read_nml_proc" : "\n        ".join(read_nml_interface),
        "write_nml_proc" : "\n        ".join(write_nml_interface),
    }

    return io_map

template_set_string_case = """
case ('{name}', '{block}%{name}')
    read(string, *, iostat=IOSTAT) params%{name}
    if (VERBOSE .or. IOSTAT/=0) write(*,*) "{block}%{name} = ", params%{name}
    if (IOSTAT /= 0) then 
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --{block}%{name}"
        else
            write(*,*) "ERROR converting string to {type}: --{block}%{name} ",trim(string)
        endif
        stop
    endif
"""

template_set_string_case_array = """
case ('{name}', '{block}%{name}')
    call string_to_array(string, params%{name}, iostat=iostat)
    if (iostat /= 0) then 
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --{block}%{name}"
        else
            write(*,*) "ERROR converting string to {type} array : --{block}%{name} ",trim(string)
        endif
        stop
    endif
"""

template_has_case = """
case ('{name}', '{block}%{name}')
"""

template_help = """
if (def) then
    write(io, *) "--{name} {type} {doc} (default: ",params%{name}," )"
else
    write(io, *) "--{name} {type} {doc}"
endif
"""
template_help_string = """
if (def) then
    write(io, *) "--{name} {type} {doc} (default: ",trim(params%{name})," )"
else
    write(io, *) "--{name} {type} {doc}"
endif
"""

def get_format_cmd(block_maps):
    """ Fill template to help parsing command-line arguments

    inputs:
        params : dict of dict (params[block_name][param_name])
    returns:
        dict with keys:
        - has_param_proc
        - has_param_routines
        - set_string_proc
        - set_string_routines
    """
    cmd_routines = []

    has_param_proc = []
    set_param_string_proc = []
    parse_command_argument_proc = []
    print_help_proc = []

    for block_map in block_maps:

        if not block_map["command_line"]:
            continue

        list_set_cases = []
        list_has_cases = []
        list_help = []

        for v_map in block_map['members']:

            # has_params routines
            list_has_cases.append(template_has_case.format(**v_map))

            # set_param_string routines
            if v_map["array"]:
                list_set_cases.append(template_set_string_case_array.format(**v_map))
            else:
                list_set_cases.append(template_set_string_case.format(**v_map))

            # print_help routines
            list_help.append(template_help.format(**v_map))

        routines_map = block_map.copy() # routine-level map
        routines_map.update( dict(
            list_set_cases="\n    ".join(list_set_cases),
            list_has_cases="\n    ".join(list_has_cases),
            list_help ="\n    ".join(list_help),
        ))

        cmd_routines.append( template_cmd.format(**routines_map) )

        set_param_string_proc.append("module procedure :: set_param_string_{block_name}".format(**block_map)) 
        has_param_proc.append("module procedure :: has_param_{block_name}".format(**block_map)) 
        parse_command_argument_proc.append("module procedure :: parse_command_argument_{block_name}".format(**block_map)) 
        print_help_proc.append("module procedure :: print_help_{block_name}".format(**block_map)) 

    # source_code = template_module.format(types = ", ".join(types), 
    cmd_map = {
        "cmd_routines" : "\n\n".join(cmd_routines),
        "has_param_proc" : "\n        ".join(has_param_proc),
        "set_param_string_proc" : "\n        ".join(set_param_string_proc),
        "parse_command_argument_proc" : "\n        ".join(parse_command_argument_proc),
        "print_help_proc" : "\n        ".join(print_help_proc),
    }

    return cmd_map


# when selecting parameter values, also accept argument names with "block_name%name" syntax
template_get_cases = """
case ('{name}', '{block}%{name}')
    value = params%{name}
"""
template_set_cases = """
case ('{name}', '{block}%{name}')
    params%{name} = value
"""

# NOTE: Is that functionality get_param / set_param really useful?
# The same thing could be achieved with set_string. 
# But well, this may come in handy for some generic application.
def get_format_setget(block_maps):
    """ Create set_param/get_param source code from a namelist template

    params : dict of dict (params[block_name][param_name])
    """
    get_param_interface = []
    set_param_interface = []
    setget_routines = []

    # loop over derived types
    for block_map in block_maps:

        if not block_map["setget"]:
            continue

        # determine the list of all members type present in this derived type
        # build a dict of [(vtype1: [p11, p12, ...]), ...]
        maps_by_type = odict()

        for v_map in block_map["members"]:
            type_interface = v_map["dtype"] + v_map["array"]*"_arr"
            if type_interface not in maps_by_type:
                maps_by_type[type_interface] = []
            maps_by_type[type_interface].append(v_map)

        # loop over all types
        for type_interface in maps_by_type:
            # vtype_short = vtypes_short[vtype]
            list_get_cases = []
            list_set_cases = []
            for v_map in maps_by_type[type_interface]:
                list_get_cases.append( template_get_cases.format(**v_map) )
                list_set_cases.append( template_set_cases.format(**v_map) )

            routine_map = block_map.copy() # usual block map
            routine_map.update({
                "type_interface" : type_interface,
                "type" : v_map["type"], # should all have the same type
                "list_get_cases" :  "\n".join(list_get_cases),
                "list_set_cases" :  "\n".join(list_set_cases),
            })

            # the subroutines
            setget_routines.append(
                template_setget.format(**routine_map)
            )

            # prepare interface of all subroutines
            get_param_interface.append(
                "module procedure :: get_param_{block_name}_{type_name}".format(**block_map)
            )
            set_param_interface.append(
                "module procedure :: set_param_{block_name}_{type_name}".format(**block_map)
            )


    fmt = dict(
        set_param_proc = "\n        ".join(set_param_interface),
        get_param_proc = "\n        ".join(get_param_interface),
        setget_routines = "\n\n".join(setget_routines),
    )

    return fmt


def make_source(map_blocks, io_mod="ioparams", source="", clen=256, verbose=True):
    """ Make source code with I/O and getter / setter
    
    map_blocks : essential info
    io_mod : generated module name
    source : source namelist or else from which the file was generated 
    version : version of this file
    """
    # choose what to include or not
    fmt = {
        "version" : __version__,
        "io_module_name" : io_mod,
        "source" : source,
        "clen" : clen, 
        "verbose" : ".true." if verbose else ".false.",
        "type_conversion" : code_typeconversion,
    }
    fmt.update( 
        get_format_typedef(map_blocks)
    )
    fmt.update( 
        get_format_io(map_blocks) 
    )
    fmt.update( 
        get_format_cmd(map_blocks) 
    )
    fmt.update( 
        get_format_setget(map_blocks)
    )
    return template_module.format(**fmt)


# --map-blocks and --map-params parameters
#
# These parameters are to be provided in json format,
# as dict '{key1:val1, key2:val2}' or list of dict '[{...},{...}]'. Mind the single
# quotes '', which may be needed for your shell to recognize this chain as a single
# argument. Watch out for the forthcoming --json option to provide a json file directly,
# comprehending all information, and possibly even replacing the <namelist.nml> input.
#
# Accepted mapping keys at the block level are 
#
#     block_name : namelist block, required
#     type_name : corresponding type name
#     mod_name : corresponding module name, if the type is already defined in a module
#         Note that if mod_name is not provided, the type will be defined in ioparams.f90
#
#     Additionally, a few parameters indicate the functionality to add to that block
#     io_nml : read_nml, write_nml
#         namelist I/O, true by default
#     command_line : parse_command_argument, print_help
#         command-line functionality, true by default
#     setget : set_param, get_param
#         generic setter/getter for the type, false by default
#
# Accepted mapping keys at the param level are:
#     
#     "block": block to which this variable belongs
#     "name" : parameter name, required
#     "dtype": "real", "integer", "logical", "character"
#         Note there is no dimension or precision specification here,
#         even though we may deal with an array.
#     "units": units as a string
#     "doc": documentation for the variable
#     "precision": integer (e.g. len= parameter for character, kind= for real)
#         By default, all real numbers are double precision, characters are 256
#         or whatever value passed by --clen
#     "array" : True, False (default)
#         add the dimension(size) qualifyer
#     "size" : integer (if array==True)
#     "value": default value 
#         This is needed if dtype is not provided
# """


def main():

    # Generate command line arguments
    parser = argparse.ArgumentParser(description=__doc__)
            # formatter_class=argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument("namelist", help="input namelist")
    parser.add_argument("module", default="ioparams", nargs="?",help="module name to be created")
    parser.add_argument("--clen", type=int, default=256, help="character length (default: %(default)s)")
    parser.add_argument("--type-suffix", default='_t', help="suffix for mapping from block_name to type name")
    parser.add_argument("--type-prefix", default='', help="prefix for mapping from block_name to type name")
    # group = parser.add_argument_group("user-defined correspondance between python and fortran")
    # group.add_argument("--map-block", type=json.loads, help='user-defined block-level mapping, in json, e.g. {"block_name":"block1", "type_name":"mytype"}')
    # group.add_argument("--map-param", type=json.loads, help='user-defined param-level mapping, in json')
    # group.add_argument("--json", type=lambda s: json.loads(open(s)), help='read spec from json file instead of namelist')
    # group.add_argument("--write-json", help='write derived specs to the provided json file')
    group = parser.add_argument_group("fortran functionality to be provided in ioparams")
    # group.add_argument("--full", help="do include get_param, set_param functions")
    group.add_argument("--no-io-nml", action="store_false", dest='io_nml', help="read_nml, write_nml")
    group.add_argument("--no-command-line", action="store_false", dest="command_line", help="no parse_command_argument, print_help")
    group.add_argument("--io-nml", action="store_true", help="read_nml, write_nml")
    group.add_argument("--command-line", action="store_true", help="parse_command_argument, print_help")
    group.add_argument("--set-get-param", action="store_true", help="get_param, set_param functions")

    args = parser.parse_args()
    print(args)

    # module name and source code file name
    io_mod = args.module
    if io_mod.endswith(".f90"):
        io_file = io_mod  
        io_mod = os.path.basename(io_file[:-4])
    else: 
        io_file = io_mod + ".f90"

    print("Convert "+args.namelist+" to "+io_file)
    print("...new module: "+io_mod)

    # read namelist
    params = Namelist.read(args.namelist)

    # Derive all variable and namelist blocks information

    # from namelist and input argument
    b_maps = []
    for b in params.keys():
        v_maps = []
        for p in params[b]:
            v_map = make_map_variable(value=params[b][p], name=p, block=b)
            # v_map = make_map_variable(value=params[b][v], name=v, units="", doc="", precision=None, dtype=None, block=b)
            v_maps.append(v_map)
        b_map = make_map_block(b, members=v_maps, suffix=args.type_suffix, prefix=args.type_prefix, 
                               setget=args.set_get_param, 
                               io_nml=args.io_nml,
                               command_line=args.command_line,
                               )
        # b_map = make_map_block(name, suffix="_t", prefix="", type_name=None, mod_name=None, members=None, io_nml=True, command_line=True, setget=False):
        b_maps.append(b_map)

    # write specs to file if required
    if args.write_json:
        with open(args.write_json,'w') as f:
            f.write(json.dumps(b_maps, indent=4))

    # also add the full fortran types, including a dynamic version (length or size guessed from input)
    for b_map in b_maps:
        for v_map in b_map['members']:
            _update_variable_type_info(v_map)

    print("...with types: "+", ".join([b_map["type_name"] for b_map in b_maps]))

    # Write the actual code
    code = make_source(b_maps, io_mod=io_mod, source=args.namelist, clen=args.clen)

    with open(io_file, 'w') as f:
        f.write(code)

    print("done.")

if __name__ == "__main__":
    main()
