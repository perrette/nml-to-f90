#!/usr/bin/env python2.7
"""Generates a fortran module from a namelist.

This script will read the namelist.nml input file and output ioparams.f90, that 
contains corresponding parameter types and I/O, setter/getter routines.
"""
from __future__ import print_function, absolute_import
import sys, os, json
from itertools import groupby
from collections import OrderedDict as odict
import datetime
import warnings
import textwrap
import argparse

from .namelist import Namelist
from .version import version as __version__

# +++++++++++++++++++++++++++++++++++++++++
# load templates
# +++++++++++++++++++++++++++++++++++++++++
template_dir = os.path.abspath(os.path.join(os.path.dirname(__file__),'templates'))
libraries_dir = os.path.abspath(os.path.join(os.path.dirname(__file__),'libraries'))
template_module = open(os.path.join(template_dir, "module_ioparams.f90")).read()

REAL_KIND = 8
INTEGER_KIND = 4
CHAR_LEN = 256
MODULE = "ioparams"

class Variable(object):
    def __init__(self, name=None, value=None, units="", help="", attrs=None, dtype=None, group=None, size=None):

        if dtype is None:
            dtype = self._get_elemental_ftype(value)

        # precision?
        if attrs is None:
            if dtype == "real":
                attrs = {"kind":"dp"}  # e.g. kind=8 ; "float" is a module-wide parameter (default to REAL_KIND)
            elif dtype == "integer":
                attrs = {"kind":"ip"} # e.g. kind=4, "int" is a module-wide parameter
            elif dtype == "character":
                attrs = {"len":"clen"} # e.g. len=4 ; same, assume that clen is defined is module

        # array?
        if size is not None:
            array = True
        else:
            if type(value) is list:
                array = True
                size = len(value)
            else:
                array = False
                size = None

        self.name = name
        self.units = units
        self.help = help
        self.dtype = dtype
        self.attrs = attrs
        self.array = array
        self.size = size
        self.value = value
        self.group = group  # group to which this variable belongs

    @staticmethod
    def _get_elemental_ftype(value):
        """ get elemental type of a variable from its (python) value
        """
        _map_type = {
            "float" : "real",
            "int" : "integer",
            "str" : "character",
            "bool" : "logical",
            "NoneType" : None, # convenience  to that value does not have to be passed to make_map_variable
        }
        if type(value) is list:
            dtype = _map_type[type(value[0]).__name__]
        else:
            dtype = _map_type[type(value).__name__]
        return dtype

    def format_type(self,dynamic=False):
        """ make fortran type from variable map
        """
        dtype = self.dtype
        if self.attrs is not None:
            if self.dtype == "character" and dynamic: 
                self.attrs = {"len":"*"}
            dtype = self.dtype+"("+",".join(["{}={}".format(k,self.attrs[k]) for k in self.attrs])+')'

        if self.array:
            if dynamic:
                size = ":"
            else:
                size = self.size
            assert size is not None, "array has no attached size: only possible when dynamic is True"
            dtype = dtype + ", dimension({})".format(size)
        return dtype

    def format(self, dynamic=False, indent=0, include_comment=False):
        vardef = indent*" "+self.format_type(dynamic)+" :: "+self.name
        if include_comment and self.help:
            vardef += " ! "+self.help
        return vardef


class Group(object):
    """ Definition of a derived type
    """
    def __init__(self, name, type_name, mod_name=None):
        assert '!' not in name, "comment in group name !"
        self.name = name
        self.type_name = type_name
        self.mod_name = mod_name
        self.content = ""
        self.variables = []

    def append_variable(self, var):
        self.variables.append(var)
        self.content += var.format(indent=2, include_comment=True) + '\n'

    def format(self, indent=0):
        header = indent*" "+"type :: "+self.type_name
        footer = indent*" "+"end type"
        content = indent*" "+self.content.rstrip()
        return "\n".join([header, content, footer])

class Module(object):
    """ The ioparams fortan module
    """
    def __init__(self, name=MODULE, verbose=False, char_len=CHAR_LEN, real_kind=REAL_KIND, int_kind=INTEGER_KIND):
        self.name = name
        self.description = self.__doc__
        self.imports = ""
        self.public = ""
        self.definition =  ""
        self.content = ""
        self.included_lib = []
        self.libcode = ""
        self.verbose = ".true." if verbose else ".false."
        self.real_kind = real_kind
        self.int_kind = int_kind
        self.char_len = char_len

        self.template = template_module
        self.groups = []
        self.features = []

    def append_group(self, group):
        """ add a type
        """
        # loop over derived types
        self.groups.append(group)
        if group.mod_name is None:
            self.definition += group.format() + '\n\n'
        else:
            self.imports += "use {}; only: {}".format(group.mod_name, group.type_name) + '\n'
        self.public += self._make_public_declarations([group.type_name])

    def include_lib(self, lib):
        self.included_lib.append(lib)
        self.imports += "use "+lib +", dp_conflict => dp\n"
        self.libcode +=  open(os.path.join(libraries_dir, lib+".f90")).read() + "\n"

    def append_feature(self, name):

        if name == "io_nml":
            feature = NmlIO()
        elif name == "command_line":
            feature = CommandLine()
        elif name == "set_get_param":
            feature = SetGetParam()
        else:
            raise ValueError("Unknown feature: "+name)

        for group in self.groups:
            feature.append_group(group)

        self.description += feature.name + '\n' + '+'*20 + '\n'
        self.description += feature.__doc__ +2*'\n'
        self.public += self._make_public_declarations(feature.public)
        self.definition += "\n".join([interface.format()+"\n" for interface in feature.interfaces]) + '\n'
        # self.definition += "\n".join([self._make_interface_block(k, feature.interface[k]) for k in feature.interface]) + '\n'
        self.content += feature.content + '\n'

        for lib in feature.dependencies:
            if lib not in self.included_lib:
                self.include_lib(lib)

        self.features.append(feature) # for the record

    def format(self, indent=2):

        code = self.template.format(
            module_name=self.name, 
            description="", 
            imports=self.imports, 
            public=_indent_bloc(self.public, indent),
            char_len=self.char_len,
            real_kind=self.real_kind,
            int_kind=self.int_kind,
            verbose=self.verbose,
            definition=_indent_bloc(self.definition, indent),
            content=self.content,
            version=__version__,
            date=str(datetime.datetime.today()), # parsed file (e.g. namelist) for the generation of the module
            command_call=" ".join(sys.argv), # parsed file (e.g. namelist) for the generation of the module
            features=", ".join([feature.name for feature in self.features]),
            libraries = self.libcode,
        )
        # now include libraries as separate module in the code

        # Now make sure that every line is < 80 character
        maxl = 78
        short_code = ""
        wrap_opt = dict(break_long_words=False, break_on_hyphens=False)
        for line in code.split("\n"):
            sublines = textwrap.wrap(line, **wrap_opt)
            if "!" in line: 
                for i, subl in enumerate(sublines):
                    if "!" in subl:
                        short_code += "\n! ".join(sublines[i:]) +'\n'# only comment, long line ok 
                        break
                    else:
                        short_code += subl+' &\n'
            else:
                short_code += " & \n".join(sublines) +"\n"
        code = short_code

        return code

    @staticmethod
    def _make_public_declarations(names):
        decl = ""
        for name in names:
            decl += "public :: "+name+'\n'
        return decl

def _indent_bloc(block, indent=2, initial_indent=0):
    " indent all lines but first line "
    lines = block.split("\n")
    for i, line in enumerate(lines):
        if i==0: 
            lines[i] = initial_indent*" "+line
        else:
            lines[i] = indent*" "+line
    return "\n".join(lines)

class Interface(object):
    def __init__(self, name):
        self.name = name
        self.content = ""
    def append_procedure(self, name):
        self.content += "  module procedure :: "+name +'\n'
    def format(self, indent=0):
        header = indent*" "+"interface "+self.name
        footer = indent*" "+"end interface "
        return "\n".join([header, indent*" "+self.content.rstrip(), footer])
        
class Feature(object):
    name = ""
    public = [] # to include as public (assume an interface for each)
    dependencies = []
    def __init__(self):
        self.template = open(os.path.join(template_dir, "subroutine_"+self.name+".f90")).read()
        self.content = ""
        self.interfaces = []
        for k in self.public:
            self.interfaces.append(Interface(k))

class NmlIO(Feature):
    """ Human-readable Namelist I/O
    """
    name = "io_nml"
    public = ["read_nml", "write_nml"]
    dependencies = ["type_conversion"]

    def append_group(self, group):

        # For each parameter type, build a map specific to this routine
        variable_definitions = []
        list_of_variables = []
        assign_namelist = []
        assign_type = []
        for v in group.variables:
            variable_definitions.append( v.format() )
            list_of_variables.append(v.name)
            assign_namelist.append("{name} = params%{name}".format(name=v.name))
            assign_type.append("params%{name} = {name}".format(name=v.name))

        self.content += self.template.format(
            group_name = group.name,
            type_name = group.type_name,
            variable_definitions = "\n    ".join(variable_definitions),
            list_of_variables = ", ".join(list_of_variables),
            assign_namelist = "\n    ".join(assign_namelist),
            assign_type = "\n    ".join(assign_type),
        ) + '\n'

        # add procedure to be included in the interface
        for interface in self.interfaces:
            interface.append_procedure(interface.name+'_'+group.name)

#
#
class CommandLine(Feature):
    """ Command-line argument passing
    """
    name = "command_line"
    public = ["parse_command_argument", "print_help", "set_param_string", "has_param"]
    private = []

    # 
    # Some templates additional to the .f90 files, for bits of code
    #

    template_set_check = """
    if (IOSTAT /= 0) then 
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --{group}%{name}"
        else
            write(*,*) "ERROR converting string to {type}: --{group}%{name} ",trim(string)
        endif
        stop
    endif
    """

    # set_param_string
    template_set_string_case = """
case ('{name}', '{group}%{name}')
    read(string, *, iostat=IOSTAT) params%{name}
    if (VERBOSE .or. IOSTAT/=0) write(*,*) "{group}%{name} = ", params%{name}
"""+ template_set_check

    # set_param_string (array version)
    template_set_string_case_array = """
case ('{name}', '{group}%{name}')
    call string_to_array(string, params%{name}, iostat=iostat)
"""+ template_set_check

    # has_print help
    template_help = """
if (def) then
    write(valuestr, *) params%{name}
    write(io, *) "--{name} {help} (default: ",trim(adjustl(valuestr))," )"
else
    write(io, *) "--{name} {help} (type: {type})"
endif
"""
    # has_print help (array version)
    template_help_array = """
if (def) then
    write(valuestr, *) params%{name}(1) ! only first element
    write(valuelen, *) len(trim(adjustl(valuestr)))
    write(io, '("--{name} {help} (default: &
        [",A'//trim(valuelen)//',", ...], size=",I2,")")') &
            trim(adjustl(valuestr)), size(params%{name})
else
    write(io, *) "--{name} {help} (type: {type})"
endif
"""

    def append_group(self, group):

        # map the routine
        list_set_cases = ""
        list_has_cases = ""
        list_help = ""
        for v in group.variables:

            v_map = dict(name=v.name, group=v.group, type=v.format(), help=v.help)
            # has_params routines
            list_has_cases += "case ('{name}', '{group}%{name}')".format(name=v.name, group=v.group)+'\n'
            if v.array:
                list_set_cases += self.template_set_string_case_array.format(**v_map)
                list_help += self.template_help_array.format(**v_map)
            else:
                list_set_cases += self.template_set_string_case.format(**v_map)
                list_help += self.template_help.format(**v_map)

        self.content += self.template.format(
            list_set_cases = list_set_cases,
            list_has_cases = list_has_cases,
            list_help = list_help,
            group_name = group.name,
            type_name = group.type_name,
        ) + '\n'

        # add procedure to be included in the interface
        for interface in self.interfaces:
            interface.append_procedure(interface.name+'_'+group.name)

class SetGetParam(Feature):
    """Interface that covers all derived types and the type
    of contained variables to generically set or get parameter values:

    call set_param(params_type, name, value)
    call get_param(params_type, name, value)
    """
    name = "set_get_param"
    public = ["set_param", "get_param"]

    # when selecting parameter values, also accept argument names with "group_name%name" syntax
    template_get_cases = """
    case ('{name}', '{group}%{name}')
        value = params%{name}
    """
    template_set_cases = """
    case ('{name}', '{group}%{name}')
        params%{name} = value
    """
    def append_group(self, group):
        """ Create set_param/get_param source code from a namelist template

        params : dict of dict (params[group_name][param_name])
        """
        # function to return interface type from variable
        interface_type = lambda v : v.dtype + v.array * "_arr" 

        for t, variables in groupby(sorted(group.variables, key=interface_type), interface_type):
            # vtype_short = vtypes_short[vtype]
            list_get_cases = ""
            list_set_cases = ""
            for v in variables:
                list_get_cases += self.template_get_cases.format(name=v.name, group=v.group)
                list_set_cases += self.template_set_cases.format(name=v.name, group=v.group)

            self.content += self.template.format(
                type_interface=t,
                group_name=group.name,
                type_name=group.type_name,
                type=v.format_type(dynamic=True),
                list_get_cases=list_get_cases,
                list_set_cases=list_set_cases,
            ) + '\n'

            # add procedure to be included in the interface
            for interface in self.interfaces:
                interface.append_procedure(interface.name+'_'+group.name+'_'+t)

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
        args.io_mod = args.command_line = args.set_param = True

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

    code = mod.format()

    with open(io_file, 'w') as f:
        f.write(code)

    print("done.")

if __name__ == "__main__":
    main()
