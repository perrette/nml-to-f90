from __future__ import print_function, absolute_import
import sys, os, json
from itertools import groupby
import logging
import warnings
from collections import OrderedDict as odict
# import datetime
import textwrap

from .namelist import Namelist, Param
from . import __version__

def get_call_string():
    " return command-line string of the call "
    caller = os.path.basename(sys.argv[0]) # remove prefix (e.g. /home/.../..)
    return " ".join([caller]+sys.argv[1:]) 

# +++++++++++++++++++++++++++++++++++++++++
# load templates
# +++++++++++++++++++++++++++++++++++++++++
template_dir = os.path.abspath(os.path.join(os.path.dirname(__file__),'templates'))
libraries_dir = os.path.abspath(os.path.join(os.path.dirname(__file__),'libraries'))
template_module = open(os.path.join(template_dir, "module_ioparams.f90")).read()

FORTRAN_INDENT = 2*" "
REAL_KIND = 8
INTEGER_KIND = 4
CHAR_LEN = 256
MODULE = "ioparams"

SPACE_NAME_HELP = 20  # character for help printing

def _get_default_type_value(dtype):
    if dtype == "real":
        value = 0.
    elif dtype == "integer":
        value = 0
    elif dtype == "character":
        value = ""
    elif dtype == "logical":
        value = False
    else:
        raise ValueError("unrecognized type: "+dtype)
    return value

def _get_default_value(dtype, size):
    val = _get_default_type_value(dtype)
    if size is None:
        return val
    else:
        return  [val]*size

class Variable(object):
    def __init__(self, name=None, value=None, units="", help="", attrs=None, dtype=None, group=None, size=None):

        if dtype is None:
            dtype = self._get_elemental_ftype(value)

        # precision?
        if attrs is None:
            if dtype == "real":
                attrs = "kind=dp" 
            elif dtype == "integer":
                attrs = "kind=ip"
            elif dtype == "character":
                attrs = "len=clen"

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
        attrs = self.attrs
        if attrs is not None:
            if self.dtype == "character" and dynamic: 
                attrs = "len=*"
            dtype = self.dtype+"("+attrs+')'
            # dtype = self.dtype+"("+",".join(["{}={}".format(k,self.attrs[k]) for k in self.attrs])+')'

        if self.array:
            if dynamic:
                size = ":"
            else:
                size = self.size
            assert size is not None, "array has no attached size: only possible when dynamic is True"
            dtype = dtype + ", dimension({})".format(size)
        return dtype

    def format(self, dynamic=False, indent=0, include_comment=False, include_value=False):
        vardef = indent*" "+self.format_type(dynamic)+" :: "+self.name
        # if include_value and self.value:
        #     vardef += " = " +  _format_value(self.value) # written for namelist, probably need to adapt for source code
        if include_comment and self.help:
            vardef += " ! "+self.help
        return vardef

    def to_param(self):
        """ Return a Param object and assign default value if undefined.
        """
        if self.value is None:
            value = _get_default_value(self.dtype, self.size)
            warnings.warn("No default value found for {} :: {}, assume default to {}.".format(self.group, self.name, value))
        else:
            value = self.value
        return Param(self.name, value, self.group, self.help, self.units)

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

    def update(self, other, append_variable=False):
        """ update fields from another group (e.g. parsed from source)

        other: other Group instance with same type
        append_variable: if True, append new variables from other
            (default to False)
        """
        assert self.type_name == other.type_name, "types differ !"
        self.mod_name = other.mod_name  # module name is defined

        for o in other.variables:
            matches = [v for v in self.variables if o.name == v.name]
            assert len(matches) > 0 , "variable not found in source code in type {} : {}".format(self.type_name, o.name)
            assert len(matches) == 1  # > 1 would make no sense, just in case

            v = matches[0]
            # print("Update group",self.name,"variable",v.name,v.dtype,"==>",o.dtype)
            if o.dtype != v.dtype:
                logging.info("Update dtype for {}: {} => {}".format(v.name, v.dtype, o.dtype))
            v.dtype = o.dtype
            v.attrs = o.attrs
            if v.size != o.size: 
                print("group name:",self.name, "type name:",self.type_name)
                print("v: {!r}, other: {!r}".format(v.size, o.size))
                raise ValueError("Sizes do not match.")

            v.help = o.help or v.help
            v.units = o.units or v.units
            v.value = o.value or v.value

    def to_nml(self):
        """ Return a Namelist object matching the type
        """
        nml = Namelist()
        for v in self.variables:
            nml.append(v.to_param())
        return nml

class Module(object):
    """ The ioparams fortan module
    """
    def __init__(self, name=MODULE, verbose=False, char_len=CHAR_LEN, real_kind=REAL_KIND, int_kind=INTEGER_KIND):
        self.name = name
        self.description = self.__doc__
        self.imports = []
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
            self.imports.append("use {}, only: {}".format(group.mod_name, group.type_name))
        self.public += self._make_public_declarations([group.type_name])

    def include_lib(self, lib, external=False):
        self.included_lib.append(lib)
        self.imports.append("use "+lib +"\n")
        # also include source code
        if not external:
            self.libcode +=  open(os.path.join(libraries_dir, lib+".f90")).read() + "\n"

    def append_feature(self, name, only=None):

        if name == "io_nml":
            feature = NmlIO()
        elif name == "io_nml_nml":
            feature = NmlLib()
        elif name == "command_line":
            feature = CommandLine()
        elif name == "set_get_param":
            feature = SetGetParam()
        else:
            raise ValueError("Unknown feature: "+name)

        if only is None:
            groups = self.groups
        else:
            groups = [g for g in self.groups if g.name in only]

        for group in groups:
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
        for lib in feature.external_dependencies:
            if lib not in self.included_lib:
                self.include_lib(lib, external=True)

        self.features.append(feature) # for the record

    def format(self, indent=2):

        code = self.template.format(
            module_name=self.name, 
            description="", 
            imports=("\n" + FORTRAN_INDENT).join(self.imports),
            public=_indent_bloc(self.public, indent),
            char_len=self.char_len,
            real_kind=self.real_kind,
            int_kind=self.int_kind,
            verbose=self.verbose,
            definition=_indent_bloc(self.definition, indent),
            content=self.content,
            version=__version__,
            # date=str(datetime.datetime.today()), # parsed file (e.g. namelist) for the generation of the module
            command_call=get_call_string(),
            features=", ".join([feature.name for feature in self.features]),
            libraries = self.libcode,
        )
        # now include libraries as separate module in the code

        # Now make sure that every line is < 80 character
        # except those which are pure comment
        short_code = ""
        wrap_opt = dict(break_long_words=False, break_on_hyphens=False, width=78)
        for line in code.split("\n"):
            if line.startswith('!'):
                short_code += line + '\n'
            else:
                sublines = textwrap.wrap(line, **wrap_opt)
                if "!" in line: 
                    for i, subl in enumerate(sublines):
                        if "!" in subl:
                            short_code += "\n! ".join(sublines[i:]) +'\n'# only comment, long line ok 
                            break
                        else:
                            short_code += subl+' &\n'
                else:
                    short_code += " & \n &".join(sublines) +"\n"
        code = short_code

        return code

    @staticmethod
    def _make_public_declarations(names):
        decl = ""
        for name in names:
            decl += "public :: "+name+'\n'
        return decl

    def to_nml(self):
        """ return a Namelist object with all parameters contained in the Module
        """
        nml = Namelist()
        for g in self.groups:
            nml.extend( g.to_nml() )
        return nml


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
    external_dependencies = []
    def __init__(self):
        self.template = open(os.path.join(template_dir, "subroutine_"+self.name+".f90")).read()
        self.content = ""
        self.interfaces = []
        self.group_names = []
        for k in self.public:
            self.interfaces.append(Interface(k))

class NmlIO(Feature):
    """ Human-readable Namelist I/O
    """
    name = "io_nml"
    public = ["read_nml", "write_nml"]

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

        self.group_names.append(group.name)

class NmlLib(Feature):
    """ Human-readable Namelist I/O
    """
    name = "io_nml_nml"
    public = ["read_nml","write_nml"]
    external_dependencies = ["nml"]

    def append_group(self, group):

        # For each parameter type, build a map specific to this routine
        list_of_nml_read_calls = ""
        list_of_nml_write_calls = ""

        for v in group.variables:
            list_of_nml_read_calls += "    call nml_read('', '{group}', '{name}', params%{name}, io=iounit, init=.true.)".format(name=v.name,group=group.name)+'\n'
            list_of_nml_write_calls += "    call nml_print('{name}', params%{name}, io=iounit)".format(name=v.name,group=group.name)+'\n'

        self.content += self.template.format(
            group_name = group.name,
            type_name = group.type_name,
            list_of_nml_read_calls = list_of_nml_read_calls,
            list_of_nml_write_calls = list_of_nml_write_calls,
        ) + '\n'

        # add procedure to be included in the interface
        for interface in self.interfaces:
            interface.append_procedure(interface.name+'_'+group.name)

        self.group_names.append(group.name)
#
#
class CommandLine(Feature):
    """ Command-line argument passing
    """
    name = "command_line"
    public = ["parse_command_argument", "print_help", "set_param_string", "has_param"]
    private = []
    dependencies = ["type_conversion"]

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

    template_set_string_case_char = """
case ('{name}', '{group}%{name}')
    params%{name} = trim(string)
    if (VERBOSE) write(*,*) "{group}%{name} = ", params%{name}
"""

    # set_param_string (array version)
    template_set_string_case_array = """
case ('{name}', '{group}%{name}')
    call string_to_array(string, params%{name}, iostat=iostat)
"""+ template_set_check

    # has_print help
    template_help = """
write(nameshort, *) "{name}"
if (def) then
    write(valuestr, *) params%{name}
    write(valuelen, *) max(1,len(trim(adjustl(valuestr))))
    write(io, '("  --",A{len},"{help} (default: ",A'//trim(valuelen)//',")")') adjustl(nameshort), trim(adjustl(valuestr))
else
    write(io, '("  --",A{len},"{help}")') adjustl(nameshort)
endif
"""
    # !write(io, *) "--{name}"," {help} (default: ",trim(adjustl(valuestr))," )"
    # write(io, *) "--{name}"," {help} (type: {type})"
    # has_print help (array version)
    template_help_array = """
write(nameshort, *) "{name}"
if (def) then
    write(valuestr, *) params%{name}(1) ! only first element
    write(valuelen, *) max(1,len(trim(adjustl(valuestr))))
    write(io, '("  --",A{len},"{help} (default: &
        [",A'//trim(valuelen)//',", ...], size=",I2,")")') adjustl(nameshort), trim(adjustl(valuestr)), size(params%{name})
else
    write(io, '("  --",A{len},"{help}")') adjustl(nameshort)
endif
"""

    def append_group(self, group):

        # map the routine
        list_set_cases = ""
        list_has_cases = ""
        list_help = ""
        for v in group.variables:

            v_map = dict(name=v.name, group=v.group, type=v.format(), help=v.help.replace('"'," ").replace("'",' '), len=SPACE_NAME_HELP)
            # has_params routines
            list_has_cases += "case ('{name}', '{group}%{name}')".format(name=v.name, group=v.group)+'\n'
            if v.array:
                list_set_cases += self.template_set_string_case_array.format(**v_map)
                list_help += self.template_help_array.format(**v_map)
            else:
                if v.dtype == "character":
                    list_set_cases += self.template_set_string_case_char.format(**v_map)
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

        self.group_names.append(group.name)

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

        self.group_names.append(group.name)

