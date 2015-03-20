#!/usr/bin/env python2
"""Generates a fortran module from a namelist.

Usage:
    python nml2f90.py <namelist.nml> <ioparams>

It will read the namelist.nml input file and output ioparams.f90, that 
contains corresponding parameter types and I/O setter/getter functions.
"""
import sys, os
from collections import OrderedDict as odict
from namelist import Namelist, read_namelist_file

#
# TO BE UPDATED BY THE USER, IF NEEDED
#

# new module name to be generated with all io routines
io_mod="ioparams"

# returns derived type name based on namelist group name
# it was taken to match nml's test program
def derived_type_name(group):
    # return "pars_"+group.lower()
    return group.lower()+'_t'

# character length
clen = 256

#
# BELOW CODE IS FINE
#
_dir = os.path.abspath(os.path.dirname(__file__))

# load templates
template_module = """
module {io_module_name}
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Automatically generated module
    ! 
    ! Contains read / write subroutines for all derived types imported below.
    ! As well as setter / getter access by field name
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    implicit none

    private
    public :: read_nml, write_nml, set_param, get_param
    public :: {list_of_types}

    integer, parameter :: dp = kind(0.d0)

    {type_definitions}

    interface read_nml
        {read_nml_proc}
    end interface

    interface write_nml
        {write_nml_proc}
    end interface

    interface set_param
        {set_param_proc}
    end interface

    interface get_param
        {get_param_proc}
    end interface

contains

    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! IO routines
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    {io_routines}

    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! SET / GET routines
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    {setget_routines}

end module {io_module_name}
"""

template_io = """
subroutine read_nml_{group} (iounit, params)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Read the {group} block in a namelist file and assign to type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    integer, intent(in) :: iounit
    type({type_name}), intent(inout) :: params

    {variable_definitions}

    namelist / {group} / {list_of_variables}

    ! initialize variables
    {list_of_init}

    ! read all
    read(unit=iounit, nml={group}) 

    ! assign back to type
    {list_of_assign}
end subroutine

subroutine write_nml_{group} (iounit, params)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Read the {group} block in a namelist file and assign to type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    integer, intent(in) :: iounit
    type({type_name}), intent(inout) :: params

    {variable_definitions}

    namelist / {group} / {list_of_variables}

    ! initialize variables
    {list_of_init}

    ! write_all
    write(unit=iounit, nml={group}) 
end subroutine
"""

template_setget = """
subroutine set_param_{group}_{vtype_name} (params, name, value)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Set one field of the {group} type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type({type_name}), intent(inout) :: params
    character(len=*), intent(in) :: name
    {vtype}, intent(in) :: value

    select case (name) 
        {list_set_cases}
        case default
        write(*,*) "ERROR set_param for {group}: unknown type member: {vtype} :: ",trim(name)
            stop
    end select
end subroutine

subroutine get_param_{group}_{vtype_name} (params, name, value)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Set one field of the {group} type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type({type_name}), intent(inout) :: params
    character(len=*), intent(in) :: name
    {vtype}, intent(out) :: value

    select case (name) 
        {list_get_cases}
        case default
            write(*,*) "ERROR get_param for {group}: unknown type member {vtype} :: ",trim(name)
            stop
    end select
end subroutine
"""

def _get_vtype(v, charlen=clen, acharlen=clen):
    """ return fortran type for a particular namelist variable
    """
    vtype_short = ""
    if type(v) is int:
        vtype = "integer"
    elif type(v) is bool:
        vtype = "logical"
    elif type(v) is float:
        vtype = "real(dp)"
        vtype_short = "double"
    elif type(v) is str:
        vtype = "character(len={})".format(charlen or len(v))
        vtype_short = "char"
    elif type(v) is list:
        vtype0, vtype0_short = _get_vtype(v[0], charlen, acharlen)
        vtype = "{vtype}, dimension({len})".format(vtype=vtype0, len=len(v)) 
        vtype_short = vtype0_short+'_arr'
    else:
        print "Error with: ", v
        print "Unexpected type: ", type(v)
        raise TypeError("Unknown type")
    vtype_short = vtype_short or vtype
    return vtype, vtype_short

def get_format_io(params):
    """ Create I/O source code from a namelist template

    params : dict of dict (params[group][param_name])
    """
    groups = params.keys()
    types = [derived_type_name(g) for g in groups]

    io_routines = []
    read_nml_interface = []
    write_nml_interface = []

    # Namelist I/O for whole groups
    for G, t in zip(groups, types):

        # fill-in...
        variable_definitions = []
        list_of_variables = []
        list_of_init = []
        list_of_assign = []

        g = G.lower()

        for K in params[G]:
            k = K.lower()
            vtype, _ = _get_vtype(params[G][K])
            variable_definitions.append("{vtype} :: {name}".format(vtype=vtype, name=k))
            list_of_variables.append(k)
            list_of_init.append("{name} = params%{name}".format(name=k, group=g))
            list_of_assign.append("params%{name} = {name}".format(name=k, group=g))

        fmt = dict(
            group=g,
            type_name = t,
            variable_definitions="\n    ".join(variable_definitions),
            list_of_variables = ", ".join(list_of_variables),
            list_of_init = "\n    ".join(list_of_init),
            list_of_assign = "\n    ".join(list_of_assign),
        )

        io_routines.append( template_io.format(**fmt) )

        read_nml_interface.append("module procedure :: read_nml_{g}".format(g=g)) 
        write_nml_interface.append("module procedure :: write_nml_{g}".format(g=g)) 

    # source_code = template_module.format(types = ", ".join(types), 
    fmt = dict(types = ", ".join(types), 
               io_routines = "\n\n".join(io_routines),
               read_nml_proc = "\n        ".join(read_nml_interface),
               write_nml_proc = "\n        ".join(write_nml_interface),
               )

    return fmt


template_get_cases = """
case ('{vname}')
    value = params%{vname}
"""
template_set_cases = """
case ('{vname}')
    params%{vname} = value
"""

def get_format_setget(params):
    """ Create set_param/get_param source code from a namelist template

    params : dict of dict (params[group][param_name])
    """
    groups = params.keys()
    types = [derived_type_name(g) for g in groups]

    get_param_interface = []
    set_param_interface = []

    setget_routines = []

    # loop over derived types
    for G, t in zip(groups, types):

        g = G.lower()

        # determine the list of all variable types present in this derived type
        # build a dict of [(vtype1: [p11, p12, ...]), ...]
        # and of the vtpyes shortnames to name routines
        vtypes = {}
        vtypes_short = {}
        for K in params[G].keys():
            k = K.lower()
            vtype, vtype_short = _get_vtype(params[G][K], charlen="*")
            if vtype not in vtypes:
                vtypes[vtype] = []
            vtypes[vtype].append(k)
            vtypes_short[vtype] = vtype_short

        # loop over all types
        for vtype in vtypes:
            vtype_short = vtypes_short[vtype]
            # print 'group:',g, 'vtype', vtype
            list_get_cases = []
            list_set_cases = []
            for k in vtypes[vtype]:
                list_get_cases.append( template_get_cases.format(vname=k) )
                list_set_cases.append( template_set_cases.format(vname=k) )

            # the subroutines
            setget_routines.append(

                template_setget.format(
                    vtype=vtype,
                    vtype_name=vtype_short,
                    group=g, # namelist-like group (shorter)
                    type_name= derived_type_name(g), # actual derived type
                    list_get_cases= "\n".join(list_get_cases),
                    list_set_cases= "\n".join(list_set_cases),
                )
            )

            # prepare interface of all subroutines
            get_param_interface.append(
                "module procedure :: get_param_{group}_{vtype_name}".format(vtype_name=vtypes_short[vtype], group=g)
            )
            set_param_interface.append(
                "module procedure :: set_param_{group}_{vtype_name}".format(vtype_name=vtypes_short[vtype], group=g)
            )


    fmt = dict(
        types = ", ".join(types), 
        set_param_proc = "\n        ".join(set_param_interface),
        get_param_proc = "\n        ".join(get_param_interface),
        setget_routines = "\n\n".join(setget_routines),
    )

    return fmt


def get_format_typedef(params):
    """ Fill-in variable names for derived type definition

    params : dict of dict (params[group][param_name])
    """
    groups = params.keys()

    type_definitions = []
    list_of_types = []

    # loop over derived types
    for G in groups:

        # fill-in...
        variable_definitions = []

        g = G.lower()

        for K in params[G]:
            k = K.lower()
            vtype, _ = _get_vtype(params[G][K])
            variable_definitions.append("{vtype} :: {name}".format(vtype=vtype, name=k))

        dtype = derived_type_name(G)
        list_of_types.append(dtype)

        dtype_def = """
    type {dtype} 
        {vdef}
    end type
        """.format(dtype=dtype, vdef="\n        ".join(variable_definitions))

        type_definitions.append(dtype_def)

    fmt = dict(
        type_definitions = "\n    ".join(type_definitions),
        list_of_types = ", ".join(list_of_types),
    )

    return fmt


def make_source(params, io_mod):
    """ Make source code with I/O and getter / setter
    """
    fmt = dict(
        io_module_name = io_mod,
    )
    fmt.update( 
        get_format_io(params) 
    )
    fmt.update( 
        get_format_setget(params)
    )
    fmt.update( 
        get_format_typedef(params)
    )
    return template_module.format(**fmt)

if __name__ == "__main__":

    if len(sys.argv) < 2:
        input_nml = "namelist.nml"
    else:
        input_nml = sys.argv[1]

    if len(sys.argv) == 3:
        io_mod = sys.argv[2]

    if io_mod.endswith(".f90"):
        io_file = io_mod  
        io_mod = io_file[:-4]
    else: 
        io_file = io_mod + ".f90"

    if len(sys.argv) > 3 or input_nml in ("-h", "--help"):
        print __doc__
        sys.exit()

    print "Convert "+input_nml+" to "+io_file
    print "...new module: "+io_mod

    # read namelist template
    # nml = read_namelist_file("namelist.template.nml")
    nml = read_namelist_file(input_nml)
    params = nml.groups

    print "...with types: "+", ".join([derived_type_name(g) for g in params.keys()])

    code = make_source(params, io_mod)
    with open(io_file, 'w') as f:
        f.write(code)

    print "done."
