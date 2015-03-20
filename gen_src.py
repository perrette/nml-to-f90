""" python program to generate fortran source code capable of read/write, set/get parameters from a list of types
"""
from collections import OrderedDict as odict
from namelist import Namelist, read_namelist_file

#
# TO BE UPDATED BY THE USER, IF NEEDED
#
# module name where parameter types are defined
params_mod="params" 

# new module name to be generated with all io routines
io_mod="ioparams"

# returns derived type name based on namelist group name
# it was taken to match nml's test program
def derived_type_name(group):
    return "pars_"+group.lower()

# character length
clen = 256

#
# BELOW CODE IS FINE
#

# load templates
template_module = open("templates/module_ioparams.f90").read()
template_io = open("templates/subroutines_io.f90").read()
template_setget = open("templates/subroutines_setget.f90").read()


def _get_vtype(v, charlen=None, acharlen=None):
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
        vtype0, vtype0_short = _get_vtype(v[0], acharlen)
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
            vtype, _ = _get_vtype(params[G][K], charlen=clen, acharlen=clen)
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


def make_source(params, params_mod, io_mod):
    """ Make source code with I/O and getter / setter
    """
    fmt = dict(
        io_module_name = io_mod,
        params_module_name = params_mod,
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
    # when the types are to be output in a dummy, temporary 
    # namelist, they are grouped under this name (this can 
    # stay liek this without any impact, so leave it)
    dummygroup="ALLPARAMS"

    # read namelist template
    nml = read_namelist_file("namelist.template.nml")
    if len(nml.groups.keys()) == 1 and dummygroup in nml.groups.keys():
        print "Parse from a mixed-up, grouped dummy namelist"
        params = odict()
        for k in nml.groups[dummygroup].keys():
            g, nm = k.split("%")
            if g not in params:
                params[g] = odict()
            params[g][nm] = nml.groups[dummygroup][k]
    else:
        print "Parse from an example namelist (not a dummy one)"
        print nml.groups.keys()
        params = nml.groups

    code = make_source(params, params_mod, io_mod)
    with open(io_mod+'.f90', 'w') as f:
        f.write(code)
