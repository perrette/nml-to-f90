""" Parse fortran 90 code and return a list of modules
"""
from __future__ import print_function
import re
from .ioparams import Variable, Group, Module

def _clean_code(string):
    lines = []
    for line in string.splitlines():
        if '!' in line:
            line = line[:line.index('!')]
        line = line.strip()
        if line == "":
            continue
        lines.append(line)
    return "\n".join(lines)

def parse_vartype(string):
    """
    >>> parse_vartype("real(dp), dimension(31)")
    ('real', 'dp', True, 31)
    >>> parse_vartype("real (kind=8)")
    ('real', 'kind=8', False, None)
    >>> parse_vartype("real")
    ('real', None, False, None)
    >>> parse_vartype("real, dimension(3)")
    ('real', None, True, 3)
    """
    string = string.strip().replace(" ","") # remove white spaces

    # check main type
    dtype = re.search('\w+', string).group()

    # check if array
    m = re.search('(?<=dimension)\((.*)\)', string)
    if m is not None:
        array = True
        # size = int(m.group()[1:-1])
        size = int(m.group(1))
    else:
        array = False
        size = None

    # Check attributes, e.g. real(8) or real(kind=4)
    m = re.search('(?<={dtype})\((.*?)\)'.format(dtype=dtype), string)
    if m is not None:
        attrs = m.group(1)
    else:
        attrs = None

    return dtype, attrs, array, size

def parse_line(string):
    """
    Retrieve following:
    type1(...=...), dimension(N) :: var1 = val1, var2 = val2 ! comment
    """
    # comments
    if "!" in string:
        i = string.index("!")
        comment = string[i+1:]
        string = string[:i]
    else:
        comment = None

    # type :: variables
    try:
        typedef, variables = string.split("::")
    except:
        print(string)
        raise ValueError("Error when parsing source code. `::` missing in variable definition")

    # Un-build fortran type
    dtype, attrs, array, size = parse_vartype(typedef)

    # var1, var2
    var_defs = variables.split(",")

    # var1 = val1
    variables = []
    for i, v in enumerate(var_defs):
        if '=' in v:
            v, val = v.split("=")
        else:
            val = None
        var = Variable(name=v, value=val, dtype=dtype, attrs=attrs, size=size)
        # var = Variable(name=v, value=val, dtype=dtype, attrs=attrs, array=array, size=size)
        variables.append(var) 

    return variables

def parse_type(string):
    """ parse type content (inside type end type)
    """
    variables = []
    for line in string.splitlines():
        variables.extend( parse_line(line) )
    return variables

def parse_module(string, prefix='', suffix='_t'):
    """ parse module content (inside module end module)
    prefix and suffix: determine group name from type name...
    """
    matches = re.findall('(?<=type)( +| *:: *)(\w+) *\n(.*?)\n(?=end type)', string, re.DOTALL)
    types = []
    for _, name, content in matches:
        m = re.search("(?<={prefix})(.*)(?={suffix})".format(prefix=prefix,suffix=suffix),name) # for now
        if m is not None:
            group_name = m.group()
        else:
            group_name = name+'_group'
        typ = Group(group_name,type_name=name)
        for var in parse_type(content):
            typ.append_variable(var)
        types.append(typ)
        # variables = parse_type(content)
    return types

def parse_file(string):
    # first remove empty lines and comments, for a start...
    string = _clean_code(string)
    matches = re.findall('(?<=module) +(\w+) *\n(.*?)\n(?=end module)', string, re.DOTALL)
    modules = []
    for name, content in matches:
        mod = Module(name)
        for group in parse_module(content):
            mod.append_group(group)
        modules.append(mod)
    return modules

if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("sourcefile")
    args = parser.parse_args()

    with open(args.sourcefile) as f:
        code = f.read()

    modules = parse_file(code)
    for mod in modules:
        # print(mod)
        print(mod.format())


    # # ..., dimension(N)  : array True or False, size?
    # if ',' in string:
    #     try:
    #         type0, dimension = string.split(',')
    #         assert 'dimension' in dimension, "unknown attribute: "+dimension
    #     except:
    #         print("Expected 'dimension' attribute")
    #         print("Got", string)
    #         raise ValueError("Failed parsing variable attributes: "+string)
    #
    #     # NOTE: re.findall(r'a*\(.*\)','real(kind=435)')
    #     dimension = dimension.strip()
    #     try:
    #         size_str = dimension[len("dimension")+1:-1]
    #         size = int(size)
    #     except Exception as error:
    #         print(erorr.message)
    #         raise ValueError("Failed to retrieve size from dimension(...) :"+string)
    #     array = True
    # else:
    #     type0 = string
    #     array = False
    #     size = None
    #
    # # now get real(kind=dp)
    # if '=' in type0
    #
    # return dtype, attrs, array, size
