""" Parse fortran 90 code and return a list of modules
"""
from __future__ import print_function
import re
from .ioparams import Variable, Group, Module

# Regular expression to find modules and type:
# ...first form: use with findall to get a tuple of all modules
# ...template form: format to find a specific module or group by name
# (the template form is the most useful one here)
MODULE_RE = r' *module +(?P<name>\w+) *\n(?P<defs>.*?)contains *\n(?P<body>.*?)end *module'
MODULE_RE_TEMPLATE = r' *module +{name} *\n(?P<defs>.*?)contains *\n(?P<body>.*?)end *module'
TYPE_RE = r'type( +| *:: *)(?P<name>) *\n(?P<defs>.*?)\n *end *type'
TYPE_RE_TEMPLATE = r'type( +| *:: *){name} *\n(?P<defs>.*?)\n *end *type'

def findall_modules(source):
    " return a list of tuples (name, defs, body) "
    return re.findall(MODULE_RE, source, re.DOTALL)

def search_module(name, source):
    " return a dict with keys 'defs' and 'body'"
    m = re.search(MODULE_RE_TEMPLATE.format(name=name), source, re.DOTALL)
    if m is None:
        raise ValueError("Module "+name+" was not found")
    return m.groupdict()

def search_type(name, source):
    " search for a specific type definitions "
    m = re.search(TYPE_RE_TEMPLATE.format(name=name), source, re.DOTALL)
    if m is None:
        raise ValueError("Type "+name+" was not found")
    return m.groupdict()

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
    ('real', 'dp', 31)
    >>> parse_vartype("real (kind=8)")
    ('real', 'kind=8', None)
    >>> parse_vartype("real")
    ('real', None, None)
    >>> parse_vartype("real, dimension(3)")
    ('real', None, 3)
    """
    # left-hand side of "::"
    type_re = "(?P<dtype>\w+)(\((?P<attrs>.*?)\))*(,dimension\((?P<size>.+?)\))*"
    m = re.search(type_re, string.replace(" ",""))
    if m is None:
        raise ValueError("Failed to parse var type : "+string)
    d = m.groupdict()
    if d["size"] is None:
        size = None
    else:
        size = int(d["size"])
    return d["dtype"], d["attrs"], size

def parse_varname(string):
    """ right-hand size of "::" 
    returns a list of dict with keys "name", "value", "size"
    >>> parse_varname("a(36)=45, b")
    [{'name': 'a', 'value': '45', 'size': '36'}, {'name': 'b', 'value': None, 'size': None}]
    """
    name_re = "(?P<name>\w+)(\((?P<size>\d+)\))*(=(?P<value>\w+))*"
    variables = []
    for m in re.finditer(name_re, string.replace(" ","")):
        variables.append(m.groupdict())
    return variables

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
    dtype, attrs, size = parse_vartype(typedef)

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

def parse_modules(string):
    """ return all modules from a bunch of source code
    """
    # first remove empty lines and comments, for a start...
    string = _clean_code(string)
    matches = re.findall(' *module +(?<name>\w+) *\n(?<content>.*?)contains *\n *end *module', string, re.DOTALL)
    modules = []
    for name, content in matches:
        mod = Module(name)
        for group in parse_module(content):
            mod.append_group(group)
        modules.append(mod)
    return modules

def main():
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

if __name__ == "__main__":
    main()
