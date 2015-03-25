""" Base class to describe a parameter
"""
from __future__ import print_function

class Param(object):
    def __init__(self, name="", value=None, group="", help="", units="", **kwargs):
        self.name = name
        self.value = value
        self.group = group
        self.help = help  # .e.g. "blabla ({units})"
        self.units = units
        if (len(kwargs) > 0):
            warnings.warn("unknown parameters to Param were ignored: "+", ".join(kwargs.keys()))
        # self.__dict__.update(kwargs)

    @property
    def key(self):
        " unique ID "
        return (self.group, self.name)

    def __eq__(self, other):
        return self.key == other.key

    def __repr__(self):
        return "Param(name=%r, value=%r, group=%r)" % (self.name, self.value, self.group)

class Params(list):
    """ list of parameters
    """
    def __init__(self, *args):
        # make sure we have a list
        list.__init__(self, *args)
        for p in self:
            if not isinstance(p, Param):
                print(type(p),":",p)
                raise TypeError("Params can only contain Param instances")

    def append(self, param):
        if not isinstance(param, Param):
            raise TypeError("Params can only contain Param instances")
        list.append(self, param)

    def to_nml(self):
        return Namelist(self)

    def to_json(self, **kwargs):
        import json
        return json.dumps([vars(p) for p in self], **kwargs)

    @classmethod
    def from_json(cls, string):
        import json
        return cls([Param(**p) for p in json.loads(string)])

    # generic method to be overloaded, default to json
    parse = from_json
    format = to_json

    def write(self, filename, mode='w', **kwargs):
        with open(filename, mode) as f:
            f.write(self.format(**kwargs))

    @classmethod
    def read(cls, filename):
        with open(filename) as f:
            params = cls.parse(f.read())
        return params

    def __repr__(self):
        return "{cls}({list})".format(cls=self.__class__.__name__, list=list.__repr__(self))

