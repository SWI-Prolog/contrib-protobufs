# Test of test1.proto, test1.pl
# Assumes that swipl is on the execution path

import contextlib
import os
import shutil
import subprocess
import unittest
from test1_pb2 import Scalars1, Scalars2, Enum

SWIPL = shutil.which('swipl')

scalars1a = Scalars1(
    v_double   = 1.0,
    v_float    = 2.0,
    v_int32    = 3,
    v_int64    = 4,
    v_uint32   = 5,
    v_uint64   = 6,
    v_sint32   = 7,
    v_sint64   = 8,
    v_fixed32  = 9,
    v_fixed64  = 10,
    v_sfixed32 = 11,
    v_sfixed64 = 12,
    v_bool     = False,
    v_string   = 'écran 網目錦蛇',
    v_bytes    = b'\xc3\x28',  # See https://stackoverflow.com/questions/1301402/example-invalid-utf8-string
    v_enum     = Enum.E1,
)

# Test negative values - unsigned are caught by the Python runtime,
# so they get the values "100+..."
scalars1b = Scalars1(
    v_double   = -1.0,
    v_float    = -2.0,
    v_int32    = -3,
    v_int64    = -4,
    v_uint32   = 100+5,
    v_uint64   = 100+6,
    v_sint32   = -7,
    v_sint64   = -8,
    v_fixed32  = 100+9,
    v_fixed64  = 100+10,
    v_sfixed32 = -11,
    v_sfixed64 = -12,
    v_bool     = True,
    v_string   = 'écran 網目錦蛇',
    v_bytes    = b'\xf0\x28\x8c\x28',  # See https://stackoverflow.com/questions/1301402/example-invalid-utf8-string
    v_enum     = Enum.AnotherEnum,
)

class Test1(unittest.TestCase):

    def test1a(self):
        with contextlib.suppress(FileNotFoundError):
            os.remove('scalars1a_from_python.wire')
            os.remove('scalars1a_from_prolog.wire')
        with open('scalars1a_from_python.wire', 'wb') as f:
            f.write(scalars1a.SerializeToString())
        subprocess.run([SWIPL, 'test1.pl'], check=True)
        m2 = Scalars1()
        with open('scalars1a_from_prolog.wire', 'rb') as f:
            m2.ParseFromString(f.read())
        self.assertEqual(m2.v_double,    1.0)
        self.assertEqual(m2.v_float,     2.0)
        self.assertEqual(m2.v_int32,     3)
        self.assertEqual(m2.v_int64,     4)
        self.assertEqual(m2.v_uint32,    5)
        self.assertEqual(m2.v_uint64,    6)
        self.assertEqual(m2.v_sint32,    7)
        self.assertEqual(m2.v_sint64,    8)
        self.assertEqual(m2.v_fixed32,   9)
        self.assertEqual(m2.v_fixed64,   10)
        self.assertEqual(m2.v_sfixed32,  11)
        self.assertEqual(m2.v_sfixed64,  12)
        self.assertEqual(m2.v_bool,      False)
        self.assertEqual(m2.v_string,    'écran 網目錦蛇')
        self.assertEqual(m2.v_bytes,     b'\xc3\x28')
        self.assertEqual(m2.v_enum,      Enum.E1)
        with contextlib.suppress(FileNotFoundError):
            os.remove('scalars1a_from_python.wire')
            os.remove('scalars1a_from_prolog.wire')


if __name__ == '__main__':
    unittest.main()

