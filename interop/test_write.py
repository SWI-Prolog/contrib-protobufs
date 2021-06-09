# Test of test.proto, test.pl
# Assumes that swipl is on the execution path

import os
from test_pb2 import Scalars1, Repeated1, Packed1, MyEnum, KeyValue, M0, M1, M2

scalars1a = Scalars1(
    v_double    =  1.5,
    v_float     =  2.5,
    v_int32     =  3,
    v_int64     =  4,
    v_uint32    =  5,
    v_uint64    =  6,
    v_sint32    =  7,
    v_sint64    =  8,
    v_fixed32   =  9,
    v_fixed64   = 10,
    v_sfixed32  = 11,
    v_sfixed64  = 12,
    v_bool      = False,
    v_string    = "écran 網目錦蛇",
    v_bytes     = b"\xc3\x28",  # See https://stackoverflow.com/questions/1301402/example-invalid-utf8-string
    v_enum      = MyEnum.E1,
    v_key_value = KeyValue(key="reticulated python", value="網目錦蛇"),
)
# Test negative values - unsigned are caught by the Python runtime,
# so they get the values "10000000+..."
scalars1b = Scalars1(
    v_double    =  -1.5,
    v_float     =  -2.5,
    v_int32     =  -3,
    v_int64     =  -4,
    v_uint32    =   5+10000000,
    v_uint64    =   6+10000000,
    v_sint32    =  -7,
    v_sint64    =  -8,
    v_fixed32   =   9+1000,
    v_fixed64   =  10+1000,
    v_sfixed32  = -11,
    v_sfixed64  = -12,
    v_bool      = True,
    v_string    = "[àmímé níshíkíhéꜜbì] reticulated python",
    v_bytes     = b"\xf0\x28\x8c\x28",  # See https://stackoverflow.com/questions/1301402/example-invalid-utf8-string
    v_enum      = MyEnum.AnotherEnum,
    v_key_value = KeyValue(key="foo", value=""),
)

# Similar to Scalars1, but with additional values from Sclars2; and one test of empty list
repeated1a = Repeated1(
    v_double   = [ 1.5, -1.5],
    v_float    = [ 2.5, -2.5],
    v_int32    = [ 3, -3, 555],
    v_int64    = [ 4, -4],
    v_uint32   = [ ],
    v_uint64   = [ 6+10000000, 7,8,9],
    v_sint32   = [ 7, -7],
    v_sint64   = [ -8, 8],
    v_fixed32  = [ 9],
    v_fixed64  = [10, 10+1000],
    v_sfixed32 = [-11, 11],
    v_sfixed64 = [-12, 12],
    v_bool     = [False, True],
    v_string   = ["écran 網目錦蛇", "Hello world"],
    v_bytes    = [b"\xc3\x28"],  # See https://stackoverflow.com/questions/1301402/example-invalid-utf8-string
    v_enum     = [MyEnum.E1, MyEnum.Enum2, MyEnum.E1],
    v_key_value= [KeyValue(key="foo", value=""),
                  KeyValue(key="àmímé níshíkíhéꜜbì", value="reticulated python")],
)

# Same values as Repeated1
packed1a = Packed1(
    v_double   = [ 1.5, -1.5],
    v_float    = [ 2.5, -2.5],
    v_int32    = [ 3, -3, 555],
    v_int64    = [ 4, -4],
    v_uint32   = [ ],
    v_uint64   = [ 6+10000000, 7,8,9],
    v_sint32   = [ 7, -7],
    v_sint64   = [ -8, 8],
    v_fixed32  = [ 9],
    v_fixed64  = [10, 10+1000],
    v_sfixed32 = [-11, 11],
    v_sfixed64 = [-12, 12],
    v_bool     = [False, True],
    v_string   = ["écran 網目錦蛇", "Hello world"],
    v_bytes    = [b"\xc3\x28"],  # See https://stackoverflow.com/questions/1301402/example-invalid-utf8-string
    v_enum     = [MyEnum.E1, MyEnum.Enum2, MyEnum.E1],
    v_key_value= [],
)

def main():
    with open("scalars1a_from_python.wire", "wb") as f:
        f.write(scalars1a.SerializeToString())
    with open("scalars1b_from_python.wire", "wb") as f:
        f.write(scalars1b.SerializeToString())
    with open("repeated1a_from_python.wire", "wb") as f:
        f.write(repeated1a.SerializeToString())
    with open("packed1a_from_python.wire", "wb") as f:
        f.write(packed1a.SerializeToString())

    m0 = M0(v_enum=[MyEnum.E1, MyEnum.Enum2, MyEnum.E1])
    with open("m0.wire", "wb") as f:
        f.write(m0.SerializeToString())

    m1 = M1(v_key_value=[KeyValue(key="foo", value="bar"), KeyValue(key="x", value="y")])
    print('m1:', m1)
    with open("m1.wire", "wb") as f:
        f.write(m1.SerializeToString())

    m2 = M2(v_key_value=KeyValue(key="foo", value="bar"))
    print('m2:', m2)
    with open("m2.wire", "wb") as f:
        f.write(m2.SerializeToString())


if __name__ == "__main__":
    main()

