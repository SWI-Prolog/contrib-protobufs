/*  Part of SWI-Prolog

    Author:        Jeffrey Rosenwald, extended by Peter Ludemann
    E-mail:        jeffrose@acm.org, peter.ludemann@gmail.com
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2010-2015, Jeffrey Rosenwald
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

/* Further information on the foreign language interface functions
   used in this module:
   https://swi-prolog.discourse.group/t/foreign-language-interface-integers-exceptions/4101
*/


/* TODO: Improve how failure/errors are done.
         See https://github.com/SWI-Prolog/contrib-protobufs/issues/8
*/

#define O_DEBUG 1

#include <config.h>
#include <SWI-Prolog.h>
#ifdef _MSC_VER
#define inline __inline
typedef int int32_t;
#endif

/* Copy "size" bytes from "from" to "to", putting them in the byte
   order for protobufs. Note that protobufs use little-endian
   ordering; classic "network byte order" is big-endian. */
static inline void
cp_net_order(char *to, const char *from, size_t size)	/* must be a power of 2 */
{
  register int i = 0, j = 0;

#ifdef WORDS_BIGENDIAN
  j = size - 1;
#endif

  for (i = 0; i < size; i++)
  {
    to[i] = from[i ^ j];
  }
}

/* For documentation of this function, see protobufs.pl */
static foreign_t
int64_zigzag(term_t Integer, term_t ZigZag)
{
  if (PL_is_variable(Integer))
  {
    if (PL_is_variable(ZigZag))
    {
      return PL_instantiation_error(Integer);	/* Integer,ZigZag uninstantiated */
    } else
    {
      uint64_t v_ZigZag;
      if (!PL_get_uint64_ex(ZigZag, &v_ZigZag))
	return FALSE;
      int64_t v_Integer = (v_ZigZag >> 1) ^ (-1 * (v_ZigZag & 1));
      return PL_unify_int64(Integer, v_Integer);
    }
  } else
  {
    int64_t v_Integer;
    if (!PL_get_int64_ex(Integer, &v_Integer))
      return FALSE;
    uint64_t v_ZigZag = ((uint64_t)v_Integer << 1) ^ (v_Integer >> 63);
    return PL_unify_uint64(ZigZag, v_ZigZag);
  }
}

/* For documentation of this function, see protobufs.pl */
static foreign_t
uint32_codes(term_t Number, term_t Codes)
{
  union
  {
    uint32_t asUnsignedNumber;
    char asCodes[sizeof(uint32_t)];	/* Treated as unsigned char */
  }
  val, val1;

  /* TODO: add compile-time verification that size (int32_t) == size (uint32_t) */

  char *data;			/* Treated as unsigned char */
  size_t len;

  if (PL_is_variable(Number))
  {
    if (PL_is_variable(Codes))
    {
      return PL_instantiation_error(Number);	/* Codes also is uninstantiated */
    }
    if (PL_get_list_nchars(Codes, &len, &data, CVT_LIST)
	&& len == sizeof val.asCodes)
    {
      cp_net_order(val.asCodes, data, sizeof(val.asCodes));
      return PL_unify_uint64(Number, val.asUnsignedNumber);
    } else
    {
      return PL_type_error("list", Codes);
    }
  } else
  {
    if (!PL_cvt_i_uint(Number, &val.asUnsignedNumber))
      return FALSE;
    cp_net_order(val1.asCodes, val.asCodes, sizeof val1.asCodes);
    return PL_unify_list_ncodes(Codes, sizeof(val1.asCodes), val1.asCodes);
  }
}

/* For documentation of this function, see protobufs.pl */
static foreign_t
int32_codes(term_t Number, term_t Codes)
{
  union
  {
    int32_t asSignedNumber;
    char asCodes[sizeof(int32_t)];	/* Treated as unsigned char */
  }
  val, val1;

  /* TODO: add compile-time verification that size (int32_t) == size (int32_t) */

  char *data;			/* Treated as unsigned char */
  size_t len;

  if (PL_is_variable(Number))
  {
    if (PL_is_variable(Codes))
    {
      return PL_instantiation_error(Number);	/* Codes also is uninstantiated */
    }
    if (PL_get_list_nchars(Codes, &len, &data, CVT_LIST)
	&& len == sizeof val.asCodes)
    {
      cp_net_order(val.asCodes, data, sizeof(val.asCodes));
      return PL_unify_integer(Number, val.asSignedNumber);
    } else
    {
      return PL_type_error("list", Codes);
    }
  } else
  {
    if (!PL_cvt_i_int(Number, &val.asSignedNumber))
      return FALSE;
    cp_net_order(val1.asCodes, val.asCodes, sizeof val1.asCodes);
    return PL_unify_list_ncodes(Codes, sizeof(val1.asCodes), val1.asCodes);
  }
}

/* For documentation of this function, see protobufs.pl */
static foreign_t
uint64_codes(term_t Number, term_t Codes)
{
  union
  {
    uint64_t asUnsignedNumber;
    char asCodes[sizeof(uint64_t)];	/* Should be unsigned char */
  }
  val, val1;

  char *data;			/* Treated as unsigned char */
  size_t len;

  if (PL_is_variable(Number))
  {
    if (PL_is_variable(Codes))
    {
      return PL_instantiation_error(Number);	/* Codes also is uninstantiated */
    }
    if (PL_get_list_nchars(Codes, &len, &data, CVT_LIST)
	&& len == sizeof val.asCodes)
    {
      cp_net_order(val.asCodes, data, sizeof(val.asCodes));
      return PL_unify_uint64(Number, val.asUnsignedNumber);
    } else
    {
      return PL_type_error("list", Codes);
    }
  } else
  {
    uint64_t v_Number;
    if (!PL_get_uint64_ex(Number, &v_Number))
      return FALSE;
    val.asUnsignedNumber = v_Number;
    cp_net_order(val1.asCodes, val.asCodes, sizeof val1.asCodes);
    return PL_unify_list_ncodes(Codes, sizeof(val1.asCodes), val1.asCodes);
  }
}

/* For documentation of this function, see protobufs.pl */
static foreign_t
int64_codes(term_t Number, term_t Codes)
{
  union
  {
    int64_t asSignedNumber;
    char asCodes[sizeof(int64_t)];	/* Should be unsigned char */
  }
  val, val1;

  char *data;			/* Treated as unsigned char */
  size_t len;

  if (PL_is_variable(Number))
  {
    if (PL_is_variable(Codes))
    {
      return PL_instantiation_error(Number);	/* Codes also is uninstantiated */
    }
    if (PL_get_list_nchars(Codes, &len, &data, CVT_LIST)
	&& len == sizeof val.asCodes)
    {
      cp_net_order(val.asCodes, data, sizeof(val.asCodes));
      return PL_unify_int64(Number, val.asSignedNumber);
    } else
    {
      return PL_type_error("list", Codes);
    }
  } else
  {
    int64_t v_Number;
    if (!PL_get_int64_ex(Number, &v_Number))
      return FALSE;
    val.asSignedNumber = v_Number;
    cp_net_order(val1.asCodes, val.asCodes, sizeof val1.asCodes);
    return PL_unify_list_ncodes(Codes, sizeof(val1.asCodes), val1.asCodes);
  }
}

/* For documentation of this function, see protobufs.pl */
static
foreign_t float32_codes(term_t Number, term_t Codes)
{
	union
		{
		float asNumber;
		char asCodes[sizeof(float)];
		} val, val1;

	char *data;
	size_t len;
	double tmp;

	if(PL_get_float(Number, &tmp))
		{ val.asNumber = (float)tmp;

		cp_net_order(val1.asCodes, val.asCodes, sizeof(val1.asCodes));

		return PL_unify_list_ncodes(Codes, sizeof(val1.asCodes), val1.asCodes);
		}

	if(PL_get_list_nchars(Codes, &len, &data, CVT_LIST)
			&& len == sizeof(val.asCodes))
		{ cp_net_order(val.asCodes, data, sizeof(val.asCodes));

		tmp = val.asNumber;

		return PL_unify_float(Number, tmp);
		}

	/* TODO: Change logic to be similar to uint32_codes, for  better error messages. */
	return PL_instantiation_error(Number);
}

/* For documentation of this function, see protobufs.pl */
static
foreign_t float64_codes(term_t Number, term_t Codes)
{
	union
		{
		double asNumber;
		char asCodes[sizeof(double)];
		} val, val1;

	char *data;
	size_t len;

	if(PL_get_float(Number, &val.asNumber))
		{ cp_net_order(val1.asCodes, val.asCodes, sizeof(val1.asCodes));

		return PL_unify_list_ncodes(Codes, sizeof(val1.asCodes), val1.asCodes);
		}

	if(PL_get_list_nchars(Codes, &len, &data, CVT_LIST)
			&& len == sizeof(val.asCodes))
		{ cp_net_order(val.asCodes, data, sizeof(val.asCodes));

		return PL_unify_float(Number, val.asNumber);
		}

	/* TODO: Change logic to be similar to uint32_codes, for  better error messages. */
	return PL_instantiation_error(Number);
}

/* For documentation of this function, see protobufs.pl */
static foreign_t
uint32_int32(term_t Uint32, term_t Int32)
{
  /* TODO: might generate better code if we do
              asUnsigned = (uint32_t)asSigned
              asSigned = (int32_t)asUnsigned
   */
  union
  {
    uint32_t asUnsigned;
    int32_t asSigned;
  } val;

  if (PL_is_variable(Uint32))
  {
    if (PL_is_variable(Int32))
    {
      return PL_instantiation_error(Uint32);	/* Int32 also is uninstantiated */
    } else
    {
      if (!PL_cvt_i_int(Int32, &val.asSigned))
	return FALSE;
      return PL_unify_uint64(Uint32, val.asUnsigned);
    }
  } else
  {
    /* Can't use PL_cvt_i_uint because it disallows the 64-bit version
       of -1. Instead, get the value as uint64, then range-check it
       and mask. */
    uint64_t uint64;
    if (!PL_get_uint64_ex(Uint32, &uint64))
      return FALSE;
    if (uint64 > 0xffffffff && ((uint64 & 0xffffffff00000000) != 0xffffffff00000000))
    {
      return PL_domain_error("32_bit_integer", Uint32);
    }
    val.asUnsigned = 0xffffffff & uint64; /* TODO: mask not needed? */
    return PL_unify_int64(Int32, val.asSigned);
  }
}

/* For documentation of this function, see protobufs.pl */
static foreign_t
uint64_int64(term_t Uint64, term_t Int64)
{
  /* TODO: might generate better code if we do
              asUnsigned = (uint64_t)asSigned
              asSigned = (int64_t)asUnsigned
   */
  union
  {
    uint64_t asUnsigned;
    int64_t asSigned;
  } val;

  if (PL_is_variable(Uint64))
  {
    if (PL_is_variable(Int64))
    {
      return PL_instantiation_error(Uint64);	/* Int64 also is uninstantiated */
    } else
    {
      if (!PL_get_int64_ex(Int64, &val.asSigned))
	return FALSE;
      return PL_unify_uint64(Uint64, val.asUnsigned);
    }
  } else
  {
    if (!PL_get_uint64_ex(Uint64, &val.asUnsigned))
      return FALSE;
    return PL_unify_int64(Int64, val.asSigned);
  }
}

/* For documentation of this function, see protobufs.pl */
static foreign_t
int64_float64(term_t Int64, term_t Float64)
{
  /* TODO: might generate better code if we do
              asInt = (int64_t)asFloat
              asFloat = (double)asSigned
   */
  union
  {
    int64_t asInt;
    double asFloat;
  } val;

  if (PL_is_variable(Int64))
  {
    if (PL_is_variable(Float64))
    {
      return PL_instantiation_error(Int64);	/* Float64 also is uninstantiated */
    } else
    {
      if (!PL_get_float_ex(Float64, &val.asFloat))
	return FALSE;
      return PL_unify_int64(Int64, val.asInt);
    }
  } else
  {
    if (!PL_get_int64_ex(Int64, &val.asInt))
      return FALSE;
    return PL_unify_float(Float64, val.asFloat);
  }
}

/* For documentation of this function, see protobufs.pl */
static foreign_t
int32_float32(term_t Int32, term_t Float32)
{
  /* TODO: might generate better code if we do
              int32_t asInt = (int32_t)asFloat
              float asFloat = (float)asInt
   */
  union
  {
    int32_t asInt;
    float asFloat;
  } val;

  if (PL_is_variable(Int32))
  {
    if (PL_is_variable(Float32))
    {
      return PL_instantiation_error(Int32);	/* Float32 also is uninstantiated */
    } else
    {
      double float64;
      if (!PL_get_float_ex(Float32, &float64))
	return FALSE;
      val.asFloat = (float)float64;
      return PL_unify_int64(Int32, val.asInt);
    }
  } else
  {
    if (!PL_get_integer_ex(Int32, &val.asInt))
      return FALSE;
    return PL_unify_float(Float32, val.asFloat);
  }
}

install_t
install_protobufs()
{
  PL_register_foreign("uint32_codes",  2, uint32_codes,  0);
  PL_register_foreign("int32_codes",   2, int32_codes,   0);
  PL_register_foreign("float32_codes", 2, float32_codes, 0);
  PL_register_foreign("uint64_codes",  2, uint64_codes,  0);
  PL_register_foreign("int64_codes",   2, int64_codes,   0);
  PL_register_foreign("float64_codes", 2, float64_codes, 0);
  PL_register_foreign("int64_zigzag",  2, int64_zigzag,  0);
  PL_register_foreign("uint32_int32",  2, uint32_int32,  0);
  PL_register_foreign("uint64_int64",  2, uint64_int64,  0);
  PL_register_foreign("int64_float64", 2, int64_float64, 0);
  PL_register_foreign("int32_float32", 2, int32_float32, 0);
}
