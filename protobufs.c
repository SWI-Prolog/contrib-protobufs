/*  Part of SWI-Prolog

    Author:        Jeffrey Rosenwald
    E-mail:        jeffrose@acm.org
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

#define O_DEBUG 1

#include <config.h>
#include <SWI-Prolog.h>
#ifdef _MSC_VER
#define inline __inline
typedef int int32_t;
#endif

static functor_t FUNCTOR_error2;  /* error(Formal, Context) */

static int
instantiation_error()
{ term_t ex;

  if ( (ex = PL_new_term_ref()) &&
       PL_unify_term(ex,
		     PL_FUNCTOR, FUNCTOR_error2,
		       PL_CHARS, "instantiation_error",
		       PL_VARIABLE) )
    return PL_raise_exception(ex);

  return FALSE;
}

static inline
void cp_net_order(char * to, char * from, int size)  /* must be a power of 2 */
{	register int i = 0, j = 0;

#ifdef WORDS_BIGENDIAN
	j = size - 1;
#endif

	for(i = 0; i < size; i++)
		to[i] = from[i ^ j];
}

static
foreign_t integer_zigzag(term_t Integer, term_t ZigZag)
{	int64_t val, val1;

	if(PL_get_int64(Integer, &val))
		{
		val1 = (val << 1) ^ (val >> 63);

		return PL_unify_int64(ZigZag, val1);
		}

	if(PL_get_int64(ZigZag, &val))
		{
		val1 = (val >> 1) ^ (-1 * (val & 1));

		return PL_unify_int64(Integer, val1);
		}

	return instantiation_error();
}

static
foreign_t int32_codes(term_t Number, term_t Codes)
{	 union
		{
		int32_t asNumber;
		char asCodes[sizeof(int32_t)];
		} val, val1;

	char *data;
	size_t len;

	if(PL_get_integer(Number, &val.asNumber))
		{ cp_net_order(val1.asCodes, val.asCodes, sizeof(val1.asCodes));

		return PL_unify_list_ncodes(Codes, sizeof(val1.asCodes), val1.asCodes);
		}

	if(PL_get_list_nchars(Codes, &len, &data, CVT_LIST)
			&& len == sizeof(val.asCodes))
		{ cp_net_order(val.asCodes, data, sizeof(val.asCodes));

		return PL_unify_integer(Number, val.asNumber);
		}

	return instantiation_error();
}

static
foreign_t int64_codes(term_t Number, term_t Codes)
{
	union
		{
		int64_t asNumber;
		char asCodes[sizeof(int64_t)];
		} val, val1;

	char *data;
	size_t len;

	if(PL_get_int64(Number, &val.asNumber))
		{ cp_net_order(val1.asCodes, val.asCodes, sizeof(val1.asCodes));

		return PL_unify_list_ncodes(Codes, sizeof(val1.asCodes), val1.asCodes);
		}

	if(PL_get_list_nchars(Codes, &len, &data, CVT_LIST)
			&& len == sizeof(val.asCodes))
		{ cp_net_order(val.asCodes, data, sizeof(val.asCodes));

		return PL_unify_int64(Number, val.asNumber);
		}

	return instantiation_error();
}

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

	return instantiation_error();
}

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

	return instantiation_error();
}

install_t
install_protobufs()
{
  FUNCTOR_error2				= PL_new_functor(PL_new_atom("error"), 2);

  PL_register_foreign("int32_codes",          2, int32_codes,         0);
  PL_register_foreign("float32_codes",        2, float32_codes,       0);
  PL_register_foreign("int64_codes",          2, int64_codes,         0);
  PL_register_foreign("float64_codes",        2, float64_codes,       0);
  PL_register_foreign("integer_zigzag",       2, integer_zigzag,      0);
}
