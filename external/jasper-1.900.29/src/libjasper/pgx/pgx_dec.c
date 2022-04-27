/*
 * Copyright (c) 2001-2003 Michael David Adams.
 * All rights reserved.
 */

/* __START_OF_JASPER_LICENSE__
 * 
 * JasPer License Version 2.0
 * 
 * Copyright (c) 2001-2006 Michael David Adams
 * Copyright (c) 1999-2000 Image Power, Inc.
 * Copyright (c) 1999-2000 The University of British Columbia
 * 
 * All rights reserved.
 * 
 * Permission is hereby granted, free of charge, to any person (the
 * "User") obtaining a copy of this software and associated documentation
 * files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge,
 * publish, distribute, and/or sell copies of the Software, and to permit
 * persons to whom the Software is furnished to do so, subject to the
 * following conditions:
 * 
 * 1.  The above copyright notices and this permission notice (which
 * includes the disclaimer below) shall be included in all copies or
 * substantial portions of the Software.
 * 
 * 2.  The name of a copyright holder shall not be used to endorse or
 * promote products derived from the Software without specific prior
 * written permission.
 * 
 * THIS DISCLAIMER OF WARRANTY CONSTITUTES AN ESSENTIAL PART OF THIS
 * LICENSE.  NO USE OF THE SOFTWARE IS AUTHORIZED HEREUNDER EXCEPT UNDER
 * THIS DISCLAIMER.  THE SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS
 * "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING
 * BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
 * PARTICULAR PURPOSE AND NONINFRINGEMENT OF THIRD PARTY RIGHTS.  IN NO
 * EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, OR ANY SPECIAL
 * INDIRECT OR CONSEQUENTIAL DAMAGES, OR ANY DAMAGES WHATSOEVER RESULTING
 * FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
 * NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION
 * WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.  NO ASSURANCES ARE
 * PROVIDED BY THE COPYRIGHT HOLDERS THAT THE SOFTWARE DOES NOT INFRINGE
 * THE PATENT OR OTHER INTELLECTUAL PROPERTY RIGHTS OF ANY OTHER ENTITY.
 * EACH COPYRIGHT HOLDER DISCLAIMS ANY LIABILITY TO THE USER FOR CLAIMS
 * BROUGHT BY ANY OTHER ENTITY BASED ON INFRINGEMENT OF INTELLECTUAL
 * PROPERTY RIGHTS OR OTHERWISE.  AS A CONDITION TO EXERCISING THE RIGHTS
 * GRANTED HEREUNDER, EACH USER HEREBY ASSUMES SOLE RESPONSIBILITY TO SECURE
 * ANY OTHER INTELLECTUAL PROPERTY RIGHTS NEEDED, IF ANY.  THE SOFTWARE
 * IS NOT FAULT-TOLERANT AND IS NOT INTENDED FOR USE IN MISSION-CRITICAL
 * SYSTEMS, SUCH AS THOSE USED IN THE OPERATION OF NUCLEAR FACILITIES,
 * AIRCRAFT NAVIGATION OR COMMUNICATION SYSTEMS, AIR TRAFFIC CONTROL
 * SYSTEMS, DIRECT LIFE SUPPORT MACHINES, OR WEAPONS SYSTEMS, IN WHICH
 * THE FAILURE OF THE SOFTWARE OR SYSTEM COULD LEAD DIRECTLY TO DEATH,
 * PERSONAL INJURY, OR SEVERE PHYSICAL OR ENVIRONMENTAL DAMAGE ("HIGH
 * RISK ACTIVITIES").  THE COPYRIGHT HOLDERS SPECIFICALLY DISCLAIM ANY
 * EXPRESS OR IMPLIED WARRANTY OF FITNESS FOR HIGH RISK ACTIVITIES.
 * 
 * __END_OF_JASPER_LICENSE__
 */

/******************************************************************************\
* Includes.
\******************************************************************************/

#include <assert.h>
#include <ctype.h>

#include "jasper/jas_tvp.h"
#include "jasper/jas_stream.h"
#include "jasper/jas_image.h"
#include "jasper/jas_string.h"
#include "jasper/jas_debug.h"

#include "pgx_cod.h"

/******************************************************************************\
* Local types.
\******************************************************************************/

typedef struct {
	int allow_trunc;
	size_t max_samples;
} pgx_dec_importopts_t;

typedef enum {
	OPT_ALLOWTRUNC,
	OPT_MAXSIZE,
} optid_t;

/******************************************************************************\
* Local prototypes.
\******************************************************************************/

static int pgx_gethdr(jas_stream_t *in, pgx_hdr_t *hdr);
static int pgx_getdata(jas_stream_t *in, pgx_hdr_t *hdr, jas_image_t *image);
static int_fast32_t pgx_getword(jas_stream_t *in, bool bigendian, int prec);
static int pgx_getsgnd(jas_stream_t *in, bool *sgnd);
static int pgx_getbyteorder(jas_stream_t *in, bool *bigendian);
static int pgx_getc(jas_stream_t *in);
static int pgx_getuint32(jas_stream_t *in, uint_fast32_t *val);
static jas_seqent_t pgx_wordtoint(uint_fast32_t word, int prec, bool sgnd);

/******************************************************************************\
* Option parsing.
\******************************************************************************/

static jas_taginfo_t pgx_decopts[] = {
	// Not yet supported
	// {OPT_ALLOWTRUNC, "allow_trunc"},
	{OPT_MAXSIZE, "max_samples"},
	{-1, 0}
};

static int pgx_dec_parseopts(const char *optstr, pgx_dec_importopts_t *opts)
{
	jas_tvparser_t *tvp;

	opts->max_samples = JAS_DEC_DEFAULT_MAX_SAMPLES;
	opts->allow_trunc = 0;

	if (!(tvp = jas_tvparser_create(optstr ? optstr : ""))) {
		return -1;
	}

	while (!jas_tvparser_next(tvp)) {
		switch (jas_taginfo_nonull(jas_taginfos_lookup(pgx_decopts,
		  jas_tvparser_gettag(tvp)))->id) {
		case OPT_ALLOWTRUNC:
			opts->allow_trunc = atoi(jas_tvparser_getval(tvp));
			break;
		case OPT_MAXSIZE:
			opts->max_samples = strtoull(jas_tvparser_getval(tvp), 0, 10);
			break;
		default:
			jas_eprintf("warning: ignoring invalid option %s\n",
			  jas_tvparser_gettag(tvp));
			break;
		}
	}

	jas_tvparser_destroy(tvp);

	return 0;
}

/******************************************************************************\
* Code for load operation.
\******************************************************************************/

/* Load an image from a stream in the PGX format. */

jas_image_t *pgx_decode(jas_stream_t *in, const char *optstr)
{
	jas_image_t *image;
	pgx_hdr_t hdr;
	jas_image_cmptparm_t cmptparm;
	pgx_dec_importopts_t opts;
	size_t num_samples;

	image = 0;

	JAS_DBGLOG(10, ("pgx_decode(%p, \"%s\")\n", in, optstr ? optstr : ""));

	if (pgx_dec_parseopts(optstr, &opts)) {
		goto error;
	}

	if (pgx_gethdr(in, &hdr)) {
		jas_eprintf("cannot get header\n");
		goto error;
	}

	if (jas_getdbglevel() >= 10) {
		pgx_dumphdr(stderr, &hdr);
	}

	if (!jas_safe_size_mul(hdr.width, hdr.height, &num_samples)) {
		jas_eprintf("image too large\n");
		goto error;
	}
	if (opts.max_samples > 0 && num_samples > opts.max_samples) {
		jas_eprintf(
		  "maximum number of samples would be exceeded (%zu > %zu)\n",
		  num_samples, opts.max_samples);
		goto error;
	}

	if (!(image = jas_image_create0())) {
		goto error;
	}
	cmptparm.tlx = 0;
	cmptparm.tly = 0;
	cmptparm.hstep = 1;
	cmptparm.vstep = 1;
	cmptparm.width = hdr.width;
	cmptparm.height = hdr.height;
	cmptparm.prec = hdr.prec;
	cmptparm.sgnd = hdr.sgnd;
	if (jas_image_addcmpt(image, 0, &cmptparm)) {
		goto error;
	}
	if (pgx_getdata(in, &hdr, image)) {
		jas_eprintf("cannot get data\n");
		goto error;
	}

	jas_image_setclrspc(image, JAS_CLRSPC_SGRAY);
	jas_image_setcmpttype(image, 0,
	  JAS_IMAGE_CT_COLOR(JAS_CLRSPC_CHANIND_GRAY_Y));

	return image;

error:
	if (image) {
		jas_image_destroy(image);
	}
	return 0;
}

/******************************************************************************\
* Code for validate operation.
\******************************************************************************/

int pgx_validate(jas_stream_t *in)
{
	jas_uchar buf[PGX_MAGICLEN];
	uint_fast32_t magic;
	int i;
	int n;

	assert(JAS_STREAM_MAXPUTBACK >= PGX_MAGICLEN);

	/* Read the validation data (i.e., the data used for detecting
	  the format). */
	if ((n = jas_stream_read(in, buf, PGX_MAGICLEN)) < 0) {
		return -1;
	}

	/* Put the validation data back onto the stream, so that the
	  stream position will not be changed. */
	for (i = n - 1; i >= 0; --i) {
		if (jas_stream_ungetc(in, buf[i]) == EOF) {
			return -1;
		}
	}

	/* Did we read enough data? */
	if (n < PGX_MAGICLEN) {
		return -1;
	}

	/* Compute the signature value. */
	magic = (buf[0] << 8) | buf[1];

	/* Ensure that the signature is correct for this format. */
	if (magic != PGX_MAGIC) {
		return -1;
	}

	return 0;
}

/******************************************************************************\
*
\******************************************************************************/

static int pgx_gethdr(jas_stream_t *in, pgx_hdr_t *hdr)
{
	int c;
	jas_uchar buf[2];

	if ((c = jas_stream_getc(in)) == EOF) {
		goto error;
	}
	buf[0] = c;
	if ((c = jas_stream_getc(in)) == EOF) {
		goto error;
	}
	buf[1] = c;
	hdr->magic = buf[0] << 8 | buf[1];
	if (hdr->magic != PGX_MAGIC) {
		jas_eprintf("invalid PGX signature\n");
		goto error;
	}
	if ((c = pgx_getc(in)) == EOF || !isspace(c)) {
		goto error;
	}
	if (pgx_getbyteorder(in, &hdr->bigendian)) {
		jas_eprintf("cannot get byte order\n");
		goto error;
	}
	if (pgx_getsgnd(in, &hdr->sgnd)) {
		jas_eprintf("cannot get signedness\n");
		goto error;
	}
	if (pgx_getuint32(in, &hdr->prec)) {
		jas_eprintf("cannot get precision\n");
		goto error;
	}
	if (pgx_getuint32(in, &hdr->width)) {
		jas_eprintf("cannot get width\n");
		goto error;
	}
	if (pgx_getuint32(in, &hdr->height)) {
		jas_eprintf("cannot get height\n");
		goto error;
	}
	return 0;

error:
	return -1;
}

static int pgx_getdata(jas_stream_t *in, pgx_hdr_t *hdr, jas_image_t *image)
{
	jas_matrix_t *data;
	uint_fast32_t x;
	uint_fast32_t y;
	uint_fast32_t word;
	int_fast32_t v;

	data = 0;

	if (!(data = jas_matrix_create(1, hdr->width))) {
		goto error;
	}
	for (y = 0; y < hdr->height; ++y) {
		for (x = 0; x < hdr->width; ++x) {
			/* Need to adjust signed value. */
			if ((v = pgx_getword(in, hdr->bigendian, hdr->prec)) < 0) {
				goto error;
			}
			word = v;
			v = pgx_wordtoint(word, hdr->prec, hdr->sgnd);
			jas_matrix_set(data, 0, x, v);
		}
		if (jas_image_writecmpt(image, 0, 0, y, hdr->width, 1, data)) {
			goto error;
		}
	}
	jas_matrix_destroy(data);
	return 0;

error:
	if (data) {
		jas_matrix_destroy(data);
	}
	return -1;
}

static int_fast32_t pgx_getword(jas_stream_t *in, bool bigendian, int prec)
{
	uint_fast32_t val;
	int i;
	int j;
	int c;
	int wordsize;

	wordsize = (prec + 7) / 8;

	if (prec > 32) {
		goto error;
	}

	val = 0;
	for (i = 0; i < wordsize; ++i) {
		if ((c = jas_stream_getc(in)) == EOF) {
			goto error;
		}
		j = bigendian ? (wordsize - 1 - i) : i;
		val = val | ((c & 0xff) << (8 * j));
	}
	val &= (1 << prec) - 1;
	return val;

error:
	return -1;
}

static int pgx_getc(jas_stream_t *in)
{
	int c;
	for (;;) {
		if ((c = jas_stream_getc(in)) == EOF) {
			return -1;
		}
		if (c != '#') {
			return c;
		}
		do {
			if ((c = jas_stream_getc(in)) == EOF) {
				return -1;
			}
		} while (c != '\n' && c != '\r');
	}
}

static int pgx_getbyteorder(jas_stream_t *in, bool *bigendian)
{
	int c;
	char buf[2];

	do {
		if ((c = pgx_getc(in)) == EOF) {
			return -1;
		}
	} while (isspace(c));

	buf[0] = c;
	if ((c = pgx_getc(in)) == EOF) {
		goto error;
	}
	buf[1] = c;
	if (buf[0] == 'M' && buf[1] == 'L') {
		*bigendian = true;
	} else if (buf[0] == 'L' && buf[1] == 'M') {
		*bigendian = false;
	} else {
		goto error;
	}

	while ((c = pgx_getc(in)) != EOF && !isspace(c)) {
		;
	}
	if (c == EOF) {
		goto error;
	}
	return 0;

error:
	return -1;
}

static int pgx_getsgnd(jas_stream_t *in, bool *sgnd)
{
	int c;

	do {
		if ((c = pgx_getc(in)) == EOF) {
			return -1;
		}
	} while (isspace(c));

#if 0
	if (c == '+') {
		*sgnd = false;
	} else if (c == '-') {
		*sgnd = true;
	} else {
		*sgnd = false;
		if (jas_stream_ungetc(in, c)) {
			goto error;
		}
		return 0;
	}

	while ((c = pgx_getc(in)) != EOF && !isspace(c)) {
		;
	}
	if (c == EOF) {
		goto error;
	}
#else
	if (c == '+' || c == '-') {
		*sgnd = (c == '-');
		while ((c = pgx_getc(in)) != EOF && !isspace(c)) {
			;
		}
		if (c == EOF) {
			goto error;
		}
	} else {
		*sgnd = false;
		if (jas_stream_ungetc(in, c)) {
			goto error;
		}
	}
#endif

	return 0;

error:
	return -1;
}

static int pgx_getuint32(jas_stream_t *in, uint_fast32_t *val)
{
	int c;
	uint_fast32_t v;

	do {
		if ((c = pgx_getc(in)) == EOF) {
			return -1;
		}
	} while (isspace(c));

	v = 0;
	while (isdigit(c)) {
		v = 10 * v + c - '0';
		if ((c = pgx_getc(in)) < 0) {
			return -1;
		}
	}
	if (!isspace(c)) {
		return -1;
	}
	*val = v;

	return 0;
}

static jas_seqent_t pgx_wordtoint(uint_fast32_t v, int prec, bool sgnd)
{
	jas_seqent_t ret;
	v &= (1 << prec) - 1;
	ret = (sgnd && (v & (1 << (prec - 1)))) ? (v - (1 << prec)) : v;
	return ret;
}
