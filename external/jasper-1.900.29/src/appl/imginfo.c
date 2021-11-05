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

/*
 * Image Information Program
 *
 * $Id$
 */

/******************************************************************************\
* Includes.
\******************************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <float.h>
#include <assert.h>
#include <stdint.h>

#include <jasper/jasper.h>

/******************************************************************************\
*
\******************************************************************************/

typedef enum {
	OPT_HELP,
	OPT_VERSION,
	OPT_VERBOSE,
	OPT_INFILE,
	OPT_DEBUG,
	OPT_MAXSAMPLES,
	OPT_MAXMEM
} optid_t;

/******************************************************************************\
*
\******************************************************************************/

static void usage(void);
static void cmdinfo(void);

/******************************************************************************\
*
\******************************************************************************/

static jas_opt_t opts[] = {
	{OPT_HELP, "help", 0},
	{OPT_VERSION, "version", 0},
	{OPT_VERBOSE, "verbose", 0},
	{OPT_INFILE, "f", JAS_OPT_HASARG},
	{OPT_DEBUG, "debug-level", JAS_OPT_HASARG},
	{OPT_MAXSAMPLES, "max-samples", JAS_OPT_HASARG},
#if defined(JAS_DEFAULT_MAX_MEM_USAGE)
	{OPT_MAXMEM, "memory-limit", JAS_OPT_HASARG},
#endif
	{-1, 0, 0}
};

static char *cmdname = 0;

/******************************************************************************\
* Main program.
\******************************************************************************/

int main(int argc, char **argv)
{
	int fmtid;
	int id;
	char *infile;
	jas_stream_t *instream;
	jas_image_t *image;
	int width;
	int height;
	int depth;
	int numcmpts;
	int verbose;
	char *fmtname;
	int debug;
	size_t max_mem;
	size_t max_samples;
	bool max_samples_valid;
	char optstr[32];

	if (jas_init()) {
		abort();
	}

	cmdname = argv[0];

	max_samples = 0;
	max_samples_valid = false;
	infile = 0;
	verbose = 0;
	debug = 0;
#if defined(JAS_DEFAULT_MAX_MEM_USAGE)
	max_mem = JAS_DEFAULT_MAX_MEM_USAGE;
#endif

	/* Parse the command line options. */
	while ((id = jas_getopt(argc, argv, opts)) >= 0) {
		switch (id) {
		case OPT_VERBOSE:
			verbose = 1;
			break;
		case OPT_VERSION:
			printf("%s\n", JAS_VERSION);
			exit(EXIT_SUCCESS);
			break;
		case OPT_DEBUG:
			debug = atoi(jas_optarg);
			break;
		case OPT_INFILE:
			infile = jas_optarg;
			break;
		case OPT_MAXSAMPLES:
			max_samples = strtoull(jas_optarg, 0, 10);
			max_samples_valid = true;
			break;
		case OPT_MAXMEM:
			max_mem = strtoull(jas_optarg, 0, 10);
			break;
		case OPT_HELP:
		default:
			usage();
			break;
		}
	}

	jas_setdbglevel(debug);
#if defined(JAS_DEFAULT_MAX_MEM_USAGE)
	jas_set_max_mem_usage(max_mem);
#endif

	/* Open the image file. */
	if (infile) {
		/* The image is to be read from a file. */
		if (!(instream = jas_stream_fopen(infile, "rb"))) {
			fprintf(stderr, "cannot open input image file %s\n", infile);
			exit(EXIT_FAILURE);
		}
	} else {
		/* The image is to be read from standard input. */
		if (!(instream = jas_stream_fdopen(0, "rb"))) {
			fprintf(stderr, "cannot open standard input\n");
			exit(EXIT_FAILURE);
		}
	}

	if ((fmtid = jas_image_getfmt(instream)) < 0) {
		fprintf(stderr, "unknown image format\n");
	}

	optstr[0] = '\0';
	if (max_samples_valid) {
#if defined(JAS_HAVE_SNPRINTF)
		snprintf(optstr, sizeof(optstr), "max_samples=%-zu", max_samples);
#else
		sprintf(optstr, "max_samples=%-zu", max_samples);
#endif
	}

	/* Decode the image. */
	if (!(image = jas_image_decode(instream, fmtid, optstr))) {
		jas_stream_close(instream);
		fprintf(stderr, "cannot load image\n");
		return EXIT_FAILURE;
	}

	/* Close the image file. */
	jas_stream_close(instream);

	if (!(fmtname = jas_image_fmttostr(fmtid))) {
		jas_eprintf("format name lookup failed\n");
		return EXIT_FAILURE;
	}

	if (!(numcmpts = jas_image_numcmpts(image))) {
		fprintf(stderr, "warning: image has no components\n");
	}
	if (numcmpts) {
		width = jas_image_cmptwidth(image, 0);
		height = jas_image_cmptheight(image, 0);
		depth = jas_image_cmptprec(image, 0);
	} else {
		width = 0;
		height = 0;
		depth = 0;
	}
	printf("%s %d %d %d %d %ld\n", fmtname, numcmpts, width, height, depth,
	  JAS_CAST(long, jas_image_rawsize(image)));

	jas_image_destroy(image);
	jas_image_clearfmts();

	return EXIT_SUCCESS;
}

/******************************************************************************\
*
\******************************************************************************/

static void cmdinfo()
{
	fprintf(stderr, "Image Information Utility (Version %s).\n",
	  JAS_VERSION);
	fprintf(stderr,
	  "Copyright (c) 2001 Michael David Adams.\n"
	  "All rights reserved.\n"
	  );
}

static void usage()
{
	cmdinfo();
	fprintf(stderr, "usage:\n");
	fprintf(stderr,"%s ", cmdname);
	fprintf(stderr, "[-f image_file]\n");
	exit(EXIT_FAILURE);
}
