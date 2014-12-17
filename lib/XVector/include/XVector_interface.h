/*****************************************************************************
 XVector C interface: prototypes
 -------------------------------

   The XVector C interface is split in 2 files:
     1. XVector_defines.h (in this directory): contains the typedefs and
        defines of the interface.
     2. XVector_interface.h (this file): contains the prototypes of the
        XVector C routines that are part of the interface.

 *****************************************************************************/
#include "XVector_defines.h"


/*
 * Ocopy_byteblocks.c
 */

void Ocopy_byteblocks_from_i1i2(
	int i1,
	int i2,
	char *dest,
	size_t dest_nblocks,
	const char *src,
	size_t src_nblocks,
	size_t blocksize
);

void Ocopy_byteblocks_from_subscript(
	const int *subset,
	int n,
	char *dest,
	size_t dest_nblocks,
	const char *src,
	size_t src_nblocks,
	size_t blocksize
);

void Ocopy_byteblocks_to_i1i2(
	int i1,
	int i2,
	char *dest,
	size_t dest_nblocks,
	const char *src,
	size_t src_nblocks,
	size_t blocksize
);

void Ocopy_byteblocks_to_subscript(
	const int *subset,
	int n,
	char *dest,
	size_t dest_nblocks,
	const char *src,
	size_t src_nblocks,
	size_t blocksize
);

void Ocopy_bytes_from_i1i2_with_lkup(
	int i1,
	int i2,
	char *dest,
	int dest_nbytes,
	const char *src,
	int src_nbytes,
	const int *lkup,
	int lkup_length
);

void Ocopy_bytes_from_subscript_with_lkup(
	const int *subset,
	int n,
	char *dest,
	int dest_nbytes,
	const char *src,
	int src_nbytes,
	const int *lkup,
	int lkup_length
);

void Ocopy_bytes_to_i1i2_with_lkup(
	int i1,
	int i2,
	char *dest,
	int dest_nbytes,
	const char *src,
	int src_nbytes,
	const int *lkup,
	int lkup_length
);

void Ocopy_bytes_to_subscript_with_lkup(
	const int *subset,
	int n,
	char *dest,
	int dest_nbytes,
	const char *src,
	int src_nbytes,
	const int *lkup,
	int lkup_length
);

void Orevcopy_byteblocks_from_i1i2(
	int i1,
	int i2,
	char *dest,
	size_t dest_nblocks,
	const char *src,
	size_t src_nblocks,
	size_t blocksize
);

void Orevcopy_bytes_from_i1i2_with_lkup(
	int i1,
	int i2,
	char *dest,
	int dest_nbytes,
	const char *src,
	int src_nbytes,
	const int *lkup,
	int lkup_length
);

void Ocopy_bytes_from_i1i2_to_complex(
	int i1,
	int i2,
	Rcomplex *dest,
	int dest_nbytes,
	const char *src,
	int src_nbytes,
	const Rcomplex *lkup,
	int lkup_length
);

/*
 * Low-level manipulation of SharedVector objects.
 * (see SharedVector_class.c)
 */

SEXP new_SharedVector(const char *classname, SEXP tag);

SEXP get_SharedVector_tag(SEXP x);

int get_SharedVector_length(SEXP x);

/*
 * Low-level manipulation of XVector objects.
 * (see XVector_class.c)
 */

SEXP get_XVector_shared(SEXP x);

int get_XVector_offset(SEXP x);

int get_XVector_length(SEXP x);

SEXP get_XVector_tag(SEXP x);

Chars_holder hold_XRaw(SEXP x);

Ints_holder hold_XInteger(SEXP x);

Doubles_holder hold_XDouble(SEXP x);

SEXP new_XVector(const char *classname, SEXP shared, int offset, int length);

SEXP new_XRaw_from_tag(const char *classname, SEXP tag);

SEXP new_XInteger_from_tag(const char *classname, SEXP tag);

SEXP new_XDouble_from_tag(const char *classname, SEXP tag);

SEXP alloc_XRaw(const char *classname, int length);

SEXP alloc_XInteger(const char *classname, int length);

SEXP alloc_XDouble(const char *classname, int length);

/*
 * Low-level manipulation of XVectorList objects.
 * (see XVectorList_class.c)
 */

int get_XVectorList_length(SEXP x);

SEXP get_XVectorList_width(SEXP x);

SEXP get_XVectorList_names(SEXP x);

XVectorList_holder hold_XVectorList(SEXP x);

int get_length_from_XVectorList_holder(const XVectorList_holder *x_holder);

Chars_holder get_elt_from_XRawList_holder(
	const XVectorList_holder *x_holder,
	int i
);

Ints_holder get_elt_from_XIntegerList_holder(
	const XVectorList_holder *x_holder,
	int i
);

Doubles_holder get_elt_from_XDoubleList_holder(
	const XVectorList_holder *x_holder,
	int i
);

XVectorList_holder get_linear_subset_from_XVectorList_holder(
	const XVectorList_holder *x_holder,
	int offset,
	int length
);

void set_XVectorList_names(SEXP x, SEXP names);

SEXP new_XRawList_from_tags(
	const char *classname,
	const char *element_type,
	SEXP tags,
	SEXP ranges,
	SEXP ranges_group
);

SEXP new_XIntegerList_from_tags(
	const char *classname,
	const char *element_type,
	SEXP tags,
	SEXP ranges,
	SEXP ranges_group
);

SEXP new_XDoubleList_from_tags(
	const char *classname,
	const char *element_type,
	SEXP tags,
	SEXP ranges,
	SEXP ranges_group
);

SEXP new_XRawList_from_tag(
	const char *classname,
	const char *element_type,
	SEXP tag,
	SEXP ranges
);

SEXP new_XIntegerList_from_tag(
	const char *classname,
	const char *element_type,
	SEXP tag,
	SEXP ranges
);

SEXP new_XDoubleList_from_tag(
	const char *classname,
	const char *element_type,
	SEXP tag,
	SEXP ranges
);

SEXP alloc_XRawList(
	const char *classname,
	const char *element_type,
	SEXP width
);

SEXP alloc_XIntegerList(
	const char *classname,
	const char *element_type,
	SEXP width
);

SEXP alloc_XDoubleList(
	const char *classname,
	const char *element_type,
	SEXP width
);

SEXP new_XRawList_from_CharAEAE(
	const char *classname,
	const char *element_type,
	const CharAEAE *char_aeae,
	SEXP lkup
);

SEXP new_XIntegerList_from_IntAEAE(
	const char *classname,
	const char *element_type,
	const IntAEAE *int_aeae
);

