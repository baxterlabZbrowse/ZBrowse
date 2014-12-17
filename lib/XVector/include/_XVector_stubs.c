#include "XVector_interface.h"

#define DEFINE_CCALLABLE_STUB(retT, stubname, Targs, args) \
typedef retT(*__ ## stubname ## _funtype__)Targs; \
retT stubname Targs \
{ \
	static __ ## stubname ## _funtype__ fun = NULL; \
	if (fun == NULL) \
		fun = (__ ## stubname ## _funtype__) R_GetCCallable("XVector", "_" #stubname); \
	return fun args; \
}

/*
 * Using the above macro when retT (the returned type) is void will make Sun
 * Studio 12 C compiler unhappy. So we need to use the following macro to
 * handle that case.
 */
#define DEFINE_NOVALUE_CCALLABLE_STUB(stubname, Targs, args) \
typedef void(*__ ## stubname ## _funtype__)Targs; \
void stubname Targs \
{ \
	static __ ## stubname ## _funtype__ fun = NULL; \
	if (fun == NULL) \
		fun = (__ ## stubname ## _funtype__) R_GetCCallable("XVector", "_" #stubname); \
	fun args; \
	return; \
}


/*
 * Stubs for callables defined in Ocopy_byteblocks.c
 */

DEFINE_NOVALUE_CCALLABLE_STUB(Ocopy_byteblocks_from_i1i2,
	(int i1, int i2, char *dest, size_t dest_nblocks, const char *src, size_t src_nblocks, size_t blocksize),
	(    i1,     i2,       dest,        dest_nblocks,             src,        src_nblocks,        blocksize)
)

DEFINE_NOVALUE_CCALLABLE_STUB(Ocopy_byteblocks_from_subscript,
	(const int *subset, int n, char *dest, size_t dest_nblocks, const char *src, size_t src_nblocks, size_t blocksize),
	(           subset,     n,       dest,        dest_nblocks,             src,        src_nblocks,        blocksize)
)

DEFINE_NOVALUE_CCALLABLE_STUB(Ocopy_byteblocks_to_i1i2,
	(int i1, int i2, char *dest, size_t dest_nblocks, const char *src, size_t src_nblocks, size_t blocksize),
	(    i1,     i2,       dest,        dest_nblocks,             src,        src_nblocks,        blocksize)
)

DEFINE_NOVALUE_CCALLABLE_STUB(Ocopy_byteblocks_to_subscript,
	(const int *subset, int n, char *dest, size_t dest_nblocks, const char *src, size_t src_nblocks, size_t blocksize),
	(           subset,     n,       dest,        dest_nblocks,             src,        src_nblocks,        blocksize)
)

DEFINE_NOVALUE_CCALLABLE_STUB(Ocopy_bytes_from_i1i2_with_lkup,
	(int i1, int i2, char *dest, int dest_nbytes, const char *src, int src_nbytes, const int *lkup, int lkup_length),
	(    i1,     i2,       dest,     dest_nbytes,             src,     src_nbytes,            lkup,     lkup_length)
)

DEFINE_NOVALUE_CCALLABLE_STUB(Ocopy_bytes_from_subscript_with_lkup,
	(const int *subset, int n, char *dest, int dest_nbytes, const char *src, int src_nbytes, const int *lkup, int lkup_length),
	(           subset,     n,       dest,     dest_nbytes,             src,     src_nbytes,            lkup,     lkup_length)
)

DEFINE_NOVALUE_CCALLABLE_STUB(Ocopy_bytes_to_i1i2_with_lkup,
	(int i1, int i2, char *dest, int dest_nbytes, const char *src, int src_nbytes, const int *lkup, int lkup_length),
	(    i1,     i2,       dest,     dest_nbytes,             src,     src_nbytes,            lkup,     lkup_length)
)

DEFINE_NOVALUE_CCALLABLE_STUB(Ocopy_bytes_to_subscript_with_lkup,
	(const int *subset, int n, char *dest, int dest_nbytes, const char *src, int src_nbytes, const int *lkup, int lkup_length),
	(           subset,     n,       dest,     dest_nbytes,             src,     src_nbytes,            lkup,     lkup_length)
)

DEFINE_NOVALUE_CCALLABLE_STUB(Orevcopy_byteblocks_from_i1i2,
	(int i1, int i2, char *dest, size_t dest_nblocks, const char *src, size_t src_nblocks, size_t blocksize),
	(    i1,     i2,       dest,        dest_nblocks,             src,        src_nblocks,        blocksize)
)

DEFINE_NOVALUE_CCALLABLE_STUB(Orevcopy_bytes_from_i1i2_with_lkup,
	(int i1, int i2, char *dest, int dest_nbytes, const char *src, int src_nbytes, const int *lkup, int lkup_length),
	(    i1,     i2,       dest,     dest_nbytes,             src,     src_nbytes,            lkup,     lkup_length)
)

DEFINE_NOVALUE_CCALLABLE_STUB(Ocopy_bytes_from_i1i2_to_complex,
	(int i1, int i2, Rcomplex *dest, int dest_nbytes, const char *src, int src_nbytes, const Rcomplex *lkup, int lkup_length),
	(    i1,     i2,           dest,     dest_nbytes,             src,     src_nbytes,                 lkup,     lkup_length)
)

/*
 * Stubs for callables defined in SharedVector_class.c
 */

DEFINE_CCALLABLE_STUB(SEXP, new_SharedVector,
	(const char *classname, SEXP tag),
	(            classname,      tag)
)

DEFINE_CCALLABLE_STUB(SEXP, get_SharedVector_tag,
	(SEXP x),
	(     x)
)

DEFINE_CCALLABLE_STUB(int, get_SharedVector_length,
	(SEXP x),
	(     x)
)

/*
 * Stubs for callables defined in XVector_class.c
 */

DEFINE_CCALLABLE_STUB(SEXP, get_XVector_shared,
	(SEXP x),
	(     x)
)

DEFINE_CCALLABLE_STUB(int, get_XVector_offset,
	(SEXP x),
	(     x)
)

DEFINE_CCALLABLE_STUB(int, get_XVector_length,
	(SEXP x),
	(     x)
)

DEFINE_CCALLABLE_STUB(SEXP, get_XVector_tag,
	(SEXP x),
	(     x)
)

DEFINE_CCALLABLE_STUB(Chars_holder, hold_XRaw,
	(SEXP x),
	(     x)
)

DEFINE_CCALLABLE_STUB(Ints_holder, hold_XInteger,
	(SEXP x),
	(     x)
)

DEFINE_CCALLABLE_STUB(Doubles_holder, hold_XDouble,
	(SEXP x),
	(     x)
)

DEFINE_CCALLABLE_STUB(SEXP, new_XVector,
	(const char *classname, SEXP shared, int offset, int length),
	(            classname,      shared,     offset,     length)
)

DEFINE_CCALLABLE_STUB(SEXP, new_XRaw_from_tag,
	(const char *classname, SEXP tag),
	(            classname,      tag)
)

DEFINE_CCALLABLE_STUB(SEXP, new_XInteger_from_tag,
	(const char *classname, SEXP tag),
	(            classname,      tag)
)

DEFINE_CCALLABLE_STUB(SEXP, new_XDouble_from_tag,
	(const char *classname, SEXP tag),
	(            classname,      tag)
)

DEFINE_CCALLABLE_STUB(SEXP, alloc_XRaw,
	(const char *classname, int length),
	(            classname,     length)
)

DEFINE_CCALLABLE_STUB(SEXP, alloc_XInteger,
	(const char *classname, int length),
	(            classname,     length)
)

DEFINE_CCALLABLE_STUB(SEXP, alloc_XDouble,
	(const char *classname, int length),
	(            classname,     length)
)

/*
 * Stubs for callables defined in XVectorList_class.c
 */

DEFINE_CCALLABLE_STUB(int, get_XVectorList_length,
	(SEXP x),
	(     x)
)

DEFINE_CCALLABLE_STUB(SEXP, get_XVectorList_width,
	(SEXP x),
	(     x)
)

DEFINE_CCALLABLE_STUB(SEXP, get_XVectorList_names,
	(SEXP x),
	(     x)
)

DEFINE_CCALLABLE_STUB(XVectorList_holder, hold_XVectorList,
	(SEXP x),
	(     x)
)

DEFINE_CCALLABLE_STUB(int, get_length_from_XVectorList_holder,
	(const XVectorList_holder *x_holder),
	(                          x_holder)
)

DEFINE_CCALLABLE_STUB(Chars_holder, get_elt_from_XRawList_holder,
	(const XVectorList_holder *x_holder, int i),
	(                          x_holder,     i)
)

DEFINE_CCALLABLE_STUB(Ints_holder, get_elt_from_XIntegerList_holder,
	(const XVectorList_holder *x_holder, int i),
	(                          x_holder,     i)
)

DEFINE_CCALLABLE_STUB(Doubles_holder, get_elt_from_XDoubleList_holder,
	(const XVectorList_holder *x_holder, int i),
	(                          x_holder,     i)
)

DEFINE_CCALLABLE_STUB(XVectorList_holder, get_linear_subset_from_XVectorList_holder,
	(const XVectorList_holder *x_holder, int offset, int length),
	(                          x_holder,     offset,     length)
)

DEFINE_NOVALUE_CCALLABLE_STUB(set_XVectorList_names,
	(SEXP x, SEXP names),
	(     x,      names)
)

DEFINE_CCALLABLE_STUB(SEXP, new_XRawList_from_tags,
	(const char *classname, const char *element_type, SEXP tags, SEXP ranges, SEXP ranges_group),
	(            classname,             element_type,      tags,      ranges,      ranges_group)
)

DEFINE_CCALLABLE_STUB(SEXP, new_XIntegerList_from_tags,
	(const char *classname, const char *element_type, SEXP tags, SEXP ranges, SEXP ranges_group),
	(            classname,             element_type,      tags,      ranges,      ranges_group)
)

DEFINE_CCALLABLE_STUB(SEXP, new_XDoubleList_from_tags,
	(const char *classname, const char *element_type, SEXP tags, SEXP ranges, SEXP ranges_group),
	(            classname,             element_type,      tags,      ranges,      ranges_group)
)

DEFINE_CCALLABLE_STUB(SEXP, new_XRawList_from_tag,
	(const char *classname, const char *element_type, SEXP tag, SEXP ranges),
	(            classname,             element_type,      tag,      ranges)
)

DEFINE_CCALLABLE_STUB(SEXP, new_XIntegerList_from_tag,
	(const char *classname, const char *element_type, SEXP tag, SEXP ranges),
	(            classname,             element_type,      tag,      ranges)
)

DEFINE_CCALLABLE_STUB(SEXP, new_XDoubleList_from_tag,
	(const char *classname, const char *element_type, SEXP tag, SEXP ranges),
	(            classname,             element_type,      tag,      ranges)
)

DEFINE_CCALLABLE_STUB(SEXP, alloc_XRawList,
	(const char *classname, const char *element_type, SEXP width),
	(            classname,             element_type,      width)
)

DEFINE_CCALLABLE_STUB(SEXP, alloc_XIntegerList,
	(const char *classname, const char *element_type, SEXP width),
	(            classname,             element_type,      width)
)

DEFINE_CCALLABLE_STUB(SEXP, alloc_XDoubleList,
	(const char *classname, const char *element_type, SEXP width),
	(            classname,             element_type,      width)
)

DEFINE_CCALLABLE_STUB(SEXP, new_XRawList_from_CharAEAE,
	(const char *classname, const char *element_type, const CharAEAE *char_aeae, SEXP lkup),
	(            classname,             element_type,                 char_aeae,      lkup)
)

DEFINE_CCALLABLE_STUB(SEXP, new_XIntegerList_from_IntAEAE,
	(const char *classname, const char *element_type, const IntAEAE *int_aeae),
	(            classname,             element_type,                int_aeae)
)

