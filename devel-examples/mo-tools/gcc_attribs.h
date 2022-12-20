#ifndef GCC_ATTRIBUTES
#define GCC_ATTRIBUTES

/* FIXME: does this handle C++? */
/* FIXME: add the explanation from the GCC documentation to each attribute. */
#if __GNUC__ >= 3
# define inline		__inline__ /* __attribute__ ((always_inline)) */
/* always_inline produces errors like:
   sorry, unimplemented: inlining failed
*/
# define __pure_func	__attribute__ ((pure))
/* Many functions have no effects except the return value and their
   return value depends only on the parameters and/or global
   variables. Such a function can be subject to common subexpression
   elimination and loop optimization just as an arithmetic operator
   would be.  

   Some of common examples of pure functions are strlen or
   memcmp. Interesting non-pure functions are functions with infinite
   loops or those depending on volatile memory or other system
   resource, that may change between two consecutive calls (such as
   feof in a multithreading environment).  */
# define __const_func	__attribute__ ((const))
/* Many functions do not examine any values except their arguments,
   and have no effects except the return value. Basically this is just
   slightly more strict class than the pure attribute below, since
   function is not allowed to read global memory.

   Note that a function that has pointer arguments and examines the
   data pointed to must not be declared const. Likewise, a function
   that calls a non-const function usually must not be const. It does
   not make sense for a const function to return void.  */
# define __noreturn	__attribute__ ((noreturn))
/* The noreturn keyword tells the compiler to assume that function
   cannot return. It can then optimize without regard to what would
   happen if fatal ever did return. This makes slightly better
   code. More importantly, it helps avoid spurious warnings of
   uninitialized variables. */
# define __malloc	__attribute__ ((malloc))
/* The malloc attribute is used to tell the compiler that a function
   may be treated as if any non-NULL pointer it returns cannot alias
   any other pointer valid when the function returns. This will often
   improve optimization. Standard functions with this property include
   malloc and calloc. realloc-like functions have this property as
   long as the old pointer is never referred to (including comparing
   it to the new pointer) after the function returns a non-NULL
   value.  */
# define __must_check	__attribute__ ((warn_unused_result))
/* The warn_unused_result attribute causes a warning to be emitted if
   a caller of the function with this attribute does not use its
   return value.  */
# define __deprecated	__attribute__ ((deprecated))
/* The deprecated attribute results in a warning if the function is
   used anywhere in the source file.  */
# define __used		__attribute__ ((used))
# define __unused	__attribute__ ((unused))
# define __packed	__attribute__ ((packed))
# define likely(x)	__builtin_expect (!!(x), 1)
# define unlikely(x)	__builtin_expect (!!(x), 0)
#else
# define inline		/* no inline */
# define  __attribute__(x) /* If we're not using GNU C, elide __attribute__ */
# define likely(x)	(x)
# define unlikely(x)	(x)
#endif

#endif
