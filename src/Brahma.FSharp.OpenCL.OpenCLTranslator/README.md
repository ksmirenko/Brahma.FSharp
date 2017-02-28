### Restrictions:

* only function definitions are supported as top-level declarations
* only basic data types are supported (see below in the grammar)
* unions are not supported
* anonymous structs as function parameters are not supported
* `__attribute__` specifiers for kernel functions are not supported
* `inline` function specifier is not supported in OpenCL C
* Unicode in identifiers is not supported
* type casting in function definitions is not supported
* comments in OpenCL C source files are not supported

### C99 keywords:

    auto break case char const continue default do double else enum extern
    float for goto if inline int long register restrict return short signed
    sizeof static struct switch typedef union unsigned void volatile while
    _Bool _Complex _Imaginary

### OpenCL C 2.0 additional keywords:

* Data types (see grammar)

* Address space qualifiers:

        __global global __local local
        __constant constant __private private
        __generic generic (* reserved *)

* Function qualifiers:

        __kernel kernel

* Access qualifiers:

        __read_only read_only
        __write_only write_only
        __read_write read_write

* Additional:

        uniform pipe
