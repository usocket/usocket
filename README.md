## USOCKET - Universal socket library for Common Lisp

https://usocket.common-lisp.dev


This is the `usocket` Common Lisp sockets library - a library to bring
sockets access to the broadest of Common Lisp implementations as possible.

## The library currently supports the following CL implementations:

1. [Armed Bear Common Lisp (ABCL)](https://armedbear.common-lisp.dev/)
2. [Allegro Common Lisp](https://franz.com/products/allegro-common-lisp/)
3. [AmiLisp (formerly CL-Amiga)](https://github.com/mdbergmann/amilisp)
4. [Clasp](https://clasp-developers.github.io/)
5. [GNU CLISP](https://www.gnu.org/software/clisp/)
6. [Clozure CL (formerly OpenMCL)](https://ccl.clozure.com/)
7. [CMU Common Lisp (CMUCL)](https://cmucl.org/)
8. [Corman Lisp](https://github.com/sharplispers/cormanlisp)
9. [dotcl (Common Lisp implementation on .NET/CLR)](https://github.com/dotcl/dotcl)
10. [Embeddable Common-Lisp (ECL)](https://ecl.common-lisp.dev/)
11. [Digitool MCL and RMCL (5.0 and up)](https://github.com/binghe/MCL)
12. [Symbolics Lisp Machine (Genera)](https://www.symbolics.biz/)
13. [LispWorks](https://www.lispworks.com/products/lispworks.html)
14. [Mezzano](https://github.com/froggey/Mezzano)
15. [MKCL](https://mkcl.common-lisp.dev/)
16. [MOCL](https://github.com/Wukix/mocl-example-lisp-contacts-ios)
17. [SBCL](https://www.sbcl.org/)
18. [Scieneer CL](https://www.liquisearch.com/scieneer_common_lisp)

If your favorite Common Lisp is missing from the above list, please contact
usocket-devel@common-lisp.net (or [GitHub]()) and submit a request.  Please include
references to available sockets functions in your Lisp implementation.

The library is [ASDF](http://cliki.net/ASDF)-enabled, meaning
that you can tar up a checkout and use that to `asdf-install:install`
the package in your system package site.  (Or use your usual ASDF
tricks to use the checkout directly.)

## Remarks on licensing

Even though the source code has an MIT-style license attached to it,
when compiling this code with some of the supported Lisp implementations
you may not end up with an MIT-style binary version due to the licensing
of the implementations themselves.  ECL is such an example and - when
it comes to be supported - so is GCL.

## Non-support of :external-format

Because of its definition in the Hyperspec, there's no common
external-format between Lisp implementations: every vendor has chosen
a different way to solve the problem of newline translation or
character set recoding.

Because there's no way to avoid platform-specific code in the application
when using external-format, the purpose of a portability layer gets
defeated.  So, for now, `usocket` doesn't support external-format.

The workaround to get reasonably portable external-format support is to
layer a `flexi-stream` (from `flexi-streams`) on top of a `usocket` stream.

## API definition

 - `usocket` (class)
 - `stream-usocket` (class; usocket derivative)
 - `stream-server-usocket` (class; usocket derivative)
 - `socket-connect (host port &key element-type)` (function)

   Create an active/connected socket. `host` can be a vectorized IP, or a string representation of a dotted IP address, or a hostname for lookup in the DNS system.

 - `socket-listen (host port &key reuseaddress backlog element-type)` (function)

   Create a passive/listening socket. For possible values of `host`, see `socket-connect`.

 - `socket-accept (socket &key element-type)` (method)

   Create an active/connected socket. Returns (server side) a connected socket derived from a listening/passive socket.

 - `socket-close (socket)` (method)

   Where `socket` is a previously-returned socket.

 - `socket` (usocket slot accessor),

   Returns the internal/implementation-defined socket representation.

 - `socket-stream (socket)` (usocket slot accessor),

   Returns a value which satisfies the normal stream interface.

 - `socket-shutdown`

### Errors:
 - `address-in-use-error`
 - `address-not-available-error`
 - `bad-file-descriptor-error`
 - `connection-refused-error`
 - `connection-aborted-error`
 - `connection-reset-error`
 - `invalid-argument-error`
 - `no-buffers-error`
 - `operation-not-supported-error`
 - `operation-not-permitted-error`
 - `protocol-not-supported-error`
 - `socket-type-not-supported-error`
 - `network-unreachable-error`
 - `network-down-error`
 - `network-reset-error`
 - `host-down-error`
 - `host-unreachable-error`
 - `shutdown-error`
 - `timeout-error`
 - `unknown-error`

### Non-fatal conditions:
 - `interrupted-condition`
 - `unknown-condition`

(for a description of the API methods and functions see
  https://usocket.common-lisp.dev/api-docs.shtml)

## Test suite

The test suite unfortunately isn't mature enough yet to run without
some manual configuration.  Several elements are required which are
hard to programatically detect.  Please adjust the test file before
running the tests, for these variables:

- `+non-existing-host+`: The stringified IP address of a host on the
     same subnet.  No physical host may be present.
- `+unused-local-port+`: A port number of a port not in use on the
     machine the tests run on.
- `+common-lisp-net+`: A vector with 4 integer elements which make up
     an IP address. This must be the IP "common-lisp.net" resolves to.

## Known problems

- CMUCL error reporting wrt sockets raises only simple-errors
  meaning there's no way to tell different error conditions apart.
  All errors are mapped to unknown-error on CMUCL.

- The ArmedBear backend doesn't do any error mapping (yet). Java
  defines exceptions at the wrong level (IMO), since the exception
  reported bears a relation to the function failing, not the actual
  error that occurred: for example 'Address already in use' (when
  creating a passive socket) is reported as a `BindException` with
  an error text of 'Address already in use'. There's no way to sanely
  map `BindException` to a meaningfull error in usocket. [This does not
  mean the backend should not at least map to `unknown-error`!]

- When using the library with ECL, you need the C compiler installed
  to be able to compile and load the Foreign Function Interface.
  Not all ECL targets support DFFI yet, so on some targets this would
  be the case anyway.  By depending on this technique, usocket can
  reuse the FFI code on all platforms (including Windows).  This benefit
  currently outweighs the additional requirement. (hey, it's *Embeddable*
  Common Lisp, so, you probably wanted to embed it all along, right?)
