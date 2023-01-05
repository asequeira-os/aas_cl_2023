# Common Lisp code

The goal was to develop a backend system for doing public calendar scheduling.

There is code for various things.  
This is not runnable as is since the third party libs, config etc are needed and the code hasn't been run since 2016.

Thee system used to run (on [CCL](https://github.com/Clozure/ccl) and [SBCL](https://sbcl.sourceforge.io/)) and the tests used to pass.

Following is some of the functionality in this repo:

- json rpc server with typed common lisp structures as arguments
- auto generated (CL Macros) rpc serialization functions
- user and host auth for user-host and host-host communications
- attempt at developing ACL
- various CL utilities
- a recaptcha client
- postgres db client utlities
- credit card logic helpers
- US zipcodes utlitities

The [timezone](./timezone/) module has code that can parse timezone database (text files) and provide timezone functionality.

Most modules (excluding [xed](./xed/)) are of general use, but depend on other modules.  

Please see [thirdparty.asd](./common/thirdparty.asd) asdf file for dependencies.

## History

Developed by Antony Sequeira between 2007-2016.

All of the code was written by me. During those years I have asked questions on the `comp.lang.lisp` NNTP group for help.

Please see [LICENSE](./LICENSE) for use.

The changes in 2023 are just addition of the LICENSE file and this readme.  
Moved from my private account to here and set the repo to public. Added MIT license.

I no longer code in CL and unable to offer any support.

The various dev credentials/secrets in files and previous commits are all obsolete/redacted.
