refuge - Data should be yours
-----------------------------

Refuge main goal is to design an opensource, free, decentralized and
secured and eventually anonymous platform. This platform will allow to
share, render information and exchange messages offline or online.
Information is on each nodes.


## Features:

- Apache CouchDB 1.2x based (current trunk right now)
- Geocouch integrated
- dnssd support
- upnp support (coming)
- P2P layer (coming)
- Fully opensource. All the sources are on refuge GIT repository
  (http://github.com/refuge) under Apache License 2.

## Requirements

- OS supported: Linux, OSX, BSDs (windows support is coming)
- Erlang R14
- Curl
- ICU (if not built statically)

## Installation

Installation is pretty simple. Just run the command line:

    $ make rel

and it will generate a refuge folder in rel/refuge. This release is
fully relocatable, so you can put it where you want on your system.



### Notes about static build 

Refuge will depend on the ICU library version that was present in
your system at build time. To easily bundle this library with the
package, build with:
    
    $ make rel USE_STATIC_ICU=1

Check whether your package depends on Ncurses:

    $ ldd ./rel/refuge/erts-*/bin/erlexec|grep ncurses
    
If it does, copy the .so file to ./rel/refuge/lib/ or rebuild Erlang
without this dependency.

Decide whether you need SSL support in your package and check whether it
depends on OpenSSL:

    $ ldd ./rel/refuge/lib/ssl-*/priv/bin/ssl_esock|grep 'libcrypto\|libssl'

If it does, copy the .so file to ./rel/refuge/lib/ or rebuild Erlang
without this dependency.

If you copied any .so files in the last 2 steps, run this command, so
that your app can find the libraries:

    $ sed -i '/^RUNNER_USER=/a\\nexport LD_LIBRARY_PATH="$RUNNER_BASE_DIR/lib"' ./rel/refuge/bin/refuge

### Development

When developping on top of refuge, you could also run the command line:

    $ make dev

Then ./dev/dev[1,2,3]/bin/refuge


### Packageing

To create package for your system run `make package` . For now we build
packages for OSX, Debian, Redhat & Solaris.
