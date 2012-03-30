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

[![Build Status](https://secure.travis-ci.org/refuge/refuge.png)](http://travis-ci.org/refuge/refuge)


## Requirements

- OS supported: Linux, OSX, BSDs (windows support is coming)
- Erlang R14
- Curl
- ICU (if not built statically)
- Latest version of [rebar](http://github.com/basho/rebar) installed on
  your system.

## Installation

Installation is pretty simple. Just run the command line:

    $ make rel

and it will generate a refuge folder in rel/refuge. This release is
fully relocatable, so you can put it where you want on your system.

Then create a certificate for your instance:

    $ ./rel/refuge/bin/refuge makecert

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

### Binding port 80

On most UNIX systems binding port 80 is a privileged operation (requires
root). Running Erlang as root is not recommended so some configuration
will need to be done so that refuge can bind port 80.

If you run a recent Linux kernel with capabilities you can give Erlang
the privilege using the setcap command (you may need to install a
package named lxc or similar to obtain this command).

    $ setcap 'cap_net_bind_service=+ep' /path/to/rel/refuge/erts-5.8.5/bin/beam`
    $ setcap 'cap_net_bind_service=+ep' /path/to/rel/refuge/erts-5.8.5/bin/beam.smp

On FreeBSD all ports can be made accessible to all users by issuing:

$ sysctl net.inet.ip.portrange.reservedhigh=0
