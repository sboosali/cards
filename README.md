# Cards

Search through cards with smart queries and a nice UI. 

For card games like Magic The Gathering.


## Development

The project structure comes from `reflex-platform`:

https://github.com/reflex-frp/reflex-platform/blob/develop/docs/project-development.md

and thus:

* is FULLHASKELL
* uses the `nix` package manager (available on both `Linux` and `OSX`, but not `Windows`) for explicit dependency management and miraculously reproducible builds
* can be compiled both natively (into assembly) via `ghc` and into javascript via `ghcjs`
* even supports cross-compilation to mobile platforms (`android` only on `Linux`, `iOS` only on `OSX`)
* has separate `frontend`/`backend`/`common` packages
* uses `reflex` for FRP ("Functional Reactive Programming")
* uses DOM as the GUI model (HTML/CSS/JS)
* from the same code, it can create: a single-page web app (via `./install-webapp.sh`); a desktop application (via `./install-desktop.sh`); and a mobile app (via `./install-android.sh` or `./install-ios.sh`)

### Setup

1. Install `nix`

Download:

    curl https://nixos.org/nix/install | sh

And extend your `PATH`:

    nano ~/.bashrc

    # add these lines
    if [ -f  "$HOME/.nix-profile/etc/profile.d/nix.sh" ]; then
      source "$HOME/.nix-profile/etc/profile.d/nix.sh"
    fi

You must be on either a `Linux` or `OSX`.

2. Add a `nix` cache

Add a "trusted nix binary cache" for the `reflex-platform` cache

This isn't necessary, but it's definitely preferred. 
Downloading the binaries takes less than 1han hour, building from source takes several hours. 

If on NixOS:

    # idk, but you 

If on a Linux:

edit `nix.conf`:

    sudo nano /etc/nix/nix.conf

to add:

    build-max-jobs = 8
    build-cores = 1
    build-use-sandbox = false
    trusted-users = root
    allowed-users = *
    signed-binary-caches = *

    # NOTE if a `binary-caches` field is already present,
    # duplicates overwrite, they don't append.
    binary-caches = https://cache.nixos.org https://nixcache.reflex-frp.org
    binary-cache-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=

Then restart the nix-daemon (which doesn't automatically reload its config),
with whatever your service manager is. Something like:

    SERVICE_MANAGER=systemctl    # e.g. systemd, etc
    sudo ${SYSTEM_MANAGER} stop  org.nixos.nix-daemon
    sudo ${SYSTEM_MANAGER} start org.nixos.nix-daemon
    # NOTE I didn't need this, so I haven't tested it

(This reboot isn't always necessary, and if the above doesn't work,
rebooting your computer should).

If on NixOS:

I think you add the equivalent options somewhere in your `configuration.nix`.

If on OSX:

Like Linux, but the operating systems have different service managers, so restarting daemons uses `launchctl`. i.e...

edit `nix.conf`:

    sudo nano /etc/nix/nix.conf

to add:

    build-max-jobs = 8
    build-cores = 1
    build-use-sandbox = false
    trusted-users = root
    allowed-users = *
    signed-binary-caches = *

    binary-caches = https://cache.nixos.org https://nixcache.reflex-frp.org
    binary-cache-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=

then restart the nix-daemon:

    sudo systemctl stop  org.nixos.nix-daemon 
    sudo systemctl start org.nixos.nix-daemon

3. Provision The Environment

Downloads ghc, any system packages, and compiled haskell packages (object files):

    ./environment-ghc.sh

Downloads ghcjs, and compiled haskell packages (javascript files):

    ./environment-ghcjs.sh

downloading (or building) any dependencies necessary.

These are like the `try-reflex` script mentioned in `reflex-platform`. 

### Build

Build everything:

    ./build-ghc.sh

    ./build-ghcjs.sh

    ./build-ghc-frontend.sh

to make sure the build files have created the correct environment, 
and that the code works.  

(These scripts (`*.sh`) are all one-liners (or a few lines), named for convenience.
They call `nix-shell`, or `nix-build`, on `default.nix`. 

Build and launch the web app:

    ./install-webapp.sh

Build and launch the desktop app:

    ./install-desktop.sh

Build and launch a (platform-specific) mobile app:

    ./install-mobile.sh


### Develop

Enter a `nix-shell`, then use `cabal new-build`. 

    nix-shell -A shells.<COMPILER> --run 'cabal new-build <TARGET>'

where `<TARGET>` is either (1) a package listed in a `.project` (2) or `all`.

The `./build-*.sh` scripts do this. 

Or the `./build` script, for convenience:

    ./build c f

a.k.a.

    ./build ghc cards-frontend

or

    ./build j a

a.k.a.

    ./build ghcjs all

where

    $ cat ./build
    ...
    nix-shell -A "shells.$COMPILER" --run "cabal new-build $PACKAGE"

(edit `./build` to customize it). 

e.g. test your frontend with ghc, which is faster than ghcjs, and has better editor support (via its working interpreter):

    nix-shell -A shells.ghc --run 'cabal new-build cards-frontend'

a.k.a

    ./build-ghc-frontend.sh 

###

