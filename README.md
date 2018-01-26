# Cards

Search through cards with smart queries and a nice UI. 

For card games like Magic The Gathering.


## Development

The project structure comes from @reflex-platform@:

https://github.com/reflex-frp/reflex-platform/blob/develop/docs/project-development.md

and thus:

* is FULLHASKELL
* uses the @nix@ package manager (available on both @Linux@ and @OSX@, but not @Windows@) for explicit dependency management and miraculously reproducible builds
* can be compiled both natively (into assembly) via `ghc` and into javascript via `ghcjs`
* even supports cross-compilation to mobile platforms (@android@ only on @Linux@, @iOS@ only on @OSX@)
* has separate `frontend`/`backend`/`common` packages
* uses @reflex@ for FRP ("Functional Reactive Programming")
* uses DOM as the GUI model (HTML/CSS/JS)
* from the same code, it can create: a single-page web app (via @./install-webapp.sh@); a desktop application (via @./install-desktop.sh@); and a mobile app (via @./install-android.sh@ or @./install-ios.sh@)

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

You must be on either a @Linux@ or @OSX@.

2. Add a `nix` cache

Add a "trusted nix binary cache" for the @reflex-platform@ cache:

If not on NixOS:

    sudo nano /etc/nix/nix.conf

    # add these lines
    binary-caches = https://cache.nixos.org https://nixcache.reflex-frp.org 
    binary-cache-public-keys = ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=

If on NixOS:

    # I think you add those somewhere in your `configuration.nix`

This isn't necessary, but it's definitely preferred. 
Downloading the binaries takes less than 1han hour, building from source takes several hours. 

3. Provision The Environment

Downloads ghc, any system packages, and compiled haskell packages (object files):

    ./environment-ghc.sh

Downloads ghcjs, and compiled haskell packages (javascript files):

    ./environment-ghcjs.sh

downloading (or building) any dependencies necessary.

### Build

Build everything:

    ./build-ghc.sh

    ./build-ghcjs.sh

    ./build-ghc-frontend.sh

to make sure the build files have created the correct environment, 
and that the code works.  

(These scripts (@*.sh@) are all one-liners (or a few lines), named for convenience.
They call `nix-shell`, or `nix-build`, on `default.nix`. 

Build and launch the web app:

    ./install-webapp.sh

Build and launch the desktop app:

    ./install-desktop.sh

Build and launch a (platform-specific) mobile app:

    ./install-mobile.sh

### Develop

Enter a `nix-shell`, then use `cabal new-build`. 

