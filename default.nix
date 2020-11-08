let
  miso-src = builtins.fetchTarball {
    url = "https://github.com/dmjio/miso/archive/ea25964565074e73d4052b56b60b6e101fa08bc5.tar.gz";
    sha256 = "1yb9yvc0ln4yn1jk2k5kwwa1s32310abawz40yd8cqqkm1z7w6wg";
  };

  miso = import miso-src { };

  # This is just a short cut for accessing the library of Haskell-related
  # functions provided by nixpkgs.
  haskell-lib = miso.pkgs.haskell.lib;

  # This is a set of overrides for the Haskell package set we use to compile
  # miso. We can define new packages in this set, as well as override/modify
  # existing packages.
  my-ghcjs-pkg-set-overrides = final: prev: {

    app = prev.callCabal2nix "app" ./. {};

    # json-to-haskell isn't in the set of Hackage packages that the nixpkgs
    # from miso knows about, so we can just get it directly from Hackage.
    # This is generally easier than manually calling `cabal2nix` and storing
    # the file locally.
    #
    # If miso gets updated to provide a more recent version of nixpkgs, then
    # this can be changed to use the `callHackage` function instead of
    # `callHackageDirect`.
    json-to-haskell = final.callHackageDirect {
      pkg = "json-to-haskell";
      ver = "0.0.1.0";
      sha256 = "0a9v8cmcidpav62b1cks41q4s62s34p60ad8isn8kgi34s331xwf";
    } {};

    # There are various dependencies that appear to be marked as broken.  Here,
    # I manually mark them as unbroken.  Let's hope that they do actually
    # compile.

    aeson-extra = haskell-lib.overrideCabal prev.aeson-extra { broken = false; };
    hedgehog-fn = haskell-lib.overrideCabal prev.hedgehog-fn { broken = false; };
    time-parsers = haskell-lib.overrideCabal prev.time-parsers { broken = false; };
    nonempty-containers = haskell-lib.overrideCabal prev.nonempty-containers { broken = false; };
  };

  # Here we create the actual package set by passing our set of overrides.
  my-ghcjs-pkg-set = miso.pkgs.haskell.packages.ghcjs.override {
    overrides = my-ghcjs-pkg-set-overrides;
  };

in

# We can pull out our "app" that is defined in our set of overrides.
my-ghcjs-pkg-set.app