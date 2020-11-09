let
  miso-src = builtins.fetchTarball {
    url = "https://github.com/dmjio/miso/archive/ea25964565074e73d4052b56b60b6e101fa08bc5.tar.gz";
    sha256 = "1yb9yvc0ln4yn1jk2k5kwwa1s32310abawz40yd8cqqkm1z7w6wg";
  };

  miso = import miso-src { };

  # This is just a shortcut for accessing the library of Haskell-related
  # functions provided by nixpkgs.
  haskell-lib = miso.pkgs.haskell.lib;

  # This is a set of overrides for the Haskell package set we use to compile
  # miso. We can define new packages in this set, as well as override/modify
  # existing packages.
  my-ghcjs-pkg-set-overrides = final: prev: {

    # Our local app.
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
      ver = "0.1.1.1";
      sha256 = "1fmniii0y40qgr32ddmzwyj8vn6q4kwz6cg1nslfp3m9v6l9wycr";
    } {};

    # There are various dependencies that appear to be marked as broken.  Here,
    # I manually mark them as unbroken.  Let's hope that they do actually
    # compile.
    aeson-extra = haskell-lib.overrideCabal prev.aeson-extra { broken = false; };
    time-parsers = haskell-lib.overrideCabal prev.time-parsers { broken = false; };

    # Nonempty-containers depends on the hedgehog-fn package for tests, but it
    # doesn't have required dependencies.  The easiest fix is just to disable
    # tests for nonempty-containers.
    #
    # Also, haddock generation seems messed up for some reason, so disable that
    # too.
    nonempty-containers =
      haskell-lib.overrideCabal
        (haskell-lib.dontHaddock (haskell-lib.dontCheck prev.nonempty-containers))
        { broken = false; };

    # Test suits for various packages don't work when run with GHCJS.  We just
    # disable these.
    QuickCheck = haskell-lib.dontCheck prev.QuickCheck;
    tasty-quickcheck = haskell-lib.dontCheck prev.tasty-quickcheck;
    scientific = haskell-lib.dontCheck prev.scientific;
    base-compat-batteries = haskell-lib.dontCheck prev.base-compat-batteries;

    # A bunch of things use doctest in their tests, but doctest doesn't appear
    # to be able to be compiled, so we just disable tests for all these.
    comonad = haskell-lib.dontCheck prev.comonad;
    lens = haskell-lib.dontCheck prev.lens;
    semigroupoids = haskell-lib.dontCheck prev.semigroupoids;
    http-types = haskell-lib.dontCheck prev.http-types;
    servant = haskell-lib.dontCheck prev.servant;
  };

  # Here we create the actual package set by passing our set of overrides.
  my-ghcjs-pkg-set = miso.pkgs.haskell.packages.ghcjs.override {
    overrides = my-ghcjs-pkg-set-overrides;
  };

in

# We can pull out our "app" that is defined in our set of overrides.
my-ghcjs-pkg-set.app
