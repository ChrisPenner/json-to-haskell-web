self: super:
let haskell-overlay = hself: hsuper:
    {
      # https://github.com/Gabriel439/haskell-nix/issues/58#issuecomment-409711895
      json-to-haskell = hself.callHackage "json-to-haskell" "0.0.1.0";
    };
in {
  haskell = super.haskell // {
    packageOverrides = super.lib.composeExtensions
      (super.haskell.packageOverrides or (self: super: {}))
      haskell-overlay;
  };
}
