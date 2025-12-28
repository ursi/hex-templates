p:
let
  deku-src.git = {
    repo = "https://github.com/mikesol/purescript-deku.git";
    rev = "98c67533cc8c399aa643b495d3c02bab963e5b80";
  };
in
_: _: {
  deku-core = {
    src = deku-src;
    info = {
      version = "0.12.1";
      dependencies = [
        "aff"
        "arrays"
        "catenable-lists"
        "control"
        "effect"
        "either"
        "fast-vect"
        "filterable"
        "foldable-traversable"
        "foreign-object"
        "free"
        "heterogeneous"
        "hyrule"
        "maybe"
        "newtype"
        "nullable"
        "ordered-collections"
        "prelude"
        "profunctor"
        "quickcheck"
        "record"
        "safe-coerce"
        "st"
        "strings"
        "stringutils"
        "tldr"
        "transformers"
        "typelevel-prelude"
        "tuples"
        "unsafe-coerce"
        "unsafe-reference"
        "untagged-union"
        "web-dom"
        "web-dom-parser"
        "web-events"
        "web-html"
        "web-uievents"
        "yoga-json"
      ];
      src = "deku-core";
    };
  };

  deku-dom = {
    src = deku-src;
    info = {
      version = "0.12.1";
      dependencies = [
        "deku-core"
        "hyrule"
        "web-html"
        "web-uievents"
        "web-pointerevents"
        "web-touchevents"
      ];
      src = "deku-dom";
      foreign."Deku.Pursx".node_modules =
        p.importNpmLock.buildNodeModules
          {
            npmRoot = ./.;
            inherit (p) nodejs;
          } + /node_modules;
    };
  };

  hyrule = {
    src.git = {
      repo = "https://github.com/mikesol/purescript-hyrule.git";
      rev = "122ee1745c9faa60d6e9659db4291032cb5db053";
    };

    info = {
      version = "2.5.2";
      dependencies = [
        "aff"
        "arrays"
        "avar"
        "contravariant"
        "control"
        "datetime"
        "effect"
        "either"
        "filterable"
        "foldable-traversable"
        "foreign-object"
        "free"
        "functors"
        "js-timers"
        "maybe"
        "newtype"
        "now"
        "ordered-collections"
        "partial"
        "prelude"
        "profunctor"
        "random"
        "refs"
        "safe-coerce"
        "st"
        "tailrec"
        "tuples"
        "unsafe-coerce"
        "web-events"
        "web-html"
        "web-uievents"
      ];
    };
  };
}
