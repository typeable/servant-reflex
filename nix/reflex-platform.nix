let
  rev = "e8e56516b1df0c6e7024bc0e690f54fcddf79a19";
in import (builtins.fetchTarball
  {
    url = "https://github.com/reflex-frp/reflex-platform/archive/${rev}.tar.gz";
    sha256 = "0xb2hdps7bsyd6y84if3317w9fbcpcjh4whrvrhz6mskcw069xjp" ;
  }) {}
