# This script adds the Stdlib__ prefixes to the module aliases in
# stdlib.ml and stdlib.mli
BEGIN { in_aliases=0 }
NR == 1 { printf ("# 1 \"%s\"\n", FILENAME) }
$0 == "(*MODULE_ALIASES*)" { in_aliases=1 }
$1 == "module" && in_aliases \
  { $4="Stdlib__" tolower(substr($4,1,1)) substr($4,2) }
{ print }
