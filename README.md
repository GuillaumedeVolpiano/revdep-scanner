# revdep-scanner
Scan Gentoo repositories for reverse dependencies

```
Usage: revdep-scanner [OPTION...] <cat/pkg[-ver]>

This utility will scan a Gentoo repository and gather dependency information.

--matching (default when no version is provided)
Looks for dependencies that match the given package atom.

--non-matching (default when version is provided)
Looks for dependency constraints that would reject the provided
package/version.
For example: revdep-scanner dev-haskell/network-3.2 would match
"<dev-haskell/network-3.2" as a problematic dependency.

  -h             --help             Show this help text
  -r REPOSITORY  --repo=REPOSITORY  Limit to a repository (defaults to "haskell")
                 --debug            Display debug information
                 --matching         Look for matching dependencies
                 --non-matching     Look for non-matching relevant dependencies
```

---

Example:

```
$ revdep-scanner dev-haskell/network-3.2
Packages with at least one problematic constraint:

package
    ( relevant constraints ):

dev-haskell/dbus-1.3.2
    ( >=dev-haskell/network-3.1.2.1 <dev-haskell/network-3.2 )
dev-haskell/hookup-0.7
    ( >=dev-haskell/network-3.0 <dev-haskell/network-3.2 )
dev-haskell/reflex-backend-socket-0.2.0.1-r1
    ( >=dev-haskell/network-2.6 <dev-haskell/network-3.2 )
net-irc/glirc-2.39.0.1-r2
    ( >=dev-haskell/network-2.6.2 <dev-haskell/network-3.2 )
www-apps/clckwrks-0.28.0.1-r1
    ( >dev-haskell/network-2.6 <dev-haskell/network-3.2 )
www-apps/clckwrks-cli-0.3.0.5
    ( >=dev-haskell/network-2.3 <dev-haskell/network-3.2 )
```
