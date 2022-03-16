(use-modules
  ((guix licenses) #:prefix license:)
  (guix packages)
  (guix download)
  (guix git-download)
  (guix build-system haskell)
  (gnu packages pkg-config)
  (gnu packages tls)
  (gnu packages gsasl)
  (gnu packages libidn)
  (gnu packages xml)
  (gnu packages dhall)
  (gnu packages haskell)
  (gnu packages haskell-check)
  (gnu packages haskell-crypto)
  (gnu packages haskell-web)
  (gnu packages haskell-xyz)
  (ice-9 rdelim)
  (ice-9 popen)
)

(define-public ghc-unexceptionalio-trans
  (package
    (name "ghc-unexceptionalio-trans")
    (version "0.5.1")
    (source
      (origin
        (method url-fetch)
        (uri (hackage-uri "unexceptionalio-trans" version))
        (sha256
          (base32 "100sfbrpaldz37a176qpfkk1nx5acyh8pchjmb8g5vhzbhyrqniz"))))
    (build-system haskell-build-system)
    (inputs (list ghc-unexceptionalio))
    (arguments
      `(#:cabal-revision
        ("1" "0f15n8hqqczwjrcqxwjp2mrd9iycv53sylv407c95nb6d4hw93ci")))
    (home-page "https://github.com/singpolyma/unexceptionalio-trans")
    (synopsis "A wrapper around UnexceptionalIO using monad transformers")
    (description
      "UnexceptionalIO provides a basic type to witness having caught all exceptions you can safely handle.  This library builds on that with transformers like ExceptT to provide a more ergonomic tool for many cases. . It is intended that you use qualified imports with this library. . > import UnexceptionalIO.Trans (UIO) > import qualified UnexceptionalIO.Trans as UIO")
    (license #f)))

(define-public ghc-cache
  (package
    (name "ghc-cache")
    (version "0.1.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/cache/cache-"
               version
               ".tar.gz"))
        (sha256
          (base32 "0d75257kvjpnv95ja50x5cs77pj8ccfr0nh9q5gzvcps83qdksa2"))))
    (build-system haskell-build-system)
    (inputs
      `(("ghc-clock" ,ghc-clock)
        ("ghc-hashable" ,ghc-hashable)
        ("ghc-unordered-containers" ,ghc-unordered-containers)))
    (native-inputs
      `(("ghc-hspec" ,ghc-hspec)
        ("hspec-discover" ,hspec-discover)))
    (home-page "https://github.com/hverr/haskell-cache#readme")
    (synopsis "An in-memory key/value store with expiration support")
    (description
      "An in-memory key/value store with expiration support, similar to patrickmn/go-cache for Go. . The cache is a shared mutable HashMap implemented using STM and with support for expiration times.")
    (license license:bsd-3)))

(define-public ghc-scanner
  (package
    (name "ghc-scanner")
    (version "0.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/scanner/scanner-"
               version
               ".tar.gz"))
        (sha256
          (base32 "1mhqh94qra08zidqfsq0gxi83cgflqldnk9rr53haynbgmd5y82k"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-fail" ,ghc-fail)))
    (native-inputs
      `(("ghc-hspec" ,ghc-hspec)
        ("pkg-config" ,pkg-config)))
    (home-page "https://github.com/Yuras/scanner")
    (synopsis
      "Fast non-backtracking incremental combinator parsing for bytestrings")
    (description
      "Parser combinator library designed to be fast. It doesn't support backtracking.")
    (license license:bsd-3)))

(define-public ghc-hedis
  (package
    (name "ghc-hedis")
    (version "0.12.11")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/hedis/hedis-"
               version
               ".tar.gz"))
        (sha256
          (base32 "1n83zwg011n9w2v1zz4mwpms9jh3c8mk700zya4as1jg83748xww"))))
    (build-system haskell-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             ; The main tests require redis-server running, but not doctest
             (invoke "runhaskell" "Setup.hs" "test" "doctest")
             #t)))))
    (inputs
      `(("ghc-scanner" ,ghc-scanner)
        ("ghc-async" ,ghc-async)
        ("ghc-bytestring-lexing" ,ghc-bytestring-lexing)
        ("ghc-unordered-containers" ,ghc-unordered-containers)
        ("ghc-network" ,ghc-network)
        ("ghc-resource-pool" ,ghc-resource-pool)
        ("ghc-tls" ,ghc-tls)
        ("ghc-vector" ,ghc-vector)
        ("ghc-http" ,ghc-http)
        ("ghc-errors" ,ghc-errors)
        ("ghc-network-uri" ,ghc-network-uri)))
    (native-inputs
      `(("ghc-hunit" ,ghc-hunit)
        ("ghc-test-framework" ,ghc-test-framework)
        ("ghc-test-framework-hunit" ,ghc-test-framework-hunit)
        ("ghc-doctest" ,ghc-doctest)))
    (home-page "https://github.com/informatikr/hedis")
    (synopsis
      "Client library for the Redis datastore: supports full command set, pipelining.")
    (description
      "Redis is an open source, advanced key-value store. It is often referred to as a data structure server since keys can contain strings, hashes, lists, sets and sorted sets. This library is a Haskell client for the Redis datastore. Compared to other Haskell client libraries it has some advantages: . [Compatibility with Latest Stable Redis:] Hedis is intended to be used with the latest stable version of Redis (currently 5.0). Most redis commands (<http://redis.io/commands>) are available as haskell functions, although MONITOR and SYNC are intentionally omitted. Additionally, a low-level API is exposed that  makes it easy for the library user to implement further commands, such as new commands from an experimental Redis version. . [Automatic Optimal Pipelining:] Commands are pipelined (<http://redis.io/topics/pipelining>) as much as possible without any work by the user. See <http://informatikr.com/2012/redis-pipelining.html> for a technical explanation of automatic optimal pipelining. . [Enforced Pub\\/Sub semantics:] When subscribed to the Redis Pub\\/Sub server (<http://redis.io/topics/pubsub>), clients are not allowed to issue commands other than subscribing to or unsubscribing from channels. This library uses the type system to enforce the correct behavior. . [Connect via TCP or Unix Domain Socket:] TCP sockets are the default way to connect to a Redis server. For connections to a server on the same machine, Unix domain sockets offer higher performance than the standard TCP connection. . For detailed documentation, see the \"Database.Redis\" module. .")
    (license license:bsd-3)))

(define-public ghc-libxml-sax
  (package
    (name "ghc-libxml-sax")
    (version "0.7.5")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/libxml-sax/libxml-sax-"
               version
               ".tar.gz"))
        (sha256
          (base32 "0lbdq6lmiyrnzk6gkx09vvp928wj8qnqnqfzy14mfv0drj21f54r"))))
    (build-system haskell-build-system)
    (inputs
      `(("ghc-xml-types" ,ghc-xml-types)
        ("libxml2" ,libxml2)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://john-millikin.com/software/haskell-libxml/")
    (synopsis "Bindings for the libXML2 SAX interface")
    (description "")
    (license license:expat)))

(define-public ghc-gsasl
  (package
    (name "ghc-gsasl")
    (version "0.3.7")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/gsasl/gsasl-"
               version
               ".tar.gz"))
        (sha256
          (base32 "11i12r9s30jrq8hkgqagf2fd129r6ya607s9ibw549ablsxgr507"))))
    (build-system haskell-build-system)
    (arguments
      `(#:cabal-revision
        ("1" "1c806a82qd1hkxxfh1mwk0i062bz6fkaap5ys3n4x9n6wjv7ilin")))
    (inputs
      `(("ghc-monad-loops" ,ghc-monad-loops)
        ("gsasl" ,gsasl)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://git.sr.ht/~singpolyma/gsasl-haskell")
    (synopsis "Bindings for GNU libgsasl")
    (description "")
    (license license:gpl3)))

(define-public ghc-gnutls
  (package
    (name "ghc-gnutls")
    (version "0.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/gnutls/gnutls-"
               version
               ".tar.gz"))
        (sha256
          (base32 "1c5pm0d80wpgh2bkcgbvmc72agf89h8ghfnrn1m1x3fljbgzvrn0"))))
    (build-system haskell-build-system)
    (inputs
      `(("ghc-monads-tf" ,ghc-monads-tf)
        ("gnutls" ,gnutls)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://john-millikin.com/software/haskell-gnutls/")
    (synopsis "Bindings for GNU libgnutls")
    (description
      "You almost certainly don't want to depend on this release. . This is a pre-alpha, almost useless release; its only purpose is to enable TLS support in some of my other libraries. More complete bindings for GNU TLS will be released at a later date.")
    (license license:gpl3)))

(define-public ghc-gnuidn
  (package
    (name "ghc-gnuidn")
    (version "0.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/gnuidn/gnuidn-"
               version
               ".tar.gz"))
        (sha256
          (base32 "0vxrcp9xz5gsvx60k12991zn5c9nk3fgg0yw7dixbsjcfqgnnd31"))))
    (build-system haskell-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'less-strict-dependencies
           (lambda _
             (substitute* "gnuidn.cabal"
               (("chell >= 0.4 && < 0.5") "chell <0.6"))
             #t)))))
    (inputs `(("libidn" ,libidn)))
    (native-inputs
      `(("ghc-chell" ,ghc-chell)
        ("ghc-c2hs" ,ghc-c2hs)
        ("ghc-chell-quickcheck" ,ghc-chell-quickcheck)
        ("ghc-quickcheck" ,ghc-quickcheck)
        ("pkg-config" ,pkg-config)))
    (home-page "https://john-millikin.com/software/haskell-gnuidn/")
    (synopsis "Bindings for GNU IDN")
    (description "")
    (license license:gpl3)))

(define-public ghc-network-simple
  (package
    (name "ghc-network-simple")
    (version "0.4.5")
    (source
      (origin
        (method url-fetch)
        (uri (hackage-uri "network-simple" version))
        (sha256
          (base32 "17hpgcwrsx2h8lrb2wwzy0anp33mn80dnwcgnqmb8prajwjvz807"))))
    (build-system haskell-build-system)
    (inputs (list ghc-network ghc-network-bsd ghc-safe-exceptions ghc-socks))
    (home-page "https://github.com/k0001/network-simple")
    (synopsis "Simple network sockets usage patterns.")
    (description
      "This module exports functions that abstract simple network socket usage patterns. . See the @changelog.md@ file in the source distribution to learn about any important changes between versions.")
    (license license:bsd-3)))

(define-public ghc-base58-bytestring
  (package
    (name "ghc-base58-bytestring")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (hackage-uri "base58-bytestring" version))
        (sha256
          (base32 "1ls05nzswjr6aw0wwk3q7cpv1hf0lw7vk16a5khm6l21yfcgbny2"))))
    (build-system haskell-build-system)
    (native-inputs
      (list ghc-quickcheck-assertions
            ghc-quickcheck-instances
            ghc-tasty
            ghc-tasty-quickcheck))
    (home-page "https://bitbucket.org/s9gf4ult/base58-bytestring")
    (synopsis "Implementation of BASE58 transcoding for ByteStrings")
    (description
      "Implementation of BASE58 transcoding copy-pasted from haskoin package")
    (license license:public-domain)))

(define-public ghc-network-protocol-xmpp
  (package
    (name "ghc-network-protocol-xmpp")
    (version "0.4.10")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/network-protocol-xmpp/network-protocol-xmpp-"
               version
               ".tar.gz"))
        (sha256
          (base32 "03xlw8337lzwp7f5jvbvgirf546pfmfsfjvnik08qjjy1rfn5jji"))))
    (build-system haskell-build-system)
    (inputs
      `(("ghc-gnuidn" ,ghc-gnuidn)
        ("ghc-gnutls" ,ghc-gnutls)
        ("ghc-gsasl" ,ghc-gsasl)
        ("ghc-libxml-sax" ,ghc-libxml-sax)
        ("ghc-monads-tf" ,ghc-monads-tf)
        ("ghc-network" ,ghc-network)
        ("ghc-network-simple" ,ghc-network-simple)
        ("ghc-xml-types" ,ghc-xml-types)))
    (home-page "https://git.sr.ht/~singpolyma/network-protocol-xmpp")
    (synopsis "Client library for the XMPP protocol.")
    (description "")
    (license license:gpl3)))

(define-public ghc-hstatsd
  (package
    (name "ghc-hstatsd")
    (version "0.1")
    (source
      (origin
        (method url-fetch)
        (uri (hackage-uri "hstatsd" version))
        (sha256
          (base32 "092q52yyb1xdji1y72bdcgvp8by2w1z9j717sl1gmh2p89cpjrs4"))))
    (build-system haskell-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-for-network-3
           (lambda _
             (substitute* "src/Network/StatsD.hs"
               (("sClose") "close"))
             #t)))))
    (inputs (list ghc-network))
    (home-page "https://github.com/mokus0/hstatsd")
    (synopsis "Quick and dirty statsd interface")
    (description "Quick and dirty statsd interface")
    (license license:public-domain)))

(define-public ghc-random-shuffle
  (package
    (name "ghc-random-shuffle")
    (version "0.0.4")
    (source
      (origin
        (method url-fetch)
        (uri (hackage-uri "random-shuffle" version))
        (sha256
          (base32 "0586bnlh0g2isc44jbjvafkcl4yw6lp1db8x6vr0pza0y08l8w2j"))))
    (build-system haskell-build-system)
    (inputs (list ghc-random ghc-monadrandom))
    (home-page "http://hackage.haskell.org/package/random-shuffle")
    (synopsis "Random shuffle implementation.")
    (description
      "Random shuffle implementation, on immutable lists. Based on `perfect shuffle' implementation by Oleg Kiselyov, available on http://okmij.org/ftp/Haskell/perfect-shuffle.txt")
    (license license:bsd-3)))

(define-public ghc-stm-delay
  (package
    (name "ghc-stm-delay")
    (version "0.1.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (hackage-uri "stm-delay" version))
        (sha256
          (base32 "0cla21v89gcvmr1iwzibq13v1yq02xg4h6k9l6kcprj7mhd5hcmi"))))
    (build-system haskell-build-system)
    (home-page "https://github.com/joeyadams/haskell-stm-delay")
    (synopsis "Updatable one-shot timer polled with STM")
    (description
      "This library lets you create a one-shot timer, poll it using STM, and update it to ring at a different time than initially specified. . It uses GHC event manager timeouts when available (GHC 7.2+, @-threaded@, non-Windows OS), yielding performance similar to @threadDelay@ and @registerDelay@.  Otherwise, it falls back to forked threads and @threadDelay@. . [0.1.1] Add tryWaitDelayIO, improve performance for certain cases of @newDelay@ and @updateDelay@, and improve example.")
    (license license:bsd-3)))

(define-public ghc-hostandport
  (package
    (name "ghc-hostandport")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (hackage-uri "HostAndPort" version))
        (sha256
          (base32 "1rjv6c7j6fdy6gnn1zr5jnfmiqiamsmjfw9h3bx119giw3sjb9hm"))))
    (build-system haskell-build-system)
    (native-inputs
      (list ghc-hspec ghc-doctest))
    (home-page "https://github.com/bacher09/hostandport")
    (synopsis "Parser for host and port pairs like localhost:22")
    (description
      "Simple parser for parsing host and port pairs. Host can be either ipv4, ipv6 or domain name and port are optional. . IPv6 address should be surrounded by square brackets. . Examples: . * localhost . * localhost:8080 . * 127.0.0.1 . * 127.0.0.1:8080 . * [::1] . * [::1]:8080")
    (license license:expat)))

(define-public ghc-binary-varint
  (package
    (name "ghc-binary-varint")
    (version "0.1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (hackage-uri "binary-varint" version))
        (sha256
          (base32 "1i183ab4bbq3yarijnb2pwgbi9k1w1nc0fs6ph8d8xnysj6ws8l8"))))
    (build-system haskell-build-system)
    (home-page "https://github.com/oscoin/ipfs")
    (synopsis "VarInt encoding/decoding via Data.Binary")
    (description "")
    (license license:bsd-3)))

(define-public ghc-multihash-cryptonite
  (package
    (name "ghc-multihash-cryptonite")
    (version "0.1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (hackage-uri "multihash-cryptonite" version))
        (sha256
          (base32 "0gl13kjqz14lnwz7x162fad3j99qs1xa3zabpr30q53pkzk8adsi"))))
    (build-system haskell-build-system)
    (inputs (list ghc-binary-varint ghc-cryptonite ghc-hashable ghc-memory))
    (native-inputs (list ghc-hedgehog ghc-doctest ghc-cabal-doctest))
    (home-page "https://github.com/oscoin/ipfs")
    (synopsis
      "Self-identifying hashes, implementation of <https://github.com/multiformats/multihash>")
    (description "")
    (license license:bsd-3)))

(define-public ghc-cpu
  (package
    (name "ghc-cpu")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (hackage-uri "cpu" version))
        (sha256
          (base32 "0x19mlanmkg96h6h1i04w2i631z84y4rbk22ki4zhgsajysgw9sn"))))
    (build-system haskell-build-system)
    (home-page "http://github.com/vincenthz/hs-cpu")
    (synopsis "Cpu information and properties helpers.")
    (description
      "Lowlevel cpu routines to get basic properties of the cpu platform, like endianness and architecture.")
    (license license:bsd-3)))

(define-public ghc-base32-z-bytestring
  (package
    (name "ghc-base32-z-bytestring")
    (version "1.0.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (hackage-uri "base32-z-bytestring" version))
        (sha256
          (base32 "1r0235a2qqnavsm7jl807m555yd2k2vi2kfacw878v83zdr5qyix"))))
    (build-system haskell-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-internal-reference
           (lambda _
             (substitute* "base32-z-bytestring.cabal"
               (("z-base32-bytestring") "base32-z-bytestring"))
             #t)))))
    (inputs (list ghc-cpu))
    (native-inputs
      (list ghc-hedgehog
            ghc-tasty
            ghc-tasty-fail-fast
            ghc-tasty-hedgehog
            ghc-tasty-hspec))
    (home-page "https://github.com/oscoin/z-base32-bytestring")
    (synopsis "Fast z-base32 and z-base32hex codec for ByteStrings")
    (description
      "base32 and base32hex codec according to RFC4648 <http://tools.ietf.org/html/rfc4648>, extended to support z-base32 encoding according to <https://gist.github.com/maaku/8996338#file-bip-ecc32-mediawiki> . The package API is similar to base64-bytestring.")
    (license license:bsd-3)))

(define-public ghc-formatting
  (package
    (name "ghc-formatting")
    (version "7.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (hackage-uri "formatting" version))
        (sha256
          (base32 "1vrc2i1b6lxx2aq5hysfl3gl6miq2wbhxc384axvgrkqjbibnqc0"))))
    (build-system haskell-build-system)
    (inputs
      (list ghc-clock ghc-old-locale ghc-scientific ghc-double-conversion))
    (native-inputs (list ghc-hspec))
    (home-page "https://github.com/AJChapman/formatting#readme")
    (synopsis
      "Combinator-based type-safe formatting (like printf() or FORMAT)")
    (description
      "Combinator-based type-safe formatting (like printf() or FORMAT), modelled from the HoleyMonoids package. . See the README at <https://github.com/AJChapman/formatting#readme> for more info.")
    (license license:bsd-3)))

(define-public ghc-tasty-tap
  (package
    (name "ghc-tasty-tap")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (hackage-uri "tasty-tap" version))
        (sha256
          (base32 "16i7pd0xis1fyqgmsy4mq04y87ny61dh2lddnjijcf1s9jz9b6x8"))))
    (build-system haskell-build-system)
    (inputs (list ghc-tasty))
    (native-inputs (list ghc-tasty-hunit ghc-tasty-golden))
    (home-page "https://github.com/michaelxavier/tasty-tap")
    (synopsis "TAP (Test Anything Protocol) Version 13 formatter for tasty")
    (description "A tasty ingredient to output test results in TAP 13 format.")
    (license license:expat)))

(define-public ghc-tasty-fail-fast
  (package
    (name "ghc-tasty-fail-fast")
    (version "0.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/MichaelXavier/tasty-fail-fast")
             (commit "68d7f182f4d1f7b97a724c26f554e5da27fe9413")))
       (file-name (git-file-name name version))
       (sha256
         (base32 "05x4ly5sfj5fmjsxxrfys20qc6n078vwaxxzlk2l354l7kng5512"))))
    (build-system haskell-build-system)
    (inputs (list ghc-tasty ghc-tagged))
    (native-inputs (list ghc-tasty-hunit ghc-tasty-golden ghc-tasty-tap))
    (home-page "http://github.com/MichaelXavier/tasty-fail-fast#readme")
    (synopsis
      "Adds the ability to fail a tasty test suite on first test failure")
    (description
      "tasty-fail-fast wraps any ingredient to fail as soon as the first test fails. For example: . @ defaultMainWithIngredients (map failFast defaultIngredients) tests @ . Your test suite will now get a @--fail-fast@ flag.")
    (license license:bsd-3)))

(define-public ghc-multibase
  (package
    (name "ghc-multibase")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (hackage-uri "multibase" version))
        (sha256
          (base32 "036caj0dzhzp065dhy05flz2j5qml5pirs1y95np4hf2xv9jk32h"))))
    (build-system haskell-build-system)
    (inputs
      (list ghc-aeson
            ghc-base16-bytestring
            ghc-base32-z-bytestring
            ghc-base58-bytestring
            ghc-base64-bytestring
            ghc-formatting
            ghc-hashable
            ghc-sandi
            ghc-serialise
            ghc-tagged))
    (native-inputs (list ghc-doctest ghc-quickcheck ghc-cabal-doctest))
    (home-page "https://github.com/oscoin/ipfs")
    (synopsis
      "Self-identifying base encodings, implementation of <https://github.com/multiformats/multihash>")
    (description "")
    (license license:bsd-3)))

(define-public ghc-ipld-cid
  (package
    (name "ghc-ipld-cid")
    (version "0.1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (hackage-uri "ipld-cid" version))
        (sha256
          (base32 "1y08j0ibcrpfcm0zv1h17zdgbl3hm3sjvm0w9bk1lzdipd6p6cwj"))))
    (build-system haskell-build-system)
    (inputs
      (list ghc-binary-varint
            ghc-cryptonite
            ghc-hashable
            ghc-multibase
            ghc-multihash-cryptonite))
    (native-inputs (list ghc-hedgehog))
    (home-page "https://github.com/oscoin/ipfs")
    (synopsis "IPLD Content-IDentifiers <https://github.com/ipld/cid>")
    (description "")
    (license license:bsd-3)))

(define-public ghc-jingle
  (package
    (name "ghc-jingle")
    (version "4c93bbd")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (recursive? #t)
             (url "https://git.singpolyma.net/jingle-xmpp")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
         (base32 "09y3wbskv11dg5vcvgw5zlii0brijr0rd5m1s8r6ws5h53k53n7r"))))
    (build-system haskell-build-system)
    (inputs
      (list
        ghc-base64-bytestring
        ghc-basic-prelude
        ghc-cache
        ghc-clock
        ghc-cryptonite
        ghc-errors
        ghc-ipld-cid
        ghc-network
        ghc-network-protocol-xmpp
        ghc-multihash-cryptonite
        ghc-socks
        ghc-unexceptionalio))
    (home-page "https://github.com/bacher09/hostandport")
    (synopsis "Parser for host and port pairs like localhost:22")
    (description
      "Simple parser for parsing host and port pairs. Host can be either ipv4, ipv6 or domain name and port are optional. . IPv6 address should be surrounded by square brackets. . Examples: . * localhost . * localhost:8080 . * 127.0.0.1 . * 127.0.0.1:8080 . * [::1] . * [::1]:8080")
    (license license:expat)))

;;;;

(define %source-dir (dirname (current-filename)))
(define %git-dir (string-append %source-dir "/.git"))

; double-escaped template of the cheogram sexp
; This allows us to bake the expression without doing a full eval to a record,
; so it can be written
(define-public cheogram-template
  '(package
    (name "cheogram")
    (version (read-line (open-pipe* OPEN_READ "git" "--git-dir" %git-dir "describe" "--always" "--dirty")))
    (source
     `(origin
       (method git-fetch)
       (uri (git-reference
             (recursive? #t)
             (url "https://git.singpolyma.net/cheogram")
             (commit ,(read-line (open-pipe* OPEN_READ "git" "--git-dir" %git-dir "rev-parse" "HEAD")))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         ,(read-line (open-pipe* OPEN_READ "guix" "hash" "-rx" %source-dir))))))
    (build-system 'haskell-build-system)
    (inputs
      '(list
        dhall
        ghc-attoparsec
        ghc-base58-bytestring
        ghc-base64-bytestring
        ghc-basic-prelude
        ghc-cache
        ghc-clock
        ghc-errors
        ghc-hedis
        ghc-hostandport
        ghc-hstatsd
        ghc-http
        ghc-http-streams
        ghc-http-types
        ghc-jingle
        ghc-magic
        ghc-mmorph
        ghc-monad-loops
        ghc-monads-tf
        ghc-mime-types
        ghc-network
        ghc-network-protocol-xmpp
        ghc-network-uri
        ghc-pcre-light
        ghc-random-shuffle
        ghc-safe
        ghc-sha
        ghc-stm-delay
        ghc-unexceptionalio-trans
        ghc-utility-ht
        ghc-uuid
        ghc-xml-types))
    (home-page "https://git.singpolyma.net/cheogram")
    (synopsis "")
    (description "")
    (license 'license:agpl3)))

; Baked version of cheogram-template with leaves eval'd
(define-public cheogram-baked
 (cons
  (car cheogram-template)
  (map
   (lambda (x) (list (car x) (eval (cadr x) (current-module))))
   (cdr cheogram-template))))

; Build clean from git the version from a local clone
; To build whatever is sitting in local use:
; guix build --with-source=$PWD -f guix.scm

(eval cheogram-baked (current-module))
