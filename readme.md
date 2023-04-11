# asn1-ber-inspect

Inspect BER-encoded data without any schema available. Use:

    asn1-ber-inspect path/to/data.bin
    asn1-ber-inspect < path/to/data.bin

Using a root certificate from the `example` directory:

    >>> asn1-ber-inspect examples/cfca-ev-root.bin

    universal:16: sequence
      universal:16: sequence
        context:0: constructed
          universal:2: 2
        universal:2: 407555286
        universal:16: sequence
          universal:6: [oid] 1.2.840.113549.1.1.11
          universal:5: null
        universal:16: sequence
          universal:17: set
            universal:16: sequence
              universal:6: [oid] 2.5.4.6
              universal:19: [printable-string] CN
          universal:17: set
            universal:16: sequence
              universal:6: [oid] 2.5.4.10
              universal:12: [utf8-string] China Financial Certification Authority
              ...

This intended to be used as an aid when writing decoders for BER syntax trees.
