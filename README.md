# ksum

`ksum` is a program to print checksums using algorithms based on Keccak[1]
such as SHA-3.

The following algorithms are supported by `ksum`:
* SHA-3 and SHAKE as defined in NIST FIPS 202 [2]:
  * SHA3-224, SHA3-256, SHA3-384, and SHA3-512
  * SHAKE128 and SHAKE256
  * RawSHAKE128 and RawSHAKE256
* Keccak hash functions as defined by the Keccak team:
  * Keccak-224, Keccak-256, Keccak-384, and Keccak-512
* cSHAKE, KMAC and ParallelHash as defined in NIST SP 800-185 [3]:
  * cSHAKE128 and cSHAKE256
  * KMAC128 and KMAC256
  * ParallelHash128 and ParallelHash256
* KangarooTwelve as defined by the Keccak authors in [4]

## Usage

`ksum` is invoked by selecting an algorithm (e.g. `--sha3-256`) and passing in
the names of one or more file names. `kmac` will then output the checksum
for each file, in order. For example:

```
$ ksum --sha3-256 file1 file2 file3
be5215abf72333a73b992dafdf4ab59884b948452e0015cfaddaa0b87a0e4515  file1
006ef4138df934503f34702cfc24b743664b78635dd65844413d464e2867729c  file2
83abc349ca290d8be32afe3d2d1774af58fd799d33afbe8db64afb4572611d39  file3
```

If `-` is given on the command line then `ksum` will read from the standard
input. For example:
```
$ cat file1 | ksum --sha3-256 -
be5215abf72333a73b992dafdf4ab59884b948452e0015cfaddaa0b87a0e4515  -
```

### Variable output length

Several of the algorithms supported by `ksum` can output a variable length
checksum. For these algorithms, the `-n` or `--output-size` switches control
the length of the checksum in bytes. Here's an example using KangarooTwelve
(see [4]) to produce different length checksums:

```
$ ksum --kangarootwelve --output-size=16 file1
7290cf088e4898b86fa4c8cb8f6b5225  file1

$ ksum --kangarootwelve --output-size=32 file1
7290cf088e4898b86fa4c8cb8f6b522599ef37635628165300a52cbb63a0eceb  file1
```

ParallelHash and KMAC (see [3]) have two different output modes based on the
presence of the `-x` or `--xof` switch. 
**Without** `--xof` the output checksum depends on the requested output length. 
I.e. different output lengths will produce different checksums. 
For example:

```
$ ksum --parallelhash128 --output-size=16 file1
996a86e8b085365c4b25e4805a58814c  file1

$ ksum --parallelhash128 --output-size=32 file1
867b91a64e1a169adcfee20c3b0a128cc7a45635e9ce8393c4bed74289f3919a  file1
```

Notice that completely different checksums are produced with different lengths.
**With** the `--xof` switch the output checksum does not depend on the output
length. For example:

```
$ ksum --parallelhash128 --xof --output-size=16 file1
8f061ea9ea89fd17c258c819a08d58b8  file1

$ ksum --parallelhash128 --xof --output-size=32 file1
8f061ea9ea89fd17c258c819a08d58b8af4bf2f767b15de7c5d39287157f4478  file1
```

Notice that _with_ `--xof` the same checksum is produced, just with a different
length.

### KMAC Key

When `--kmac128` or `--kmac256` is used a variable-length key can be set using
the `-k` or `--key` switches which take a hexadecimal string specifying the key.
Here's an example of using KMAC128 with a 128-bit (16 bytes) key:

```
$ ksum --kmac128 --key="000102030405060708090a0b0c0d0e0f" file1
e324342e19ef845694570ede5e53a53b  file1
```

The default key for KMAC is an empty key (length of 0 bytes).

### Customization Strings
KangarooTwelve, KMAC, ParallelHash, and cSHAKE all take an optional customization
string which may be used to produce _domain separation_ among different uses
of the algorithms. I.e. using different customization strings with the same input
will produce unrelated outputs. 
The customization string is set using the `-C` or `--customization` switches.
Here's an example:

```
$ ksum --parallelhash128 --customization="hello" file1
52a4c643ad365c44e9552d88c0dd5917  file1

$ ksum --parallelhash128 --customization="world" file1
149122d772260cdb969511ae5aa56a85  file1
```

The default customization string is the empty string.

### ParallelHash Block Size
ParallelHash takes an optional block size parameter which configures the length
of each block (in bytes) that are processed in parallel. The block size (in bytes)
is configured using the `-B` or `--block-size` switches. The output checksum is
dependent on the block size. I.e. different block sizes will produce different
(and unrelated) output checksums.
For example:

```
$ ksum --parallelhash128 --block-size=8192 file1
996a86e8b085365c4b25e4805a58814c  file1
$ ksum --parallelhash128 --block-size=4096 file1
111b197c98641f63feab7d24390c5b1c  file1
```

The default block size for ParallelHash is 8192 bytes (8 kiB).

## Building `ksum`

Building `ksum` requires an Ada 2012 compatible compiler which also understands
SPARK 2014. One such compiler is
[GNAT GPL 2017 from AdaCore](https://libre.adacore.com/download).

`ksum` is built using `gprbuild`, which is included in GNAT GPL 2017 or can be
downloaded separately at the above AdaCore link.

You will also need to build and install [`libkeccak`](https://github.com/damaki/libkeccak):
  1. `git clone git@github.com:damaki/libkeccak.git`
  2. `cd libkeccak`
  3. `make build`
  4. `make install`

To build `ksum`:
  1. `gprbuild -p -P ksum.gpr -XLIBKECCAK_BUILD=default`
  
The `ksum` executable will be placed in the `bin` directory.

## Performance

The performance of `ksum` depends on the selected algorithm. The following table
shows the time taken to process a 1 GiB file filled with data from `/dev/urandom` 
on my machine (64-bit Linux on an Intel Core i7-2630QM CPU) as measured by the
`time` program.
The table also includes the output of other checksum programs for reference 
(marked in **bold**). The fastest out of 3 runs (according to the
"real" value from `time`) is shown.

| Program                        | real      | user      | sys      |
| ------------------------------ | --------- | --------- | -------- |
| ksum --kangarootwelve bigfile  | 0m2.053s  | 0m1.904s  | 0m0.148s |
| **md5sum bigfile**             | 0m2.537s  | 0m2.396s  | 0m0.140s |
| ksum --parallelhash128 bigfile | 0m3.344s  | 0m3.180s  | 0m0.160s |
| **sha1sum bigfile**            | 0m3.690s  | 0m3.536s  | 0m0.156s |
| ksum --parallelhash256 bigfile | 0m4.050s  | 0m3.892s  | 0m0.156s |
| ksum --shake128 bigfile        | 0m5.217s  | 0m5.028s  | 0m0.188s |
| ksum --kmac128 bigfile         | 0m5.227s  | 0m5.076s  | 0m0.148s |
| ksum --cshake128 bigfile       | 0m5.242s  | 0m5.108s  | 0m0.132s |
| **sha384sum bigfile**          | 0m5.447s  | 0m5.300s  | 0m0.144s |
| **sha512sum bigfile**          | 0m5.491s  | 0m5.316s  | 0m0.168s |
| ksum --sha3-224 bigfile        | 0m5.982s  | 0m5.812s  | 0m0.164s |
| ksum --cshake256 bigfile       | 0m6.315s  | 0m6.132s  | 0m0.180s |
| ksum --shake256 bigfile        | 0m6.318s  | 0m6.136s  | 0m0.180s |
| ksum --kmac256 bigfile         | 0m6.328s  | 0m6.208s  | 0m0.116s |
| ksum --sha3-256 bigfile        | 0m6.333s  | 0m6.176s  | 0m0.152s |
| ksum --sha3-384 bigfile        | 0m8.094s  | 0m7.976s  | 0m0.116s |
| **sha256sum bigfile**          | 0m8.276s  | 0m8.124s  | 0m0.148s |
| **sha224sum bigfile**          | 0m8.296s  | 0m8.124s  | 0m0.168s |
| ksum --sha3-512 bigfile        | 0m11.437s | 0m11.260s | 0m0.172s |

## TODO

Things not yet implemented:
  * Check mode to verify files against pre-generated checksums.
  * Testing against test vectors.
    (although `libkeccak` itself is tested against test vectors)

## References

* [1] The Keccak Reference Version 3.0. January 2011
http://keccak.noekeon.org/Keccak-reference-3.0.pdf
* [2] NIST FIPS PUB 202 - SHA-3 Standard: Permutation-Based Hash and Extendable
output Functions. August 2015 http://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.202.pdf
* [3] NIST SP 800-185 - SHA-3 Derived Functions: cSHAKE, KMAC, TupleHash, and ParallelHash. December 2016
http://nvlpubs.nist.gov/nistpubs/SpecialPublications/NIST.SP.800-185.pdf
* [4] KangarooTwelve: fast hashing based on Keccak-p
http://keccak.noekeon.org/kangarootwelve.html
