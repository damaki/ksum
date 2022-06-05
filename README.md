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
* KangarooTwelve and MarsupilamiFourteen as defined by the Keccak authors in [4]

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

### Checking checksums

The `-c` or `--check` switch can be used to read checksums from the `FILE`s
and check them.

For example, to generate a file called `checksums.txt` that contains the
checksums of `file1`, `file2`, and `file3`:

```
$ ksum --sha3-256 file1 file2 file3 > checksums.txt
```

Then, to verify the checksums use the `--check` switch (or its `-c` short
version):
```
$ ksum --sha3-256 --check checksums.txt
```

`ksum` exits with a status of 0 if all checksums are valid. Otherwise,
`ksum` prints diagnostic messages and exits with a non-zero status.
Here's an example error output:
```
$ ksum --sha3-256 --check checksums.txt
file2: FAILED
ksum: WARNING: checksums.txt: 1 computed checksum did NOT match
```

Note that `ksum` must be invoked with the same settings that was used to
generate the checksums. This is particularly important for algorithms
that are customizable, e.g. KMAC, ParallelHash, cSHAKE.

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

Building `ksum` requires Alire.

```sh
alr build --release
```

The `ksum` executable will be placed in the `bin` directory.

`ksum` depends on [libkeccak](https://github.com/damaki/libkeccak). libkeccak
can be built with SSE2 and AVX2 instructions for better performance of parallel
hashes, if your platform supports them. To build with AVX2 instructions enabled:

```sh
alr build --release -- -XLIBKECCAK_ARCH=x86_64 -XLIBKECCAK_SIMD=AVX2
```

>:warning: `AVX2` is not guaranteed to work on Windows since GCC does not ensure 32-byte
stack alignment. See [GCC Bug #54412](https://gcc.gnu.org/bugzilla/show_bug.cgi?id=54412)

## Performance

The performance of `ksum` depends on the selected algorithm. The following table
shows the time taken to process a 1 GiB file filled with data from `/dev/urandom`
on my machine (64-bit Windows 10 on an AMD Ryzen 7 5800X) as measured by the
`time` program. For these tests, `ksum` was built using GNAT 11.2.0, and with AVX2
instructions enabled in `libkeccak`.

The table also includes the output of other checksum programs from GNU coreutils
8.32, marked in **bold**.

The fastest out of 3 runs (according to the "real" value from `time`) is shown.

| Program                        | real      | user      | sys      |
| ------------------------------ | --------- | --------- | -------- |
| ksum --kangarootwelve bigfile  | 0m0.667s  | 0m0.000s  | 0m0.000s |
| ksum --parallelhash128 bigfile | 0m0.884s  | 0m0.000s  | 0m0.000s |
| ksum --parallelhash256 bigfile | 0m1.008s  | 0m0.000s  | 0m0.015s |
| **sha1sum bigfile**            | 0m0.249s  | 0m1.062s  | 0m0.187s |
| **md5sum bigfile**             | 0m1.284s  | 0m1.156s  | 0m0.141s |
| ksum --shake128 bigfile        | 0m1.675s  | 0m0.000s  | 0m0.000s |
| ksum --cshake128 bigfile       | 0m1.675s  | 0m0.000s  | 0m0.000s |
| ksum --kmac128 bigfile         | 0m1.686s  | 0m0.000s  | 0m0.015s |
| ksum --sha3-224 bigfile        | 0m1.903s  | 0m0.000s  | 0m0.015s |
| ksum --kmac256 bigfile         | 0m1.995s  | 0m0.000s  | 0m0.000s |
| ksum --shake256 bigfile        | 0m1.996s  | 0m0.000s  | 0m0.015s |
| ksum --cshake256 bigfile       | 0m1.996s  | 0m0.000s  | 0m0.015s |
| ksum --sha3-256 bigfile        | 0m1.996s  | 0m0.000s  | 0m0.015s |
| **sha512sum bigfile**          | 0m2.020s  | 0m1.875s  | 0m0.140s |
| **sha384sum bigfile**          | 0m2.021s  | 0m1.828s  | 0m0.202s |
| ksum --sha3-384 bigfile        | 0m2.512s  | 0m0.000s  | 0m0.000s |
| **sha256sum bigfile**          | 0m2.791s  | 0m2.625s  | 0m0.171s |
| **sha224sum bigfile**          | 0m2.789s  | 0m2.593s  | 0m0.187s |
| ksum --sha3-512 bigfile        | 0m3.483s  | 0m0.000s  | 0m0.015s |

## Testing

The test suite runs `ksum` against a set of test vectors in `tests/vectors/`,
and tests of the various CLI options, e.g. quiet mode.

Running the tests requires Python 3. To run the tests:

```
alr build --validation
cd tests
python run_tests.py
```

## References

* [1] The Keccak Reference Version 3.0. January 2011
http://keccak.noekeon.org/Keccak-reference-3.0.pdf
* [2] NIST FIPS PUB 202 - SHA-3 Standard: Permutation-Based Hash and Extendable
output Functions. August 2015 http://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.202.pdf
* [3] NIST SP 800-185 - SHA-3 Derived Functions: cSHAKE, KMAC, TupleHash, and ParallelHash. December 2016
http://nvlpubs.nist.gov/nistpubs/SpecialPublications/NIST.SP.800-185.pdf
* [4] KangarooTwelve: fast hashing based on Keccak-p
http://keccak.noekeon.org/kangarootwelve.html
