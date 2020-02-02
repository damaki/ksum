#!/usr/bin/python3
import binascii
import os
import os.path
import re
import subprocess
import tempfile
import testvectors
import unittest

# List of algorithm switches supported by ksum
algorithms = [
    "--cshake128",
    "--cshake256",
    "--kangarootwelve",
    "--marsupilamifourteen",
    "--keccak-224",
    "--keccak-256",
    "--keccak-384",
    "--keccak-512",
    "--kmac128",
    "--kmac256",
    "--parallelhash128",
    "--parallelhash256",
    "--rawshake128",
    "--rawshake256",
    "--sha3-224",
    "--sha3-256",
    "--sha3-384",
    "--sha3-512",
    "--shake128",
    "--shake256",
]

class TestVectors(unittest.TestCase):
    """
    Test cases to check the output of ksum against various test vectors.
    """

    def test_sha3_224_shortmsgkat_vectors(self):
        vectors = testvectors.load('vectors/ShortMsgKAT_SHA3-224.txt')
        self.run_msgkat_vector(vectors, "--sha3-224")

    def test_sha3_256_shortmsgkat_vectors(self):
        vectors = testvectors.load('vectors/ShortMsgKAT_SHA3-256.txt')
        self.run_msgkat_vector(vectors, "--sha3-256")

    def test_sha3_384_shortmsgkat_vectors(self):
        vectors = testvectors.load('vectors/ShortMsgKAT_SHA3-384.txt')
        self.run_msgkat_vector(vectors, "--sha3-384")

    def test_sha3_512_shortmsgkat_vectors(self):
        vectors = testvectors.load('vectors/ShortMsgKAT_SHA3-512.txt')
        self.run_msgkat_vector(vectors, "--sha3-512")

    def test_keccak_224_shortmsgkat_vectors(self):
        vectors = testvectors.load('vectors/ShortMsgKAT_224.txt')
        self.run_msgkat_vector(vectors, "--keccak-224")

    def test_keccak_256_shortmsgkat_vectors(self):
        vectors = testvectors.load('vectors/ShortMsgKAT_256.txt')
        self.run_msgkat_vector(vectors, "--keccak-256")

    def test_keccak_384_shortmsgkat_vectors(self):
        vectors = testvectors.load('vectors/ShortMsgKAT_384.txt')
        self.run_msgkat_vector(vectors, "--keccak-384")

    def test_keccak_512_shortmsgkat_vectors(self):
        vectors = testvectors.load('vectors/ShortMsgKAT_512.txt')
        self.run_msgkat_vector(vectors, "--keccak-512")

    def test_keccak_224_longmsgkat_vectors(self):
        vectors = testvectors.load('vectors/LongMsgKAT_224.txt', last='MD')
        self.run_msgkat_vector(vectors, "--keccak-224")

    def test_keccak_256_longmsgkat_vectors(self):
        vectors = testvectors.load('vectors/LongMsgKAT_256.txt', last='MD')
        self.run_msgkat_vector(vectors, "--keccak-256")

    def test_keccak_384_longmsgkat_vectors(self):
        vectors = testvectors.load('vectors/LongMsgKAT_384.txt', last='MD')
        self.run_msgkat_vector(vectors, "--keccak-384")

    def test_keccak_512_longmsgkat_vectors(self):
        vectors = testvectors.load('vectors/LongMsgKAT_512.txt', last='MD')
        self.run_msgkat_vector(vectors, "--keccak-512")

    def test_kmac128_vectors(self):
        vectors = testvectors.load('vectors/KMAC_128_samples.txt')
        self.run_kmac_vector(vectors, "--kmac128", xof=False)

    def test_kmac256_vectors(self):
        vectors = testvectors.load('vectors/KMAC_256_samples.txt')
        self.run_kmac_vector(vectors, "--kmac256", xof=False)

    def test_kmacxof128_vectors(self):
        vectors = testvectors.load('vectors/KMACXOF_128_samples.txt')
        self.run_kmac_vector(vectors, "--kmac128", xof=True)

    def test_kmacxof256_vectors(self):
        vectors = testvectors.load('vectors/KMACXOF_256_samples.txt')
        self.run_kmac_vector(vectors, "--kmac256", xof=True)

    def test_parallelhash128_vectors(self):
        vectors = testvectors.load('vectors/ParallelHash128_samples.txt')
        self.run_parallelhash_vector(vectors, "--parallelhash128", xof=False)

    def test_parallelhash256_vectors(self):
        vectors = testvectors.load('vectors/ParallelHash256_samples.txt')
        self.run_parallelhash_vector(vectors, "--parallelhash256", xof=False)

    def test_parallelhashxof128_vectors(self):
        vectors = testvectors.load('vectors/ParallelHashXOF128_samples.txt')
        self.run_parallelhash_vector(vectors, "--parallelhash128", xof=True)

    def test_parallelhashxof256_vectors(self):
        vectors = testvectors.load('vectors/ParallelHashXOF256_samples.txt')
        self.run_parallelhash_vector(vectors, "--parallelhash256", xof=True)

    def test_cshake128_vectors(self):
        vectors = testvectors.load('vectors/cSHAKE_128_samples.txt')
        self.run_cshake_vector(vectors, "--cshake128")

    def test_cshake256_vectors(self):
        vectors = testvectors.load('vectors/cSHAKE_256_samples.txt')
        self.run_cshake_vector(vectors, "--cshake256")

    def run_msgkat_vector(self, vectors, algorithm):
        """
        Helper to invoke ksum on a (Long|Short)MsgKAT test vectors file
        and check the output.
        """
        self.assertGreater(len(vectors), 0)
        for v in vectors:
            # Only run test vectors with a length of a multiple of 8 bits
            bit_len = int(v['Len'])
            if bit_len % 8 == 0:
                with self.subTest(vector=v):
                    with tempfile.NamedTemporaryFile(delete=False) as tmp:
                        try:
                            # Write the Msg to the file
                            msg = binascii.unhexlify(v['Msg'])
                            tmp.write(msg[0:bit_len // 8])
                            tmp.close()

                            # Generate the checksum
                            p = subprocess.run(
                                args = ["../bin/ksum", algorithm, "--binary", tmp.name],
                                stdout=subprocess.PIPE,
                                stderr=subprocess.PIPE
                            )
                            expected_stdout = v['MD'].lower() + ' *' + tmp.name + '\n'

                            self.assertEqual(p.returncode, 0)
                            self.assertEqual(p.stdout.decode('LATIN-1').replace('\r',''), expected_stdout)
                            self.assertEqual(p.stderr, b'')
                        finally:
                            os.unlink(tmp.name)

    def run_kmac_vector(self, vectors, algorithm, xof=False):
        """
        Helper to invoke ksum on a KMAC test vectors file and check the output.
        """
        self.assertGreater(len(vectors), 0)
        for v in vectors:
            # Only run test vectors with a length of a multiple of 8 bits
            in_bit_len = int(v['InLen'])
            key_bit_len = int(v['KeyLen'])
            out_bit_len = int(v['OutLen'])
            if (in_bit_len % 8 == 0) and (key_bit_len % 8 == 0) and (out_bit_len % 8 == 0):
                with self.subTest(vector=v):
                    with tempfile.NamedTemporaryFile(delete=False) as tmp:
                        try:
                            # Write the Msg to the file
                            msg = binascii.unhexlify(v['In'])
                            tmp.write(msg[0:in_bit_len // 8])
                            tmp.close()

                            args = [
                                "../bin/ksum",
                                algorithm,
                                "--key",
                                v['Key'],
                                "-n",
                                str(out_bit_len // 8),
                                "--binary"
                            ]

                            # Add customization string, if not empty
                            custom = v['S'].replace('"', '')
                            if custom != '':
                                args.append("-C")
                                args.append(custom)

                            # Set XOF mode, if requested
                            if xof:
                                args.append("--xof")

                            args.append(tmp.name)

                            # Generate the checksum
                            p = subprocess.run(
                                args,
                                stdout=subprocess.PIPE,
                                stderr=subprocess.PIPE
                            )
                            expected_stdout = v['Out'].lower() + ' *' + tmp.name + '\n'

                            self.assertEqual(p.returncode, 0)
                            self.assertEqual(p.stdout.decode('LATIN-1').replace('\r',''), expected_stdout)
                            self.assertEqual(p.stderr, b'')
                        finally:
                            os.unlink(tmp.name)

    def run_cshake_vector(self, vectors, algorithm):
        """
        Helper to invoke ksum on a KMAC test vectors file and check the output.
        """
        self.assertGreater(len(vectors), 0)
        for v in vectors:
            # Only run test vectors with a length of a multiple of 8 bits
            in_bit_len = int(v['InLen'])
            out_bit_len = int(v['OutLen'])
            if (in_bit_len % 8 == 0) and (out_bit_len % 8 == 0):
                with self.subTest(vector=v):
                    with tempfile.NamedTemporaryFile(delete=False) as tmp:
                        try:
                            # Write the Msg to the file
                            msg = binascii.unhexlify(v['In'])
                            tmp.write(msg[0:in_bit_len // 8])
                            tmp.close()

                            args = [
                                "../bin/ksum",
                                algorithm,
                                "-n",
                                str(out_bit_len // 8),
                                "--binary"
                            ]

                            # Add customization string, if not empty
                            custom = v['S'].replace('"', '')
                            if custom != '':
                                args.append("-C")
                                args.append(custom)

                            # Add function name string, if not empty
                            funcname = v['N'].replace('"', '')
                            if funcname != '':
                                args.append("-f")
                                args.append(funcname)

                            args.append(tmp.name)

                            # Generate the checksum
                            p = subprocess.run(
                                args,
                                stdout=subprocess.PIPE,
                                stderr=subprocess.PIPE
                            )
                            expected_stdout = v['Out'].lower() + ' *' + tmp.name + '\n'

                            self.assertEqual(p.returncode, 0)
                            self.assertEqual(p.stdout.decode('LATIN-1').replace('\r',''), expected_stdout)
                            self.assertEqual(p.stderr, b'')
                        finally:
                            os.unlink(tmp.name)

    def run_parallelhash_vector(self, vectors, algorithm, xof=False):
        """
        Helper to invoke ksum on a ParallelHash test vectors file and check the output.
        """
        self.assertGreater(len(vectors), 0)
        for v in vectors:
            with self.subTest(vector=v):
                with tempfile.NamedTemporaryFile(delete=False) as tmp:
                    try:
                        # Write the Msg to the file
                        msg = binascii.unhexlify(v['Msg'])
                        tmp.write(msg)
                        tmp.close()

                        args = [
                            "../bin/ksum",
                            algorithm,
                            "-B",
                            v['B'],
                            "-n",
                            str(len(v['MD']) // 2),
                            "--binary"
                        ]

                        # Add customization string, if not empty
                        custom = v['S'].replace('"', '')
                        if custom != '':
                            args.append("-C")
                            args.append(custom)

                        # Set XOF mode, if requested
                        if xof:
                            args.append("--xof")

                        args.append(tmp.name)

                        # Generate the checksum
                        p = subprocess.run(
                            args,
                            stdout=subprocess.PIPE,
                            stderr=subprocess.PIPE
                        )
                        expected_stdout = v['MD'].lower() + ' *' + tmp.name + '\n'

                        self.assertEqual(p.returncode, 0)
                        self.assertEqual(p.stdout.decode('LATIN-1').replace('\r',''), expected_stdout)
                        self.assertEqual(p.stderr, b'')
                    finally:
                        os.unlink(tmp.name)

class TestCheckMode(unittest.TestCase):
    """
    Tests to check the checksum verification capabilities of ksum
    """

    def test_loopback(self):
        """
        Test that ksum can verify checksums that it generated.

        This also verifies that the output of ksum --check is in
        the expected format.
        """
        for algo in algorithms:
            with self.subTest(algorithm=algo):
                with tempfile.NamedTemporaryFile(delete=False) as tmp:
                    try:
                        # Write some temporary data
                        tmp.write("The quick brown fox jumps over the lazy dog.".encode('LATIN-1'))
                        tmp.close()

                        # Generate a checksum
                        p = subprocess.run(
                            args = ["../bin/ksum", algo, tmp.name],
                            stdout=subprocess.PIPE,
                            stderr=subprocess.PIPE
                        )
                        self.assertEqual(p.returncode, 0)
                        self.assertEqual(p.stderr, b'')

                        # Save the checksum to a file
                        with tempfile.NamedTemporaryFile(delete=False) as tmp_checksum:
                            try:
                                tmp_checksum.write(p.stdout.decode('LATIN-1').replace('\r','').encode('LATIN-1'))
                                tmp_checksum.close()

                                # Verify the checksum
                                p = subprocess.run(
                                    args = ["../bin/ksum", algo, "-c", tmp_checksum.name],
                                    stdout=subprocess.PIPE,
                                    stderr=subprocess.PIPE
                                )
                                self.assertEqual(p.returncode, 0)
                                self.assertEqual(p.stdout.decode('LATIN-1').replace('\r',''), tmp.name + ": OK\n")
                                self.assertEqual(p.stderr, b'')
                            finally:
                                os.unlink(tmp_checksum.name)
                    finally:
                        os.unlink(tmp.name)

    def test_invalid_checksum(self):
        """
        Test that ksum correctly detects invalid checksums.

        This tests all supported algorithms to test that they can properly detect when a
        checksum is corrupted. For each algorithm, the test is repeated with different bytes
        being corrupted.
        """
        for algo in algorithms:
            with self.subTest(algorithm=algo):
                with tempfile.NamedTemporaryFile(delete=False) as tmp:
                    try:
                        # Write some temporary data
                        tmp.write("The quick brown fox jumps over the lazy dog.".encode('LATIN-1'))
                        tmp.close()

                        # Generate a checksum
                        p = subprocess.run(
                            args = ["../bin/ksum", algo, tmp.name],
                            stdout=subprocess.PIPE,
                            stderr=subprocess.PIPE
                        )
                        self.assertEqual(p.returncode, 0)
                        self.assertEqual(p.stderr, b'')

                        # Corrupt the checksum by changing the last byte.
                        m = re.match(r'(\w+) [\s\*](.+)$', p.stdout.decode('LATIN-1').replace('\r',''))
                        self.assertIsNotNone(m)
                        checksum = m.group(1)
                        checksum = binascii.unhexlify(checksum)

                        # Corrupt every checksum byte individually
                        # This checks that ksum actually considers every byte
                        for n in range(len(checksum)):
                            with self.subTest(n=n):
                                corrupted = bytearray(checksum)
                                corrupted[n] = (corrupted[n] + 1) % 256
                                corrupted = binascii.hexlify(corrupted)

                                # Save the checksum to a file
                                with tempfile.NamedTemporaryFile(delete=False) as tmp_checksum:
                                    try:
                                        tmp_checksum.write(corrupted + (' *' + m.group(2)).encode('LATIN-1'))
                                        tmp_checksum.close()

                                        # Verify the checksum
                                        p = subprocess.run(
                                            args = ["../bin/ksum", algo, "-c", tmp_checksum.name],
                                            stdout=subprocess.PIPE,
                                            stderr=subprocess.PIPE
                                        )
                                        self.assertNotEqual(p.returncode, 0)
                                        self.assertEqual(p.stdout, b'') # Should print to stderr

                                        # Expect the first line of the stderr output to contain the failed file name
                                        # E.g. filename: FAILED
                                        lines = p.stderr.decode('LATIN-1').replace('\r','').split('\n')
                                        self.assertEqual(len(lines), 3)
                                        self.assertEqual(lines[0], tmp.name + ": FAILED")
                                        self.assertEqual(lines[-1], '')
                                    finally:
                                        os.unlink(tmp_checksum.name)
                    finally:
                        os.unlink(tmp.name)

    def test_multiple_files(self):
        """
        Test that ksum can correctly generate and verify checksums of multiple files.
        """
        for algo in algorithms:
            with self.subTest(algorithm=algo):
                with tempfile.NamedTemporaryFile(delete=False) as tmp1:
                    with tempfile.NamedTemporaryFile(delete=False) as tmp2:
                        with tempfile.NamedTemporaryFile(delete=False) as tmp3:
                            try:
                                # Write some temporary data
                                tmp1.write("The quick brown fox jumps over the lazy dog.".encode('LATIN-1'))
                                tmp1.close()
                                tmp2.write("Lorem ipsum dolor sit amet".encode('LATIN-1'))
                                tmp2.close()
                                tmp3.write("abcdefghijklmnopqrstuvwxyz".encode('LATIN-1'))
                                tmp3.close()

                                # Generate a checksum
                                p = subprocess.run(
                                    args = ["../bin/ksum", algo, tmp1.name, tmp2.name, tmp3.name],
                                    stdout=subprocess.PIPE,
                                    stderr=subprocess.PIPE
                                )
                                self.assertEqual(p.returncode, 0)
                                self.assertEqual(p.stderr, b'')

                                # Check stdout format
                                stdout_lines = p.stdout.decode('LATIN-1').replace('\r','').split('\n')
                                self.assertEqual(len(stdout_lines), 4)
                                self.assertTrue(stdout_lines[0].endswith(tmp1.name))
                                self.assertTrue(stdout_lines[1].endswith(tmp2.name))
                                self.assertTrue(stdout_lines[2].endswith(tmp3.name))
                                self.assertEqual(stdout_lines[3], '')

                                # Save the checksums to a file
                                with tempfile.NamedTemporaryFile(delete=False) as tmp_checksum:
                                    try:
                                        tmp_checksum.write(p.stdout)
                                        tmp_checksum.close()

                                        # Verify the checksum
                                        p = subprocess.run(
                                            args = ["../bin/ksum", algo, "-c", tmp_checksum.name],
                                            stdout=subprocess.PIPE,
                                            stderr=subprocess.PIPE
                                        )
                                        self.assertEqual(p.returncode, 0)
                                        self.assertEqual(p.stderr, b'')

                                        stdout_lines = p.stdout.decode('LATIN-1').replace('\r','').split('\n')
                                        self.assertEqual(len(stdout_lines), 4)
                                        self.assertEqual(stdout_lines[0], tmp1.name + ': OK')
                                        self.assertEqual(stdout_lines[1], tmp2.name + ': OK')
                                        self.assertEqual(stdout_lines[2], tmp3.name + ': OK')
                                        self.assertEqual(stdout_lines[3], '')
                                    finally:
                                        os.unlink(tmp_checksum.name)
                            finally:
                                os.unlink(tmp1.name)
                                os.unlink(tmp2.name)
                                os.unlink(tmp3.name)

    def test_quiet(self):
        """
        Test that ksum writes nothing to stdout when --quiet is used.
        """
        for algo in algorithms:
            with self.subTest(algorithm=algo):
                with tempfile.NamedTemporaryFile(delete=False) as tmp:
                    try:
                        # Write some temporary data
                        tmp.write("The quick brown fox jumps over the lazy dog.".encode('LATIN-1'))
                        tmp.close()

                        # Generate a checksum
                        p = subprocess.run(
                            args = ["../bin/ksum", algo, tmp.name],
                            stdout=subprocess.PIPE,
                            stderr=subprocess.PIPE
                        )
                        self.assertEqual(p.returncode, 0)
                        self.assertEqual(p.stderr, b'')

                        # Save the checksum to a file
                        with tempfile.NamedTemporaryFile(delete=False) as tmp_checksum:
                            try:
                                tmp_checksum.write(p.stdout)
                                tmp_checksum.close()

                                # Verify the checksum
                                p = subprocess.run(
                                    args = ["../bin/ksum", algo, "-c", "--quiet", tmp_checksum.name],
                                    stdout=subprocess.PIPE,
                                    stderr=subprocess.PIPE
                                )
                                self.assertEqual(p.returncode, 0)
                                self.assertEqual(p.stdout, b'')
                                self.assertEqual(p.stderr, b'')
                            finally:
                                os.unlink(tmp_checksum.name)
                    finally:
                        os.unlink(tmp.name)


    def test_strict(self):
        """
        Test that ksum returns non-zero exit code for improperly formatted lines
        when --strict is used
        """
        for algo in algorithms:
            with self.subTest(algorithm=algo):
                with tempfile.NamedTemporaryFile(delete=False) as tmp:
                    try:
                        # Write some temporary data
                        tmp.write("The quick brown fox jumps over the lazy dog.".encode('LATIN-1'))
                        tmp.close()

                        # Generate a checksum
                        p = subprocess.run(
                            args = ["../bin/ksum", algo, tmp.name],
                            stdout=subprocess.PIPE,
                            stderr=subprocess.PIPE
                        )
                        self.assertEqual(p.returncode, 0)
                        self.assertEqual(p.stderr, b'')

                        # Save a bad checksum to a file
                        with tempfile.NamedTemporaryFile(delete=False) as tmp_checksum:
                            try:
                                tmp_checksum.write(("0123456789abcdef0123456789abcdefgh  " + tmp.name).encode())
                                tmp_checksum.close()

                                # Verify the (bad) checksum in --strict mode
                                p = subprocess.run(
                                    args = ["../bin/ksum", algo, "-c", "--strict", tmp_checksum.name],
                                    stdout=subprocess.PIPE,
                                    stderr=subprocess.PIPE
                                )
                                self.assertNotEqual(p.returncode, 0)
                                self.assertEqual(p.stdout, b'')

                                # Verify the (bad) checksum in non-strict mode
                                p = subprocess.run(
                                    args = ["../bin/ksum", algo, "-c", tmp_checksum.name],
                                    stdout=subprocess.PIPE,
                                    stderr=subprocess.PIPE
                                )
                                self.assertEqual(p.returncode, 0)
                                self.assertEqual(p.stdout, b'')
                            finally:
                                os.unlink(tmp_checksum.name)
                    finally:
                        os.unlink(tmp.name)

    def test_line_endings(self):
        """
        Test that ksum can handle both \n and \r\n line ending styles.
        """
        with tempfile.NamedTemporaryFile(delete=False) as tmp1:
            with tempfile.NamedTemporaryFile(delete=False) as tmp2:
                with tempfile.NamedTemporaryFile(delete=False) as tmp3:
                    try:
                        # Write some temporary data
                        tmp1.write("The quick brown fox jumps over the lazy dog.".encode('LATIN-1'))
                        tmp1.close()
                        tmp2.write("Lorem ipsum dolor sit amet".encode('LATIN-1'))
                        tmp2.close()
                        tmp3.write("abcdefghijklmnopqrstuvwxyz".encode('LATIN-1'))
                        tmp3.close()

                        # Generate a checksum
                        p = subprocess.run(
                            args = ["../bin/ksum", tmp1.name, tmp2.name, tmp3.name],
                            stdout=subprocess.PIPE,
                            stderr=subprocess.PIPE
                        )
                        self.assertEqual(p.returncode, 0)
                        self.assertEqual(p.stderr, b'')

                        # Get the canonical output (line feed endings only)
                        canonical = p.stdout.decode('LATIN-1').replace('\r','')

                        # Try verifying a checksum file using \n line endings only
                        with tempfile.NamedTemporaryFile(delete=False) as tmp_checksum:
                            try:
                                tmp_checksum.write(canonical.encode('LATIN-1'))
                                tmp_checksum.close()

                                # Verify the checksum (run in strict mode to disallow badly formatted lines)
                                p = subprocess.run(
                                    args = ["../bin/ksum", "--strict", "-c", tmp_checksum.name],
                                    stdout=subprocess.PIPE,
                                    stderr=subprocess.PIPE
                                )
                                self.assertEqual(p.returncode, 0)
                                self.assertEqual(p.stderr, b'')

                                stdout_lines = p.stdout.decode('LATIN-1').replace('\r','').split('\n')
                                self.assertEqual(len(stdout_lines), 4)
                                self.assertEqual(stdout_lines[0], tmp1.name + ': OK')
                                self.assertEqual(stdout_lines[1], tmp2.name + ': OK')
                                self.assertEqual(stdout_lines[2], tmp3.name + ': OK')
                                self.assertEqual(stdout_lines[3], '')
                            finally:
                                os.unlink(tmp_checksum.name)

                        # Try verifying a checksum file using \r\n line endings
                        with tempfile.NamedTemporaryFile(delete=False) as tmp_checksum:
                            try:
                                tmp_checksum.write(canonical.replace('\n', '\r\n').encode('LATIN-1'))
                                tmp_checksum.close()

                                # Verify the checksum (run in strict mode to disallow badly formatted lines)
                                p = subprocess.run(
                                    args = ["../bin/ksum", "--strict", "-c", tmp_checksum.name],
                                    stdout=subprocess.PIPE,
                                    stderr=subprocess.PIPE
                                )
                                self.assertEqual(p.returncode, 0)
                                self.assertEqual(p.stderr, b'')

                                stdout_lines = p.stdout.decode('LATIN-1').replace('\r','').split('\n')
                                self.assertEqual(len(stdout_lines), 4)
                                self.assertEqual(stdout_lines[0], tmp1.name + ': OK')
                                self.assertEqual(stdout_lines[1], tmp2.name + ': OK')
                                self.assertEqual(stdout_lines[2], tmp3.name + ': OK')
                                self.assertEqual(stdout_lines[3], '')
                            finally:
                                os.unlink(tmp_checksum.name)
                    finally:
                        os.unlink(tmp1.name)
                        os.unlink(tmp2.name)
                        os.unlink(tmp3.name)


if __name__ == '__main__':
    unittest.main()
