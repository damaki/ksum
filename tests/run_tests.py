#!/usr/bin/python3
import os
import os.path
import subprocess
import tempfile
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

class TestAlgorithms(unittest.TestCase):

    def test_loopback(self):
        """
        Test that ksum can verify checksums that it generated.

        This also verifies that the output of ksum --check is in
        the expected format.
        """
        for algo in algorithms:
            with self.subTest(algorithm=algo):
                with tempfile.NamedTemporaryFile() as tmp:
                    # Write some temporary data
                    tmp.write("The quick brown fox jumps over the lazy dog.".encode('LATIN-1'))
                    tmp.flush()

                    # Generate a checksum
                    p = subprocess.run(
                        args = ["../bin/ksum", algo, tmp.name],
                        stdout=subprocess.PIPE,
                        stderr=subprocess.PIPE
                    )
                    self.assertEqual(p.returncode, 0)
                    self.assertEqual(p.stderr, b'')

                    # Save the checksum to a file
                    with tempfile.NamedTemporaryFile() as tmp_checksum:
                        tmp_checksum.write(p.stdout)
                        tmp_checksum.flush()

                        # Verify the checksum
                        p = subprocess.run(
                            args = ["../bin/ksum", algo, "-c", tmp_checksum.name],
                            stdout=subprocess.PIPE,
                            stderr=subprocess.PIPE
                        )
                        self.assertEqual(p.returncode, 0)
                        self.assertEqual(p.stdout, (tmp.name + ": OK\n").encode('LATIN-1'))
                        self.assertEqual(p.stderr, b'')

if __name__ == '__main__':
    unittest.main()