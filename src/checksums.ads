-------------------------------------------------------------------------------
--  Copyright (c) 2019, Daniel King
--  All rights reserved.
--
--  This file is part of ksum.
--
--  ksum is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  ksum is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with ksum.  If not, see <http://www.gnu.org/licenses/>.
-------------------------------------------------------------------------------
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with File_CSHAKE;
with File_Hashing;
with File_K12;
with File_KMAC;
with File_ParallelHash;
with File_XOF;

with CSHAKE;
with KangarooTwelve;
with MarsupilamiFourteen;
with KMAC;
with Parallel_Hash;
with RawSHAKE;
with SHA3;
with SHAKE;

package Checksums
is

   package K12_File_Hashing is new File_K12 (KangarooTwelve.K12);
   package M14_File_Hashing is new File_K12 (MarsupilamiFourteen.M14);

   package ParallelHash128_File_Hashing is new File_ParallelHash (Parallel_Hash.ParallelHash128);
   package ParallelHash256_File_Hashing is new File_ParallelHash (Parallel_Hash.ParallelHash256);

   package KMAC128_File_Hashing is new File_KMAC (KMAC.KMAC128);
   package KMAC256_File_Hashing is new File_KMAC (KMAC.KMAC256);

   package SHA3_224_File_Hashing is new File_Hashing (SHA3.SHA3_224);
   package SHA3_256_File_Hashing is new File_Hashing (SHA3.SHA3_256);
   package SHA3_384_File_Hashing is new File_Hashing (SHA3.SHA3_384);
   package SHA3_512_File_Hashing is new File_Hashing (SHA3.SHA3_512);

   package Keccak_224_File_Hashing is new File_Hashing (SHA3.Keccak_224);
   package Keccak_256_File_Hashing is new File_Hashing (SHA3.Keccak_256);
   package Keccak_384_File_Hashing is new File_Hashing (SHA3.Keccak_384);
   package Keccak_512_File_Hashing is new File_Hashing (SHA3.Keccak_512);

   package RawSHAKE128_File_Hashing is new File_XOF (RawSHAKE.RawSHAKE128);
   package RawSHAKE256_File_Hashing is new File_XOF (RawSHAKE.RawSHAKE256);

   package SHAKE128_File_Hashing is new File_XOF (SHAKE.SHAKE128);
   package SHAKE256_File_Hashing is new File_XOF (SHAKE.SHAKE256);

   package CSHAKE128_File_Hashing is new File_CSHAKE
     (CSHAKE.CSHAKE128,
      SHAKE128_File_Hashing);

   package CSHAKE256_File_Hashing is new File_CSHAKE
     (CSHAKE.CSHAKE256,
      SHAKE256_File_Hashing);

   procedure Print_Checksums;
   --  Compute the checksum over all files listed on the command line, and
   --  print the checksum to the standard output along with the file name.

   procedure Check_Checksums;
   --  Open the checksum file(s) specified on the command line, and check the
   --  list of checksums contained in that file.

end Checksums;
