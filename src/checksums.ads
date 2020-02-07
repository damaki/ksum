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
--
--  This package (and its children) deal with generating and verifying
--  checksums over files and/or the standard input.
--
--  Note that Ada.Text_IO is NOT used to read files or standard input,
--  because the 'Read' stream function for Ada.Text_IO.Text_Streams
--  always reads in binary mode, even if it was opened in text mode or if
--  set_text_mode is used. See Ada RM A.12.2/6
--
--  So instead, we use Ada.Streams.Stream_IO (which does not alter the
--  mode of the file during stream reading), and use Interfaces.C_Streams
--  to force the mode of the file to the appropriate mode.
-------------------------------------------------------------------------------
with Ada.Streams.Stream_IO;
with Stream_CSHAKE;
with Stream_Hashing;
with Stream_K12;
with Stream_KMAC;
with Stream_ParallelHash;
with Stream_XOF;
with Configurations;

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

   ---------------------------
   --  Hash Instantiations  --
   ---------------------------

   --  The generic stream hashes are instantiated here for each specific
   --  hash algorithm from libkeccak.

   package K12_Stream_Hashing is new Stream_K12 (KangarooTwelve.K12);
   package M14_Stream_Hashing is new Stream_K12 (MarsupilamiFourteen.M14);

   package ParallelHash128_Stream_Hashing
   is new Stream_ParallelHash (Parallel_Hash.ParallelHash128);

   package ParallelHash256_Stream_Hashing
   is new Stream_ParallelHash (Parallel_Hash.ParallelHash256);

   package KMAC128_Stream_Hashing is new Stream_KMAC (KMAC.KMAC128);
   package KMAC256_Stream_Hashing is new Stream_KMAC (KMAC.KMAC256);

   package SHA3_224_Stream_Hashing is new Stream_Hashing (SHA3.SHA3_224);
   package SHA3_256_Stream_Hashing is new Stream_Hashing (SHA3.SHA3_256);
   package SHA3_384_Stream_Hashing is new Stream_Hashing (SHA3.SHA3_384);
   package SHA3_512_Stream_Hashing is new Stream_Hashing (SHA3.SHA3_512);

   package Keccak_224_Stream_Hashing is new Stream_Hashing (SHA3.Keccak_224);
   package Keccak_256_Stream_Hashing is new Stream_Hashing (SHA3.Keccak_256);
   package Keccak_384_Stream_Hashing is new Stream_Hashing (SHA3.Keccak_384);
   package Keccak_512_Stream_Hashing is new Stream_Hashing (SHA3.Keccak_512);

   package RawSHAKE128_Stream_Hashing is new Stream_XOF (RawSHAKE.RawSHAKE128);
   package RawSHAKE256_Stream_Hashing is new Stream_XOF (RawSHAKE.RawSHAKE256);

   package SHAKE128_Stream_Hashing is new Stream_XOF (SHAKE.SHAKE128);
   package SHAKE256_Stream_Hashing is new Stream_XOF (SHAKE.SHAKE256);

   package CSHAKE128_Stream_Hashing is new Stream_CSHAKE
     (CSHAKE.CSHAKE128,
      SHAKE128_Stream_Hashing);

   package CSHAKE256_Stream_Hashing is new Stream_CSHAKE
     (CSHAKE.CSHAKE256,
      SHAKE256_Stream_Hashing);

private

   Standard_Input_File_Name : constant String := "-";
   --  Special file name, which if specified on the command line will read
   --  from the standard input.

   procedure Set_File_Mode (File : in out Ada.Streams.Stream_IO.File_Type;
                            Mode : in     Configurations.Read_Modes);
   --  Sets the file mode to either text or binary mode

end Checksums;
