-------------------------------------------------------------------------------
--  Copyright (c) 2017, Daniel King
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
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;
with Argument_Parser;
with Keccak.Types;               use Keccak.Types;

package Configurations
is


   type Byte_Array_Access is access Byte_Array;

   procedure Deallocate_Byte_Array is new Ada.Unchecked_Deallocation
     (Object => Byte_Array,
      Name   => Byte_Array_Access);


   type Algorithm_Names is
     (CSHAKE128,
      CSHAKE256,
      KangarooTwelve,
      MarsupilamiFourteen,
      Keccak_224,
      Keccak_256,
      Keccak_384,
      Keccak_512,
      KMAC128,
      KMAC256,
      ParallelHash128,
      ParallelHash256,
      RawSHAKE128,
      RawSHAKE256,
      SHA3_224,
      SHA3_256,
      SHA3_384,
      SHA3_512,
      SHAKE128,
      SHAKE256);


   type Read_Modes is (Text, Binary);


   Algorithm_Strings : constant array (Algorithm_Names) of Unbounded_String :=
     (CSHAKE128           => To_Unbounded_String ("cshake128"),
      CSHAKE256           => To_Unbounded_String ("cshake256"),
      KangarooTwelve      => To_Unbounded_String ("kangarootwelve"),
      MarsupilamiFourteen => To_Unbounded_String ("marsupilamifourteen"),
      Keccak_224          => To_Unbounded_String ("keccak-224"),
      Keccak_256          => To_Unbounded_String ("keccak-256"),
      Keccak_384          => To_Unbounded_String ("keccak-384"),
      Keccak_512          => To_Unbounded_String ("keccak-512"),
      KMAC128             => To_Unbounded_String ("kmac128"),
      KMAC256             => To_Unbounded_String ("kmac256"),
      ParallelHash128     => To_Unbounded_String ("parallelhash128"),
      ParallelHash256     => To_Unbounded_String ("parallelhash256"),
      RawSHAKE128         => To_Unbounded_String ("rawshake128"),
      RawSHAKE256         => To_Unbounded_String ("rawshake256"),
      SHA3_224            => To_Unbounded_String ("sha3-224"),
      SHA3_256            => To_Unbounded_String ("sha3-256"),
      SHA3_384            => To_Unbounded_String ("sha3-384"),
      SHA3_512            => To_Unbounded_String ("sha3-512"),
      SHAKE128            => To_Unbounded_String ("shake128"),
      SHAKE256            => To_Unbounded_String ("shake256"));

   Default_Output_Length : constant array (Algorithm_Names) of Long_Long_Integer :=
     (CSHAKE128           => 128 / 8,
      CSHAKE256           => 256 / 8,
      KangarooTwelve      => 128 / 8,
      MarsupilamiFourteen => 256 / 8,
      Keccak_224          => 224 / 8,
      Keccak_256          => 256 / 8,
      Keccak_384          => 384 / 8,
      Keccak_512          => 512 / 8,
      KMAC128             => 128 / 8,
      KMAC256             => 256 / 8,
      ParallelHash128     => 128 / 8,
      ParallelHash256     => 256 / 8,
      RawSHAKE128         => 128 / 8,
      RawSHAKE256         => 256 / 8,
      SHA3_224            => 224 / 8,
      SHA3_256            => 256 / 8,
      SHA3_384            => 384 / 8,
      SHA3_512            => 512 / 8,
      SHAKE128            => 128 / 8,
      SHAKE256            => 256 / 8);


   Read_Mode      : Read_Modes := Text;
   --  Sets the read mode for reading files (text or binary mode).
   --  Set using -t or -b

   Algorithm      : Algorithm_Names := SHA3_256;
   --  Algorithm to use.
   --  Set using -a or --algorithm or one of the specific flags (e.g. --sha3-256).

   Key            : Byte_Array_Access := null;
   --  Hex string for KMAC key.
   --  Set using -k or --key

   Function_Name  : Unbounded_String := Null_Unbounded_String;
   --  Function name used for CSHAKE.
   --  Set using -f or --function

   Customization  : Unbounded_String := Null_Unbounded_String;
   --  Customization string used for CSHAKE, KMAC, ParallelHash, KangarooTwelve
   --  Set using -C or --customization

   Block_Size     : Positive := 8192;
   --  Block size to use for ParallelHash.
   --  Set using -B or --block-size

   Buffer_Size    : Positive := 64*1024;
   --  Buffer size to use when reading file data.
   --  Set using -z or --buffer-size

   Output_Length  : Long_Long_Integer := -1;
   --  Number of output bytes.
   --  Set using -n or --output-size

   XOF_Mode       : Boolean := False;
   --  Configures variable-output length mode for ParallelHash and KMAC.
   --  Set using -x of --xof

   Help_Displayed : Boolean := False;
   --  Set to true if the --help message was displayed.
   --  When this is set, the program should exit without doing anything

   Files          : Argument_Parser.Unbounded_Strings_Vectors.Vector :=
     Argument_Parser.Unbounded_Strings_Vectors.Empty_Vector;
   --  List of file names to process

   procedure Parse_Command_Line;

end Configurations;
