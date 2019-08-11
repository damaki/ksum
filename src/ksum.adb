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
with Ada.Command_Line;      use Ada.Command_Line;
with Ada.Directories;       use Ada.Directories;
with Ada.Exceptions;        use Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;
with Argument_Parser;       use Argument_Parser;
with Configurations;        use Configurations;
with File_CSHAKE;
with File_Hashing;
with File_K12;
with File_KMAC;
with File_ParallelHash;
with File_XOF;
with Keccak.Types;          use Keccak.Types;
with Diagnostics;           use Diagnostics;

with CSHAKE;
with KangarooTwelve;
with MarsupilamiFourteen;
with KMAC;
with Parallel_Hash;
with RawSHAKE;
with SHA3;
with SHAKE;

procedure Ksum
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

   type Hash_File_Procedure_Access is access procedure
     (File   : in     Ada.Text_IO.File_Type;
      Buffer : in out Keccak.Types.Byte_Array);

   type Check_File_Procedure_Access is access procedure
     (File          : in     Ada.Text_IO.File_Type;
      Buffer        : in out Keccak.Types.Byte_Array;
      Expected_Hash : in     Keccak.Types.Byte_Array;
      Result        :    out Diagnostic);

   Hash_File_Procs : constant array (Algorithm_Names) of Hash_File_Procedure_Access :=
     (Configurations.CSHAKE128           => CSHAKE128_File_Hashing.Hash_File'Access,
      Configurations.CSHAKE256           => CSHAKE256_File_Hashing.Hash_File'Access,
      Configurations.KangarooTwelve      => K12_File_Hashing.Hash_File'Access,
      Configurations.MarsupilamiFourteen => M14_File_Hashing.Hash_File'Access,
      Configurations.Keccak_224          => Keccak_224_File_Hashing.Hash_File'Access,
      Configurations.Keccak_256          => Keccak_256_File_Hashing.Hash_File'Access,
      Configurations.Keccak_384          => Keccak_384_File_Hashing.Hash_File'Access,
      Configurations.Keccak_512          => Keccak_512_File_Hashing.Hash_File'Access,
      Configurations.KMAC128             => KMAC128_File_Hashing.Hash_File'Access,
      Configurations.KMAC256             => KMAC256_File_Hashing.Hash_File'Access,
      Configurations.ParallelHash128     => ParallelHash128_File_Hashing.Hash_File'Access,
      Configurations.ParallelHash256     => ParallelHash256_File_Hashing.Hash_File'Access,
      Configurations.RawSHAKE128         => RawSHAKE128_File_Hashing.Hash_File'Access,
      Configurations.RawSHAKE256         => RawSHAKE256_File_Hashing.Hash_File'Access,
      Configurations.SHA3_224            => SHA3_224_File_Hashing.Hash_File'Access,
      Configurations.SHA3_256            => SHA3_256_File_Hashing.Hash_File'Access,
      Configurations.SHA3_384            => SHA3_384_File_Hashing.Hash_File'Access,
      Configurations.SHA3_512            => SHA3_512_File_Hashing.Hash_File'Access,
      Configurations.SHAKE128            => SHAKE128_File_Hashing.Hash_File'Access,
      Configurations.SHAKE256            => SHAKE256_File_Hashing.Hash_File'Access);

   Check_File_Procs : constant array (Algorithm_Names) of Check_File_Procedure_Access :=
     (Configurations.CSHAKE128           => CSHAKE128_File_Hashing.Check_File'Access,
      Configurations.CSHAKE256           => CSHAKE256_File_Hashing.Check_File'Access,
      Configurations.KangarooTwelve      => K12_File_Hashing.Check_File'Access,
      Configurations.MarsupilamiFourteen => M14_File_Hashing.Check_File'Access,
      Configurations.Keccak_224          => Keccak_224_File_Hashing.Check_File'Access,
      Configurations.Keccak_256          => Keccak_256_File_Hashing.Check_File'Access,
      Configurations.Keccak_384          => Keccak_384_File_Hashing.Check_File'Access,
      Configurations.Keccak_512          => Keccak_512_File_Hashing.Check_File'Access,
      Configurations.KMAC128             => KMAC128_File_Hashing.Check_File'Access,
      Configurations.KMAC256             => KMAC256_File_Hashing.Check_File'Access,
      Configurations.ParallelHash128     => ParallelHash128_File_Hashing.Check_File'Access,
      Configurations.ParallelHash256     => ParallelHash256_File_Hashing.Check_File'Access,
      Configurations.RawSHAKE128         => RawSHAKE128_File_Hashing.Check_File'Access,
      Configurations.RawSHAKE256         => RawSHAKE256_File_Hashing.Check_File'Access,
      Configurations.SHA3_224            => SHA3_224_File_Hashing.Check_File'Access,
      Configurations.SHA3_256            => SHA3_256_File_Hashing.Check_File'Access,
      Configurations.SHA3_384            => SHA3_384_File_Hashing.Check_File'Access,
      Configurations.SHA3_512            => SHA3_512_File_Hashing.Check_File'Access,
      Configurations.SHAKE128            => SHAKE128_File_Hashing.Check_File'Access,
      Configurations.SHAKE256            => SHAKE256_File_Hashing.Check_File'Access);

   File      : Ada.Text_IO.File_Type;

   Buffer    : Byte_Array_Access;

begin
   Configurations.Parse_Command_Line;

   --  Don't do anything if the help message was displayed.
   if not Configurations.Help_Displayed then

      Buffer := new Byte_Array (1 .. Configurations.Buffer_Size);

      --  For a negative output length, use the default output length for the
      --  specifed algorithm.
      if Output_Length < 0 then
         Output_Length := Default_Output_Length (Algorithm);
      end if;

      --  Iterate through each file name and hash it.
      for C in Configurations.Files.Iterate loop

         declare
            File_Name : constant String := To_String (Unbounded_Strings_Vectors.Element (C));
         begin

            --  Special case for '-' which means Standard_Input
            if File_Name = "-" then
               Hash_File_Procs (Configurations.Algorithm)
                 (File   => Standard_Input,
                  Buffer => Buffer.all);

            else
               --  GNAT's Open procedure succeeds even when File_Name is a
               --  directory. A Device_Error exception is then thrown when
               --  trying to read the File, and the exception does not include
               --  any meaningful error message.
               --
               --  We therefore handle the case of attempting to open a
               --  directory
               if Kind (File_Name) = Directory then
                  raise Ada.IO_Exceptions.Name_Error
                  with File_Name & ": Is a directory";
               end if;

               Ada.Text_IO.Open
                 (File => File,
                  Mode => Ada.Text_IO.In_File,
                  Name => File_Name,
                  Form => (if Read_Mode = Text
                           then "text_translation=yes"
                           else "text_translation=no"));

               declare
               begin
                  Hash_File_Procs (Configurations.Algorithm)
                    (File   => File,
                     Buffer => Buffer.all);

               exception
                  when others =>
                     --  Ensure file is closed if any exception occurs.
                     Ada.Text_IO.Close (File);
                     raise;
               end;

               Ada.Text_IO.Close (File);

            end if;

            if Read_Mode = Text then
               Put ("  ");
            else
               Put (" *");
            end if;
            Put_Line (File_Name);

         exception
            when Error : Ada.IO_Exceptions.Name_Error |
                 Ada.IO_Exceptions.Mode_Error |
                 Ada.IO_Exceptions.Device_Error |
                 Ada.IO_Exceptions.Data_Error |
                 Ada.IO_Exceptions.Status_Error
               =>
               Put (Standard_Error, "ksum: ");
               Put (Standard_Error, Exception_Message (Error));
               New_Line;
         end;
      end loop;
   end if;

exception
   when Error : Argument_Parser.Argument_Error =>
      Put (Standard_Error, "ksum: ");
      Put (Standard_Error, Exception_Message (Error));
      New_Line (Standard_Error);
      Set_Exit_Status (Failure);

   when Error : others =>
      Put (Standard_Error, "ksum: unhandled exception: ");
      Put (Standard_Error, Exception_Name (Error));
      New_Line;
      Put_Line (Standard_Error, Exception_Message (Error));
      Set_Exit_Status (Failure);
end Ksum;
