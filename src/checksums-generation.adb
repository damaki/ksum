-------------------------------------------------------------------------------
--  Copyright (c) 2020, Daniel King
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
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Streams.Stream_IO.C_Streams;
with Ada.Strings.Unbounded;           use Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Directories;                 use Ada.Directories;
with Ada.Exceptions;                  use Ada.Exceptions;
with Ada.IO_Exceptions;

with Interfaces.C_Streams;            use Interfaces.C_Streams;

with Configurations;                  use Configurations;

with Keccak.Types;                    use Keccak.Types;

package body Checksums.Generation
is

   -------------------------
   --  Hash_Stream_Procs  --
   -------------------------
   --  This lookup table is used to print the checksum of a specific stream
   --  based on the desired algorithm.

   type Hash_Stream_Procedure_Access is access procedure
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Buffer : in out Keccak.Types.Byte_Array);

   Hash_Stream_Procs : constant array (Algorithm_Names) of Hash_Stream_Procedure_Access :=
     (Configurations.CSHAKE128           => CSHAKE128_Stream_Hashing.Hash_Stream'Access,
      Configurations.CSHAKE256           => CSHAKE256_Stream_Hashing.Hash_Stream'Access,
      Configurations.KangarooTwelve      => K12_Stream_Hashing.Hash_Stream'Access,
      Configurations.MarsupilamiFourteen => M14_Stream_Hashing.Hash_Stream'Access,
      Configurations.Keccak_224          => Keccak_224_Stream_Hashing.Hash_Stream'Access,
      Configurations.Keccak_256          => Keccak_256_Stream_Hashing.Hash_Stream'Access,
      Configurations.Keccak_384          => Keccak_384_Stream_Hashing.Hash_Stream'Access,
      Configurations.Keccak_512          => Keccak_512_Stream_Hashing.Hash_Stream'Access,
      Configurations.KMAC128             => KMAC128_Stream_Hashing.Hash_Stream'Access,
      Configurations.KMAC256             => KMAC256_Stream_Hashing.Hash_Stream'Access,
      Configurations.ParallelHash128     => ParallelHash128_Stream_Hashing.Hash_Stream'Access,
      Configurations.ParallelHash256     => ParallelHash256_Stream_Hashing.Hash_Stream'Access,
      Configurations.RawSHAKE128         => RawSHAKE128_Stream_Hashing.Hash_Stream'Access,
      Configurations.RawSHAKE256         => RawSHAKE256_Stream_Hashing.Hash_Stream'Access,
      Configurations.SHA3_224            => SHA3_224_Stream_Hashing.Hash_Stream'Access,
      Configurations.SHA3_256            => SHA3_256_Stream_Hashing.Hash_Stream'Access,
      Configurations.SHA3_384            => SHA3_384_Stream_Hashing.Hash_Stream'Access,
      Configurations.SHA3_512            => SHA3_512_Stream_Hashing.Hash_Stream'Access,
      Configurations.SHAKE128            => SHAKE128_Stream_Hashing.Hash_Stream'Access,
      Configurations.SHAKE256            => SHAKE256_Stream_Hashing.Hash_Stream'Access);

   -----------------------
   --  Print_Checksums  --
   -----------------------

   procedure Print_Checksums
   is
      File      : Ada.Streams.Stream_IO.File_Type;
      Buffer    : Byte_Array_Access := null;

   begin
      Buffer := new Byte_Array (1 .. Configurations.Buffer_Size);

      --  For a negative output length, use the default output length for the
      --  specifed algorithm.
      if Output_Length < 0 then
         Output_Length := Default_Output_Length (Algorithm);
      end if;

      --  Iterate through each file name and hash it.
      for File_Name of Configurations.Files loop
         declare
         begin

            --  Special case for '-' which means Standard_Input
            if File_Name = Standard_Input_File_Name then
               Ada.Streams.Stream_IO.C_Streams.Open
                 (File     => File,
                  Mode     => Ada.Streams.Stream_IO.In_File,
                  C_Stream => Interfaces.C_Streams.stdin);
            else
               --  GNAT's Open procedure succeeds even when File_Name is a
               --  directory. A Device_Error exception is then thrown when
               --  trying to read the File, and the exception does not include
               --  any meaningful error message.
               --
               --  We therefore handle the case of attempting to open a
               --  directory
               if Kind (To_String (File_Name)) = Directory then
                  raise Ada.IO_Exceptions.Name_Error
                  with To_String (File_Name) & ": Is a directory";
               end if;

               Ada.Streams.Stream_IO.Open
                 (File => File,
                  Mode => Ada.Streams.Stream_IO.In_File,
                  Name => To_String (File_Name));
            end if;

            Set_File_Mode (File, Configurations.Read_Mode);

            declare
            begin
               Hash_Stream_Procs (Configurations.Algorithm)
                 (Stream => Ada.Streams.Stream_IO.Stream (File).all,
                  Buffer => Buffer.all);

            exception
               when others =>
                  --  Ensure file is closed if any exception occurs.
                  Ada.Streams.Stream_IO.Close (File);
                  raise;
            end;

            Ada.Streams.Stream_IO.Close (File);

            if Read_Mode = Text then
               Ada.Text_IO.Put ("  ");
            else
               Ada.Text_IO.Put (" *");
            end if;
            Ada.Text_IO.Put_Line (To_String (File_Name));

         exception
            when Error : Ada.IO_Exceptions.Name_Error |
                 Ada.IO_Exceptions.Mode_Error |
                 Ada.IO_Exceptions.Device_Error |
                 Ada.IO_Exceptions.Data_Error |
                 Ada.IO_Exceptions.Status_Error |
                 Ada.IO_Exceptions.Use_Error
               =>
               Ada.Text_IO.Put (Ada.Text_IO.Standard_Error, "ksum: ");
               Ada.Text_IO.Put (Ada.Text_IO.Standard_Error, Exception_Message (Error));
               Ada.Text_IO.New_Line;

               Ada.Command_Line.Set_Exit_Status (1);
         end;
      end loop;

      Deallocate_Byte_Array (Buffer);

   exception
      when others =>
         Deallocate_Byte_Array (Buffer);
         raise;
   end Print_Checksums;

end Checksums.Generation;
