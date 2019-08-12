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
with Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;
with Ada.Command_Line;
with Ada.Directories;       use Ada.Directories;
with Ada.Exceptions;        use Ada.Exceptions;
with Ada.IO_Exceptions;

with Configurations;        use Configurations;
with Diagnostics;           use Diagnostics;

with Hex_Strings;           use Hex_Strings;

with Keccak.Types;          use Keccak.Types;

package body Checksums
is

   -----------------------
   --  Hash_File_Procs  --
   -----------------------
   --  This lookup table is used to print the checksum of a specific file
   --  based on the desired algorithm.

   type Hash_File_Procedure_Access is access procedure
     (File   : in     Ada.Text_IO.File_Type;
      Buffer : in out Keccak.Types.Byte_Array);

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

   type Check_File_Procedure_Access is access procedure
     (File          : in     Ada.Text_IO.File_Type;
      Buffer        : in out Keccak.Types.Byte_Array;
      Expected_Hash : in     Keccak.Types.Byte_Array;
      Result        :    out Diagnostic);

   ------------------------
   --  Check_File_Procs  --
   ------------------------
   --  This lookup table is used to check the checksum of a specific file
   --  based on the desired algorithm.

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

   procedure Check_Checksums_File (File          : in     Ada.Text_IO.File_Type;
                                   Buffer        : in out Keccak.Types.Byte_Array;
                                   Checked_Files : in out Natural;
                                   Format_Errors : in out Natural;
                                   Failed_Files  : in out Natural;
                                   IO_Errors     : in out Natural);
   --  Check each checksum in a file containing a list of checksums and file names.
   --
   --  This iterates through each line in a file and for each well-formatted
   --  line, computes the actual checksum of the named file against the expected
   --  checksum.
   --
   --  Lines are expected in the format:
   --  <checksum> <space> <space|asteisk> <file name>
   --
   --  For example:
   --  e7d29df82428e8de6273a5a95f26a526ca9cdf7a2428e06e42864e84b3e37fda *filename.txt

   procedure Check_File (File          : in     Ada.Text_IO.File_Type;
                         Buffer        : in out Keccak.Types.Byte_Array;
                         Expected_Hash : in     Unbounded_String;
                         Result        :    out Diagnostic);
   --  Computes the checksum over the contents of the specified file, and
   --  compares it against the specified expected hash.

   function Is_Hexadecimal_Character (C : in Character) return Boolean
   is (C in '0' .. '9' | 'a' .. 'f' | 'A' .. 'F')
     with Inline;

   -----------------------
   --  Print_Checksums  --
   -----------------------

   procedure Print_Checksums
   is
      File      : Ada.Text_IO.File_Type;
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
            if File_Name = "-" then
               Hash_File_Procs (Configurations.Algorithm)
                 (File   => Ada.Text_IO.Standard_Input,
                  Buffer => Buffer.all);

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

               Ada.Text_IO.Open
                 (File => File,
                  Mode => Ada.Text_IO.In_File,
                  Name => To_String (File_Name),
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

   -----------------------
   --  Check_Checksums  --
   -----------------------

   procedure Check_Checksums
   is
      File      : Ada.Text_IO.File_Type;
      Buffer    : Byte_Array_Access := null;

      Checked_Files : Natural;
      Format_Errors : Natural;
      Failed_Files  : Natural;
      IO_Errors     : Natural;

   begin
      Buffer := new Byte_Array (1 .. Configurations.Buffer_Size);

      --  For a negative output length, use the default output length for the
      --  specifed algorithm.
      if Output_Length < 0 then
         Output_Length := Default_Output_Length (Algorithm);
      end if;

      --  Iterate through each file name and hash it.
      for File_Name of Configurations.Files loop

         Checked_Files := 0;
         Format_Errors := 0;
         Failed_Files  := 0;
         IO_Errors     := 0;

         --  Special case for '-' which means Standard_Input
         if File_Name = "-" then
            Check_Checksums_File (File          => Ada.Text_IO.Standard_Input,
                                  Buffer        => Buffer.all,
                                  Checked_Files => Checked_Files,
                                  Format_Errors => Format_Errors,
                                  Failed_Files  => Failed_Files,
                                  IO_Errors     => IO_Errors);

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

            Ada.Text_IO.Open
              (File => File,
               Mode => Ada.Text_IO.In_File,
               Name => To_String (File_Name),
               Form => (if Read_Mode = Text
                        then "text_translation=yes"
                        else "text_translation=no"));

            declare
            begin
               Check_Checksums_File (File          => File,
                                     Buffer        => Buffer.all,
                                     Checked_Files => Checked_Files,
                                     Format_Errors => Format_Errors,
                                     Failed_Files  => Failed_Files,
                                     IO_Errors     => IO_Errors);

            exception
               when others =>
                  --  Ensure file is closed if any exception occurs.
                  Ada.Text_IO.Close (File);
                  raise;
            end;

            Ada.Text_IO.Close (File);

         end if;

         if Format_Errors > 0 then
            Ada.Text_IO.Put (Ada.Text_IO.Standard_Error, "ksum: WARNING: ");
            Ada.Text_IO.Put (Ada.Text_IO.Standard_Error, To_String (File_Name));
            Ada.Text_IO.Put (Ada.Text_IO.Standard_Error, ":");
            Ada.Text_IO.Put (Ada.Text_IO.Standard_Error, Integer'Image (Format_Errors));
            if Format_Errors > 1 then
               Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                                     " lines are improperly formatted");
            else
               Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                                     " line is improperly formatted");
            end if;
         end if;

         if Failed_Files > 0 then
            Ada.Command_Line.Set_Exit_Status (1);

            Ada.Text_IO.Put (Ada.Text_IO.Standard_Error, "ksum: WARNING: ");
            Ada.Text_IO.Put (Ada.Text_IO.Standard_Error, To_String (File_Name));
            Ada.Text_IO.Put (Ada.Text_IO.Standard_Error, ":");
            Ada.Text_IO.Put (Ada.Text_IO.Standard_Error, Integer'Image (Failed_Files));
            if Failed_Files > 1 then
               Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                                     " computed checksums did NOT match");
            else
               Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                                     " computed checksum did NOT match");
            end if;
         end if;

         if IO_Errors > 0 then
            Ada.Command_Line.Set_Exit_Status (1);

            Ada.Text_IO.Put (Ada.Text_IO.Standard_Error, "ksum: WARNING: ");
            Ada.Text_IO.Put (Ada.Text_IO.Standard_Error, To_String (File_Name));
            Ada.Text_IO.Put (Ada.Text_IO.Standard_Error, ":");
            Ada.Text_IO.Put (Ada.Text_IO.Standard_Error, Integer'Image (IO_Errors));
            if IO_Errors > 1 then
               Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                                     " files could not be opened or read");
            else
               Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                                     " file could not be opened or read");
            end if;
         end if;

         if Checked_Files = 0 then
            Ada.Text_IO.Put (Ada.Text_IO.Standard_Error, "ksum: ");
            Ada.Text_IO.Put (Ada.Text_IO.Standard_Error, To_String (File_Name));
            Ada.Text_IO.Put (Ada.Text_IO.Standard_Error, ": no properly formatted ");
            Ada.Text_IO.Put (Ada.Text_IO.Standard_Error,
                             To_String (Algorithm_Strings (Configurations.Algorithm)));
            Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, " checksum lines found");
         end if;
      end loop;

      Deallocate_Byte_Array (Buffer);

   exception
      when others =>
         Deallocate_Byte_Array (Buffer);
         raise;
   end Check_Checksums;

   ----------------------------
   --  Check_Checksums_File  --
   ----------------------------

   procedure Check_Checksums_File (File          : in     Ada.Text_IO.File_Type;
                                   Buffer        : in out Keccak.Types.Byte_Array;
                                   Checked_Files : in out Natural;
                                   Format_Errors : in out Natural;
                                   Failed_Files  : in out Natural;
                                   IO_Errors     : in out Natural)
   is
   begin
      File_Loop :
      while not Ada.Text_IO.End_Of_File (File) loop
         declare
            Line : constant Unbounded_String := Ada.Text_IO.Unbounded_IO.Get_Line (File);
            File_Name     : Unbounded_String;
            Expected_Hash : Unbounded_String;

            Target_File     : Ada.Text_IO.File_Type;

            Hash_Last       : Integer := -1;

            File_Name_First : Integer;

            Read_Mode_Pos   : Integer;
            File_Mode       : Read_Modes;

            Result          : Diagnostic;

         begin
            --  Ignore blank lines
            if Length (Line) > 0 then

               --  Find the last hex character in the checksum part of the line
               Line_Loop :
               for I in 1 .. Length (Line) loop
                  if Element (Line, I) = ' ' then
                     Hash_Last := I - 1;
                     exit Line_Loop;

                  elsif not Is_Hexadecimal_Character (Element (Line, I)) then
                     Hash_Last := -1;
                     exit Line_Loop;

                  end if;
               end loop Line_Loop;

               if Hash_Last < 1 or Length (Line) <= Hash_Last + 1 then
                  Format_Errors := Format_Errors + 1;
               else
                  --  Determine read mode
                  Read_Mode_Pos := Hash_Last + 2;
                  case Element (Line, Read_Mode_Pos) is
                     when ' ' =>
                        File_Mode := Text;
                        Result := No_Error;

                     when '*' =>
                        File_Mode := Binary;
                        Result := No_Error;

                     when others =>
                        Result := Format_Error;
                  end case;

                  File_Name_First := Hash_Last + 3;

                  Expected_Hash := Unbounded_Slice
                    (Source => Line,
                     Low    => 1,
                     High   => Hash_Last);

                  File_Name := Unbounded_Slice
                    (Source => Line,
                     Low    => File_Name_First,
                     High   => Length (Line));

                  --  Check the file
                  if Result /= No_Error then
                     null;

                  elsif Kind (To_String (File_Name)) = Directory then
                     raise Ada.IO_Exceptions.Name_Error
                     with To_String (File_Name) & ": Is a directory";

                  else
                     Ada.Text_IO.Open
                       (File => Target_File,
                        Mode => Ada.Text_IO.In_File,
                        Name => To_String (File_Name),
                        Form => (if File_Mode = Text
                                 then "text_translation=yes"
                                 else "text_translation=no"));

                     Check_File (File          => Target_File,
                                 Buffer        => Buffer,
                                 Expected_Hash => Expected_Hash,
                                 Result        => Result);

                     Ada.Text_IO.Close (Target_File);
                  end if;

                  --  Print result
                  case Result is
                     when No_Error =>
                        Checked_Files := Checked_Files + 1;
                        Ada.Text_IO.Put (To_String (File_Name));
                        Ada.Text_IO.Put_Line (": OK");

                     when Format_Error =>
                        Format_Errors := Format_Errors + 1;

                     when Checksum_Error =>
                        Checked_Files := Checked_Files + 1;
                        Failed_Files := Failed_Files + 1;

                        Ada.Text_IO.Put (File => Ada.Text_IO.Standard_Error,
                                         Item => To_String (File_Name));

                        Ada.Text_IO.Put_Line (File => Ada.Text_IO.Standard_Error,
                                              Item => ": FAILED");
                  end case;
               end if;
            end if;

         exception
            when Error : Ada.IO_Exceptions.Name_Error |
                 Ada.IO_Exceptions.Mode_Error |
                 Ada.IO_Exceptions.Device_Error |
                 Ada.IO_Exceptions.Data_Error |
                 Ada.IO_Exceptions.Status_Error |
                 Ada.IO_Exceptions.Use_Error |
                 Ada.IO_Exceptions.Name_Error
               =>
               Ada.Text_IO.Put_Line (File => Ada.Text_IO.Standard_Error,
                                     Item => Exception_Message (Error));

               Ada.Text_IO.Put (File => Ada.Text_IO.Standard_Error,
                                Item => To_String (File_Name));

               Ada.Text_IO.Put_Line (File => Ada.Text_IO.Standard_Error,
                                     Item => ": FAILED open or read");

               IO_Errors := IO_Errors + 1;
         end;
      end loop File_Loop;
   end Check_Checksums_File;

   ------------------
   --  Check_File  --
   ------------------

   procedure Check_File (File          : in     Ada.Text_IO.File_Type;
                         Buffer        : in out Keccak.Types.Byte_Array;
                         Expected_Hash : in     Unbounded_String;
                         Result        :    out Diagnostic)
   is
      Expected_Hash_Byte_Array : Byte_Array_Access := null;
      Str_Length : constant Natural := Length (Expected_Hash);

   begin
      if Str_Length mod 2 /= 0 then
         Result := Format_Error;
      else

         Expected_Hash_Byte_Array := new Keccak.Types.Byte_Array (1 .. Str_Length / 2);

         Parse_Hex_String (Str  => Expected_Hash,
                           Data => Expected_Hash_Byte_Array.all);

         Check_File_Procs (Configurations.Algorithm)
           (File          => File,
            Buffer        => Buffer,
            Expected_Hash => Expected_Hash_Byte_Array.all,
            Result        => Result);

         Deallocate_Byte_Array (Expected_Hash_Byte_Array);
      end if;

   exception
      when others =>
         Deallocate_Byte_Array (Expected_Hash_Byte_Array);
         raise;
   end Check_File;

end Checksums;
