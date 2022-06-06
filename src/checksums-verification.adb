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
with Ada.Characters.Latin_1;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Text_IO.Text_Streams;
with Ada.Text_IO.Unbounded_IO;
with Ada.Command_Line;
with Ada.Directories;       use Ada.Directories;
with Ada.Exceptions;        use Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Strings.Maps;

with Configurations;        use Configurations;

with Hex_Strings;           use Hex_Strings;

with Keccak.Types;          use Keccak.Types;

package body Checksums.Verification
is

   --------------------------
   --  Check_Stream_Procs  --
   --------------------------
   --  This lookup table is used to check the checksum of a specific stream
   --  based on the desired algorithm.

   type Check_Stream_Procedure_Access is access procedure
     (Stream        : in out Ada.Streams.Root_Stream_Type'Class;
      Buffer        : in out Keccak.Types.Byte_Array;
      Expected_Hash : in     Keccak.Types.Byte_Array;
      Result        :    out Diagnostic);

   Check_Stream_Procs : constant array (Algorithm_Names) of Check_Stream_Procedure_Access :=
     (Configurations.CSHAKE128           => CSHAKE128_Stream_Hashing.Check_Stream'Access,
      Configurations.CSHAKE256           => CSHAKE256_Stream_Hashing.Check_Stream'Access,
      Configurations.KangarooTwelve      => K12_Stream_Hashing.Check_Stream'Access,
      Configurations.MarsupilamiFourteen => M14_Stream_Hashing.Check_Stream'Access,
      Configurations.Keccak_224          => Keccak_224_Stream_Hashing.Check_Stream'Access,
      Configurations.Keccak_256          => Keccak_256_Stream_Hashing.Check_Stream'Access,
      Configurations.Keccak_384          => Keccak_384_Stream_Hashing.Check_Stream'Access,
      Configurations.Keccak_512          => Keccak_512_Stream_Hashing.Check_Stream'Access,
      Configurations.KMAC128             => KMAC128_Stream_Hashing.Check_Stream'Access,
      Configurations.KMAC256             => KMAC256_Stream_Hashing.Check_Stream'Access,
      Configurations.ParallelHash128     => ParallelHash128_Stream_Hashing.Check_Stream'Access,
      Configurations.ParallelHash256     => ParallelHash256_Stream_Hashing.Check_Stream'Access,
      Configurations.RawSHAKE128         => RawSHAKE128_Stream_Hashing.Check_Stream'Access,
      Configurations.RawSHAKE256         => RawSHAKE256_Stream_Hashing.Check_Stream'Access,
      Configurations.SHA3_224            => SHA3_224_Stream_Hashing.Check_Stream'Access,
      Configurations.SHA3_256            => SHA3_256_Stream_Hashing.Check_Stream'Access,
      Configurations.SHA3_384            => SHA3_384_Stream_Hashing.Check_Stream'Access,
      Configurations.SHA3_512            => SHA3_512_Stream_Hashing.Check_Stream'Access,
      Configurations.SHAKE128            => SHAKE128_Stream_Hashing.Check_Stream'Access,
      Configurations.SHAKE256            => SHAKE256_Stream_Hashing.Check_Stream'Access);

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
               Name => To_String (File_Name));

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
            if Configurations.Strict then
               Ada.Command_Line.Set_Exit_Status (1);
            end if;

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
      Line_Number : Positive := 1;

   begin
      File_Loop :
      while not Ada.Text_IO.End_Of_File (File) loop
         declare
            --  Trim carriage return characters from the end of the line.
            --  On Windows this is done automatically by Get_Line, but on Unix
            --  Get_Line does not consume the CR. Therefore, we trim it manually
            --  to ensure that ksum on Unix can parse checksum files generated
            --  on Windows.
            Line : constant Unbounded_String :=
              Trim (Source => Ada.Text_IO.Unbounded_IO.Get_Line (File),
                    Left   => Ada.Strings.Maps.Null_Set,
                    Right  => Ada.Strings.Maps.To_Set (Ada.Characters.Latin_1.CR));

            File_Name     : Unbounded_String;
            Expected_Hash : Unbounded_String;

            Target_File     : Ada.Streams.Stream_IO.File_Type;

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

               if Hash_Last < 1 or else Length (Line) <= Hash_Last + 1 then
                  Format_Errors := Format_Errors + 1;
                  Format_Warning (Ada.Text_IO.Name (File), Line_Number);

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

                  elsif File_Name = Standard_Input_File_Name then
                     Check_Stream
                       (Stream        => Ada.Text_IO.Text_Streams.Stream
                          (Ada.Text_IO.Standard_Input).all,
                        Buffer        => Buffer,
                        Expected_Hash => Expected_Hash,
                        Result        => Result);

                  elsif Kind (To_String (File_Name)) = Directory then
                     raise Ada.IO_Exceptions.Name_Error
                     with To_String (File_Name) & ": Is a directory";

                  else
                     Ada.Streams.Stream_IO.Open
                       (File => Target_File,
                        Mode => Ada.Streams.Stream_IO.In_File,
                        Name => To_String (File_Name));

                     Set_File_Mode (Target_File, File_Mode);

                     Check_Stream (Stream        => Ada.Streams.Stream_IO.Stream
                                   (Target_File).all,
                                   Buffer        => Buffer,
                                   Expected_Hash => Expected_Hash,
                                   Result        => Result);

                     Ada.Streams.Stream_IO.Close (Target_File);
                  end if;

                  --  Print result
                  case Result is
                     when No_Error =>
                        Checked_Files := Checked_Files + 1;
                        Check_Success (File_Name);

                     when Format_Error =>
                        Format_Errors := Format_Errors + 1;

                        Format_Warning (Ada.Text_IO.Name (File), Line_Number);

                     when Checksum_Error =>
                        Checked_Files := Checked_Files + 1;
                        Failed_Files  := Failed_Files  + 1;

                        Ada.Text_IO.Put (Ada.Text_IO.Standard_Error, To_String (File_Name));
                        Ada.Text_IO.Put (Ada.Text_IO.Standard_Error, ": FAILED");
                        Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);
                  end case;
               end if;
            end if;

            Line_Number := Line_Number + 1;

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
   --  Check_Stream  --
   ------------------

   procedure Check_Stream (Stream : in out Ada.Streams.Root_Stream_Type'Class;
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

         Check_Stream_Procs (Configurations.Algorithm)
           (Stream        => Stream,
            Buffer        => Buffer,
            Expected_Hash => Expected_Hash_Byte_Array.all,
            Result        => Result);

         Deallocate_Byte_Array (Expected_Hash_Byte_Array);
      end if;

   exception
      when others =>
         Deallocate_Byte_Array (Expected_Hash_Byte_Array);
         raise;
   end Check_Stream;

   ----------------------
   --  Format_Warning  --
   ----------------------

   procedure Format_Warning (File_Name   : in String;
                             Line_Number : in Positive)
   is
   begin
      if Configurations.Warn then
         Ada.Text_IO.Put (Ada.Text_IO.Standard_Error, "ksum: ");
         Ada.Text_IO.Put (Ada.Text_IO.Standard_Error, File_Name);
         Ada.Text_IO.Put (Ada.Text_IO.Standard_Error, ':');
         Ada.Text_IO.Put (Ada.Text_IO.Standard_Error, Integer'Image (Line_Number));
         Ada.Text_IO.Put (Ada.Text_IO.Standard_Error,
                          ": improperly formatted checksum line");
         Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);
      end if;
   end Format_Warning;

   ---------------------
   --  Check_Success  --
   ---------------------

   procedure Check_Success (File_Name : in Unbounded_String)
   is
   begin
      if not Configurations.Quiet then
         Ada.Text_IO.Put (To_String (File_Name));
         Ada.Text_IO.Put_Line (": OK");
      end if;
   end Check_Success;

end Checksums.Verification;
