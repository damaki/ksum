with Ada.Command_Line;
with Ada.Directories;       use Ada.Directories;
with Ada.Exceptions;        use Ada.Exceptions;
with Ada.IO_Exceptions;

with Hex_Strings;           use Hex_Strings;

package body Checksums
is

   procedure Check_Checksums_File (File          : in     Ada.Text_IO.File_Type;
                                   Buffer        : in out Keccak.Types.Byte_Array;
                                   Checked_Files : in out Natural;
                                   Format_Errors : in out Natural;
                                   Failed_Files  : in out Natural);

   procedure Check_File (File          : in     Ada.Text_IO.File_Type;
                         Buffer        : in out Keccak.Types.Byte_Array;
                         Expected_Hash : in     String;
                         Result        :    out Diagnostic);

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
                 Ada.IO_Exceptions.Status_Error
               =>
               Ada.Text_IO.Put (Ada.Text_IO.Standard_Error, "ksum: ");
               Ada.Text_IO.Put (Ada.Text_IO.Standard_Error, Exception_Message (Error));
               Ada.Text_IO.New_Line;
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

         --  Special case for '-' which means Standard_Input
         if File_Name = "-" then
            Check_Checksums_File (File          => Ada.Text_IO.Standard_Input,
                                  Buffer        => Buffer.all,
                                  Checked_Files => Checked_Files,
                                  Format_Errors => Format_Errors,
                                  Failed_Files  => Failed_Files);

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
                                     Failed_Files  => Failed_Files);

            exception
               when others =>
                  --  Ensure file is closed if any exception occurs.
                  Ada.Text_IO.Close (File);
                  raise;
            end;

            Ada.Text_IO.Close (File);

         end if;

         if Checked_Files = 0 then
            Ada.Text_IO.Put ("ksum: ");
            Ada.Text_IO.Put (To_String (File_Name));
            Ada.Text_IO.Put (": no properly formatted ");
            Ada.Text_IO.Put (To_String (Algorithm_Strings (Configurations.Algorithm)));
            Ada.Text_IO.Put_Line (" checksum lines found");
         end if;

         if Format_Errors > 0 then
            Ada.Text_IO.Put ("ksum: ");
            Ada.Text_IO.Put (To_String (File_Name));
            Ada.Text_IO.Put (": WARNING:");
            Ada.Text_IO.Put (Integer'Image (Format_Errors));
            if Format_Errors > 1 then
               Ada.Text_IO.Put_Line (" lines are improperly formatted");
            else
               Ada.Text_IO.Put_Line (" line is improperly formatted");
            end if;
         end if;

         if Failed_Files > 0 then
            Ada.Command_Line.Set_Exit_Status (1);

            Ada.Text_IO.Put ("ksum: ");
            Ada.Text_IO.Put (To_String (File_Name));
            Ada.Text_IO.Put (": WARNING:");
            Ada.Text_IO.Put (Integer'Image (Failed_Files));
            if Format_Errors > 1 then
               Ada.Text_IO.Put_Line (" computed checksums did NOT match");
            else
               Ada.Text_IO.Put_Line (" computed checksum did NOT match");
            end if;
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
                                   Failed_Files  : in out Natural)
   is
   begin
      File_Loop :
      while not Ada.Text_IO.End_Of_File (File) loop
         declare
            type Local_String_Access is access String;

            Line : constant Local_String_Access := new String'(Ada.Text_IO.Get_Line (File));

            Target_File     : Ada.Text_IO.File_Type;

            Hash_Last       : Integer := Line'First - 1;

            File_Name_First : Integer;

            Read_Mode_Pos   : Integer;
            File_Mode       : Read_Modes;

            Result          : Diagnostic;

         begin
            --  Ignore blank lines
            if Line.all'Length > 0 then

               --  Find the last hex character in the checksum part of the line
               Line_Loop :
               for I in Line'Range loop
                  if Line.all (I) = ' ' then
                     Hash_Last := I - 1;
                     exit Line_Loop;

                  elsif not Is_Hexadecimal_Character (Line.all (I)) then
                     Hash_Last := Line.all'First - 1;
                     exit Line_Loop;

                  end if;
               end loop Line_Loop;

               if Hash_Last < Line'First or Line'Last <= Hash_Last + 1 then
                  Format_Errors := Format_Errors + 1;
               else
                  --  Determine read mode
                  Read_Mode_Pos := Hash_Last + 2;
                  case Line.all (Read_Mode_Pos) is
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

                  --  Check the file
                  if Result = No_Error then
                     Ada.Text_IO.Open
                       (File => Target_File,
                        Mode => Ada.Text_IO.In_File,
                        Name => Line.all (Hash_Last + 3 .. Line'Last),
                        Form => (if File_Mode = Text
                                 then "text_translation=yes"
                                 else "text_translation=no"));

                     Check_File (File          => Target_File,
                                 Buffer        => Buffer,
                                 Expected_Hash => Line.all (Line.all'First .. Hash_Last),
                                 Result        => Result);

                     Ada.Text_IO.Close (Target_File);
                  end if;

                  --  Print result
                  case Result is
                     when No_Error =>
                        Checked_Files := Checked_Files + 1;
                        Ada.Text_IO.Put (Line.all (File_Name_First .. Line'Last));
                        Ada.Text_IO.Put_Line (": OK");

                     when Format_Error =>
                        Format_Errors := Format_Errors + 1;

                     when Checksum_Error =>
                        Checked_Files := Checked_Files + 1;
                        Failed_Files := Failed_Files + 1;
                        Ada.Text_IO.Put (Line.all (File_Name_First .. Line'Last));
                        Ada.Text_IO.Put_Line (": FAILED");
                  end case;
               end if;
            end if;
         end;
      end loop File_Loop;
   end Check_Checksums_File;

   ------------------
   --  Check_File  --
   ------------------

   procedure Check_File (File          : in     Ada.Text_IO.File_Type;
                         Buffer        : in out Keccak.Types.Byte_Array;
                         Expected_Hash : in     String;
                         Result        :    out Diagnostic)
   is
      Expected_Hash_Byte_Array : Byte_Array_Access := null;

   begin
      if Expected_Hash'Length mod 2 /= 0 then
         Result := Format_Error;
      else

         Expected_Hash_Byte_Array := new Keccak.Types.Byte_Array (1 .. Expected_Hash'Length / 2);

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
