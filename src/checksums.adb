with Ada.Directories;       use Ada.Directories;
with Ada.Exceptions;        use Ada.Exceptions;
with Ada.IO_Exceptions;
with Argument_Parser;       use Argument_Parser;

package body Checksums
is

   -----------------------
   --  Print_Checksums  --
   -----------------------

   procedure Print_Checksums
   is
      File      : Ada.Text_IO.File_Type;
      Buffer    : Byte_Array_Access;

   begin
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
   end Print_Checksums;

   -----------------------
   --  Check_Checksums  --
   -----------------------

   procedure Check_Checksums
   is
   begin
      null;
   end Check_Checksums;

end Checksums;
