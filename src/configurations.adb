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
with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Strings;       use Ada.Strings;
with Argument_Parser;   use Argument_Parser;
with Hex_Strings;       use Hex_Strings;

package body Configurations
is

   procedure Set_Key           (Short_Name, Long_Name, Arg : in String);
   procedure Set_Block_Size    (Short_Name, Long_Name, Arg : in String);
   procedure Set_Buffer_Size   (Short_Name, Long_Name, Arg : in String);
   procedure Set_Output_Size   (Short_Name, Long_Name, Arg : in String);
   procedure Set_Read_Mode     (Short_Name, Long_Name, Arg : in String);
   procedure Set_XOF_Mode      (Short_Name, Long_Name, Arg : in String);
   procedure Set_Function      (Short_Name, Long_Name, Arg : in String);
   procedure Set_Customization (Short_Name, Long_Name, Arg : in String);
   procedure Set_Algorithm     (Short_Name, Long_Name, Arg : in String);
   procedure Add_Stdin         (Short_Name, Long_Name, Arg : in String);
   procedure Print_Help        (Short_Name, Long_Name, Arg : in String);


   Switch_Table : constant Switch_Descriptior_Array :=
     ((Short_Name   => To_Unbounded_String ("-"),
       Long_Name    => Null_Unbounded_String,
       Description  => Null_Unbounded_String,
       Has_Argument => False,
       Handler      => Add_Stdin'Access),

      (Short_Name   => To_Unbounded_String ("-h"),
       Long_Name    => To_Unbounded_String ("--help"),
       Description  => To_Unbounded_String ("Print this message"),
       Has_Argument => False,
       Handler      => Print_Help'Access),

      (Short_Name   => To_Unbounded_String ("-k"),
       Long_Name    => To_Unbounded_String ("--key"),
       Description  => To_Unbounded_String ("Key used for KMAC as a hex string"),
       Has_Argument => True,
       Handler      => Set_Key'Access),

      (Short_Name   => To_Unbounded_String ("-B"),
       Long_Name    => To_Unbounded_String ("--block-size"),
       Description  => To_Unbounded_String ("Set the block size (bytes) for ParallelHash"),
       Has_Argument => True,
       Handler      => Set_Block_Size'Access),

      (Short_Name   => To_Unbounded_String ("-z"),
       Long_Name    => To_Unbounded_String ("--buffer-size"),
       Description  => To_Unbounded_String ("Set the buffer size used for reading file/stdin data"),
       Has_Argument => True,
       Handler      => Set_Buffer_Size'Access),

      (Short_Name   => To_Unbounded_String ("-n"),
       Long_Name    => To_Unbounded_String ("--output-size"),
       Description  => To_Unbounded_String ("Number of bytes to output"),
       Has_Argument => True,
       Handler      => Set_Output_Size'Access),

      (Short_Name   => To_Unbounded_String ("-x"),
       Long_Name    => To_Unbounded_String ("--xof"),
       Description  => To_Unbounded_String ("Use XOF mode for output"),
       Has_Argument => False,
       Handler      => Set_XOF_Mode'Access),

      (Short_Name   => To_Unbounded_String ("-f"),
       Long_Name    => To_Unbounded_String ("--function"),
       Description  => To_Unbounded_String ("Set the function name for cSHAKE"),
       Has_Argument => False,
       Handler      => Set_Function'Access),

      (Short_Name   => To_Unbounded_String ("-t"),
       Long_Name    => To_Unbounded_String ("--text"),
       Description  => To_Unbounded_String ("read in text mode (default)"),
       Has_Argument => False,
       Handler      => Set_Read_Mode'Access),

      (Short_Name   => To_Unbounded_String ("-b"),
       Long_Name    => To_Unbounded_String ("--binary"),
       Description  => To_Unbounded_String ("read in binary mode"),
       Has_Argument => False,
       Handler      => Set_Read_Mode'Access),

      (Short_Name   => To_Unbounded_String ("-C"),
       Long_Name    => To_Unbounded_String ("--customization"),
       Description  => To_Unbounded_String ("Set the customization string"),
       Has_Argument => False,
       Handler      => Set_Customization'Access),

      (Short_Name   => Null_Unbounded_String,
       Long_Name    => To_Unbounded_String ("--cshake128"),
       Description  => To_Unbounded_String ("Use cSHAKE128"),
       Has_Argument => False,
       Handler      => Set_Algorithm'Access),

      (Short_Name   => Null_Unbounded_String,
       Long_Name    => To_Unbounded_String ("--cshake256"),
       Description  => To_Unbounded_String ("Use cSHAKE256"),
       Has_Argument => False,
       Handler      => Set_Algorithm'Access),

      (Short_Name   => Null_Unbounded_String,
       Long_Name    => To_Unbounded_String ("--kangarootwelve"),
       Description  => To_Unbounded_String ("Use KangarooTwelve"),
       Has_Argument => False,
       Handler      => Set_Algorithm'Access),

      (Short_Name   => Null_Unbounded_String,
       Long_Name    => To_Unbounded_String ("--keccak-224"),
       Description  => To_Unbounded_String ("Use Keccak-224"),
       Has_Argument => False,
       Handler      => Set_Algorithm'Access),

      (Short_Name   => Null_Unbounded_String,
       Long_Name    => To_Unbounded_String ("--keccak-256"),
       Description  => To_Unbounded_String ("Use Keccak-256"),
       Has_Argument => False,
       Handler      => Set_Algorithm'Access),

      (Short_Name   => Null_Unbounded_String,
       Long_Name    => To_Unbounded_String ("--keccak-384"),
       Description  => To_Unbounded_String ("Use Keccak-384"),
       Has_Argument => False,
       Handler      => Set_Algorithm'Access),

      (Short_Name   => Null_Unbounded_String,
       Long_Name    => To_Unbounded_String ("--keccak-512"),
       Description  => To_Unbounded_String ("Use Keccak-512"),
       Has_Argument => False,
       Handler      => Set_Algorithm'Access),

      (Short_Name   => Null_Unbounded_String,
       Long_Name    => To_Unbounded_String ("--kmac128"),
       Description  => To_Unbounded_String ("Use KMAC128"),
       Has_Argument => False,
       Handler      => Set_Algorithm'Access),

      (Short_Name   => Null_Unbounded_String,
       Long_Name    => To_Unbounded_String ("--kmac256"),
       Description  => To_Unbounded_String ("Use KMAC256"),
       Has_Argument => False,
       Handler      => Set_Algorithm'Access),

      (Short_Name   => Null_Unbounded_String,
       Long_Name    => To_Unbounded_String ("--parallelhash128"),
       Description  => To_Unbounded_String ("Use ParallelHash128"),
       Has_Argument => False,
       Handler      => Set_Algorithm'Access),

      (Short_Name   => Null_Unbounded_String,
       Long_Name    => To_Unbounded_String ("--parallelhash256"),
       Description  => To_Unbounded_String ("Use ParallelHash256"),
       Has_Argument => False,
       Handler      => Set_Algorithm'Access),

      (Short_Name   => Null_Unbounded_String,
       Long_Name    => To_Unbounded_String ("--rawshake128"),
       Description  => To_Unbounded_String ("Use RawSHAKE128"),
       Has_Argument => False,
       Handler      => Set_Algorithm'Access),

      (Short_Name   => Null_Unbounded_String,
       Long_Name    => To_Unbounded_String ("--rawshake256"),
       Description  => To_Unbounded_String ("Use RawSHAKE256"),
       Has_Argument => False,
       Handler      => Set_Algorithm'Access),

      (Short_Name   => Null_Unbounded_String,
       Long_Name    => To_Unbounded_String ("--sha3-224"),
       Description  => To_Unbounded_String ("Use SHA3-224"),
       Has_Argument => False,
       Handler      => Set_Algorithm'Access),

      (Short_Name   => Null_Unbounded_String,
       Long_Name    => To_Unbounded_String ("--sha3-256"),
       Description  => To_Unbounded_String ("Use SHA3-256 (default)"),
       Has_Argument => False,
       Handler      => Set_Algorithm'Access),

      (Short_Name   => Null_Unbounded_String,
       Long_Name    => To_Unbounded_String ("--sha3-384"),
       Description  => To_Unbounded_String ("Use SHA3-384"),
       Has_Argument => False,
       Handler      => Set_Algorithm'Access),

      (Short_Name   => Null_Unbounded_String,
       Long_Name    => To_Unbounded_String ("--sha3-512"),
       Description  => To_Unbounded_String ("Use SHA3-512"),
       Has_Argument => False,
       Handler      => Set_Algorithm'Access),

      (Short_Name   => Null_Unbounded_String,
       Long_Name    => To_Unbounded_String ("--shake128"),
       Description  => To_Unbounded_String ("Use SHAKE128"),
       Has_Argument => False,
       Handler      => Set_Algorithm'Access),

      (Short_Name   => Null_Unbounded_String,
       Long_Name    => To_Unbounded_String ("--shake256"),
       Description  => To_Unbounded_String ("Use SHAKE256"),
       Has_Argument => False,
       Handler      => Set_Algorithm'Access));


   procedure Set_Key (Short_Name, Long_Name, Arg : in String)
   is
      pragma Unreferenced (Short_Name);
      pragma Unreferenced (Long_Name);

   begin
      if Key /= null then
         raise Argument_Error with "error: can't set --key more than once";

      elsif Arg'Length mod 2 /= 0 then
         raise Argument_Error with "error: --key length not a multiple of 2";

      else
         Key := new Byte_Array (1 .. Arg'Length / 2);

         Parse_Hex_String (Arg, Key.all);
      end if;
   end Set_Key;


   procedure Set_Block_Size (Short_Name, Long_Name, Arg : in String)
   is
      pragma Unreferenced (Short_Name);
      pragma Unreferenced (Long_Name);

   begin
      Block_Size := Positive'Value (Arg);

   exception
      when Constraint_Error =>
         raise Argument_Error with "block size must be a positive integer in the range "
           & Positive'Image (Positive'First)
           & " .."
           & Positive'Image (Positive'Last);
   end Set_Block_Size;


   procedure Set_Buffer_Size (Short_Name, Long_Name, Arg : in String)
   is
      pragma Unreferenced (Short_Name);
      pragma Unreferenced (Long_Name);

   begin
      Buffer_Size := Positive'Value (Arg);

   exception
      when Constraint_Error =>
         raise Argument_Error with "buffer size must be a positive integer in the range "
           & Positive'Image (Positive'First)
           & " .."
           & Positive'Image (Positive'Last);
   end Set_Buffer_Size;


   procedure Set_Output_Size (Short_Name, Long_Name, Arg : in String)
   is
      pragma Unreferenced (Short_Name);
      pragma Unreferenced (Long_Name);

   begin
      Output_Length := Long_Long_Integer'Value (Arg);

   exception
      when Constraint_Error =>
         raise Argument_Error with "output size must be an integer";
   end Set_Output_Size;


   procedure Set_Read_Mode (Short_Name, Long_Name, Arg : in String)
   is
      pragma Unreferenced (Long_Name);
      pragma Unreferenced (Arg);

   begin
      if Short_Name = "-t" then
         Read_Mode := Text;
      else
         Read_Mode := Binary;
      end if;
   end Set_Read_Mode;


   procedure Set_XOF_Mode (Short_Name, Long_Name, Arg : in String)
   is
      pragma Unreferenced (Short_Name);
      pragma Unreferenced (Long_Name);
      pragma Unreferenced (Arg);

   begin
      XOF_Mode := True;
   end Set_XOF_Mode;


   procedure Set_Function (Short_Name, Long_Name, Arg : in String)
   is
      pragma Unreferenced (Short_Name);
      pragma Unreferenced (Long_Name);

   begin
      Function_Name := To_Unbounded_String (Arg);
   end Set_Function;


   procedure Set_Customization (Short_Name, Long_Name, Arg : in String)
   is
      pragma Unreferenced (Short_Name);
      pragma Unreferenced (Long_Name);

   begin
      Customization := To_Unbounded_String (Arg);
   end Set_Customization;


   procedure Set_Algorithm (Short_Name, Long_Name, Arg : in String)
   is
      pragma Unreferenced (Short_Name);
      pragma Unreferenced (Arg);

      Found : Boolean := False;

   begin
         --  Find the algorithm.
      for Algo in Algorithm_Names loop
         if Long_Name = "--" & Algorithm_Strings (Algo) then
            Algorithm := Algo;
            Found     := True;
            exit;
         end if;
      end loop;

      if not Found then
         --  This would only happen if there's a switch configured to call
         --  this procedure, but we couldn't find a matching algorithm
         --  string to the switch name.
         raise Program_Error with "Couldn't find algorithm: " & Long_Name;
      end if;
   end Set_Algorithm;


   procedure Add_Stdin (Short_Name, Long_Name, Arg : in String)
   is
      pragma Unreferenced (Short_Name);
      pragma Unreferenced (Long_Name);
      pragma Unreferenced (Arg);

   begin
      Files.Append (To_Unbounded_String ("-"));
   end Add_Stdin;


   procedure Print_Help (Short_Name, Long_Name, Arg : in String)
   is
      pragma Unreferenced (Short_Name);
      pragma Unreferenced (Long_Name);
      pragma Unreferenced (Arg);

      Max_Spaces : constant := 18;
      --  Space after switch short name to the description.
      --  This should be set to be longer than all long switch names.

   begin
      Help_Displayed := True;

      Put_Line ("Usage: ksum [OPTIONS]... [FILE]...");
      Put_Line ("Print checksums using Keccak-based algorithms");
      New_Line;
      Put_Line ("With no FILE, or when FILE is -, read standard input");
      New_Line;
      Put_Line ("ksum switches:");

      for I in Switch_Table'Range loop

         --  We implement the "-" for Standard_Input as a switch, but don't
         --  include it in the list of switches, as it's not really a switch.
         if Switch_Table (I).Short_Name /= "-" then

            if Switch_Table (I).Short_Name = Null_Unbounded_String then
               Put ("      ");
            else
               Put ("  ");
               Put (To_String (Switch_Table (I).Short_Name));
               Put (", ");
            end if;
            Put (To_String (Switch_Table (I).Long_Name));

            for J in reverse Length (Switch_Table (I).Long_Name) .. Max_Spaces loop
               Put (' ');
            end loop;

            Put_Line (To_String (Switch_Table (I).Description));
         end if;
      end loop;

   end Print_Help;


   procedure Parse_Command_Line
   is
   begin
      Argument_Parser.Parse_Command_Line (Switch_Table, Files);
   end Parse_Command_Line;

end Configurations;
