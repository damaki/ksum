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
with Ada.Streams.Stream_IO;      use Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Text_IO.Text_Streams;   use Ada.Text_IO.Text_Streams;
with Ada.Unchecked_Deallocation;
with Configurations;             use Configurations;
with Hex_Strings;                use Hex_Strings;
with Stream_Byte_Arrays;         use Stream_Byte_Arrays;

package body File_KMAC
is
   Null_Key : constant Keccak.Types.Byte_Array (1 .. 0) := (others => 0);


   procedure Print_Output (Ctx    : in out KMAC.Context;
                           Buffer : in out Keccak.Types.Byte_Array)
   is
      Remaining : Long_Long_Integer;

   begin
      if Configurations.XOF_Mode then
         Remaining := Configurations.Output_Length;

         while Remaining >= Long_Long_Integer (Buffer'Length) loop
            KMAC.Extract (Ctx, Buffer);
            Print_Hex_String (Buffer);

            Remaining := Remaining - Long_Long_Integer (Buffer'Length);
         end loop;

         if Remaining > 0 then
            KMAC.Extract (Ctx, Buffer (Buffer'First .. Buffer'First + Natural (Remaining - 1)));
            Print_Hex_String (Buffer (Buffer'First .. Buffer'First + Natural (Remaining - 1)));
         end if;

      else
         if Configurations.Output_Length > Long_Long_Integer (Natural'Last) then
            raise Constraint_Error with "Requested output length too large";
         end if;

         declare
            Out_Buffer : Byte_Array_Access := new Keccak.Types.Byte_Array (1 .. Natural (Configurations.Output_Length));

         begin
            KMAC.Finish (Ctx, Out_Buffer.all);

            Print_Hex_String (Out_Buffer.all);

            Deallocate_Byte_Array (Out_Buffer);
         end;
      end if;
   end Print_Output;


   procedure Hash_File (File   : in out Ada.Streams.Stream_IO.File_Type;
                        Buffer : in out Keccak.Types.Byte_Array)
   is
      Ctx    : KMAC.Context;
      Length : Natural;

   begin
      if Configurations.Key = null then
         KMAC.Init
           (Ctx           => Ctx,
            Key           => Null_Key,
            Customization => To_String (Configurations.Customization));

      else
         KMAC.Init
           (Ctx           => Ctx,
            Key           => Configurations.Key.all,
            Customization => To_String (Configurations.Customization));
      end if;


      while not End_Of_File (File) loop
         Read_Byte_Array (Stream (File), Buffer, Length);

         if Length = 0 then
            raise Program_Error with "Could not read from stream";
         end if;

         KMAC.Update (Ctx, Buffer (Buffer'First .. Buffer'First + (Length - 1)));
      end loop;

      Print_Output (Ctx, Buffer);
   end Hash_File;


   procedure Hash_Standard_Input (Buffer : in out Keccak.Types.Byte_Array)
   is
      Ctx    : KMAC.Context;
      Length : Natural;

   begin
      if Configurations.Key = null then
         KMAC.Init
           (Ctx           => Ctx,
            Key           => Null_Key,
            Customization => To_String (Configurations.Customization));

      else
         KMAC.Init
           (Ctx           => Ctx,
            Key           => Configurations.Key.all,
            Customization => To_String (Configurations.Customization));
      end if;


      while not End_Of_File (Standard_Input) loop
         Read_Byte_Array (Stream (Standard_Input), Buffer, Length);

         if Length = 0 then
            raise Program_Error with "Could not read from stream";
         end if;

         KMAC.Update (Ctx, Buffer (Buffer'First .. Buffer'First + (Length - 1)));
      end loop;

      Print_Output (Ctx, Buffer);
   end Hash_Standard_Input;

end File_KMAC;
