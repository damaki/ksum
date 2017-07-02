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
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces;  use Interfaces;

package body Hex_Strings
is

   Buffer_Length : constant := 64*1024;

   subtype Length_Number is Natural range 0 .. Buffer_Length;

   Hex_Chars : constant array (Byte range 0 .. 15) of Character :=
     ('0', '1', '2', '3', '4', '5', '6', '7',
      '8', '9', 'a', 'b', 'c', 'd', 'e', 'f');

   procedure Print_Hex_String (Data : in Byte_Array)
   is
      Buffer : String (1 .. Buffer_Length);
      Length : Length_Number;

      Remaining : Natural := Data'Length;
      Offset    : Natural := 0;

   begin

      while Remaining >= Buffer_Length / 2 loop
         for I in 0 .. (Buffer_Length / 2) - 1 loop
            Buffer ((I * 2) + 1) := Hex_Chars (Shift_Right (Data (Data'First + Offset + I), 4));
            Buffer ((I * 2) + 2) := Hex_Chars (Data (Data'First + Offset + I) and 16#0F#);
         end loop;

         Put (Buffer);

         Remaining := Remaining - (Buffer_Length / 2);
         Offset    := Offset    + (Buffer_Length / 2);
      end loop;

      Length := 0;
      while Remaining > 0 loop
         Buffer (Length + 1) := Hex_Chars (Shift_Right (Data (Data'First + Offset), 4));
         Buffer (Length + 2) := Hex_Chars (Data (Data'First + Offset) and 16#0F#);

         Length    := Length    + 2;
         Offset    := Offset    + 1;
         Remaining := Remaining - 1;
      end loop;

      if Length > 0 then
         Put (Buffer (1 .. Length));
      end if;

   end Print_Hex_String;


   procedure Parse_Hex_String (Str  : in     String;
                               Data :    out Byte_Array)
   is
      Str_Pos  : Positive := Str'First;
      Data_Pos : Keccak.Types.Index_Number := Data'First;
      C        : Character;

      Upper : Keccak.Types.Byte;
      Lower : Keccak.Types.Byte;

   begin
      while Str_Pos < Str'Last loop

         C := Str (Str_Pos);
         case C is
            when '0' .. '9' => Upper := Character'Pos (C) - Character'Pos('0');
            when 'a' .. 'f' => Upper := Character'Pos (C) - Character'Pos('a') + 10;
            when 'A' .. 'F' => Upper := Character'Pos (C) - Character'Pos('A') + 10;
            when others =>
               raise Constraint_Error with ''' & C & "' is not a valid hexadecimal digit";
         end case;

         C := Str (Str_Pos + 1);
         case C is
            when '0' .. '9' => Lower := Character'Pos (C) - Character'Pos('0');
            when 'a' .. 'f' => Lower := Character'Pos (C) - Character'Pos('a') + 10;
            when 'A' .. 'F' => Lower := Character'Pos (C) - Character'Pos('A') + 10;
            when others =>
               raise Constraint_Error with ''' & C & "' is not a valid hexadecimal digit";
         end case;

         Data (Data_Pos) := Shift_Left (Upper, 4) or Lower;

         Data_Pos := Data_Pos + 1;
         Str_Pos  := Str_Pos  + 2;
      end loop;
   end Parse_Hex_String;

end Hex_Strings;
