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
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Keccak.Types;          use Keccak.Types;

package Hex_Strings
is

   procedure Print_Hex_String (Data : in Byte_Array);
   --  Print a byte array as a hexadecimal string to the standard output.

   procedure Parse_Hex_String (Str  : in     String;
                               Data :    out Byte_Array)
     with Pre => (Str'Length mod 2 = 0
                  and then Data'Length = Str'Length / 2);
   --  Parse a hexadecimal string into a byte array.
   --
   --  The input string can contain both upper-case and lower-case hexadecimal
   --  digits.

   procedure Parse_Hex_String (Str  : in     Unbounded_String;
                               Data :    out Byte_Array)
     with Pre => (Length (Str) mod 2 = 0
                  and then Data'Length = Length (Str) / 2);

end Hex_Strings;
