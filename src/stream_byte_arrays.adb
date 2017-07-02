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
with Ada.Unchecked_Conversion;
with System;

package body Stream_Byte_Arrays
is

   procedure Read_Byte_Array
     (Stream : not null access Root_Stream_Type'Class;
      Item   : in out Keccak.Types.Byte_Array;
      Length :    out Natural)
   is
      use type Ada.Streams.Stream_Element_Offset;

      Item_Size : constant Stream_Element_Offset :=
        Item'Size / Ada.Streams.Stream_Element'Size;

      type SEA_Access is access all Stream_Element_Array (0 .. Item_Size - 1);

      function To_SEA_Access is new Ada.Unchecked_Conversion
        (Source => System.Address,
         Target => SEA_Access);

      Item_Access : constant SEA_Access := To_SEA_Access(Item'Address);

      Last : Stream_Element_Offset;
   begin
      Ada.Streams.Read(Stream.all, Item_Access.all, Last => Last);

      Length := Natural ((Last - Item_Access.all'First) + 1);
   end Read_Byte_Array;

end Stream_Byte_Arrays;
