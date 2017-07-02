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
with Ada.Streams;  use Ada.Streams;
with Keccak.Types;

package Stream_Byte_Arrays
is

   procedure Read_Byte_Array
      (Stream : not null access Root_Stream_Type'Class;
       Item   : in out Keccak.Types.Byte_Array;
       Length :    out Natural);
   --  Efficiently read bytes from a stream.
   --
   --  This procedure will read as many bytes as possible to fill the @Item@
   --  buffer. The @Length@ parameter is set to the number of bytes that were
   --  read.

end Stream_Byte_Arrays;
