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
with Ada.Streams;
with Keccak.Generic_XOF;
with Keccak.Types;
with Diagnostics;        use Diagnostics;

generic
   with package XOF is new Keccak.Generic_XOF (<>);
package Stream_XOF
is

   procedure Hash_Stream (Stream : in out Ada.Streams.Root_Stream_Type'Class;
                          Buffer : in out Keccak.Types.Byte_Array);

   procedure Check_Stream (Stream        : in out Ada.Streams.Root_Stream_Type'Class;
                           Buffer        : in out Keccak.Types.Byte_Array;
                           Expected_Hash : in     Keccak.Types.Byte_Array;
                           Result        :    out Diagnostic);

private

   procedure Print_Output (Ctx    : in out XOF.Context;
                           Buffer : in out Keccak.Types.Byte_Array);

end Stream_XOF;
