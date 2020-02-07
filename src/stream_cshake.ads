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
with Stream_XOF;
with Keccak.Generic_CSHAKE;
with Keccak.Types;
with Diagnostics;           use Diagnostics;

generic
   with package CSHAKE is new Keccak.Generic_CSHAKE (<>);
   with package SHAKE_Stream_Hashing  is new Stream_XOF (<>);
package Stream_CSHAKE
is

   procedure Hash_Stream (Stream : in out Ada.Streams.Root_Stream_Type'Class;
                          Buffer : in out Keccak.Types.Byte_Array);

   procedure Check_Stream (Stream        : in out Ada.Streams.Root_Stream_Type'Class;
                           Buffer        : in out Keccak.Types.Byte_Array;
                           Expected_Hash : in     Keccak.Types.Byte_Array;
                           Result        :    out Diagnostic);

private

   procedure Print_Output (Ctx    : in out CSHAKE.Context;
                           Buffer : in out Keccak.Types.Byte_Array);
   --  Print the output of a CSHAKE context.

   procedure Hash_Stream_CSHAKE (Stream : in out Ada.Streams.Root_Stream_Type'Class;
                                 Buffer : in out Keccak.Types.Byte_Array);
   --  Hash the specified file and print the output

   procedure Check_Stream_CSHAKE (Stream        : in out Ada.Streams.Root_Stream_Type'Class;
                                  Buffer        : in out Keccak.Types.Byte_Array;
                                  Expected_Hash : in     Keccak.Types.Byte_Array;
                                  Result        :    out Diagnostic);

end Stream_CSHAKE;
