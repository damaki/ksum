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
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Configurations;
with Hex_Strings;              use Hex_Strings;
with Stream_Byte_Arrays;       use Stream_Byte_Arrays;

package body Stream_CSHAKE
is

   --------------------
   --  Print_Output  --
   --------------------

   procedure Print_Output (Ctx    : in out CSHAKE.Context;
                           Buffer : in out Keccak.Types.Byte_Array)
   is
      Remaining : Long_Long_Integer;

   begin
      Remaining := Configurations.Output_Length;

      while Remaining >= Long_Long_Integer (Buffer'Length) loop
         CSHAKE.Extract (Ctx, Buffer);
         Print_Hex_String (Buffer);

         Remaining := Remaining - Long_Long_Integer (Buffer'Length);
      end loop;

      if Remaining > 0 then
         CSHAKE.Extract (Ctx, Buffer (Buffer'First .. Buffer'First + Natural (Remaining - 1)));
         Print_Hex_String (Buffer (Buffer'First .. Buffer'First + Natural (Remaining - 1)));
      end if;
   end Print_Output;

   --------------------------
   --  Hash_Stream_CSHAKE  --
   --------------------------

   procedure Hash_Stream_CSHAKE (Stream : in out Ada.Streams.Root_Stream_Type'Class;
                                 Buffer : in out Keccak.Types.Byte_Array)
   is
      Ctx    : CSHAKE.Context;
      Length : Natural;

   begin
      CSHAKE.Init (Ctx           => Ctx,
                   Function_Name => To_String (Configurations.Function_Name),
                   Customization => To_String (Configurations.Customization));

      loop
         Read_Byte_Array (Stream, Buffer, Length);

         exit when Length = 0;

         CSHAKE.Update (Ctx, Buffer (Buffer'First .. Buffer'First + (Length - 1)));
      end loop;

      Print_Output (Ctx, Buffer);
   end Hash_Stream_CSHAKE;

   ---------------------------
   --  Check_Stream_CSHAKE  --
   ---------------------------

   procedure Check_Stream_CSHAKE (Stream        : in out Ada.Streams.Root_Stream_Type'Class;
                                  Buffer        : in out Keccak.Types.Byte_Array;
                                  Expected_Hash : in     Keccak.Types.Byte_Array;
                                  Result        :    out Diagnostic)
   is
      use type Keccak.Types.Byte_Array;

      Ctx    : CSHAKE.Context;
      Length : Natural;

      Offset    : Natural := 0;
      Remaining : Natural := Expected_Hash'Length;

      I : Keccak.Types.Index_Number;
      J : Keccak.Types.Index_Number;

   begin
      CSHAKE.Init (Ctx           => Ctx,
                   Function_Name => To_String (Configurations.Function_Name),
                   Customization => To_String (Configurations.Customization));

      loop
         Read_Byte_Array (Stream, Buffer, Length);

         exit when Length = 0;

         CSHAKE.Update (Ctx, Buffer (Buffer'First .. Buffer'First + (Length - 1)));
      end loop;

      Result := No_Error; --  Unless proven otherwise.

      --  Verify the output in chunks of size: Buffer'Length
      while Remaining >= Buffer'Length loop

         CSHAKE.Extract (Ctx, Buffer);

         I := Expected_Hash'First + Offset;
         if Buffer /= Expected_Hash (I .. I + Buffer'Length - 1) then
            Result := Checksum_Error;
         end if;

         Offset    := Offset    + Buffer'Length;
         Remaining := Remaining - Buffer'Length;
      end loop;

      --  Verify last chunk
      if Remaining > 0 then
         CSHAKE.Extract (Ctx, Buffer (Buffer'First .. Buffer'First + Remaining - 1));

         I := Buffer'First;
         J := Expected_Hash'First + Offset;
         if Buffer (I .. I + Remaining - 1) /= Expected_Hash (J .. J + Remaining - 1) then
            Result := Checksum_Error;
         end if;
      end if;
   end Check_Stream_CSHAKE;

   -------------------
   --  Hash_Stream  --
   -------------------

   procedure Hash_Stream (Stream : in out Ada.Streams.Root_Stream_Type'Class;
                          Buffer : in out Keccak.Types.Byte_Array)
   is
   begin
      if (Configurations.Customization = Null_Unbounded_String
          and Configurations.Function_Name = Null_Unbounded_String)
      then
         --  In the case where both the customization and function name strings
         --  are the empty strings, cSHAKE is equivalent to SHAKE.
         --  See Section 3.3 of NIST SP 800-185 for details.
         SHAKE_Stream_Hashing.Hash_Stream (Stream, Buffer);

      else
         Hash_Stream_CSHAKE (Stream, Buffer);
      end if;
   end Hash_Stream;

   --------------------
   --  Check_Stream  --
   --------------------

   procedure Check_Stream (Stream        : in out Ada.Streams.Root_Stream_Type'Class;
                           Buffer        : in out Keccak.Types.Byte_Array;
                           Expected_Hash : in     Keccak.Types.Byte_Array;
                           Result        :    out Diagnostic)
   is
   begin
      if (Configurations.Customization = Null_Unbounded_String
          and Configurations.Function_Name = Null_Unbounded_String)
      then
         --  In the case where both the customization and function name strings
         --  are the empty strings, cSHAKE is equivalent to SHAKE.
         --  See Section 3.3 of NIST SP 800-185 for details.
         SHAKE_Stream_Hashing.Check_Stream (Stream, Buffer, Expected_Hash, Result);

      else
         Check_Stream_CSHAKE (Stream, Buffer, Expected_Hash, Result);
      end if;
   end Check_Stream;

end Stream_CSHAKE;
