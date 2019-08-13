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
with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Text_IO.Text_Streams; use Ada.Text_IO.Text_Streams;
with Configurations;
with Hex_Strings;              use Hex_Strings;
with Stream_Byte_Arrays;       use Stream_Byte_Arrays;

package body File_CSHAKE
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

   ------------------------
   --  Hash_File_CSHAKE  --
   ------------------------

   procedure Hash_File_CSHAKE (File   : in     Ada.Text_IO.File_Type;
                               Buffer : in out Keccak.Types.Byte_Array)
   is
      Ctx    : CSHAKE.Context;
      Length : Natural;

   begin
      CSHAKE.Init (Ctx           => Ctx,
                   Function_Name => To_String (Configurations.Function_Name),
                   Customization => To_String (Configurations.Customization));

      while not End_Of_File (File) loop
         Read_Byte_Array (Stream (File), Buffer, Length);

         if Length = 0 then
            raise Program_Error with "Could not read from stream";
         end if;

         CSHAKE.Update (Ctx, Buffer (Buffer'First .. Buffer'First + (Length - 1)));
      end loop;

      Print_Output (Ctx, Buffer);
   end Hash_File_CSHAKE;

   -------------------------
   --  Check_File_CSHAKE  --
   -------------------------

   procedure Check_File_CSHAKE (File          : in     Ada.Text_IO.File_Type;
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

      while not End_Of_File (File) loop
         Read_Byte_Array (Stream (File), Buffer, Length);

         if Length = 0 then
            raise Program_Error with "Could not read from stream";
         end if;

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
   end Check_File_CSHAKE;

   -----------------
   --  Hash_File  --
   -----------------

   procedure Hash_File (File   : in     Ada.Text_IO.File_Type;
                        Buffer : in out Keccak.Types.Byte_Array)
   is
   begin
      if (Configurations.Customization = Null_Unbounded_String
          and Configurations.Function_Name = Null_Unbounded_String)
      then
         --  In the case where both the customization and function name strings
         --  are the empty strings, cSHAKE is equivalent to SHAKE.
         --  See Section 3.3 of NIST SP 800-185 for details.
         SHAKE_File_Hashing.Hash_File (File, Buffer);

      else
         Hash_File_CSHAKE (File, Buffer);
      end if;
   end Hash_File;

   ------------------
   --  Check_File  --
   ------------------

   procedure Check_File (File          : in     Ada.Text_IO.File_Type;
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
         SHAKE_File_Hashing.Check_File (File, Buffer, Expected_Hash, Result);

      else
         Check_File_CSHAKE (File, Buffer, Expected_Hash, Result);
      end if;
   end Check_File;

end File_CSHAKE;
