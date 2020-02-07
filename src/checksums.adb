-------------------------------------------------------------------------------
--  Copyright (c) 2019, Daniel King
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
with Ada.Streams.Stream_IO.C_Streams;
with Interfaces.C_Streams;  use Interfaces.C_Streams;

with Configurations; use Configurations;

package body Checksums
is

   ---------------------
   --  Set_File_Mode  --
   ---------------------

   procedure Set_File_Mode (File : in out Ada.Streams.Stream_IO.File_Type;
                            Mode : in     Configurations.Read_Modes) is
   begin
      if Mode = Text then
         set_text_mode (fileno (Ada.Streams.Stream_IO.C_Streams.C_Stream (File)));
      else
         set_binary_mode (fileno (Ada.Streams.Stream_IO.C_Streams.C_Stream (File)));
      end if;
   end Set_File_Mode;

end Checksums;
