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
with Ada.Command_Line;      use Ada.Command_Line;
with Ada.Exceptions;        use Ada.Exceptions;
with Ada.Text_IO;           use Ada.Text_IO;
with Argument_Parser;
with Configurations;        use Configurations;

with Checksums;

procedure Ksum
is
begin
   Configurations.Parse_Command_Line;

   --  Don't do anything if the help message was displayed.
   if not Configurations.Help_Displayed then
      if Configurations.Check_Mode then
         Checksums.Check_Checksums;
      else
         Checksums.Print_Checksums;
      end if;
   end if;

exception
   when Error : Argument_Parser.Argument_Error =>
      Put (Standard_Error, "ksum: ");
      Put (Standard_Error, Exception_Message (Error));
      New_Line (Standard_Error);
      Set_Exit_Status (Failure);

   when Error : others =>
      Put (Standard_Error, "ksum: unhandled exception: ");
      Put (Standard_Error, Exception_Name (Error));
      New_Line (Standard_Error);
      Put_Line (Standard_Error, Exception_Message (Error));
      Set_Exit_Status (Failure);
end Ksum;
