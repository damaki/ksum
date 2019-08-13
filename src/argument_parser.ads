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
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;

package Argument_Parser
is

   package Unbounded_Strings_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Unbounded_String);

   type Argument_Handler is
     access not null procedure (Short_Name, Long_Name, Arg : in String);

   type Group_Type is
     (Main_Switches,
      Check_Switches,
      Customization,
      Algorithms);

   type Switch_Descriptor is record
      Short_Name   : Unbounded_String;
      Long_Name    : Unbounded_String;
      Description  : Unbounded_String;
      Group        : Group_Type;
      Has_Argument : Boolean;
      Handler      : Argument_Handler;
   end record;

   type Switch_Descriptior_Array is
     array (Natural range <>)
     of Switch_Descriptor;

   procedure Parse_Command_Line
     (Switches : in Switch_Descriptior_Array;
      Args     : in out Unbounded_Strings_Vectors.Vector);

   Argument_Error : exception;

private

   type States is (Switch, Argument);

   procedure Extract_Switch_Argument
     (Arg        : in     String;
      Switch_Str : in     String;
      Match      :    out Boolean;
      First      :    out Natural);

   procedure Parse_Switch
     (Switches        : in     Switch_Descriptior_Array;
      Arg             : in     String;
      Current_State   : in out States;
      Current_Switch  : in out Natural;
      Short_Name_Used :    out Boolean);

end Argument_Parser;
