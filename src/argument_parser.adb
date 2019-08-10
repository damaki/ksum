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
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

package body Argument_Parser
is

   -------------------------------
   --  Extract_Switch_Argument  --
   -------------------------------

   procedure Extract_Switch_Argument
     (Arg        : in     String;
      Switch_Str : in     String;
      Match      :    out Boolean;
      First      :    out Natural)
   is
   begin
      --  Check if the switch begins with --switch=
      if Head (Arg, Switch_Str'Length + 1) = Switch_Str & '=' then
         Match := True;
         First := Switch_Str'Length + 2;

      else
         Match := False;
         First := 0;
      end if;

   end Extract_Switch_Argument;

   --------------------
   --  Parse_Switch  --
   --------------------

   procedure Parse_Switch
     (Switches        : in     Switch_Descriptior_Array;
      Arg             : in     String;
      Current_State   : in out States;
      Current_Switch  : in out Natural;
      Short_Name_Used :    out Boolean)
   is
      Match           : Boolean;
      Value_First     : Natural;

   begin
      --  Search for the corresponding switch in our table.
      Match := False;
      for J in Switches'Range loop

      --  Check if the argument exactly matches the switch name
      --  In this case, the switch is in the format --switch value
         if (Arg = Switches (J).Short_Name
             or Arg = Switches (J).Long_Name)
         then
            Match           := True;

            Short_Name_Used := Arg = Switches (J).Short_Name;

            if Switches (J).Has_Argument then
               Current_Switch  := J;
               Current_State   := Argument;

            else
               Switches (J).Handler (To_String (Switches (J).Short_Name),
                                     To_String (Switches (J).Long_Name),
                                     "");

            end if;

            exit;

         else
            --  If it doesn't match the argument name, then check if
            --  the switch is in the format --switch=value

            Extract_Switch_Argument
              (Arg        => Arg,
               Switch_Str => To_String (Switches (J).Short_Name),
               Match      => Match,
               First      => Value_First);

            Short_Name_Used := Match;

            if not Match then
               Extract_Switch_Argument
                 (Arg        => Arg,
                  Switch_Str => To_String (Switches (J).Long_Name),
                  Match      => Match,
                  First      => Value_First);
            end if;

            if Match then
               if not Switches (J).Has_Argument then
                  raise Argument_Error with Arg & " does not take an argument";
               end if;

               if Value_First in Arg'Range then
                  Switches (J).Handler (To_String (Switches (J).Short_Name),
                                        To_String (Switches (J).Long_Name),
                                        Arg (Value_First .. Arg'Last));

               elsif Short_Name_Used then
                  raise Argument_Error with
                    "missing argument for " & To_String (Switches (J).Short_Name);
               else
                  raise Argument_Error with
                    "missing argument for " & To_String (Switches (J).Long_Name);
               end if;

               exit;
            end if;

         end if;
      end loop;

      if not Match then
         raise Argument_Error with "unknown switch " & Arg;
      end if;

   end Parse_Switch;

   --------------------------
   --  Parse_Command_Line  --
   --------------------------

   procedure Parse_Command_Line
     (Switches : in Switch_Descriptior_Array;
      Args     : in out Unbounded_Strings_Vectors.Vector)
   is
      NArgs : constant Natural := Ada.Command_Line.Argument_Count;

      Current_State   : States  := Switch;
      Current_Switch  : Natural := 0;
      Short_Name_Used : Boolean := False;

   begin
      for I in 1 .. NArgs loop
         declare
            Arg : String renames Argument (I);
         begin

            --  Ignore any empty parameters
            if Arg'Length > 0 then

               if Current_State = Switch then

                  if Arg (Arg'First) = '-' then
                     Parse_Switch (Switches        => Switches,
                                   Arg             => Arg,
                                   Current_State   => Current_State,
                                   Current_Switch  => Current_Switch,
                                   Short_Name_Used => Short_Name_Used);
                  else
                     Args.Append (To_Unbounded_String (Arg));
                  end if;
               else
                  Current_State := Switch;

                  Switches (Current_Switch).Handler
                    (Short_Name => To_String (Switches (Current_Switch).Short_Name),
                     Long_Name  => To_String (Switches (Current_Switch).Short_Name),
                     Arg        => Arg);
               end if;
            end if;
         end;
      end loop;

      if Current_State = Argument then
         if Short_Name_Used then
            raise Argument_Error
              with "missing argument for " & To_String (Switches (Current_Switch).Short_Name);
         else
            raise Argument_Error with
              "missing argument for " & To_String (Switches (Current_Switch).Long_Name);
         end if;
      end if;

   end Parse_Command_Line;

end Argument_Parser;
