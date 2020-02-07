-------------------------------------------------------------------------------
--  Copyright (c) 2020, Daniel King
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
with Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Keccak.Types;

with Diagnostics; use Diagnostics;

package Checksums.Verification
is

   procedure Check_Checksums;
   --  Open the checksum file(s) specified on the command line, and check the
   --  list of checksums contained in those files.
   --
   --  For each file specified on the command line, this function will
   --  open that file and parse each line. Each line is expected to be
   --  in the same format as Checksums.Generation.Print_Checksums, i.e.:
   --
   --  0123456789abcdef *filename
   --
   --  Lines may be terminated by LF or CRLF. In case of CRLF, all CR characters
   --  preceding the LF are stripped.
   --
   --  For each line in the checksum file, the file is opened, hashed, and
   --  its checksum compared with the checksum in the file. "OK" is printed
   --  if the checksums are the same, otherwise error information is printed.

private

   procedure Check_Checksums_File (File          : in     Ada.Text_IO.File_Type;
                                   Buffer        : in out Keccak.Types.Byte_Array;
                                   Checked_Files : in out Natural;
                                   Format_Errors : in out Natural;
                                   Failed_Files  : in out Natural;
                                   IO_Errors     : in out Natural);
   --  Parse each line in the specified file and verify the checksums.
   --
   --  File is the file that contains the checksum lines to parse.
   --  Lines are expected in the format:
   --  <checksum> <space> <space|asteisk> <file name>
   --
   --  For example:
   --  e7d29df82428e8de6273a5a95f26a526ca9cdf7a2428e06e42864e84b3e37fda *filename.txt
   --
   --  Checked_Files is incremented for each line that was successfully parsed.
   --
   --  Format_Errors is incremented for each line that is incorrectly formatted.
   --
   --  Failed_Files is incremented for each file whose checksum is incorrect.
   --
   --  IO_Errors is incremented for each file that could not be opened or read.

   procedure Check_Stream (Stream        : in out Ada.Streams.Root_Stream_Type'Class;
                           Buffer        : in out Keccak.Types.Byte_Array;
                           Expected_Hash : in     Unbounded_String;
                           Result        :    out Diagnostic);
   --  Generate the checksum of the specified Stream and compare it against
   --  the Expected_Hash. Result is No_Error if the checksums match, otherwise
   --  it is set to the appropriate error code.

   procedure Format_Warning (File_Name   : in String;
                             Line_Number : in Positive);
   --  Prints a message to the standard error in the format:
   --  ksum: filename:line: improperly formatted checksum line

   procedure Check_Success (File_Name : in Unbounded_String);
   --  Prints a message to the standard output in the form:
   --  filename: OK

   function Is_Hexadecimal_Character (C : in Character) return Boolean
   is (C in '0' .. '9' | 'a' .. 'f' | 'A' .. 'F')
     with Inline;

end Checksums.Verification;
