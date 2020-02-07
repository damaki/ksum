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
package Checksums.Generation
is

   procedure Print_Checksums;
   --  Compute the checksum over all files listed on the command line, and
   --  print the checksum to the standard output along with the file name.
   --
   --  Checksums are printed in the following form (one line per file):
   --
   --  0123456789abcdef *filename
   --
   --  The filename is preceded by either '*' if the file was read in
   --  binary mode, or ' ' if the file was read in text mode.

end Checksums.Generation;
