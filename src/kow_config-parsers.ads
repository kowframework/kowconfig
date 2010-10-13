------------------------------------------------------------------------------
--                                                                          --
--                          Ada Works :: Library                            --
--                                                                          --
--                                Ada Works                                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--               Copyright (C) 2007-2009, Ada Works Project                 --
--                                                                          --
--                                                                          --
-- KOW_Config is free library; you can redistribute it and/or modify it under--
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. KOW_Lib is distributed in the hope that it will be useful, but WITH---
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with KOW_Lib; see file COPYING. If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- This is the KOW_Lib.Calendar package                                      --
--                                                                          --
-- parser for plain text/properties file
------------------------------------------------------------------------------


with Ada.Strings.Unbounded;	use Ada.Strings.Unbounded;

with Ada.Text_IO;		use Ada.Text_IO;

package KOW_Config.Parsers is

	type Parser is private;


	procedure Prepare(	P: in out Parser;
				File_Name: in String );
	-- prepare the parser to parse the file with the
	-- absolute path File_Name.
	-- read the 1st field

	procedure Finish( P: in out Parser );
	-- close the file and do whatever it's needed to finish it.

	procedure Next( P: in out Parser );
	-- move the parser to the next field, if it exists
	-- if not prepare the parser to return CONSTRAINT_ERROR
	-- everytime Key and Value are called


	function Key( P: in Parser ) return Unbounded_String;
	-- return the key of the current field
	-- raise CONSTRAINT_ERROR if there is nothing else to read

	function Element( P: in Parser ) return Unbounded_String;
	-- return the value of the current field
	-- raise CONSTRAINT_ERROR if there is nothing else to read

	
	function Get_File_Name( P: in Parser; Original: in String ) return String;
	-- returns the filename Original with expected extension
	-- ie, Original & ".cfg" in case of Text Parser


	function File_To_Config_Name( P: in Parser; File_Name: in String ) return String;

	procedure Save( P : in Parser;
			Config: in KOW_Config.Config_File; 
			File : in File_Type );
	-- save config file

	private

	type String_Access is access String;

	type File_Blocks is ( B_NONE, B_SECTION, B_KEY, B_ELEMENT );
	-- represents the known structures in the file

	type Parser is record
		First_Key_Value_Pair: Boolean := True;
		-- controls if it's the 1st pair to be read
		Current_Block: File_Blocks := B_NONE;
		C: Character;
		File: File_Type;
		File_Name: String_Access;
		Current_Key, Current_Element, Current_Section: Unbounded_String;
	end record;

end KOW_Config.Parsers;
	
