------------------------------------------------------------------------------
--                                                                          --
--                         KOW Framework :: Config                          --
--                                                                          --
--                              KOW Framework                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--               Copyright (C) 2007-2011, KOW Framework Project             --
--                                                                          --
--                                                                          --
-- KOW Config is free software; you can redistribute it  and/or modify it   --
-- under terms of the  GNU General Public License as published  by the Free --
-- Software  Foundation;  either version 2,  or (at your option) any later  --
-- version. KOW Config is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY;  without even the  implied warranty of MERCHAN---
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public--
-- License for  more details.  You should have  received  a copy of the GNU --
-- General Public License distributed with KOW Config; see file COPYING.    --
-- If not, write to  the Free Software Foundation,  59 Temple Place - Suite --
-- 330,  Boston,  MA 02111-1307, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- Some utilities for KOW_Config                                            --
------------------------------------------------------------------------------


--------------
-- Ada 2005 --
--------------
with Ada.Strings.Unbounded;

-------------------
-- KOW Framework --
-------------------
with KOW_Config.Generic_Util;
with KOW_Lib.Json;
with KOW_Lib.Locales;

package KOW_Config.Util is
	

	--------------------
	-- Integer Values --
	--------------------

	package Integers is new Generic_Util(
						Element_Type	=> Integer,
						From_String	=> Integer'Value,
						To_String	=> Integer'Image
					);

	--------------------
	-- Boolean Values --
	--------------------

	package Booleans is new Generic_Util(
						Element_Type	=> Boolean,
						From_String	=> Boolean'Value,
						To_String	=> Boolean'Image
					);

	----------------------
	-- Unbounded String --
	----------------------

	package Unbounded_Strings is new Generic_Util(
						Element_Type	=> Ada.Strings.Unbounded.Unbounded_String,
						From_String	=> Ada.Strings.Unbounded.To_Unbounded_String,
						To_String	=> Ada.Strings.Unbounded.To_String
					);
	-- NOTE: it has been a design decision not to offer direct access to unbounded string values
	-- in the main kowconfig package even though they are used internally
	--
	-- this will leave the code more organized an with a cleaner API, easier to understand and maintain.
	-- 
	-- we do intend, however, to change the implementation to some other (possibly more efficient) data type

	---------------------
	-- Json Data Types --
	---------------------

	package Json_Objects is new Generic_Util(
						Element_Type	=> KOW_Lib.Json.Object_Type,
						From_String	=> KOW_Lib.Json.From_Json,
						To_String	=> KOW_Lib.Json.To_Json
					);

	package Json_Arrays is new Generic_Util(
						Element_Type	=> KOW_Lib.Json.Array_Type,
						From_String	=> KOW_Lib.Json.From_Json,
						To_String	=> KOW_Lib.Json.To_Json
					);
	package Json_Data is new Generic_Util(
						Element_Type	=> KOW_Lib.Json.Json_Data_Type,
						From_String	=> KOW_Lib.Json.From_Json,
						To_String	=> KOW_Lib.Json.To_Json
					);

	-----------------
	-- Locale Code --
	-----------------

	package Locale_Code is new Generic_Util(
						Element_Type	=> KOW_Lib.Locales.Locale_Code_Type,
						From_String	=> KOW_Lib.Locales.From_String,
						To_String	=> KOW_Lib.Locales.To_String
					);
end KOW_Config.Util;
