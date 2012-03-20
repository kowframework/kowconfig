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



-------------------
-- KOW Framework --
-------------------
with KOW_Lib.Locales;		use KOW_Lib.Locales;

generic
	type Element_Type is private;
	with function From_String( Str : in String ) return Element_Type;
	with function To_String( Element : in Element_Type ) return String;
package KOW_Config.Generic_Util is



	------------------
	-- Item Methods --
	------------------

	function Value(
				Item		: in     Config_Item_Type;
				Locale_Code	: in     Locale_Code_Type
			) return Element_Type;
	-- gets the value in the given locale


	function Default_Value(
				Item		: in     Config_Item_Type
			) return Element_Type;
	-- get the default value


	
	procedure Set_Value(
				Item		: in out Config_Item_Type;
				Locale_Code	: in     Locale_Code_Type;
				Value		: in     Element_Type
			);

	procedure Set_Default_Value(
				Item		: in out Config_Item_Type;
				Value		: in     Element_Type
			);

	--------------------
	-- Config Methods --
	--------------------

	function Value(
				Config		: in     Config_File_Type;
				Key		: in     String;
				Locale_Code	: in     Locale_Code_Type
			) return Element_Type;

	function Default_Value(
				Config		: in     Config_File_Type;
				Key		: in     String
			) return Element_Type;

	
	function Value(
				Config		: in     Config_File_Type;
				Key		: in     String;
				Locale_Code	: in     Locale_Code_Type;
				Fallback	: in     Element_Type
			) return Element_Type;
	-- checks if the key is set; if true, returns it's value
	-- or else, returns Default value
	
	function Default_Value(
				Config		: in     Config_File_Type;
				Key		: in     String;
				Fallback	: in     Element_Type
			) return Element_Type;
	-- checks if the key is set; if true, returns it's default value
	-- or else, returns fallback value

end KOW_Config.Generic_Util;
