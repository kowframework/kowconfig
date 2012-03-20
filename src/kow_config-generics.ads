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

package KOW_Config.Generics is





	generic
		type Return_Type is private;
		with function From_String( Str : in String ) return Return_Type;
	function Generic_Element(
				Config	: in     Config_File_Type;
				Key	: in     String;
				Locale	: in     Locale_Code_Type
			) return Return_Type;
	

end KOW_Config.Generics;
