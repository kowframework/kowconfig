------------------------------------------------------------------------------
--                                                                          --
--                         KOW Framework :: Config                          --
--                                                                          --
--                              KOW Framework                               --
--                                                                          --
--                                 B o d y                                  --
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
-- This program converts a .cfg file into a .tex file using the following   --
-- rules:                                                                   --
--                                                                          --
--    1. each parameter becomes a \newcommand                               --
--    2. some_parameter becomes someParameter                               --
--    3. some_key.some_parameter becomes someKeySomeParameter               --
--                                                                          --
------------------------------------------------------------------------------


-------------------
-- KOW Framework --
-------------------
with KOW_Config;			use KOW_Config;
with KOW_Lib.File_System;		use KOW_Lib.File_System;
with KOW_Lib.String_Util;
with KOW_Lib.Ustring_Hashed_Maps;


--------------
-- Ada 2005 --
--------------
with Ada.Command_Line;			use Ada.Command_Line;
with Ada.Directories;			use Ada.Directories;
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;
with Ada.Text_IO;			use Ada.Text_IO;

procedure cfg2tex is

	type String_Ptr is access String;
	-- we don't care about freeing memory as this process is really small

	Orig_Name : String_Ptr;
	Dest_Name : String_Ptr;

	Config    : Config_File;

	procedure Usage is
	begin
		Set_Exit_Status( Failure );
		Put_Line( "Usage error!" );
		Put_Line( Command_name & " config_file.cfg [destination_file.tex]" );
		Put_Line( "  if destination_file.tex is ommited will use the same file name as config_file.cfg" );
	end Usage;


	function To_Command_Name( Key : in String ) return String is
	begin
		return "FIXME" & Key;
	end To_Command_Name;


	function To_New_Command( Key, Value : in String ) return String is
	begin
		return "\newcommand\" & To_Command_Name( Key ) & '{' & KOW_Lib.String_Util.Scriptify( Value ) & '}';
	end To_New_Command;

	procedure Iterator( C : in KOW_Lib.UString_Hashed_Maps.Cursor ) is
		Key	: constant String := To_String( KOW_Lib.UString_Hashed_Maps.Key( C ) );
		Value	: constant String := To_String( KOW_Lib.UString_Hashed_Maps.Element( C ) );
	begin
		Put_line( To_New_Command( Key, Value ) );
	end Iterator;
begin
	if Argument_Count /= 1 and then Argument_Count /= 2 then
		Usage;
		return;
	end if;

	Orig_Name := new String'( Argument( 1 ) );

	if Argument_Count = 2 then
		Dest_Name := new String'( Argument( 2 ) );
	else
		declare
			Folder		: String := Containing_Directory( Orig_Name.all );
			File_name	: String := Base_Name( Orig_Name.all );
		begin
			Dest_name := New String'( Folder / File_Name & ".tex" );
		end;
	end if;


	Add_Config_Path( "." );
	-- if we don't do this it fails miserably
	Config := New_Config_File( Orig_Name.all, true );

	KOW_Lib.UString_Hashed_Maps.Iterate( Get_Contents_Map( Config ), Iterator'Access );
end cfg2tex;
