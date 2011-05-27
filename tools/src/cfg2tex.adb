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
with Ada.Characters.Handling;
with Ada.Command_Line;			use Ada.Command_Line;
with Ada.Directories;			use Ada.Directories;
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;
with Ada.Text_IO;			use Ada.Text_IO;

procedure cfg2tex is

	type String_Ptr is access String;
	-- we don't care about freeing memory as this process is really small

	Orig_Name : String_Ptr;
	Dest_Name : String_Ptr;
	Dest_File : File_Type;

	Config    : Config_File;

	procedure Usage is
	begin
		Set_Exit_Status( Failure );
		Put_Line( "Usage error!" );
		Put_Line( Command_name & " config_file.cfg [destination_file.tex]" );
		Put_Line( "  if destination_file.tex is ommited will use the same file name as config_file.cfg" );
	end Usage;



	function To_Command_Name( Key : in String ) return String is
		New_Name	: String( 1 .. Key'Length );
		Idx		: Positive := New_Name'First;


		type Key_Type is (
				Normal_Key,
				Upper_Key
			);
		Current : Key_Type := Normal_Key;


		function Set_Key_Type( C : in Character ) return Boolean is
		begin
			if C in 'a' .. 'z' or C in 'A' .. 'Z' then
				Current := Normal_Key;
				return true;
			else
				Current := Upper_Key;
				return false;
			end if;
		end Set_Key_Type;

		procedure Set( C : in Character ) is
		begin
			New_Name( Idx ) := C;
			Idx := Idx + 1;
		end Set;

		function Command_Name return String is
		begin
			return New_Name( 1 .. Idx - 1 );
		end Command_Name;
	begin
		for i in Key'Range loop
			case Current is
				when Normal_Key =>
					if Set_Key_Type( Key( i ) ) then
						Set( Key( i ) );
					end if;
				when Upper_Key =>
					if Set_Key_Type( Key( i ) ) then
						Set( Ada.Characters.Handling.To_Upper( Key( i ) ) );
					end if;
			end case;
		end loop;


		return Command_Name;
	end To_Command_Name;



	Path_URI : constant String := "path:";
	function To_New_Command( Key, Value : in String ) return String is
		
		function The_Value return String is
		begin
			if Value'Length > Path_URI'Length and then Value( Value'First .. Path_URI'Length + Value'First - 1 ) = Path_URI then
				return KOW_Lib.String_Util.Texify( Value( Value'First + Path_URI'Length .. Value'Last ), False );
			else
				return KOW_Lib.String_Util.Texify( Value, True );
			end if;
		end The_Value;
	begin
		return "\newcommand\" & To_Command_Name( Key ) & '{' & The_Value & '}';
	end To_New_Command;



	procedure Open_File is
	begin
		if Exists( Dest_Name.all ) then
			Open( Dest_File, Out_File, Dest_Name.all );
		else
			Create( Dest_File, Out_File, Dest_Name.all );
		end if;
	end Open_File;


	procedure Iterator( C : in KOW_Lib.UString_Hashed_Maps.Cursor ) is
		Key	: constant String := To_String( KOW_Lib.UString_Hashed_Maps.Key( C ) );
		Value	: constant String := To_String( KOW_Lib.UString_Hashed_Maps.Element( C ) );
	begin
		Put_line( Dest_File, To_New_Command( Key, Value ) );
	end Iterator;

	procedure Close_File is
	begin
		Close( Dest_File );
	end Close_File;
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

	Open_File;
	KOW_Lib.UString_Hashed_Maps.Iterate( Get_Contents_Map( Config ), Iterator'Access );
	Close_File;
end cfg2tex;
