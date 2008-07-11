------------------------------------------------------------------------------
--                                                                          --
--                          Ada Works :: Library                            --
--                                                                          --
--                                Ada Works                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2007-2008, Ydea Desenv. de Softwares Ltda          --
--                                                                          --
--                                                                          --
-- Aw_Lib is free library;  you can redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. Aw_Lib is distributed in the hope that it will be useful, but WITH---
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with Aw_Lib; see file COPYING. If not, write --
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
-- This is the Aw_Config package                                            --
--                                                                          --
-- This is the main AdaConfig package.                                      --
-- Here you'll find the types you should use in your application and all    -- 
-- visible procedures and functions.                                        --
------------------------------------------------------------------------------
with ada.text_io;

with Ada.Containers.Ordered_Maps;
with Ada.Directories;
with Ada.Environment_Variables;
with Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;

with Aw_Lib.File_System;
with Aw_Lib.String_Util;
with Aw_Lib.UString_Vectors;
with Aw_Lib.UString_Ordered_Maps;

package body Aw_Config is


	procedure pl(str: in string) renames ada.text_io.put_line;
	------------------------
	-- Exception Handling --
	------------------------

	procedure Raise_Syntax_Error(	File_Name: in String;
					Line_Number: in Natural := 0;
					Column_Number: in Natural := 0;
					Message: in String ) is
		-- Raise SYNTAX_ERROR exception with message composed by:
		-- "["& File_Name &":"&Line_Number & "] " & Message
		msg: Unbounded_String := To_Unbounded_String( "[" & File_Name );
	begin
		if Line_Number /= 0 then
			Append( msg, ":" & Integer'Image( Line_Number ) );
			if Column_Number /= 0 then
				Append( msg, ":" & Integer'Image( Column_Number ) );
			end if;
		end if;
		Append( msg, "] " & Message );
		Ada.Exceptions.Raise_Exception( SYNTAX_ERROR'Identity, To_String( msg ) );
	end Raise_Syntax_Error;


	------------------------------------
	-- Methods for Project Management --
	------------------------------------

	procedure Set_Project_Name( Str: in String ) is
		-- Set the project name so AdaConfig can find for 
		-- config files search path
		-- This will reset the config path
	begin
		Project_Name := To_Unbounded_String( Str );
		Reset_Config_Path;
	end Set_Project_Name;

	procedure Set_Project_Name( Str: in Unbounded_String ) is
		-- Set the project name so AdaConfig can find for 
		-- config files search path
		-- This will reset the config path
	begin
		Project_Name := Str;
		Reset_Config_Path;
	end Set_Project_Name;

	function Get_Project_Name return String is
		-- return the current project name
	begin
		return To_String( Project_Name );
	end Get_Project_Name;

	function Get_Project_Name return Unbounded_String is
		-- return the current project name
	begin
		return Project_Name;
	end Get_Project_Name;

	procedure Reset_Config_Path is
		-- reset the config path using the environment variable:
		-- [PROJECT_NAME]_CONFIG_PATH
		use Aw_Lib.String_Util;
	begin
		Config_Path := Aw_Lib.File_System.To_Vector (
			Ada.Environment_Variables.Value(
				Str_Replace( ' ', '_', To_String( Project_Name ) ) &
				"_CONFIG_PATH" 	)
				);
	exception
		when  CONSTRAINT_ERROR =>
			Config_Path := Aw_Lib.UString_Vectors.Empty_Vector;
	end Reset_Config_Path;


	procedure Add_Config_Path( Str: in String ) is
		-- add Str to config path.
	begin
		Aw_Lib.UString_Vectors.Append( Config_Path, To_Unbounded_String( Str ) );
	end Add_Config_Path;

	procedure Add_Config_Path( Str: in Unbounded_String ) is
		-- add Str to config path.
	begin
		Aw_Lib.UString_Vectors.Append( Config_Path, Str );
	end Add_Config_Path;

	function Get_Config_Path return Aw_Lib.UString_Vectors.Vector is
		-- return the current config path
	begin
		return Config_Path;
	end Get_Config_Path;


	-------------------
	-- File handling --
	-------------------


	function Scan_Relative_Path( Relative_Path : in String; P: in Parser_Access ) return AW_Lib.UString_Ordered_Maps.Map is
		-- Scan a given relative path within the Config_Path for the project.
		-- Return all the config files found without the extension.
		
		use Ada.Directories;
		My_Map: Aw_Lib.UString_Ordered_Maps.Map;


		-- the 1 is for the directory separator
		Current_Root_Path_Length: Positive;
		Current_Root_With_Relative_Path_Length: Positive;

		function Get_Config_Name( Name: in String ) return Unbounded_String is
			First: Integer := Name'First + Current_Root_With_Relative_Path_Length;
		begin
			return To_Unbounded_String(
				Name( First .. Name'Last )
				);
		end Get_Config_Name;

		function Get_Relative_Path( Absolute_Path: in String ) return Unbounded_String is
			New_First : Integer := Absolute_Path'First + Current_Root_Path_Length + 1;
			Last      : Integer := Absolute_Path'Last;
		begin
			return To_Unbounded_String(
				Absolute_Path( New_First .. Last )
				);
		end Get_Relative_Path;

		procedure Check_File( Path: in String ) is
		begin
			declare
				Name: String := File_To_Config_Name(
					P.all,
					Path );
				Config_Name	: Unbounded_String := Get_Config_Name( Name );
				Relative_Path	: Unbounded_String := Get_Relative_Path( Name );
			begin
				Aw_Lib.UString_Ordered_Maps.Include(
					My_Map,
					Config_Name,
					Relative_Path
					);
			end;
		exception
			when NOT_MY_FILE => null;
		end Check_File;


	
		Filter : Filter_Type := (	Directory	=> true,
						Ordinary_File	=> true,
						Special_File	=> false );

		procedure Process_Search( Directory_Entry : Directory_Entry_Type );

		procedure Search( Directory: in String ) is
		begin
			Search(	Directory	=> Directory,
				Pattern		=> "*",
				Filter		=> Filter,
				Process		=> Process_Search'Access );
		exception
			when ADA.IO_EXCEPTIONS.NAME_ERROR => null;
		end Search;


		To_Scan_Path : Aw_Lib.UString_Vectors.Vector;

		procedure Process_Search( Directory_Entry : Directory_Entry_Type ) is
		begin
			if Kind( Directory_Entry ) = Ordinary_File then
				-- if it's a regular file, check and tries to append it to the result
				Check_File( Full_Name( Directory_Entry ) );
			elsif Kind( Directory_Entry ) = Directory then
				-- append to a to-scan line
				if	Simple_Name( Directory_Entry ) /= "."
					AND 
					Simple_Name( Directory_Entry ) /= ".."
					then
					Aw_Lib.UString_Vectors.Append( To_Scan_Path, To_Unbounded_String( Full_Name( Directory_Entry ) ) );
				end if;
			end if;
		end Process_Search;

		procedure Path_Iterator( C : Aw_Lib.UString_Vectors.Cursor ) is
			use Aw_Lib.UString_Vectors;
			use Aw_Lib.File_System;
			use Ada.Directories;

			Current_Root_Path : String := Full_Name( To_String( Element( C ) ) );
			Current_Root_With_Relative_Path: String :=  Full_Name( 
				Current_Root_Path	&
				Separator		&
				Relative_Path );
		begin
			Current_Root_Path_Length := Current_Root_Path'Length;
			Current_Root_With_Relative_Path_Length := Current_Root_With_Relative_Path'Length;
			
			Append(
				To_Scan_Path,
				To_Unbounded_String(
					 Current_Root_Path & Separator & Relative_Path
					)
				);

			while not Is_Empty( To_Scan_Path ) loop
				Search( To_String( First_Element( To_Scan_Path ) ) );
				Delete_First( To_Scan_Path );
			end loop;

		end Path_Iterator;

		use Aw_Lib.UString_Vectors;
		use Ada.Containers;

	begin

		Iterate( 	Config_Path,
				Path_Iterator'Access );
	
		return My_Map;
	end Scan_Relative_Path;



	procedure Generic_Iterate(	Map	: in Aw_Lib.UString_Ordered_Maps.Map;
					P	: in Parser_Access 
				) is
		-- Iterate over the elements returned by Scan_Relative_Path.
		-- The parameters are the initialized config file and the config name within the relative_path parameter


		use Aw_Lib.UString_Ordered_Maps;
		
		procedure Inner_Iterator( C: in Cursor ) is
			Config: Config_File := New_Config_File( 
				To_String( Element( C ) ),
				P );

		begin
			Path_Iterator(
				Name	=> To_String( Key( C ) ),
				Config	=> Config
				);
		end Inner_Iterator;
	begin
		Iterate( Map, Inner_Iterator'Access );
	end Generic_Iterate;



	function New_Config_File( N: in String; P: in Parser_Access; Is_Complete_Path: Boolean := False ) return Config_File is
		-- opens a new config file that will be handled by parser P
		-- read it's contents and return an object representing it.
		-- the file is closed right after it've been read


		-- AW_Lib packages
		use Aw_Lib.File_System;
		use Aw_Lib.UString_Vectors;
		

		-- this is used to check when the file has been found
		
		F: Config_File;
		-- this is the object that is used as return value

		FOUND_IT: Boolean := FALSE;
		-- controls if it did find the file already



		File_Name : Unbounded_String;

		-- Iteractors:
		procedure Path_Iterator( C: Aw_Lib.UString_Vectors.Cursor ) is
		begin
			if FOUND_IT then
				return;
			end if;

			F.File_Name := Element( C ) & Separator;
			F.File_Name := F.File_Name & File_Name;

			declare
				use Ada.Directories;
				S_File_Name: String := To_String( F.File_Name );
			begin
				if Exists( S_File_Name ) AND Kind( S_File_Name ) = Ordinary_File then
					F.My_Parser := P;
					FOUND_IT := TRUE;
					-- tell the main unit that we've found a winner! :)
				end if;
			exception
				when Ada.IO_Exceptions.Name_Error => null;
			end;
		end Path_Iterator;

	begin
		if Is_Empty( Config_Path ) then
			raise NO_CONFIG_PATH;
		end if;
		
		if Is_Complete_Path then
			File_Name := To_Unbounded_String( N );
		else
			File_Name := To_Unbounded_String( Get_File_Name( P.all, N ) );
		end if;
		
		Iterate( Config_Path, Path_Iterator'Access );
		-- iterate over the config path looking for the file

		if not FOUND_IT then
			Ada.Exceptions.Raise_Exception( FILE_NOT_FOUND'Identity, N );
		end if;
		Reload_Config( F );
		return F;
	end New_Config_File;

	procedure Reload_Config( F: in out Config_File ) is
		-- reloads the configuration from the flie. :D
		use Aw_Lib.UString_Ordered_Maps;
	begin
		F.Contents := Empty_Map;

		Prepare( F.My_Parser.All, To_String( F.File_Name ) );

		loop
			Include(	F.Contents,
					Key( F.My_Parser.All ),
					Element( F.My_Parser.All )
				);
			Next( F.My_Parser.All );
		end loop;

	exception
		when CONSTRAINT_ERROR =>
			-- the file has reached the end
			Finish( F.My_Parser.all );
	end Reload_Config;


	----------------------------------
	-- Methods for Config Iteration --
	----------------------------------

	procedure Set_Section( F: in out Config_File; S: in String ) is
		-- set the current section of the config file.
	begin
		F.Current_Section := To_Unbounded_String( S );
	end Set_Section;

	procedure Set_Section( F: in out Config_File; S: in Unbounded_String ) is
		-- set the current section of the config file.
	begin
		F.Current_Section := S;
	end Set_Section;

	function Get_Section( F: in Config_File ) return String is
		-- return the current section or "" if there is no section active
	begin
		return To_String( F.Current_Section );
	end Get_Section;

	function Get_Section( F: in Config_File ) return Unbounded_String is
		-- return the current section or "" if there is no section active
	begin
		return F.Current_Section;
	end Get_Section;


	function Element( F: Config_File; Key: Unbounded_String ) return Unbounded_String is
		-- return the value of element inside the current section with
		-- key Key
		-- if no current section active, return propertie relative
		-- to root section; ie expects Key to be of the form "sectionName.key"
		use Aw_Lib.UString_Ordered_Maps;
	begin
		if F.Current_Section = "" then
			return Element( F.Contents, Key );
		end if;
		return Element(	F.Contents,
				F.Current_Section & '.' & Key );
	end Element;

	function Element( F: Config_File; Key: String ) return Unbounded_String is
		-- return the value of element inside the current section with
		-- key Key
		-- if no current section active, return propertie relative
		-- to root section; ie expects Key to be of the form "sectionName.key"
	begin
		return Element( F, To_Unbounded_String( Key ) );
	end Element;



	function Extract( F: Config_File; Prefix: Unbounded_String ) return Config_File is
		-- return a new config file with the data prefixed by the give prefix
	begin
		return Extract( F, To_String( Prefix ) );
	end Extract;

	function Extract( F: Config_File; Prefix: String ) return Config_File is
		-- return a new config file with the data prefixed by the give prefix
		
		
		use Aw_Lib.UString_Ordered_Maps;
		
		My_File: Config_File;
		-- Notice this config file won't have any special property except for the
		-- data it will extract from the given file.
		--
		-- This is so when the user tries to reload the config an exception is
		-- raised


		function Get_Calculated_Prefix return String is
		begin
			if F.Current_Section = "" then
				return Prefix;
			else
				return To_String( F.Current_Section ) & '.' & Prefix;
			end if;
		end Get_Calculated_Prefix;

		Calculated_Prefix : String := Get_Calculated_Prefix;

		procedure Iterator( C: in Cursor ) is
			Value: String  := To_String( Key( C ) );
			First: Integer := Value'First;
			Last : Integer := Value'First + Calculated_Prefix'Length - 1;
		begin
			if Value( First .. Last ) = Calculated_Prefix then
				Include(
					My_File.Contents,
					To_Unbounded_String( Value( Last + 1 .. Value'Last ) ),
					Element( C )
					);
			end if;
		exception
			when CONSTRAINT_ERROR => null;
		end Iterator;
	begin
		Iterate( F.Contents, Iterator'Access );

		return My_File;
	end Extract;

	function Elements_Array( F: Config_File; Key: Unbounded_String ) return Config_File_Array is
		-- return an array with elements withing the category named by:
		-- (THE_CURRENT_CATEGORY).Key.INDEX
		-- where INDEX starts with 1.
	begin
		return Elements_Array( F, To_String( Key ) );
	end Elements_Array;


	function Elements_Array( F: Config_File; Key: String ) return Config_File_Array is
		-- return an array with elements withing the category named by:
		-- (THE_CURRENT_CATEGORY).Key.INDEX
		-- where INDEX starts with 1.

		use Ada.Containers;
		use Aw_Lib.UString_Ordered_Maps;
		function Iterator( Index: in Positive ) return Config_File_Array is

			function Get_Index return String is
				Ret: String := Integer'Image( Index );
				
				use Ada.Strings;
				use Ada.Strings.Fixed;
			begin
				return Trim( Ret, BOTH );
			end Get_Index;


			My_Key: String := key & '.' & Get_Index & '.' ;
			My_Config: Config_File := Extract( F, My_Key );
			Empty: Config_File_Array( 2 .. 1 );

		begin

			if Aw_Lib.UString_Ordered_Maps.Length( My_Config.Contents ) > 0 then
				return  My_Config & Iterator( Index + 1 );
			else
				return Empty;
			end if;
		end Iterator;
		
	begin
		return Iterator( 1 );
	end Elements_Array;

	function Get_Contents_Map( F: in Config_File ) return Aw_Lib.UString_Ordered_Maps.Map is
	-- return an ordered map of Unbounded_String => Unbounded_String
	-- with all keys respecting the pattern "section.subSection.key"
	begin
		return F.Contents;
	end Get_Contents_Map;
end Aw_Config;

