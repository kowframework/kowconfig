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



with Ada.Containers.Hashed_Maps;
with Ada.Directories;
with Ada.Environment_Variables;
with Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;

with KOW_Lib.File_System;
with KOW_Lib.Log;
with KOW_Lib.String_Util;
with KOW_Lib.UString_Vectors;
with KOW_Lib.UString_Hashed_Maps;


with KOW_Config.Parsers;


--debug
with Ada.Text_IO;

package body KOW_Config is

	Logger : KOW_Lib.Log.Logger_Type := 
			KOW_Lib.Log.Get_Logger( "KOW_Config" );
	

	procedure Log(
			Message : in String;
			Level : KOW_Lib.Log.Log_Level := KOW_lib.Log.Level_Info
		) is
	begin
		KOW_lib.Log.Log(
				Logger	=> Logger,
				Level	=> Level,
				Message	=> "[SYSTEM] :: " & Message -- [SYSTEM] here is a recomendation where to put your users..
			);
	end Log;



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
	
	procedure Set_Project_Name( Str: in Unbounded_String ) is
		-- Set the project name so AdaConfig can find for 
		-- config files search path
		-- This will reset the config path
	begin
		if Str /= Null_Unbounded_String then
			Project_Name := Str;
			Reset_Config_Path;
		else
			raise INVALID_PARAMETER with
				"Project_Name must not be " &
				"Null_Unbounded_String";	   
		end if;

	end Set_Project_Name;


	procedure Set_Project_Name( Str: in String ) is
		-- Set the project name so AdaConfig can find for 
		-- config files search path
		-- This will reset the config path
	begin
		Set_Project_Name( To_Unbounded_String( Str ) );
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
		use KOW_Lib.String_Util;
	begin
		Config_Path := KOW_Lib.File_System.To_Vector (
			Ada.Environment_Variables.Value(
				Str_Replace( ' ', '_', To_String( Project_Name ) ) &
				"_CONFIG_PATH" 	)
				);
	exception
		when CONSTRAINT_ERROR =>
			Config_Path := KOW_Lib.UString_Vectors.Empty_Vector;
	end Reset_Config_Path;


	procedure Add_Config_Path( Str: in String ) is
		-- add Str to config path.
	begin
		KOW_Lib.UString_Vectors.Append( Config_Path, To_Unbounded_String( Str ) );
	end Add_Config_Path;

	function Get_Config_Path return KOW_Lib.UString_Vectors.Vector is
		-- return the current config path
	begin
		return Config_Path;
	end Get_Config_Path;


	---------------------
	-- The Config File --
	---------------------

	function New_Config_File(	N		: in String;
					Is_Complete_Path: Boolean := False
			) return Config_File is
		-- opens a new config file 
		-- read it's contents and return an object representing it.
		-- the file is closed right after it've been read

		-- KOW_Lib packages
		use KOW_Lib.File_System;
		use KOW_Lib.UString_Vectors;
		

		-- this is used to check when the file has been found
		
		F: Config_File;
		-- this is the object that is used as return value

		FOUND_IT: Boolean := FALSE;
		-- controls if it did find the file already


		File_Name : Unbounded_String;

		-- Iterators:
		procedure Path_Iterator( C: KOW_Lib.UString_Vectors.Cursor ) is
		begin
			if FOUND_IT then
				return;
			end if;

			F.File_Name := Element( C ) & KOW_Lib.File_System.Separator;
			F.File_Name := F.File_Name & File_Name;

			declare
				use Ada.Directories;
				S_File_Name: String := To_String( F.File_Name );
			begin
				if Exists( S_File_Name ) AND Kind( S_File_Name ) =
					Ordinary_File then
					FOUND_IT := TRUE;
					-- tell the main unit that we've found a winner! :)
				end if;
			exception
				when Ada.IO_Exceptions.Name_Error => null;
			end;
		end Path_Iterator;
		CF : KOW_Lib.Ustring_Vectors.Vector := Get_Config_Path;
	begin
		if not Is_Complete_Path and then Is_Empty( CF ) then
			raise NO_CONFIG_PATH;
		end if;
		
		if Is_Complete_Path then
			F.File_Name := To_Unbounded_String( N );
			Found_IT := Ada.Directories.Exists( N );
		else
			File_Name := To_Unbounded_String( KOW_Config.Parsers.Get_File_Name( N ) );
			Iterate( CF, Path_Iterator'Access );
			-- iterate over the config path looking for the file
		end if;

		if not FOUND_IT then
			Ada.Exceptions.Raise_Exception( FILE_NOT_FOUND'Identity, N );
		end if;
		Reload_Config( F );
		return F;
	end New_Config_File;


	procedure Save( F: in out Config_File ) is
		use KOW_Config.Parsers;

		Output_File	: File_Type;

		File_Name : String := Get_File_Name( To_String( F.File_Name ) );
	Begin	
		Log( "Saving to file :: " & File_Name & " :: " & To_String( F.File_Name ), KOW_Lib.Log.Level_Debug );			
		Create( Output_File, Out_File, To_String(F.File_Name));
		Save( F, Output_File );
		Close( Output_File );
	end Save;




	procedure Reload_Config( F: in out Config_File ) is
		-- reloads the configuration from the file. :D
		use KOW_Lib.UString_Hashed_Maps;
		use KOW_Lib.Locales;
		use KOW_Config.Parsers;

		P : Parser;


		procedure File_Loader( File_Name: in String; L_Code: in Unbounded_String ) is
			use Ada.Directories;

		begin
			if Exists( File_Name ) then
				begin
					Prepare( P, File_Name );
					loop
						if Is_Localized( P ) then
							Include_Item(
									F		=> F,
									Key		=> Key( P ),
									Locale_Code	=> Locale_Code( P ),
									Value		=> Element( P )
								);
						else
							Include_Item(
									F		=> F,
									Key		=> Key( P ),
									Default_Value	=> Value
								);
						end if;

						Next( P );
					end loop;
				exception
					when CONSTRAINT_ERROR =>
						-- the file has reached the end
						Finish( P );
				end;				
			end if;

		end;


		procedure Locale_Iterator( Locale : in Locale_Type ) is
			Config_Name	: String := File_To_Config_Name( To_String( F.File_Name ) );
			File_Name	: String := Get_File_Name( To_String( Config_Name & "_" & L_Code ) );
		begin
			File_Loader( File_Name, Locale.Code );
		end Locale_Iterator;
	begin
		Locales_Cursor := Locale_Tables.First( Supported_Locales );

		F.Contents := Empty_Map;
		
		-- loads locale files and puts keys in the map like key:ll_cc_ll
		while Locale_Tables.Has_Element( Locales_Cursor ) loop
			declare
				L_Code		: Unbounded_String :=  Locale_Tables.Key( Locales_Cursor );
			begin	
				File_Loader( File_Name, L_Code );	
			end;
			
			Locale_Tables.Next( Locales_Cursor );
		end loop;

		File_Loader( To_String( F.File_Name ), Null_Unbounded_String );
	end Reload_Config;


	function Get_File_Name( F: in Config_File ) return String is
		-- return the file name used for this config file.
	begin
		return To_String( F.File_Name );
	end Get_File_Name;

	procedure Dump_Contents( Config: in KOW_Config.Config_File ) is
		-- dumps the contents map into the std out
		use Ada.Text_IO;
		use KOW_Lib.UString_Hashed_Maps;
		procedure My_Iterator( C: in KOW_Lib.UString_Hashed_Maps.Cursor ) is
		begin
			Log( To_String( Key( C ) ) & " => " & To_String( Element( C ) ), KOW_lib.Log.Level_Debug );
		end My_Iterator;
	begin
		Iterate(
			KOW_Config.Get_Contents_Map( Config ),
			My_Iterator'Access
		);
	end Dump_Contents;

	
	function Merge_Configs( Parent, Child : in Config_File ) return Config_File is
		-- merge two config files, overriding all parent's keys by the child's ones


		use KOW_Lib.UString_Hashed_Maps;
		Cfg: Config_File := Child;

		procedure Iterator( C: in Cursor ) is
		begin
			Include( Cfg.Contents, Key( C ), Element( C ) );
		end Iterator;
	begin
		Cfg.Contents := Parent.Contents;
		Iterate( Child.Contents, Iterator'Access );
		return Cfg;
	end Merge_Configs;



	function Extract( F: Config_File; Prefix: String ) return Config_File is
		-- return a new config file with the data prefixed by the give prefix
		
		
		use KOW_Lib.UString_Hashed_Maps;
		
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


	function Elements_Array( F: Config_File; Key: String ) return Config_File_Array is
		-- return an array with elements within the category named by:
		-- (THE_CURRENT_CATEGORY).Key.INDEX
		-- where INDEX starts with 1.

		use Ada.Containers;
		use KOW_Lib.UString_Hashed_Maps;
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
			My_Config.File_Name := F.File_Name & To_Unbounded_String( ":" & Key );
			if KOW_Lib.UString_Hashed_Maps.Length( My_Config.Contents ) > 0 then
				return  My_Config & Iterator( Index + 1 );
			else
				return Empty;
			end if;
		end Iterator;
	begin
		return Iterator( 1 );
	end Elements_Array;


	

	procedure Set_Section( F: in out Config_File_Type; S: in String ) is
		-- set the current section of the config file.
	begin
		F.Current_Section := To_Unbounded_String( S );
	end Set_Section;


	function Get_Section( F: in Config_File_Type ) return String is
		-- return the current section or "" if there is no section active
	begin
		return To_String( F.Current_Section );
	end Get_Section;



	procedure Include_Item(
				F		: in out Config_File_Type;
				Key		: in     String;
				Locale_Code	: in     KOW_Lib.Locales.Locale_Code_Type;
				Value		: in     String
			) is
		-- include the given localized item
		Item : Config_Item_Type;
	begin
		if Contains( F, Key ) then
			Item := Element( F, Key );
		end if;

		Set_Value( Item, Locale_Code, Value );

		Include( F, Key, Item );
	end Include_Item;

	procedure Include_Item(
				F		: in out Config_File_Type;
				Key		: in     String;
				Default_Value	: in     String
			) is
		-- include the given default item
		Item : Config_Item_Type;
	begin
		if Contains( F, Key ) then
			Item := Element( F, Key );
		end if;

		Set_Default_Value( Item, Default_Value );
		
		Include( F, Key, Item );
	end Include_Item;







	--------------------------
	-- The Config File Item --
	--------------------------



	function Value(
			Item 		: in     Config_Item_Type;
			Locale_Code	: in     KOW_Lib.Locales.Locale_Code_Type
		) return String is
		-- tries to get the data in the given locale with country, then only language
		-- and if not found, return the default value
		-- For instance. If try fetching the item using the locale pt_BR
		-- 	=> tries pt_BR
		-- 	=> tries pt
		-- 	=> fallback to the default value
		use Locale_UString_Maps;
	begin
		if Contains( Item.Translated_Values, Locale_Code ) then
			return To_String( Element( Item.Translated_Values, Locale_Code ) );
		elsif Locale_Code.Country /= No_Country then
			return Value(
					Item		=> Item,
					Locale_Code	=> (
								Language	=> Locale_Code.Language,
								Country		=> No_Country
							)
					);
		else
			return Default_Value( Item );
		end if;
	end Value;


	function Default_Value(
			Item		: in     Config_Item_Type
		) return String is
		-- get the default value
	begin
		return To_String( Item.Default_Value );
	end Default_Value;


	procedure Set_Default_Value(
			Item		: in out Config_Item_Type;
			Value		: in     String
		) is
		-- set the value as the default_value
	begin
		Item.Default_Value := To_Unbounded_String( Value );
	end Set_Default_Value;

	procedure Set_Value(
			Item		: in out Config_Item_Type;
			Locale_Code	: in     KOW_Lib.Locales.Locale_Code_Type;
			Value		: in     String
		) is
		-- set the value for the given locale code
		-- if the default value is not set yet, set it as well
	begin
		Include( Item.Translated_Values, Locale_Code, To_Unbounded_String( Value ) );
		if Item.Default_Value = Null_Unbounded_String then
			Set_Default_Value( Item, Value );
		end if;
	end Set_Value;

	procedure Iterate(
			Item	: in     Config_Item_Type;
			Iterator: not null access procedure(
								Locale_Code	: in KOW_Lib.Locales.Locale_Code_Type;
								Value		: in String
							)
		) is
		-- iterate over all translated values in the config item
	
		procedure Inner_Iterator( C : in Cursor ) is
		begin
			Iterator.all( Key( C ), To_String( Element( C ) ) );
		end Inner_Iterator;
	begin
		Iterate( Item.Translated_Values, Inner_Iterator'Access );
	end Iterate;


	---------------------------------
	-- Item and Configuration Link --
	---------------------------------


	use Configuration_Maps;


	procedure Include(
				F		: in out Config_File_Type;
				Key		: in     String;
				Item		: in     Config_Item_Type
			) is
	begin
		Include( F.Contents, To_Unbounded_String( Key ), Item );
	end Include;


	function Contains(
				F	: Config_File;
				Key	: String
			) return Boolean is
		UKey : constant Unbounded_String := To_Unbounded_String( Key );
		-- check if the element exists in the config file
	begin
		if F.Current_Section = "" then
			return Contains( F.Contents, UKey );
		else
			return Contains( F.Contents, F.Current_Section & '.' & UKey );
		end if;
	end Contains;


	function Element(
				Config		: in Config_File_Type;
				Key		: in String
			) return Config_Item_Type is
		-- get the given configuration item
	begin
		return Element( Config.Contents, To_Unbounded_String( Key ) );
	end ELement;

	function Element(
				Config		: in Config_File_Type;
				Key		: in String
			) return String is
		-- get the default value for the given key
	begin
		return Default_Value( Element( Config, Key ) );
	end Element;

	function Element(
				Config		: in Config_File_Type;
				Key		: in String;
				Locale_Code	: in KOW_Lib.Locales.Locale_Code_Type
			) return String is
		-- tries getting the localized message
	begin
		return Value( Element( Config, Key ), Locale_Code );
	end Element;


	procedure Iterate(
				Config		: in Config_File_type;
				Iterator	: not null access procedure(
										Key	: in String;
										Item	: in Config_Item_Type
									)
			) is
		-- iterate over all elements in the configuration file
		
		procedure Inner_Iterator( C : Cursor ) is
		begin
			Iterator.all( To_String( Key( C ) ), Element( C ) );
		end Inner_Iterator;
	begin
		Iterate( Config.Contents, Inner_Iterator'Access );
	end Iterate;

	-------------------
	-- File handling --
	-------------------


	function Scan_Relative_Path(
				Relative_Path : in String
			) return KOW_Lib.Vectors.Vector is
		-- TODO: refactor the internals of this function; it can be much improved
		
		use Ada.Directories;
		Result: KOW_Lib.UString_Vectors.Vector;


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
				Name : constant String := KOW_Config.Parsers.File_To_Config_Name( Path );
				UName: constant Unbounded_String := To_Unbounded_String( Name );
				use KOW_Lib.UString_Vectors;
			begin
				if not Contains( Result, UName ) then
					Append( Result, UName );
				end if;
			end;
		exception
			when NOT_MY_FILE => null;
		end Check_File;


	
		Filter : constant Filter_Type := (
					Directory	=> true,
					Ordinary_File	=> true,
					Special_File	=> false 
				);

		procedure Process_Search( Directory_Entry : Directory_Entry_Type );

		procedure Search( Directory: in String ) is
		begin
			Search(
					Directory	=> Directory,
					Pattern		=> "*",
					Filter		=> Filter,
					Process		=> Process_Search'Access
				);
		exception
			when ADA.IO_EXCEPTIONS.NAME_ERROR => null;
		end Search;


		To_Scan_Path : KOW_Lib.UString_Vectors.Vector;

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
					KOW_Lib.UString_Vectors.Append( To_Scan_Path,
						To_Unbounded_String( Full_Name( Directory_Entry ) ) );
				end if;
			end if;
		end Process_Search;

		procedure Path_Iterator( C : KOW_Lib.UString_Vectors.Cursor ) is
			use KOW_Lib.UString_Vectors;
			use KOW_Lib.File_System;
			use Ada.Directories;

			Current_Root_Path : String := Full_Name( To_String( Element( C ) ) );
			Current_Root_With_Relative_Path: String :=  Full_Name( 
				Current_Root_Path		&
				KOW_Lib.File_System.Separator	&
				Relative_Path );
		begin
			Current_Root_Path_Length := Current_Root_Path'Length;
			Current_Root_With_Relative_Path_Length := Current_Root_With_Relative_Path'Length;
			
			Append(
				To_Scan_Path,
				To_Unbounded_String(
					 Current_Root_Path & KOW_Lib.File_System.Separator & Relative_Path
					)
				);

			while not Is_Empty( To_Scan_Path ) loop
				Search( To_String( First_Element( To_Scan_Path ) ) );
				Delete_First( To_Scan_Path );
			end loop;

		end Path_Iterator;

		use KOW_Lib.UString_Vectors;
		use Ada.Containers;

	begin

		Iterate( 	
				Get_Config_Path,
				Path_Iterator'Access
			);
	
		return Result;
	end Scan_Relative_Path;



	procedure Generic_Iterate( Vect : in KOW_Lib.UString_Vectors.Vector ) is
		-- Iterate over the elements returned by Scan_Relative_Path.
		-- The parameters are the initialized config file and
		-- the config name within the relative_path parameter


		use KOW_Lib.UString_Vectors;
		
		procedure Inner_Iterator( C: in Cursor ) is
			Name	: constant String := To_String( Element( C ) );
			Config	: Config_File := New_Config_File( Name );
		begin
			Path_Iterator(
					Name	=> Name,
					Config	=> Config
				);
		end Inner_Iterator;
	begin
		Iterate( Map, Inner_Iterator'Access );
	end Generic_Iterate;






	

end KOW_Config;
