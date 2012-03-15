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
-- This is the KOW_Config package                                           --
--                                                                          --
-- KOW Config is a simple library for handling .cfg files with a simplistic --
-- syntax and localization support based on KOW_Lib.Locales package.        --
--                                                                          --
-- Notice that, even though it has been taken in consideration, there is no --
-- big concern in efficiency (which can be noted in the Elements_Array meth.--
--                                                                          --
------------------------------------------------------------------------------


-- Ada Packages
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Text_IO;			use Ada.Text_IO;

-- KOW_Lib Packages
with KOW_Lib.UString_Vectors;
with KOW_Lib.Locales;			use KOW_Lib.Locales;

package KOW_Config is

	----------------
	-- Exceptions --
	----------------

	INVALID_PARAMETER : Exception;
	-- when the paramater passed to the function is not valid 

	SYNTAX_ERROR: Exception;
	-- when there is an error in the config file

	FILE_NOT_FOUND: Exception;
	-- when there is no config file found in the config path

	NO_CONFIG_PATH: Exception;
	-- when there is no config path set


	NOT_MY_FILE : Exception;
	-- when the file name is wrong or something like that
	-- used by the parser

	procedure Raise_Syntax_Error (
					File_Name	: in     String;
					Line_Number	: in     Natural := 0;
					Column_Number	: in     Natural := 0;
					Message		: in     String
			      );
	-- Raise SYNTAX_ERROR exception with message composed by:
	-- "["& File_Name &":"&Line_Number & "] " & Message


	------------------------------------
	-- Methods for Project Management --
	------------------------------------

	procedure Set_Project_Name( Str: in String );
	-- Set the project name so KOWConfig can find for 
	-- config files search path
	-- This will reset the config path

	function Get_Project_Name return String;
	-- return the current project name

	procedure Reset_Config_Path;
	-- reset the config path using the environment variable:
	-- [PROJECT_NAME]_CONFIG_PATH

	procedure Add_Config_Path( Str: in String );
	-- add Str to config path.

	function Get_Config_Path return KOW_Lib.UString_Vectors.Vector;
	-- return the current config path




	---------------------
	-- The Config File --
	---------------------

	type Config_File_Type is private;
	-- represents the configuration file
	
	type Config_File_Array is Array( Positive range<> ) of Config_File_Type;
	-- one array of config files


	function New_Config_File(
				N		: in     String;
				Is_Complete_Path: in     Boolean := False
			) return Config_File_Type;
	-- opens a new config file 
	-- read it's contents and return an object representing it.
	-- the file is closed right after it've been read

	procedure Save( F: in out Config_File_Type );
	-- save the config file.
	-- it has to be a file that was loaded directly from a file.
	

	procedure Reload_Config( F: in out Config_File );
	-- (re)load the configuration from the file

	function Get_File_Name( F: in Config_File ) return String;
	-- return the file name used for this config file.

	procedure Dump_Contents( Config: in KOW_Config.Config_File );
	-- dumps the contents map into the std out
	
	
	function Merge_Configs( Parent, Child : in Config_File ) return Config_File;
	-- merge two config files, overriding all parent's keys by the child's ones


	function Extract(
				F	: in     Config_File;
				Prefix	: in     String
			) return Config_File_Type;
	-- return a new config file with the data prefixed by the give prefix

	function Elements_Array(
				F	: in     Config_File;
				Key	: in     String
			) return Config_File_Array;
	-- return an array with elements withing the category named by:
	-- (THE_CURRENT_CATEGORY).Key.INDEX
	-- where INDEX starts with 1.



	procedure Set_Section(
				F	: in out Config_File_Type;
			       	S	: in     String
			);
	-- set the current section of the config file.


	function Get_Section( F : in Config_File ) return String;
	-- return the current section or "" if there is no section active




	--------------------------
	-- The Config File Item --
	--------------------------

	type Config_Item_Type is private;

	function Value(
			Item 		: in     Config_Item_Type;
			Locale_Code	: in     KOW_Lib.Locales.Locale_Code_Type
		) return String;
	-- tries to get the data in the given locale with country, then only language
	-- and if not found, return the default value
	-- For instance. If try fetching the item using the locale pt_BR
	-- 	=> tries pt_BR
	-- 	=> tries pt
	-- 	=> fallback to the default value



	function Default_Value(
			Item		: in     Config_Item_Type
		) return String;
	-- get the default value
	


	procedure Set_Default_Value(
			Item		: in out Config_Item_Type;
			Value		: in     String
		);
	-- set the value as the default_value
	

	procedure Set_Value(
			Item		: in out Config_Item_Type;
			Locale_Code	: in     KOW_Lib.Locales.Locale_Code_Type;
			Value		: in     String
		);
	-- set the value for the given locale code
	-- if the default value is not set yet, set it as well
	

	procedure Iterate(
			Item	: in     Config_Item_Type;
			Iterator: not null access procedure(
								Locale_Code	: in KOW_Lib.Locales.Locale_Code_Type;
								Value		: in String
							)
		);
	-- iterate over all translated values in the config item



	---------------------------------
	-- Item and Configuration Link --
	---------------------------------


	function Contains(
				F	: in     Config_File;
				Key	: in     String
			) return Boolean;
	-- check if the element exists in the config file
	

	function Element(
				Config		: in Config_File_Type;
				Key		: in String
			) return Config_Item_Type;
	-- get the given configuration item


	function Element(
				Config		: in Config_File_Type;
				Key		: in String
			) return String;
	-- get the default value for the given key
	
	function Element(
				Config		: in Config_File_Type;
				Key		: in String;
				Locale_Code	: in KOW_Lib.Locales.Locale_Code_Type
			) return String;
	-- tries getting the localized message


	-------------------
	-- File handling --
	-------------------


	function Scan_Relative_Path( Relative_Path : in String ) return KOW_Lib.UString_Vectors.Vector;
	-- Scan a given relative path within the Config_Path for the project.
	-- Return all the config files found without the extension and locale sufix
	-- If there are more and 1 config file with the same name in the configuration path
	-- the one that should be loaded is controlled by the new_config function


	generic
		with procedure Path_Iterator(
				Name	: in     String;
				Config	: in out Config_File_Type
			);
	procedure Generic_Iterate(
			Vect	: KOW_Lib.UString_Vectors.Vector
		);
	-- Iterate over the elements returned by Scan_Relative_Path.
	-- The parameters are the initialized config file and the
	-- config name within the relative_path parameter




private


	-- All the unbounded string references should be private for a simple reason
	-- The Unbounded String implementation in GNAT's opensource versions of 2011
	-- all behave weirdly in a multi-tasking environment, raising random exceptions
	-- and eventually segfaulting the application
	-- I don't know why that's so and I haven't reported the bug (if that's really a bug and
	-- not a missuse of the library)
	-- So I am hiding the unbounded string dependency and trying not to use it
	-- elsewhere in the code that might be accessed by several tasks.

	-- TODO :: if for some reason it start crashing, change the unbounded string to something else
	package Locale_UString_Maps is new Ada.Containers.Hashed_Maps(
						Key_type	=> KOW_Lib.Locales.Locale_Code_Type,
						Element_Type	=> Unbounded_String,
						Hash		=> KOW_Lib.Locales.Hash
					);

	type Config_Item_Type is record
		Default_Value		: Unbounded_String;
		Translated_Values	: Locale_UString_Maps.Map;
	end record;

	package Configuration_Maps is new Ada.Containers.Hashed_Maps(
						Key_Type	=> Unbounded_String,
						Element_Type	=> Config_Item_Type,
						Hash		=> Hash
					);



	Config_Path : KOW_Lib.UString_Vectors.Vector;

	Project_Name : Unbounded_String;

	type Config_File is record
		File_Name	: Unbounded_String;
		Current_Section	: Unbounded_String;
		Contents	: Configuration_Maps.Map;
	end record;

end KOW_Config;
