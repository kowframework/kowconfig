-- This is the main AdaConfig package.
-- Here you'll find the types you should use in your application and all 
-- visible procedures and functions.
--
-- author Marcelo C. de Freitas <marcelo.batera@gmail.com>
-- createdAt 2007-01-25
--
-- Repository information:
-- $Date$
-- $Revision$
-- $Author$




with Ada.Containers.Ordered_Maps;
with Ada.Exceptions;

with Ada.Strings.Unbounded;	use Ada.Strings.Unbounded;

with Aw_Lib.Env_Vars;
with Aw_Lib.File_System;
with Aw_Lib.String_Util;
with Aw_Lib.UString_Vectors;
with Aw_Lib.UString_Ordered_Maps;

package body Aw_Config is


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
			Aw_Lib.Env_Vars.Value(
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

	function New_Config_File( N: in String; P: in Parser_Access ) return Config_File is
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

		-- Iteractors:
		procedure Path_Iterator( C: Aw_Lib.UString_Vectors.Cursor ) is
		begin
			if FOUND_IT then
				return;
			end if;

			F.File_Name := Element( C ) & '/';
			F.File_Name := F.File_Name & To_Unbounded_String( Get_File_Name( P.all, N ) );

			if Is_File( To_String( F.File_Name ) ) then
				F.My_Parser := P;
				FOUND_IT := TRUE;
				-- tell the main unit that we've found a winner! :)
			end if;
		end Path_Iterator;

	begin
		if Is_Empty( Config_Path ) then
			raise NO_CONFIG_PATH;
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



	function Get_Contents_Map( F: in Config_File ) return Aw_Lib.UString_Ordered_Maps.Map is
	-- return an ordered map of Unbounded_String => Unbounded_String
	-- with all keys respecting the pattern "section.subSection.key"
	begin
		return F.Contents;
	end Get_Contents_Map;
end Aw_Config;

