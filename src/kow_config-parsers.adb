------------------------------------------------------------------------------
--                                                                          --
--                         KOW Framework :: Config                          --
--                                                                          --
--                              KOW Framework                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2009 .. 2011 KOW Framework Project              --
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
-- 330,  Boston,  MA 021110307, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
------------------------------------------------------------------------------


--------------
-- Ada 2005 --
--------------
with Ada.Strings;		use Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;	use Ada.Strings.Unbounded;

with Ada.Characters.Latin_1;	use Ada.Characters.Latin_1;
with Ada.Text_IO;		use Ada.Text_IO;


-------------------
-- KOW Framework --
-------------------
with KOW_Lib.Log;
package body KOW_Config.Parsers is



	Logger : KOW_Lib.Log.Logger_Type := 
			KOW_Lib.Log.Get_Logger( "KOW_Config.Parsers" );
	

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





	Ext : constant String := ".cfg";



	procedure Prepare(
				P		: in out Parser;
				File_Name	: in String
			) is
		-- prepare the parser to parse the file with the
		-- absolute path File_Name.
		-- read the 1st field

		L_Index1 : Natural := Fixed.Index( File_Name, "_", Backward );
		L_Index2 : Natural;
		E_Index  : Natural := Fixed.Index( File_Name, ".", Backward );



		function Load_Locale( From : in Natural ) return Boolean is
			-- Try loading the locale from the given index
		begin
			P.File_Locale := KOW_Lib.Locales.From_String( File_Name( From .. E_Index ) );
			return True;
		exception
			when others =>
				return False;
		end Load_Locale;
	begin
		P.File_Name := new String( File_Name'Range );
		P.File_Name.all := File_Name;
		Log( "Preparing File """ & File_Name & '"' );


		P.Localized_File := False;
		if L_Index1 /= 0 then
			if E_Index <= 1 then -- means the file has no extension
				E_Index := File_Name'Last;
			else
				E_Index := E_Index - 1;
			end if;


			L_Index2 := Fixed.Index( File_Name, "_", L_Index1, Backward );

			if L_Index2 /= 0 and then Load_Locale( L_Index2 + 1 ) then
				P.Localized_File := True;
			elsif Load_Locale( L_Index1 + 1 ) then
				P.Localized_File := True;
			end if;
		end if;
		
		Open( P.File, In_File, File_Name );
		Next( P );
		-- this will put the parser in the 1st element as required
		-- by KOW_Config specifications
	end Prepare;



	procedure Finish( P: in out Parser ) is
		-- close the file and do whatever it's needed to finish it.
	begin
		Close( P.File );
		-- as said, this close the file and cleans the memory;
	
		P.First_Key_Value_Pair := True;
		P.Current_Key := Null_Unbounded_String;
		P.Current_Value := Null_Unbounded_String;
		P.Current_Section := NUll_Unbounded_String;
	end Finish;

	procedure Next( P: in out Parser ) is
		-- move the parser to the next field, if it exists
		-- if not prepare the parser to return CONSTRAINT_ERROR
		-- everytime Key and Value are called
		-- NOTE: this is where the parsing actualy happens

		-- STEPS:
		--	1. find a Key.
		--	2. find a Value.
		-- if it finds a syntax error during the parsing throws an exception
		-- and calls Finish( P );

		TAB: Character := Character'Val(9);
		NEW_LINE: Character := Character'Val(10);
		CARRIAGE_RETURN : Character := Character'Val(13);

		


		Finished_Key_Value_Pair: Boolean := False;
		-- controls if we've finished reading this value pair
		Possible_End_Of_Element: Boolean := False;
		-- controls when a " char is read if it's the end of Element
		-- block


		--------------------------------------------
		-- PROCEDURES USED BY INTERNAL PROCEDURES --
		--------------------------------------------

		function Is_White_Space return boolean is
		begin
			return P.C  = NEW_LINE or P.C = TAB or P.C = ' ' or P.C = CARRIAGE_RETURN;
		end Is_White_Space;

		procedure Raise_Unexpected_Character is
		begin
			
			Raise_Syntax_Error(	File_Name => P.File_Name.all,
						Line_Number => Integer( Line( P.File ) ),
						Column_Number => Integer( Col( P.File ) ),
						Message => "''" & P.C & "'' not expected here" );
		end Raise_Unexpected_Character;

		procedure Raise_Unexpected_Line_Break is
		begin
			
			Raise_Syntax_Error(	File_Name => P.File_Name.all,
						Line_Number => Integer( Line( P.File ) ),
						Column_Number => Integer( Col( P.File ) ),
						Message => "unexpected end of line" );
		end Raise_Unexpected_Line_Break;

		procedure Raise_Unexpected_EOF is
		begin
			Raise_Syntax_Error(	File_Name => P.File_Name.all,
						Line_Number => Integer( Line( P.File ) ),
						Column_Number => Integer( Line( P.File ) ),
						Message => "unexpected end of file" );
		end Raise_Unexpected_EOF;


		procedure Next_Line is
			-- continue 'till the next line 
			Str: String := Get_Line( P.File );
			-- Get_Line return String is an Ada2004 function
		begin
			-- It's necessary set P.C to continue the iteration.  
			P.C := ' ';
		end Next_Line;


		procedure Next_Char is
			-- walk to the next char and increment the counter
		begin
			Get( P.File, P.C );
		end Next_Char;




		-------------------------
		-- INTERNAL PROCEDURES --
		-------------------------





		procedure Read_Section is
			-- read a section block and set P.Current_Section
		begin
				if P.C = '#' OR P.C = '"' OR P.C = ''' OR P.C = '[' then
					Raise_Unexpected_Character;
				elsif P.C = ']' then

					Trim( P.Current_Section, Both );
					if P.Current_Section = "" then
						-- the section is empty. Shouldn't happen!
						Raise_Unexpected_Character;
					end if;
					Append( P.Current_Section, '.' );
					-- we do this for better performance

					P.Current_Block := B_NONE;
					-- set block to none and continue
				else
					-- all set, let's do it. :)
					Append( P.Current_Section, P.C );
				end if;
		end Read_Section;

		procedure Read_Key is
			-- reads a key block and set P.Current_Key
		begin
			if	End_Of_Line( P.File)	OR 
				P.C = '#'			OR
				P.C = '''			OR
				P.C = '"'			OR
				P.C = '['			OR
				P.C = ']'
			then
				Raise_Unexpected_Character;
			elsif P.C = '=' then
				-- start of the Element block
				Trim( P.Current_Key, Both );
				P.Current_Block := B_ELEMENT;
				Next_Char;
				while P.C /= '"' loop
					if not Is_White_Space AND P.C /= '"' then
						Raise_Unexpected_Character;
					end if;
					Next_Char;
				end loop;
			else
				Append( P.Current_Key, P.C );
			end if;
		end Read_Key;


		procedure Read_Element is
			-- reads an Element block
		begin
			if P.C = '"' then
				Possible_End_Of_Element := TRUE;
				-- so Find_Nex_Block will see if it's the end of 
				-- Element block or not.
				P.Current_Block := B_NONE;
			else
				Append( P.Current_Value, P.C );
				-- the value is appended even it's the final "
				if End_of_Line( P.File ) then
					Append( P.Current_Value, NEW_LINE );
				end if;
			end if;
		end Read_Element;


		procedure Find_Next_Block is
			-- look for a next block start doing nothing in the meantime
			-- when the next block is found update it.
		begin

			if Possible_End_Of_Element then
				if P.C = '"' then
					P.Current_Block := B_ELEMENT;
					-- it's part of the element.
					Append( P.Current_Value, P.C );
					-- and append the " to Current_Value.
					return;
					-- and return to main looping so I can
					-- continue fetching array and stuff. :D
				else
					Possible_End_Of_Element := FALSE;
					-- it's the end of the element. ;)
					-- look for the block we are and stuff. :D
					Finished_Key_Value_Pair := TRUE;
					P.First_Key_Value_Pair := FALSE;
					-- and continue checking where I am.
				end if;
			end if;


			if  P.C = '#'  then
				-- if it's comment...
				Next_Line;
				Find_Next_Block;
			elsif Is_White_Space  then
				-- ignore blank spaces
				Next_Char;
				Find_Next_Block;
			elsif P.C = '[' then
				-- checks if it's a new section mark
				P.Current_Block := B_SECTION;
			elsif P.C = ']' OR P.C = ''' OR P.C = '"' then
				-- checks if it's an invalid section closing mark
				-- if it's a " or a ' too
				Raise_Unexpected_Character;
			else
				-- if it's any other caracter we've just 
				-- entered into a key block ;)
				P.Current_Block := B_KEY;
				if P.Current_Key = Null_Unbounded_String then
					-- if the key is empty, meaning it's a new key
					Read_Key;
				end if;
			end if;
		end Find_Next_Block;


	begin
		P.Current_Key := Null_Unbounded_String;
		-- Reset the key
		P.Current_Value := Null_Unbounded_String;
		-- Reset the value

		if P.First_Key_Value_Pair or P.C = '"' then
			-- reads the first char or skip the " that represents
			-- the ending of the last element
			Next_Char;
		elsif P.Current_Block = B_SECTION then
			-- should empty the current section and read the 1st char
			P.Current_Section := Null_Unbounded_String;
			Next_Char;
		end if;

		loop

			case P.Current_Block is
				when B_NONE =>
					Find_Next_Block;
				when B_SECTION =>
					Read_Section;
				when B_KEY =>
					Read_Key;
				when B_ELEMENT =>
					Read_Element;
			end case;

			exit when Finished_Key_Value_Pair;

			Next_Char;
		end loop;

		P.Locale_Separator_Index := Index( Source => P.Current_Key, Pattern => ":" );

	exception
		when End_Error =>
			-- if this End_Error came before reading the key
			-- then it's the expected EOF. Nothing to do here.
			if P.Current_Block /= B_NONE then
				-- if it's not expected...
				Raise_Unexpected_EOF;
			end if;
	end Next;

	function Key( P: in Parser ) return String is
		-- return the key of the current field
		-- raise CONSTRAINT_ERROR if there is nothing else to read
		K : constant String := To_String( P.Current_Key );
	begin
		if P.Current_Key = Null_Unbounded_String then
			raise CONSTRAINT_ERROR;
		end if;


		if P.Locale_Separator_Index = 0 then
			return To_String( P.Current_Section ) & K;
		else
			return To_String( P.Current_Section ) & K( K'First .. P.Locale_Separator_Index - 1 );
		end if;

	end Key;

	function Is_Localized( P : in Parser ) return Boolean is
		-- check if the current value is localized (either file or key)
	begin
		return P.Localized_File or P.Locale_Separator_Index /= 0;
	end Is_Localized;

	function Locale_Code( P : in Parser ) return KOW_Lib.Locales.Locale_Code_Type is
		-- get the locale code for this entry (either localized file or key)
	begin
		if P.Locale_Separator_Index /= 0 then
			declare
				K : constant String := To_String( P.Current_Key );
			begin
				return KOW_Lib.Locales.From_String( K( P.Locale_Separator_Index + 1 .. K'Last ) );
			end;
		elsif P.Localized_File then
			return P.File_Locale;
		else
			raise PROGRAM_ERROR with "trying to get a locale code for a non-localized key";
		end if;
	end Locale_Code;

	function Value( P: in Parser ) return String is
		-- return the value of the current field
		-- raise CONSTRAINT_ERROR if there is nothing else to read
	begin
		if P.Current_Key = Null_Unbounded_String then
			-- we check if the Key is empty here, not the element
			-- to alow use of empty or null strings
			raise CONSTRAINT_ERROR;
		end if;



		return To_String( P.Current_Value );
	end Value;

	
	function Get_File_Name( Original: in String ) return String is
	begin
		return Original & Ext;
	end Get_File_Name;

	function File_To_Config_Name( File_Name: in String ) return String is
		-- Convert the file name to a config name.
		-- Raises NOT_MY_FILE if it's not a supported config file

		F_Last: Integer := File_name'Last;
		F_Suf_First: Integer := F_Last - Ext'Length + 1;
	begin
		if Ext'Length >= File_Name'Length then
			raise KOW_Config.NOT_MY_FILE with "wrong file length";
		elsif File_Name( F_Suf_First .. F_Last ) /= Ext then
			raise KOW_Config.NOT_MY_FILE with "wrong file suffix";
		end if;

		return File_Name( File_Name'First .. File_Name'Last - Ext'Length );
	end File_To_Config_Name;

	procedure Save(
			Config	: in KOW_Config.Config_File_Type;
			File	: in File_Type
		) is 
		-- save config file
		use Ada.Text_IO;
		
		function Get_Handled_String( Element : String ) return String is
			Handled_String : Unbounded_String;
		begin
			for Index in Element'First .. Element'Last loop
				if Element(Index) = '"' then
					-- we were using Character'Val(16#22#) instead of '"'; I don't remember why
					Handled_String := Handled_String & '"' & Element( Index );
				else
					Handled_String := Handled_String & Element(Index);
				end if;
			end loop;
		
			return To_String( Handled_String );
		end Get_Handled_String;

		procedure Put_Item( Key, Value : in String ) is
		begin
			Put( File, Key & " = " );
			Put( File, '"' & Get_Handled_String( Value ) & '"' );
			New_Line( File );
		end Put_Item;
		
		procedure Item_Iterator( Key : in String; Item : in Config_Item_Type ) is

			procedure Localized_Iterator( Locale_Code : in Locale_Code_Type; Value : in String ) is
			begin
				Put_Item(
						Key	=> Key & ':' & To_String( Locale_Code ),
						Value	=> Value
					);
			end Localized_Iterator;
		begin
			Put_Item(
					Key	=> Key,
					Value	=> Default_Value( Item )
				);
			Iterate( Item, Localized_Iterator'Access );
		end Item_Iterator;
	begin
		Iterate( Config, Item_Iterator'Access );
	end Save;
	
end KOW_Config.Parsers;
	
