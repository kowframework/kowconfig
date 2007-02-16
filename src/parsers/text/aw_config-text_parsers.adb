-- parser for plain text/properties file
--
-- @author Marcelo Coraça de Freitas <marcelo.batera@gmail.com>



-- Ada Packages
with Ada.Strings;		use Ada.Strings;
with Ada.Strings.Unbounded;	use Ada.Strings.Unbounded;

with Ada.Characters.Latin_1;	use Ada.Characters.Latin_1;


package body Aw_Config.Text_Parsers is

	procedure Prepare(	P: in out Parser;
				File_Name: in String ) is
		-- prepare the parser to parse the file with the
		-- absolute path File_Name.
		-- read the 1st field
	begin
		P.File_Name := new String( File_Name'Range );
		P.File_Name.all := File_Name;
		Open( P.File, In_File, File_Name );

		Next( P );
		-- this will put the parser in the 1st element as required
		-- by Aw_Config specifications
	end Prepare;



	procedure Finish( P: in out Parser ) is
		-- close the file and do whatever it's needed to finish it.
	begin
		Close( P.File );
		-- as said, this close the file and cleans the memory;

		P.Current_Key := Null_Unbounded_String;
		P.Current_Element := Null_Unbounded_String;
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
		SPACE: Character := Character'Val(32);
		NEW_LINE: Character := Character'Val(10);

		type File_Blocks is ( B_NONE, B_SECTION, B_KEY, B_ELEMENT );
		-- represents the known structures in the file
		Current_Block: File_Blocks := B_NONE;

		C: Character; -- current char
		
		Line: Positive := 1;
		-- count the current line
		Column: Positive := 1;
		-- count the current column



		Finished_Key_Value_Pair: Boolean := False;
		-- controls if we've finished reading this value pair


		--------------------------------------------
		-- PROCEDURES USED BY INTERNAL PROCEDURES --
		--------------------------------------------

		function Is_White_Space return boolean is
		begin
			return C  = NEW_LINE or C = TAB;
		end Is_White_Space;

		procedure Raise_Unexpected_Character is
		begin
			
			Raise_Syntax_Error(	File_Name => P.File_Name.all,
						Line_Number => Line,
						Column_Number => Column,
						Message => "''" & C & "'' not expected here" );
		end Raise_Unexpected_Character;

		procedure Raise_Unexpected_Line_Break is
		begin
			
			Raise_Syntax_Error(	File_Name => P.File_Name.all,
						Line_Number => Line,
						Column_Number => Column,
						Message => "unexpected end of line" );
		end Raise_Unexpected_Line_Break;

		procedure Raise_Unexpected_EOF is
		begin
			Raise_Syntax_Error(	File_Name => P.File_Name.all,
						Line_Number => Line,
						Column_Number => Column,
						Message => "unexpected end of file" );
		end Raise_Unexpected_EOF;


		function Next_Line return Boolean is
			-- continue 'till the next line 
			-- and set the counters
			-- if there is a next like return true
			-- if not return false
		begin
			while C /= NEW_LINE loop
				-- look for next line break
				Get( P.File, C );
			end loop;

			Line := Line + 1;
			Column := 1;
			-- check if this isn't a blank line:

			Get( P.File, C );
			-- gets the 1st char on the line

			return true;
		end Next_Line;


		procedure Next_Char is
			-- walk to the next char and increment the counter
		begin
			Get( P.File, C );
			Column := Column + 1;
		end Next_Char;




		-------------------------
		-- INTERNAL PROCEDURES --
		-------------------------

		procedure Find_Next_Block is
			-- look for a next block start doing nothing in the meantime
			-- when the next block is found update it.
		begin
			if ( C = NEW_LINE OR C = '#' ) AND not Next_Line then
				-- check if it's a new line or a comment
				-- if it is and the file got an EOF, return without error
				return;
			elsif Is_White_Space  then
				-- ignore blank spaces
				return;
			elsif C = '[' then
				-- checks if it's a new section mark
				P.Current_Section := Null_Unbounded_String;
				Current_Block := B_SECTION;
			elsif C = ']' OR C = ''' OR C = '"' then
				-- checks if it's an invalid section closing mark
				-- if it's a " or a ' too
				Raise_Unexpected_Character;
			else
				-- if it's any other caracter we've just entered into a key block ;)
				P.Current_Key := To_Unbounded_String( "" & C );
				Current_Block := B_KEY;
			end if;
		end Find_Next_Block;




		procedure Read_Section is
			-- read a section block and set P.Current_Section
		begin
				if C = '#' OR C = '"' OR C = ''' OR C = '[' OR C = NEW_LINE then
					Raise_Unexpected_Character;
				elsif C = ']' then

					Trim( P.Current_Section, Both );
					if P.Current_Section = "" then
						-- the section is empty. Shouldn't happen!
						Raise_Unexpected_Character;
					end if;
					Append( P.Current_Section, '.' );
					-- we do this for better performance

					Current_Block := B_NONE;
				else
					-- all set, let's do it. :)
					Append( P.Current_Section, C );
				end if;
		end Read_Section;

		procedure Read_Key is
			-- reads a key block and set P.Current_Key
		begin
			if C = '#' OR C = ''' OR C = '"' OR C = '.' OR C = '[' OR C = ']' OR C = NEW_LINE then
				Raise_Unexpected_Character;
			elsif C = '=' then
				-- start of the Element block
				Trim( P.Current_Key, Both );
				Current_Block := B_ELEMENT;
				Next_Char;
				while C /= '"' loop
					if not Is_White_Space AND C /= '"' then
						Raise_Unexpected_Character;
					end if;
					Next_Char;
				end loop;
			else
				Append( P.Current_Key, C );
			end if;
		end Read_Key;


		procedure Read_Element is
			-- reads an Element block
		begin
			if C = '"' then
				Next_Char;
				if C = '"' then
					-- check if it's a "" mark
					Append( P.Current_Element, C );
				else
					-- if it got here it's not a "" mark. :D
					Current_Block := B_NONE;
					Finished_Key_Value_Pair := TRUE;
				end if;
			else
				-- we just add all other value to the Element
				Append( P.Current_Element, C );
			end if;
		end Read_Element;


	begin
		P.Current_Key := Null_Unbounded_String;
		-- Reset the key
		P.Current_Element := Null_Unbounded_String;
		-- Reset the value


		while NOT Finished_Key_Value_Pair loop
			Next_Char;
			case Current_Block is
				when B_NONE => 
					-- when I'm not reading a section, neither key and neither a value
					Find_Next_Block;
				when B_SECTION =>
					Read_Section;
				when B_KEY =>
					Read_Key;
				when B_ELEMENT =>
					Read_Element;
			end case;

		end loop;

	exception
		when End_Error =>
			P.Current_Key := Null_Unbounded_String;
			P.Current_Element := Null_Unbounded_String;
			if Current_Block /= B_NONE then
				Raise_Unexpected_EOF;
			end if;
	end Next;

	function Key( P: in Parser ) return Unbounded_String is
		-- return the key of the current field
		-- raise CONSTRAINT_ERROR if there is nothing else to read
	begin
		if P.Current_Key = Null_Unbounded_String then
			raise CONSTRAINT_ERROR;
		end if;

		return P.Current_Section & P.Current_Key;
	end Key;

	function Element( P: in Parser ) return Unbounded_String is
		-- return the value of the current field
		-- raise CONSTRAINT_ERROR if there is nothing else to read
	begin
		if P.Current_Key = Null_Unbounded_String then
			-- we check if the Key is empty here, not the element
			-- to alow use of empty or null strings
			raise CONSTRAINT_ERROR;
		end if;
		return P.Current_Element;
	end Element;

	
	function Get_File_Name( P: in Parser; Original: in String ) return String is
		-- returns the filename Original with expected extension
		-- ie, Original & ".cfg" in case of Text Parser
	begin
		return  Original &  ".cfg";
	end Get_File_Name;


end Aw_Config.Text_Parsers;
	
