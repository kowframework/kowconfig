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
		NEW_LINE: Character := Character'Val(10);

		


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
			return P.C  = NEW_LINE or P.C = TAB or P.C = ' ';
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
			-- Get_Line return String is an Ada2005 function
		begin
			null;
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
				P.C = '.'			OR
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
				Append( P.Current_Element, P.C );
				-- the value is appended even it's the final "
				if End_of_Line( P.File ) then
					Append( P.Current_Element, NEW_LINE );
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
					Append( P.Current_Element, P.C );
					-- and append the " to Current_Element.
					return;
					-- and return to main looping so I can continue fetching array and stuff. :D
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
				-- if it's any other caracter we've just entered into a key block ;)
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
		P.Current_Element := Null_Unbounded_String;
		-- Reset the value

		if P.First_Key_Value_Pair or P.C = '"' then
			-- reads the first char or skip the " that represents the ending of the last element
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


	exception
		when End_Error =>
			-- if this End_Error came before reading the key
			-- then it's the expected EOF. Nothing to do here.
			if P.Current_Block /= B_NONE then
				-- if it's not expected...
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
	
