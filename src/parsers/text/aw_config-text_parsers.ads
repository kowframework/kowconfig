-- parser for plain text/properties file
--
-- @author Marcelo Coraça de Freitas <marcelo.batera@gmail.com>


with Ada.Strings.Unbounded;	use Ada.Strings.Unbounded;

with Ada.Text_IO;		use Ada.Text_IO;

package Aw_Config.Text_Parsers is

	type Parser is new Aw_Config.Parser_Interface with private;


	procedure Prepare(	P: in out Parser;
				File_Name: in String );
	-- prepare the parser to parse the file with the
	-- absolute path File_Name.
	-- read the 1st field

	procedure Finish( P: in out Parser );
	-- close the file and do whatever it's needed to finish it.

	procedure Next( P: in out Parser );
	-- move the parser to the next field, if it exists
	-- if not prepare the parser to return CONSTRAINT_ERROR
	-- everytime Key and Value are called


	function Key( P: in Parser ) return Unbounded_String;
	-- return the key of the current field
	-- raise CONSTRAINT_ERROR if there is nothing else to read

	function Element( P: in Parser ) return Unbounded_String;
	-- return the value of the current field
	-- raise CONSTRAINT_ERROR if there is nothing else to read

	
	function Get_File_Name( P: in Parser; Original: in String ) return String;
	-- returns the filename Original with expected extension
	-- ie, Original & ".cfg" in case of Text Parser


	private

	type String_Access is access String;

	type Parser is new Aw_Config.Parser_Interface with record
		File: File_Type;
		File_Name: String_Access;
		Current_Key, Current_Element, Current_Section: Unbounded_String;
	end record;

end Aw_Config.Text_Parsers;
	
