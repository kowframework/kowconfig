-- This is the interface for AdaConfig's parser.
-- Each parser got to derive from the type Parser
--
-- @author Marcelo Coraça de Freitas <marcelo.batera@gmail.com>

package Ada_Config.Parsers_Interface is

	type Parser is tagged null record;

	type Parser_Access is Access all Parser'Class;

	procedure Prepare(	P: in out Parser;
				File_Name: in String ) is abstract;
	-- prepare the parser to parse the file with the
	-- absolute path File_Name.
	-- read the 1st field

	procedure Finish( P: in out Parser ) is abstract;
	-- close the file and do whatever it's needed to finish it.

	procedure Next( P: in out Parser ) is abstract;
	-- move the parser to the next field, if it exists
	-- if not prepare the parser to return CONSTRAINT_ERROR
	-- everytime Key and Value are called

	function Key( P: in Parser ) return String is abstract;
	-- return the key of the current field
	-- raise CONSTRAINT_ERROR if there is nothing else to read

	function Element( P: in Parser ) return String is abstract;
	-- return the value of the current field
	-- raise CONSTRAINT_ERROR if there is nothing else to read

	function Get_File_Name( P: in Parser; Original: in String ) return String is abstract;
	-- returns the filename Original with expected extension
	-- ie, Original & ".cfg" in case of Text Parser

end Ada_Config.Parsers_Interface;
