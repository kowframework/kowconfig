-- This is the interface for AdaConfig's parser
-- Each parser got to derive from this one class
--and implement every functions and procedures
--in it
--
-- @author Marcelo Coraça de Freitas <marcelo.batera@gmail.com>
-- @createdAt 2007-02-01
-- @lastUpdate

with Ada_Config.Parser_Interface;	use Ada_Config.Parser_Interface

package Ada_Config.Text_Parsers is

	type Text_Parser is new Parser with private record;


	procedure Prepare(	P: in out Text_Parser'Class;
				File_Name: in String );
	-- prepare the parser to parse the file with the
	-- absolute path File_Name.
	-- read the 1st field

	procedure Finish( P: in out Parser'Class ) is abstract;
	-- close the file and do whatever it's needed to finish it.

	procedure Next( P: in out Text_Parser'Class );
	-- move the parser to the next field, if it exists
	-- if not prepare the parser to return CONSTRAINT_ERROR
	-- everytime Key and Value are called


	function Key( P: in Text_Parser'Class ) return String;
	-- return the key of the current field
	-- raise CONSTRAINT_ERROR if there is nothing else to read

	function Element( P: in Text_Parser'Class ) return String;
	-- return the value of the current field
	-- raise CONSTRAINT_ERROR if there is nothing else to read


	private

	type Text_Parser is new Parser with record
		-- TODO: definir as propriedades do parser
	end record;

end Ada_Config.Text_Parsers;
	
