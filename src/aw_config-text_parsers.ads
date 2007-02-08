-- parser for plain text/properties file
--
-- @author Marcelo Coraça de Freitas <marcelo.batera@gmail.com>
-- @createdAt 2007-02-02
-- @lastUpdate

package Ada_Config.Text_Parsers is

	type Parser is new Ada_Config.Parser_Interface with private record;


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


	function Key( P: in Parser ) return String;
	-- return the key of the current field
	-- raise CONSTRAINT_ERROR if there is nothing else to read

	function Element( P: in Parser ) return String;
	-- return the value of the current field
	-- raise CONSTRAINT_ERROR if there is nothing else to read

	
	function Get_File_Name( P: in Parser; Original: in String ) return String;
	-- returns the filename Original with expected extension
	-- ie, Original & ".cfg" in case of Text Parser


	private

	type Parser is new Parser with record
		-- TODO: definir as propriedades do parser
	end record;

end Ada_Config.Text_Parsers;
	
