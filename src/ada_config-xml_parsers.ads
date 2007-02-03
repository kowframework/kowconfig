-- parser for XML file
--
-- @author Marcelo Coraça de Freitas <marcelo.batera@gmail.com>
-- @createdAt 2007-02-03
-- @lastUpdate



-- Ada Packages
with Ada.Strings_Unbounded;	use Ada.Strings_Unbounded;


-- XML/Ada Packages
with Sax.Attributes;
with Sax.Readers;
with Unicode.CES;


with Ada_Config.Parsers_Interface;




package Ada_Config.Xml_Parsers is

	type Parser is new Parser with private record;


	procedure Prepare(	P: in out Parser'Class;
				File_Name: in String );
	-- prepare the parser to parse the file with the
	-- absolute path File_Name.
	-- read the 1st field

	procedure Finish( P: in out Parser'Class ) is abstract;
	-- close the file and do whatever it's needed to finish it.

	procedure Next( P: in out Parser'Class );
	-- move the parser to the next field, if it exists
	-- if not prepare the parser to return CONSTRAINT_ERROR
	-- everytime Key and Value are called


	function Key( P: in Parser'Class ) return String;
	-- return the key of the current field
	-- raise CONSTRAINT_ERROR if there is nothing else to read

	function Element( P: in Parser'Class ) return String;
	-- return the value of the current field
	-- raise CONSTRAINT_ERROR if there is nothing else to read


private

	type Reader is new Sax.Readers.Reader with record
		Section: Alos.UString_Vectors.Vector;
		-- where the current section is stored

		In_Key: Boolean := false;
		-- true if reading a key. ;)

		Current_Key: Unbounded_String;
		-- the current key name, without section

		Current_Value: Unbounded_String;
		-- the value of current key.

		Values: Alos.UString_Ordered_Maps.Map;
		-- the values must be stored into a map before returned in 
		-- order to be compilant with Parsers_Interface.Parser and
		-- SAX' callbacks

		File_Name: Unbounded_String;
		-- string representing the file name
	end record;

	procedure Start_Element
		(Handler	: in out Reader;
		 Namespace_URI	: Unicode.CES.Byte_Sequence := "";
		 Local_Name	: Unicode.CES.Byte_Sequence := "";
		 Qname		: Unicode.CES.Byte_Sequence := "";
		 Atts		: Sax.Attributes.Attributes'Class);
	
	procedure End_Element
		(Handler	: in out Reader;
		 Namespace_URI	: Unicode.CES.Byte_Sequence := "";
		 Local_Name	: Unicode.CES.Byte_Sequence := "";
		 Qname		: Unicode.CES.Byte_Sequence := "");
	
	procedure Characters
		(Handler: in out Reader;
		Ch	: Unicode.CES.Byte_Sequence);	



	type Parser is new Parser with record
		My_Reader: Reader;
		-- the reader. :P
		My_Cursor: Alos.UString_Ordered_Maps.Cursor;
	end record;

end Ada_Config.Xlm_Parsers;
	
