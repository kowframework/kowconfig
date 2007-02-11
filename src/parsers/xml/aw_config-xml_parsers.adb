-- parser for XML file
--
-- @author Marcelo Coraça de Freitas <marcelo.batera@gmail.com>
-- @createdAt 2007-02-03
-- @lastUpdate



-- Ada Packages
with Ada.Strings.Unbounded;	use Ada.Strings.Unbounded;
with Ada.Characters.Handling;

-- XML/Ada Packages
with Input_Sources.File;
with Sax.Attributes;
with Sax.Readers;		
with Unicode.CES;


with AW_Lib.String_Util;
with AW_Lib.UString_Vectors;
with AW_Lib.Ustring_Ordered_Maps;



package body Aw_Config.Xml_Parsers is

	procedure Prepare(	P: in out Aw_Config.Xml_Parsers.Parser;
				File_Name: in String ) is
		-- prepare the parser to parse the file with the
		-- absolute path File_Name.
		use Input_Sources.File;
		use AW_Lib.UString_Vectors;
		use AW_Lib.UString_Ordered_Maps;
		use Sax.Readers;

		F: File_Input;
	begin
		P.My_Reader.File_Name := To_Unbounded_String( File_Name );
		Open( File_Name, F );
		-- now we remove the features we don't want from XMLAda:
		Set_Feature( P.My_Reader, Namespace_Prefixes_feature, False);
		Set_Feature( P.My_Reader, Namespace_Feature, False);
		Set_Feature( P.My_Reader, Validation_Feature, False);
		
		-- Now we parse the damn file using our Reader:
		Parse( P.My_Reader, F );
		Close( F );

		Clear( P.My_Reader.Section );


		P.My_Cursor := First( P.My_Reader.Values );
	end Prepare;

	procedure Finish( P: in out Parser ) is 
		-- close the file and do whatever it's needed to finish it.
		use AW_Lib.UString_Ordered_Maps;
	begin
		Clear( P.My_Reader.Values );
	end Finish;


	procedure Next( P: in out Parser ) is
		-- move the parser to the next field, if it exists
		-- if not prepare the parser to return CONSTRAINT_ERROR
		-- everytime Key and Value are called
	begin
		AW_Lib.UString_Ordered_Maps.Next( P.My_Cursor );
	end Next;

	function Key( P: in Parser ) return Unbounded_String is
		-- return the key of the current field
		-- raise CONSTRAINT_ERROR if there is nothing else to read
	begin
		return AW_Lib.UString_Ordered_Maps.Key( P.My_Cursor );
	end Key;

	function Element( P: in Parser ) return Unbounded_String is
		-- return the value of the current field
		-- raise CONSTRAINT_ERROR if there is nothing else to read
	begin
		return AW_Lib.UString_Ordered_Maps.Element( P.My_Cursor );
	end Element;


	function Get_File_Name( P: in Parser; Original: in String ) return String is
	begin
		return Original & ".cfg.xml";
	end Get_File_Name;


	-- private

	procedure Start_Element
		(Handler	: in out Reader;
		 Namespace_URI	: Unicode.CES.Byte_Sequence := "";
		 Local_Name	: Unicode.CES.Byte_Sequence := "";
		 Qname		: Unicode.CES.Byte_Sequence := "";
		 Atts		: Sax.Attributes.Attributes'Class) is

		 use Ada.Characters.Handling;
		 use Sax.Attributes;
		 L: Unicode.CES.Byte_Sequence := To_Lower( Local_Name );
	begin
		Handler.Current_Value := Null_Unbounded_String;
		if L = "key" then
			Handler.In_Key := true;
			Handler.Current_Value := To_Unbounded_String( 
				Get_Value( Atts, "", "value" )
				);
			Handler.Current_Key := To_Unbounded_String(
				Get_Value( Atts, "", "name" )
				);
		elsif not Handler.In_Key and L = "section" then
			AW_Lib.UString_Vectors.Append(
				Handler.Section,
				To_Unbounded_String( Get_Value( Atts, "name" ) )
					);
		else
			-- just in case SAX isn't validating
			Raise_Syntax_Error(	To_String( Handler.File_Name ),
						0,
						"Parse Error: " & Local_Name);	
		end if;
	end Start_Element;
	
	procedure End_Element
		(Handler	: in out Reader;
		 Namespace_URI	: Unicode.CES.Byte_Sequence := "";
		 Local_Name	: Unicode.CES.Byte_Sequence := "";
		 Qname		: Unicode.CES.Byte_Sequence := "") is

		 K: Unbounded_String := AW_Lib.String_Util.Implode( '.', Handler.Section );
	begin
		K := K & '.';
		K := K & Handler.Current_key;
		if Handler.In_Key then
			AW_Lib.UString_Ordered_Maps.Include(
				Handler.Values,
				k,
				Handler.Current_Value );
			Handler.Current_Key := Null_Unbounded_String;
			Handler.Current_Key := Null_Unbounded_String;
			Handler.In_Key := false;
		else
			AW_Lib.UString_Vectors.Delete_Last( Handler.Section );
		end if;
	end End_Element;
	
	procedure Characters
		(Handler: in out Reader;
		Ch	: Unicode.CES.Byte_Sequence) is
	begin
		Handler.Current_Value := Handler.Current_Value & Ch;
	end Characters;
end Aw_Config.Xml_Parsers;
	
