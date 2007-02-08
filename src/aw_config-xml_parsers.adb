-- parser for XML file
--
-- @author Marcelo Coraça de Freitas <marcelo.batera@gmail.com>
-- @createdAt 2007-02-03
-- @lastUpdate



-- Ada Packages
with Ada.Strings_Unbounded;	use Ada.Strings_Unbounded;
with Ada.Characters.Handling;

-- XML/Ada Packages
with Input_Sources.File;
with Sax.Attributes;
with Sax.Readers;
with Unicode.CES;


with Parsers_Interface;


with Alos.UString_Vectors;
with Alos.Ustring_Ordered_Maps;



package Ada_Config.Xml_Parsers is

	procedure Prepare(	P: in out Parser;
				File_Name: in String ) is
		-- prepare the parser to parse the file with the
		-- absolute path File_Name.
		use Input_Sources.File;
		use Alos.UString_Vectors;

		F: File_Input;
	begin
		P.My_Reader.File_Name := To_Unbounded_String( File_Name );
		Open( File_Name, F );
		-- now we remove the features we don't want from XMLAda:
		Set_Feature( My_Blist_Reader, Namespace_Prefixes_feature, False);
		Set_Feature( My_Blist_Reader, Namespace_Feature, False);
		Set_Feature( My_Blist_Reader, Validation_Feature, False);
		
		-- Now we parse the damn file using our Reader:
		Parse( P.My_Reader, F );
		Close( F );

		Clear( P.My_Reader.Section );

		P.My_Cursor := First( P.My_Reader.Values );
	end Prepare;

	procedure Finish( P: in out Parser ) is 
		-- close the file and do whatever it's needed to finish it.
		use Alos.UString_Ordered_Maps;
	begin
		Clear( P.My_Reader.Values );
	end Finish;


	procedure Next( P: in out Parser ) is
		-- move the parser to the next field, if it exists
		-- if not prepare the parser to return CONSTRAINT_ERROR
		-- everytime Key and Value are called
		use Alos.UString_Ordered_Maps;
	begin
		Next( P.My_Cursor );
	end Next;

	function Key( P: in Parser ) return String is
		-- return the key of the current field
		-- raise CONSTRAINT_ERROR if there is nothing else to read
		use Alos.UString_Ordered_Maps;
	begin
		return To_String( Key( P.My_Cursor ) );
	end Key;

	function Element( P: in Parser ) return String is
		-- return the value of the current field
		-- raise CONSTRAINT_ERROR if there is nothing else to read
		use Alos.UString_Ordered_Maps;
	begin
		return To_String( Element( P.My_Cursor ) );
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
		elsif if not Handler.In_Key and L = "section" then
			Alos.UString_Vectors.Append(
				Section,
				To_Unbounded_String(
					Get_Value( Atts, "name" );
					);
		else
			-- just in case SAX isn't validating
			Raise_Syntax_Error(	Handler.File_Name,
						0,
						"Parse Error: " & Local_Name);	
		end if;
	end Start_Element;
	
	procedure End_Element
		(Handler	: in out Reader;
		 Namespace_URI	: Unicode.CES.Byte_Sequence := "";
		 Local_Name	: Unicode.CES.Byte_Sequence := "";
		 Qname		: Unicode.CES.Byte_Sequence := "") is
	begin
		if Handler.In_Key then
			Alos.UString_Ordered_Maps.Include(
				Handler.Values,
				Implode( '.', Handler.Section & '.' & Handler.Current_Key),
				Handler.Current_Value
				);
			Handler.Current_Key := Null_Unbounded_String;
			Handler.Current_Key := Null_Unbounded_String;
			Handler.In_Key := false;
		else
			Alos.UString_Vectos.Delete_Last( Handler.Section );
		end if;
	end End_Element;
	
	procedure Characters
		(Handler: in out Reader;
		Ch	: Unicode.CES.Byte_Sequence) is
	begin
		Handler.Current_Value := Handler.Current_Value & Ch;
	end Characters;
end Ada_Config.Xlm_Parsers;
	
