-------------
-- Ada2005 --
-------------

with Ada.Containers.Ordered_Maps;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Text_IO;

--------------
-- AdaWorks --
--------------

with KOW_Config;
with KOW_Lib;
with KOW_Lib.File_System;
with KOW_Lib.Log;
with KOW_Lib.String_Util;		use KOW_Lib.String_Util;
with KOW_Lib.UString_Ordered_Maps;


package body KOW_Config.Generic_Registry is
	Logger : KOW_Lib.Log.Logger_Type := 
			KOW_Lib.Log.Get_Logger( "KOW_Config.Generic_Registry[" & Relative_Path & "]" );
	

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




	protected body Factory_Registry is
		-- Registro de Factories
		-- Nota: ao criar o registro de elementos (ver Registry a seguir) esse registro é usado
		-- A factory a ser chamada depende do parâmetro de configuração "type".
		-- Assim que a factory é localizada, 

		procedure Register( Factory_Type: in String; Factory: in Element_Factory ) is
			-- Registra um novo factory
			F_Type: Unbounded_String := To_Unbounded_String( Factory_Type );
		begin
			if F_Type /= Null_Unbounded_String then
				if Factory_Maps.Contains( My_Map, F_Type ) then
					raise DUPLICATED_FACTORY_TYPE with 
						"Trying to add duplicated Factory_Type " &
						Factory_Type & ", in map whose Relative_Path is " &
						Relative_Path & ".";
				elsif Factory = Null then
					raise CONSTRAINT_ERROR with "Trying to register Factory_Type " &
						 Factory_Type  & "with null factory " &
						"in map whose Relative_Path is " & Relative_Path & ".";
				end if;

				Factory_Maps.Insert( My_Map, F_Type, Factory );
			else
				raise INVALID_FACTORY_TYPE with 
					"Trying to add Null_Unbounded_String Factory_Type " &
					"in map whose Relative_Path is " & Relative_Path & "." ;
			end if;
		end Register;

		function Get( Factory_Type: in String ) return Element_Factory is
			-- Pega o factory informado.
		begin
			return Get( To_Unbounded_String( Factory_Type ) );
		end Get;

		function Get( Factory_Type: in Unbounded_String ) return Element_Factory is
			-- Pega o factory informado.
		begin
			if not Factory_Maps.Contains( My_Map, Factory_Type ) then
				raise CONSTRAINT_ERROR with "Factory_Type " &
					To_String( Factory_Type ) &
					" doesn't exist in map whose Relative_Path is " &
					Relative_Path & ".";
			end if;

			return Factory_Maps.Element( My_Map, Factory_Type );
		end Get;


		function Get_Ids return KOW_Lib.String_Util.UString_Array is
			Length: Integer := Integer( Factory_Maps.Length( My_Map ) );
		begin
			declare
				Ret_Val: KOW_Lib.String_Util.UString_Array(1 .. Length);
				Ptr    : Integer := 1;
				procedure Iterator( C: in Factory_Maps.Cursor ) is
				begin
					Ret_Val( Ptr ) := Factory_Maps.Key( C );
					Ptr := Ptr + 1;
				end Iterator;
			begin
				Factory_Maps.Iterate( My_Map, Iterator'Access );

				return Ret_Val;
			end;
		end Get_Ids;


		function Get_Ids return KOW_Lib.UString_Vectors.Vector is
			Ret_Val : KOW_Lib.UString_Vectors.Vector;

			procedure Iterator( C : Factory_Maps.Cursor ) is
			begin
				KOW_Lib.Ustring_Vectors.Append( Ret_Val, Factory_Maps.Key( C ) );
			end Iterator;
		begin
			Factory_Maps.Iterate( My_Map, Iterator'Access );
			return Ret_Val;
		end Get_Ids;

	end Factory_Registry;


	procedure Path_Iterate is new KOW_Config.Generic_Iterate( Path_Iterator => Registry.Iterator );


	procedure Reload_Registry is
		-- escaneia o diretório informado e recria o registro
		Config_Map : KOW_Lib.UString_Ordered_Maps.Map := 
			KOW_Config.Scan_Relative_Path( Relative_Path => Relative_Path, P => Parser );
	begin
		Path_Iterate( Config_Map, Parser );
	end Reload_Registry;


	protected body Registry is

		procedure Iterator( Id: in String; Config: in out KOW_Config.Config_File ) is
			-- this procedure is used internally and shouldn't be used anywhere else!
			-- Reload_Registry utilize this one to iterate over the configuration and call the factories
			
			
			function Get_Type return Unbounded_String is
				My_Type: Unbounded_String;
			begin
				My_Type := KOW_Config.Element( Config, "type" );
				return My_Type;
			exception
				when CONSTRAINT_ERROR =>
					KOW_Config.Dump_Contents( Config );
					raise CONSTRAINT_ERROR with 
						"Type didn't declare in configuration """
						& Id 
						& "@"
						& KOW_Config.Get_File_Name( Config )
						& """";
			end Get_Type;
			
			Factory		: Element_Factory;
			Factory_Type	: Unbounded_String := Get_Type;

			Element		: Element_Type;

			Element_Id	: Unbounded_String := To_Unbounded_String( Id );
		begin
			if Element_Maps.Contains( My_Map, Element_Id ) then
				raise DUPLICATED_ELEMENT with 
					"Detected duplicated element " &
					To_String(Element_Id) & " in " &
					KOW_Config.Get_File_Name( Config );
			end if;

			Factory := Factory_Registry.Get( Factory_Type );

			begin
				Element := Factory.all( Id, Config );
			exception
				when e : others =>
					Log(
						"Exception while processing the factory '" & To_String( Factory_Type ) & "'",
						KOW_Lib.Log.Level_Error
						);
					Ada.Exceptions.Reraise_Occurrence( e );
			end;

			Register( Element_Id, Element );
			Create_Factory_Type_Index( Factory_Type, Element_Id );
		end Iterator;
		
		
		procedure Register_And_Save( Element_Id: in String; Config: in out KOW_Config.Config_File ) is
			-- register a new element from it's config file.
			-- also, write this new element to disk;
			use Ada.Text_IO;

			Output_Dir_Name		: Unbounded_String;
			Output_File_Name	: Unbounded_String;
			Output_FIle		: File_Type;

		begin
			
			Output_Dir_Name := KOW_Lib.UString_Vectors.Element( KOW_Config.Get_Config_Path, 0 );
			-- we aways save to the first element in the config path.

			Output_Dir_Name := Output_Dir_Name & To_Unbounded_String( '/' & Relative_Path & '/' );

			Output_File_Name := Output_Dir_Name & To_Unbounded_String( Element_Id );

			Iterator( Element_Id, Config );

			-- if it got here, no exception has been raised... so we can safelly save it to disk.
			
			declare
				F_Id : String := KOW_Lib.File_System.To_System_Path(
							KOW_Config.Get_File_Name( Parser.all, To_String( Output_File_Name ) )
						);
			begin

				-- First we make sure the destination directory exists..
				
			        Ada.Directories.Create_Path( Ada.Directories.Containing_Directory( F_Id ) );


				-- Then the file must be created..

				Create( Output_File, Out_File, F_Id );
			end;

			KOW_Config.Save(
				p	=> Parser.all,
				Config	=> Config,
				File	=> Output_File
				);

			Close( Output_File );
		end Register_And_Save;


		procedure Delete( Element_Id: in String ) is
			use KOW_Config;
			F: Config_File := New_Config_File( Relative_Path & KOW_Lib.File_System.Separator & Element_Id, Parser ); 
			
			UElement_Id : Unbounded_String := To_Unbounded_String( Element_Id );
		begin
			Element_Maps.Delete( My_Map, UElement_Id );
			Remove_Factory_Type_Index( Element_Id => UElement_Id );
			Ada.Directories.Delete_File( Get_File_Name( F ) );
		end Delete;



		procedure Register( Element_Id: in String; Element: in Element_Type ) is
		begin
			Register( Str_Replace( "//", "/", Element_Id ), Element );
		end Register;

		procedure Register( Element_Id: in Unbounded_String; Element: in Element_type ) is
		begin
			Element_Maps.Include(
				My_Map,
				Element_Id,
				Element
				);
		end Register;


		function Contains( Id: in String ) return Boolean is
			-- checks if the element is available
		begin
			return Element_Maps.Contains( My_Map, Str_Replace( "//", "/", Id ) );
		end Contains;



		function Get( Id: in String ) return Element_Type is
			-- pega o elemento informado
		begin
			return Get( Str_Replace( "//", "/", Id ) );
		end Get;


		function Get( Id: in Unbounded_String ) return Element_Type is
			-- pega o elemento informado
			I : Integer := Integer( Element_Maps.Length( My_Map ) );
			
			C : Element_Maps.Cursor := Element_Maps.First( My_Map );
		begin
			if not Element_Maps.Contains( My_Map, Id ) then
				raise CONSTRAINT_ERROR with "Element " &
					To_String( Id ) &
					" doesn't exist in map whose Relative_Path is " &
					Relative_Path & ".";
			end if;

			return Element_Maps.Element( My_Map, Id );
		end Get;

		function Get_Ids return KOW_Lib.String_Util.UString_Array is
			Length: Integer := Integer( Element_Maps.Length( My_Map ) );
		begin
			declare
				Ret_Val: KOW_Lib.String_Util.UString_Array(1 .. Length);
				Ptr    : Integer := 1;
				procedure Iterator( C: in Element_Maps.Cursor ) is
				begin
					Ret_Val( Ptr ) := Element_Maps.Key( C );
					Ptr := Ptr + 1;
				end Iterator;
			begin
				Element_Maps.Iterate( My_Map, Iterator'Access );

				return Ret_Val;
			end;
		end Get_Ids;


		function Get_Ids return KOW_Lib.UString_Vectors.Vector is
			Ret_Val : KOW_Lib.UString_Vectors.Vector;

			procedure Iterator( C : Element_Maps.Cursor ) is
			begin
				KOW_Lib.Ustring_Vectors.Append( Ret_Val, Element_Maps.Key( C ) );
			end Iterator;
		begin
			Element_Maps.Iterate( My_Map, Iterator'Access );
			return Ret_Val;
		end Get_Ids;


		function Get_Ids_by_Type( Factory_Type : in String ) return KOW_Lib.UString_Vectors.Vector is
			-- get the Id for all elements fabricated using the Factory_Type type;
		begin
			return Get_Ids_by_Type( To_Unbounded_String( Factory_Type ) );
		end Get_Ids_by_Type;

		function Get_Ids_by_Type( Factory_Type : in Unbounded_String ) return KOW_Lib.UString_Vectors.Vector is
			-- get the Id for all elements fabricated using the Factory_Type type
		begin
			if Element_Index_Maps.Contains( My_Indexes, Factory_Type ) then
				return Element_Index_Maps.Element( My_Indexes, Factory_Type );
			else
				declare
					Empty : KOW_Lib.UString_Vectors.Vector;
				begin
					return Empty;
				end;
			end if;
		end Get_Ids_by_Type;

		procedure Create_Factory_Type_Index( Factory_Type : in Unbounded_String; Element_Id : in Unbounded_String ) is
			-- create an entry in the element type index for this element.

			use KOW_Lib.UString_Vectors;

			My_Ids: KOW_Lib.UString_Vectors.Vector;
		begin
			if Element_Index_Maps.Contains( My_Indexes, Factory_Type ) then
				My_Ids := Element_Index_Maps.Element( My_Indexes, Factory_Type );
			end if;

			if Find( My_Ids, Element_Id ) = No_Element then
				-- we only add the index if it's not already there
				-- no exception os raised otherwise
				Append( My_Ids, Element_Id );
				Element_Index_Maps.Include( My_Indexes, Factory_Type, My_Ids );
			end if;
		end Create_Factory_Type_Index;

		procedure Remove_Factory_Type_Index( Factory_Type : in Unbounded_String; Element_Id : in Unbounded_String ) is
			-- remove the index, knowing he factory type
			My_Ids	: KOW_Lib.UString_Vectors.Vector;
			C	: KOW_Lib.UString_Vectors.Cursor;
		begin
		
			if not Element_Index_Maps.Contains( My_Indexes, Factory_Type ) then
				raise INVALID_FACTORY_TYPE with "There is no such factory :: """ & To_String( Factory_Type ) & """";
			end if;

			My_Ids := Element_Index_Maps.Element( My_Indexes, Factory_Type );
			C := KOW_Lib.UString_Vectors.Find( My_Ids, Element_Id );
			KOW_Lib.UString_Vectors.Delete( My_Ids, C );

			-- substituimos todos os indices ::

			Element_Index_Maps.Include( My_Indexes, Factory_Type, My_Ids );
		exception
			when CONSTRAINT_ERROR =>
				raise CONSTRAINT_ERROR with "Element not found :: """ & To_String( Element_id ) & """";
		end Remove_Factory_Type_Index;
		
		procedure Remove_Factory_Type_Index( Element_Id : in Unbounded_String ) is
			-- remove the index, not knowing the factory type (quite slow though) 

			Found		: Boolean := False;
			Found_At	: Unbounded_String;
			My_Ids		: KOW_Lib.UString_Vectors.Vector;
			procedure Factory_Iterator( C : Element_Index_Maps.Cursor ) is
				use Element_Index_Maps;
				use KOW_Lib.UString_Vectors;
				EC	: KOW_Lib.UString_Vectors.Cursor;
			begin
				if not Found then
					My_Ids	:= Element( C );
					EC	:= Find( My_Ids, Element_Id );
					if EC /= KOW_Lib.UString_Vectors.No_Element then
						Found_At := Key( C );
						Delete( My_Ids, EC );
						Found := True;
					end if;
				end if;
			end Factory_Iterator;
		begin
			Element_Index_Maps.Iterate( My_Indexes, Factory_Iterator'Access );
			if Found then
				Element_Index_Maps.Include( My_Indexes, Found_At, My_Ids );
			end if;
		end Remove_Factory_Type_Index;

	end Registry;

end KOW_Config.Generic_Registry;
