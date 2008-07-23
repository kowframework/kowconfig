-------------
-- Ada2005 --
-------------

with Ada.Containers.Ordered_Maps;
with Ada.Exceptions;
with Ada.Text_IO;

--------------
-- AdaWorks --
--------------

with Aw_Config;
with Aw_Lib;
with Aw_Lib.UString_Ordered_Maps;


package body Aw_Config.Generic_Registry is


	procedure pl(Str: in String ) renames Ada.Text_IO.Put_Line;


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
	end Factory_Registry;


	procedure Path_Iterate is new Aw_Config.Generic_Iterate( Path_Iterator => Registry.Iterator );


	procedure Reload_Registry is
		-- escaneia o diretório informado e recria o registro
		Config_Map : Aw_Lib.UString_Ordered_Maps.Map := 
			Aw_Config.Scan_Relative_Path( Relative_Path => Relative_Path, P => Parser );
	begin
		Path_Iterate( Config_Map, Parser );
	end Reload_Registry;


	protected body Registry is

		procedure Iterator( Name: in String; Config: in out Aw_Config.Config_File ) is
			-- this procedure is used internally and shouldn't be used anywhere else!
			-- Reload_Registry utilize this one to iterate over the configuration and call the factories
			
			
			function Get_Type return String is
				My_Type: Unbounded_String;
			begin
				My_Type := Aw_Config.Get_Compulsory_UString( Config, "type" );
				return To_String( My_Type );
			exception
				when CONSTRAINT_ERROR =>
					Aw_Config.Dump_Contents( Config );
					raise CONSTRAINT_ERROR with 
						"Type didn't declare in configuration """
						& Name 
						& "@"
						& Aw_Config.Get_File_Name( Config )
						& """";
			end Get_Type;
			
			Factory		: Element_Factory;
			Factory_Type	: String := Get_Type;

			Element		: Element_Type;

			Element_Name	: Unbounded_String := To_Unbounded_String( Name );
		begin
			if Element_Maps.Contains( My_Map, Element_Name ) then
				raise DUPLICATED_ELEMENT with 
					"Detected duplicated element " &
					To_String(Element_Name) & " in " &
					Aw_Config.Get_File_Name( Config );
			end if;

			Factory := Factory_Registry.Get( Factory_Type );

			begin
				Element := Factory.all( Name, Config );
			exception
				when e : others =>
					Ada.Text_IO.Put_Line(	
						Ada.Text_IO.Standard_Error,
						"Exception while processing the factory '" & Factory_Type & "'"
						);
					Ada.Exceptions.Reraise_Occurrence( e );
			end;

			Element_Maps.Include(
				My_Map,
				Element_Name,
				Element
				);
		end Iterator;


		function Get( Name: in String ) return Element_Type is
			-- pega o elemento informado
		begin
			return Get( To_Unbounded_String( Name ) );
		end Get;


		function Get( Name: in Unbounded_String ) return Element_Type is
			-- pega o elemento informado
		begin
			if not Element_Maps.Contains( My_Map, Name ) then
				raise CONSTRAINT_ERROR with "Element " &
					To_String( Name ) &
					" doesn't exist in map whose Relative_Path is " &
					Relative_Path & ".";
			end if;

			return Element_Maps.Element( My_Map, Name );
		end Get;

	end Registry;

end Aw_Config.Generic_Registry;
