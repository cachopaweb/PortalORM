unit UnitTabela.Helpers;

interface

uses
	System.Json,
	System.Threading,
	UnitPortalORM.Model,
	System.Rtti,
	UnitClientREST.Model.Interfaces,
	System.SysUtils,
	System.Generics.Collections,
	System.Classes, UnitConnection.Model.Interfaces;

type
	THelperTTabelaREST = class helper for TTabela
	private
		function BuscaBaseURL: string;
		function PreparaFiltros(Filtros: TArray<string>): string;
	public
		function Clone<T: TTabela, constructor>(Tabela: T): T; overload;
		function Clone(Source: TObject): TObject; overload;
		function ToJson: string;
		function ToJsonObject: TJSONObject; overload;
		function ToJsonObject(Source: TObject): TJSONObject; overload;
		function fromJson<T: TTabela, constructor>(Json: string): T;
		function TemBaseURL: Boolean;
		// metodos HTTP
		function Get<T: TTabela, constructor>: TList<T>; overload;
		function Get<T: TTabela, constructor>(QueryParams: TArray<string>): TList<T>; overload;
		function Get<T: TTabela, constructor>(id: integer): T; overload;
		function Post: TClientResult;
		function Put: TClientResult;
		function Delete(id: integer): TClientResult;
		function GetDataComboBox<T: TTabela, constructor>(DisplayField: string; SearchFieldName: string = ''; SearchFieldValue: string = ''): TStringList;
		function PreencheLista<T: TTabela, constructor>(CampoReferencia: string; ValorBusca: integer): TList<T>; overload;
		function PreencheLista<T: TTabela, constructor>(CampoReferencia: string; ValorBusca: string): TList<T>; overload;
		function PreencheLista<T: TTabela, constructor>(_CamposBusca, ValoresBusca: array of string; OrderBy: string = ''): TList<T>; overload;
		function PreencheListaWhere<T: TTabela, constructor>(CondicaoWhere: string; OrderBy: string = ''): TList<T>;
	end;

var
	TodasTarefas: array of ITask;

implementation

uses
	Rest.Json,
	UnitClientREST.Model,
	UnitConfiguracaoServidor.Singleton,
	UnitDatabase;

{ THelperTTabela }
function THelperTTabelaREST.BuscaBaseURL: string;
var
	Tipo    : TRttiType;
	Atributo: TCustomAttribute;
begin
	{ Extract type information for TSomeType type }
	Tipo := Contexto.GetType(Self.ClassType);
	try
		// busca dados da tabela
		for Atributo in Tipo.GetAttributes do
		begin
			if Atributo is TRecursoServidor then
			begin
				Result := TConfiguracaoServidor.BaseURL + '/' + TRecursoServidor(Atributo).Recurso.Replace('/', '');
				Break;
			end;
		end;
	finally
		Tipo.DisposeOf;
	end;
end;

function THelperTTabelaREST.PreencheLista<T>(CampoReferencia: string; ValorBusca: integer): TList<T>;
begin
	Result := PreencheLista<T>(CampoReferencia, ValorBusca.ToString);
end;

function THelperTTabelaREST.PreencheLista<T>(CampoReferencia, ValorBusca: string): TList<T>;
var
	Query    : iQuery;
begin
	Query             := TDatabase.Query;
	Result            := TList<T>.Create;
	try
		Query.Clear;
		Query.Add('SELECT * FROM ' + Self.Nome + ' WHERE ' + CampoReferencia + ' = ' + '''' + ValorBusca + '''');
		Query.Open;
		Query.Dataset.First;
		while not Query.Dataset.Eof do
		begin
			Result.Add(TTabela(T.Create).Create(TDatabase.Connection));
			TTabela(Result[Pred(Result.Count)]).BuscaDadosTabela(Query.Dataset.FieldByName(Self.CampoBusca).AsInteger);
			Query.Dataset.Next;
		end;
	except on E: Exception do
  	raise Exception.Create(E.Message);
	end;
end;

function THelperTTabelaREST.PreencheLista<T>(_CamposBusca, ValoresBusca: array of string; OrderBy: string): TList<T>;
var
	Query    : iQuery;
	CamposAux: string;
	i        : integer;
begin
	CamposAux := '';
	for i     := Low(_CamposBusca) to High(_CamposBusca) do
	begin
		CamposAux := CamposAux + _CamposBusca[i] + ' = ''' + ValoresBusca[i] + ''' AND ';
	end;
	CamposAux := CamposAux.Substring(0, CamposAux.Length - 5);
	//
	Query             := TDatabase.Query;
	Result            := TList<T>.Create;
	try
		Query.Clear;
		Query.Add('SELECT ' + Self.CampoBusca + ' FROM ' + Self.Nome + ' WHERE ' + CamposAux);
		if OrderBy <> '' then
			Query.Add(' ORDER BY ' + OrderBy);
		Query.Open;
		Query.DataSet.First;
		while not Query.DataSet.Eof do
		begin
			Result.Add(TTabela(T.Create).Create(TDatabase.Connection));
			TTabela(Result[Pred(Result.Count)]).BuscaDadosTabela(Query.DataSet.FieldByName(Self.CampoBusca).AsInteger);
			Query.DataSet.Next;
		end;
	except on E: Exception do
  	raise Exception.Create(E.Message);
	end;
end;

function THelperTTabelaREST.PreencheListaWhere<T>(CondicaoWhere, OrderBy: string): TList<T>;
var
	Query    : iQuery;
begin
	Query             := TDatabase.Query;
	Result            := TList<T>.Create;
	try
		Query.Clear;
		Query.Add('SELECT ' + Self.CampoBusca + ' FROM ' + Self.Nome + ' WHERE ' + CondicaoWhere);
		if OrderBy <> '' then
			Query.Add(' ORDER BY ' + OrderBy);
		Query.Open;
		Query.DataSet.First;
		while not Query.DataSet.Eof do
		begin
			Result.Add(TTabela(T.Create).Create(TDatabase.Connection));
			TTabela(Result[Pred(Result.Count)]).BuscaDadosTabela(Query.DataSet.FieldByName(Self.CampoBusca).AsInteger);
			Query.DataSet.Next;
		end;
	except on E: Exception do
  	raise Exception.Create(E.Message);
	end;
end;

function THelperTTabelaREST.PreparaFiltros(Filtros: TArray<string>): string;
begin
	Result := ''.Join('&', Filtros);
end;

function THelperTTabelaREST.Get<T>(QueryParams: TArray<string>): TList<T>;
var
	Response: IFuture<TClientResult>;
	ajson   : TJSONArray;
	ojson   : TJSONValue;
	BaseURL : string;
begin
	BaseURL := BuscaBaseURL;
	Result  := TList<T>.Create;
	try
		// Criando uma tarefa assíncrona
		Response := TTask.Future<TClientResult>(
			function: TClientResult
			begin
				Result := TClientREST.New(BaseURL + '?' + PreparaFiltros(QueryParams)).Get();
			end);
		if Response.Value.StatusCode = 200 then
		begin
			ajson := TJSONObject.ParseJSONValue(Response.Value.Content) as TJSONArray;
			for ojson in ajson do
			begin
				Result.Add(T.Create.fromJson<T>(ojson.ToJson));
			end;
		end
		else
		begin
			raise Exception.Create('Response: ' + Response.Value.Content + sLineBreak + 'Error:' + Response.Value.Error + sLineBreak + 'StatusCode:' + Response.Value.StatusCode.ToString);
		end;
	finally
	end;
end;

function THelperTTabelaREST.Get<T>: TList<T>;
var
	Response: IFuture<TClientResult>;
	ajson   : TJSONArray;
	ojson   : TJSONValue;
	BaseURL : string;
begin
	BaseURL := BuscaBaseURL;
	Result  := TList<T>.Create;
	try
		// Criando uma tarefa assíncrona
		Response := TTask.Future<TClientResult>(
			function: TClientResult
			begin
				Result := TClientREST.New(BaseURL).Get();
			end);
		if Response.Value.StatusCode = 200 then
		begin
			ajson := TJSONObject.ParseJSONValue(Response.Value.Content) as TJSONArray;
			for ojson in ajson do
			begin
				Result.Add(T.Create.fromJson<T>(ojson.ToJson));
			end;
		end
		else
		begin
			raise Exception.Create('Response: ' + Response.Value.Content + sLineBreak + 'Error:' + Response.Value.Error + sLineBreak + 'StatusCode:' + Response.Value.StatusCode.ToString);
		end;
	finally
	end;
end;

function THelperTTabelaREST.GetDataComboBox<T>(DisplayField: string; SearchFieldName: string = ''; SearchFieldValue: string = ''): TStringList;
var
	ajson   : TJSONArray;
	Response: TClientResult;
	Json    : TJSONValue;
	Model   : T;
	BaseURL : string;
begin
	Result := TStringList.Create;
	Result.Clear;
	ajson := TJSONArray.Create;
	try
		BaseURL := BuscaBaseURL;
		if not SearchFieldName.IsEmpty then
			Response := TClientREST.New(BaseURL + '?' + SearchFieldName + '=' + SearchFieldValue).Get()
		else
			Response := TClientREST.New(BaseURL).Get();
		if Response.StatusCode = 200 then
		begin
			ajson := TJSONObject.ParseJSONValue(Response.Content) as TJSONArray;
			for Json in ajson do
			begin
				Model := T.Create;
				Result.AddObject(Json.GetValue<string>(DisplayField), Model.fromJson<T>(Json.ToJson));
			end;
		end;
	finally
		ajson.DisposeOf;
	end;
end;

function THelperTTabelaREST.Clone<T>(Tabela: T): T;
var
	Tipo             : TRttiType;
	Propriedade      : TRttiProperty;
	ListaPropriedades: TList<TRttiProperty>;
	Valor            : TValue;
	index            : integer;
begin
	{ extraindo dados da tabela a ser clonada }
	ListaPropriedades := TList<TRttiProperty>.Create;
	Tipo              := Contexto.GetType(Tabela.ClassType);
	try
		// busca dados da tabela
		for Propriedade in Tipo.GetProperties do
		begin
			ListaPropriedades.Add(Propriedade)
		end;
		// clonando os dados da tabela
		Tipo := Contexto.GetType(Self.ClassType);
		// busca dados da tabela
		for Propriedade in Tipo.GetProperties do
		begin
			if ListaPropriedades.Contains(Propriedade) and (not Propriedade.Name.Contains('RefCount')) and (not Propriedade.Name.Contains('Disposed')) then
			begin
				index := ListaPropriedades.IndexOf(Propriedade);
				Propriedade.SetValue(Pointer(Self), ListaPropriedades.Items[index].GetValue(Pointer(Tabela)));
			end;
		end;
	finally
		Tipo.DisposeOf;
		ListaPropriedades.DisposeOf;
	end;
	Result := T(Self);
end;

function THelperTTabelaREST.Clone(Source: TObject): TObject;
var
	ctx     : TRttiContext;
	typ     : TRttiType;
	prop    : TRttiProperty;
	Instance: TObject;
begin
	ctx := TRttiContext.Create;
	try
		typ      := ctx.GetType(Source.ClassType);
		Instance := Source.ClassType.Create; // Cria um novo objeto do mesmo tipo

		for prop in typ.GetProperties do
		begin
			if prop.IsWritable then
				prop.SetValue(Instance, prop.GetValue(Source)); // Copia valores das propriedades
		end;

		Result := Instance;
	finally
		ctx.Free;
	end;
end;

function THelperTTabelaREST.Delete(id: integer): TClientResult;
var
	BaseURL : string;
	Response: IFuture<TClientResult>;
begin
	BaseURL  := BuscaBaseURL;
	Response := TTask.Future<TClientResult>(
		function: TClientResult
		begin
			// envia para o servidor
			Result := TClientREST.New(BaseURL + '/' + id.ToString).AddHeader('Content-Type', 'application/json').Delete();
		end);
	Result := Response.Value;
end;

function THelperTTabelaREST.Post: TClientResult;
var
	ojson   : TJSONObject;
	BaseURL : string;
	Response: IFuture<TClientResult>;
begin
	BaseURL := BuscaBaseURL;
	// transforma em json
	ojson    := ToJsonObject(Clone(Self));
	Response := TTask.Future<TClientResult>(
		function: TClientResult
		begin
			// envia para o servidor
			Result := TClientREST.New(BaseURL).AddHeader('Content-Type', 'application/json').AddBody(ojson).Post();
		end);
	Result := Response.Value;
end;

function THelperTTabelaREST.Put: TClientResult;
var
	ojson   : TJSONObject;
	BaseURL : string;
	Response: IFuture<TClientResult>;
begin
	BaseURL := BuscaBaseURL;
	// transforma em json
	ojson    := ToJsonObject(Clone(Self));
	Response := TTask.Future<TClientResult>(
		function: TClientResult
		begin
			// envia para o servidor
			Result := TClientREST.New(BaseURL).AddHeader('Content-Type', 'application/json').AddBody(ojson).Put();
		end);
	Result := Response.Value;
end;

function THelperTTabelaREST.fromJson<T>(Json: string): T;
var
	Model: T;
begin
	Model  := TJson.JsonToObject<T>(Json);
	Result := T(Self.Clone<T>(Model));
end;

function THelperTTabelaREST.Get<T>(id: integer): T;
var
	BaseURL : string;
	Response: IFuture<TClientResult>;
	ajson   : TJSONArray;
	i       : integer;
	ojson   : TJSONObject;
begin
	BaseURL := BuscaBaseURL;
	try
		// Criando uma tarefa assíncrona
		Response := TTask.Future<TClientResult>(
			function: TClientResult
			begin
				Result := TClientREST.New(BaseURL + '/' + id.ToString).Get();
			end);
		if Response.Value.StatusCode = 200 then
		begin
			ojson := TJSONObject.ParseJSONValue(Response.Value.Content) as TJSONObject;
			if not ojson.ToJson.IsEmpty then
				Result := Self.fromJson<T>(ojson.ToJson);
		end
		else
			raise Exception.Create(Response.Value.Error);
	finally
	end;
end;

function THelperTTabelaREST.TemBaseURL: Boolean;
begin
	Result := not BuscaBaseURL.IsEmpty
end;

function THelperTTabelaREST.ToJson: string;
begin
	Result := TJson.ObjectToJsonString(Self);
end;

function THelperTTabelaREST.ToJsonObject(Source: TObject): TJSONObject;
begin
	Result := TJson.ObjectToJsonObject(Source);
end;

function THelperTTabelaREST.ToJsonObject: TJSONObject;
begin
	Result := TJson.ObjectToJsonObject(Self);
end;

end.
