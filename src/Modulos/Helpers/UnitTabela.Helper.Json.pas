unit UnitTabela.Helper.Json;

interface

uses
  System.Json,
  System.Threading,
  UnitPortalORM.Model,
  System.Rtti,
  UnitClientREST.Model.Interfaces,
  System.SysUtils,
  IBX.IBDatabase,
  IBX.IBQuery, System.Generics.Collections, System.Classes;

type
  THelperTTabelaREST = class helper for TTabela
  private
    function BuscaBaseURL: string;
    function Clone<T: TTabela, constructor>(Tabela: T): T;
  public
    function ToJson: string;
    function fromJson<T: TTabela, constructor>(Json: string): T;
    function TemBaseURL: Boolean;
    // metodos HTTP
		function Get<T: TTabela, constructor>: TList<T>;overload;
    function Get<T: TTabela, constructor>(id: integer): T;overload;
    function Post: TClientResult;
    function Put: TClientResult;
		function Delete(id: integer): TClientResult;
		function GetDataComboBox<T: TTabela, constructor>(DisplayField: string; SearchField: string = ''): TStringList;
	end;

implementation

uses
  Rest.Json,
  UnitClientREST.Model,
  UnitConfiguracaoServidor.Singleton;

{ THelperTTabela }
function THelperTTabelaREST.BuscaBaseURL: string;
var
  Tipo: TRttiType;
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
        Result := TConfiguracaoServidor.BaseURL+'/'+TRecursoServidor(Atributo).Recurso.Replace('/', '');
        Break;
      end;
    end;
  finally
    Tipo.DisposeOf;
  end;
end;

function THelperTTabelaREST.Get<T>: TList<T>;
var
  Response: TClientResult;
  ajson: TJSONArray;
  i: integer;
  ojson: TJSONValue;
  BaseURL: string;
  FutureResponse: IFuture<TClientResult>;
begin
	Result := TList<T>.Create;
	BaseURL  := BuscaBaseURL;
	FutureResponse := TTask.Future<TClientResult>(
    function: TClientResult
    begin
      Result := TClientREST.New(BaseURL).Get()
    end);
  if FutureResponse.Value.StatusCode = 200 then
  begin
		ajson  := TJSONObject.ParseJSONValue(FutureResponse.Value.Content) as TJSONArray;
		for ojson in ajson do
		begin
			Result.Add(Self.fromJson<T>(ojson.ToJson));
		end;			
  end
  else
  begin
    raise Exception.Create('Response: '+FutureResponse.Value.Content+sLineBreak
                          +'Error:'+FutureResponse.Value.Error+sLineBreak
                          +'StatusCode:'+FutureResponse.Value.StatusCode.ToString);
  end;
end;

function THelperTTabelaREST.GetDataComboBox<T>(DisplayField: string; SearchField: string = ''): TStringList;
var
  aJson: TJSONArray;
  Response: TClientResult;
	json: TJSONValue;
	model: T;
	BaseURL: string;
begin
	Result := TStringList.Create;
  Result.Clear;
  aJson := TJSONArray.Create;
	try
		BaseURL  := BuscaBaseURL;
		if not SearchField.IsEmpty then		
			Response := TClientREST.New(BaseURL+'/'+SearchField).Get()
		else
			Response := TClientREST.New(BaseURL).Get();
		if Response.StatusCode = 200 then
    begin
      aJson := TJSONObject.ParseJSONValue(Response.Content) as TJSONArray;
			for json in aJson do
      begin
				 model := T.Create;
				 Result.AddObject(json.GetValue<string>(DisplayField), model.fromJson<T>(json.ToJSON));
      end;
    end;
  finally
    aJson.DisposeOf;
	end;
end;

function THelperTTabelaREST.Clone<T>(Tabela: T): T;
var
  Tipo: TRttiType;
  Propriedade: TRttiProperty;
  ListaPropriedades: TList<TRttiProperty>;
  Valor: TValue;
  index: Integer;
begin
  { extraindo dados da tabela a ser clonada }
  ListaPropriedades := TList<TRttiProperty>.Create;
  Tipo := Contexto.GetType(Tabela.ClassType);
  try
    // busca dados da tabela
    for Propriedade in Tipo.GetProperties do
    begin
      ListaPropriedades.Add(Propriedade)
    end;
    //clonando os dados da tabela
    Tipo := Contexto.GetType(Self.ClassType);
    // busca dados da tabela
    for Propriedade in Tipo.GetProperties do
    begin
      if ListaPropriedades.Contains(Propriedade) then
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

function THelperTTabelaREST.Delete(id: integer): TClientResult;
var
  BaseURL: string;
begin
  BaseURL := BuscaBaseURL;
  // envia para o servidor
  Result := TClientREST.New(BaseURL + '/' + id.ToString).AddHeader('Content-Type', 'application/json').Delete();
end;

function THelperTTabelaREST.Post: TClientResult;
var
  ojson: TJSONObject;
  BaseURL: string;
begin
  BaseURL := BuscaBaseURL;
  // transforma em json
  ojson := TJSONObject.ParseJSONValue(TEncoding.UTF8.GetBytes(Self.ToJson), 0) as TJSONObject;
  // envia para o servidor
  Result := TClientREST.New(BaseURL).AddHeader('Content-Type', 'application/json').AddBody(ojson).Post();
end;

function THelperTTabelaREST.Put: TClientResult;
var
  ojson: TJSONObject;
  BaseURL: string;
begin
  BaseURL := BuscaBaseURL;
  // transforma em json
  ojson := TJSONObject.ParseJSONValue(TEncoding.UTF8.GetBytes(Self.ToJson), 0) as TJSONObject;
  // envia para o servidor
  Result := TClientREST.New(BaseURL).AddHeader('Content-Type', 'application/json').AddBody(ojson).Put();
end;

function THelperTTabelaREST.fromJson<T>(Json: string): T;
var
  model: T;
begin
  model := TJson.JsonToObject<T>(Json);
  Result := T(Self.Clone<T>(model));
end;

function THelperTTabelaREST.Get<T>(id: integer): T;
var
  Response: TClientResult;
  ajson: TJSONArray;
  i: integer;
  ojson: TJSONObject;
  BaseURL: string;
  FutureResponse: IFuture<TClientResult>;
begin
  BaseURL  := BuscaBaseURL;
  FutureResponse := TTask.Future<TClientResult>(
    function: TClientResult
    begin
      Result := TClientREST.New(BaseURL+'/'+Id.ToString).Get()
    end);
  if FutureResponse.Value.StatusCode = 200 then
  begin
		ojson  := TJSONObject.ParseJSONValue(FutureResponse.Value.Content) as TJSONObject;
		if not ojson.ToJSON.IsEmpty then
			Result := Self.fromJson<T>(ojson.ToJSON);
  end
  else
    raise Exception.Create(FutureResponse.Value.Error);
end;

function THelperTTabelaREST.TemBaseURL: Boolean;
begin
  Result := not BuscaBaseURL.IsEmpty
end;

function THelperTTabelaREST.ToJson: string;
begin
  Result := TJson.ObjectToJsonString(Self);
end;

end.
