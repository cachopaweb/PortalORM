unit UnitPrincipal;

interface

uses
	ToolsAPI,
	SysUtils,
	Vcl.Dialogs,
	Classes,
	Vcl.Menus,
	Vcl.Forms,
	Winapi.Windows;

type
	TModelWizard = class(TNotifierObject, IOTAWizard)
	private
		FMainMenu: TMainMenu;
		NewItem  : TMenuItem;
		PopupMenu: TPopupMenu;
		function LoadTemplate: string;
		function GetSelectedUnitPath: string;
		function GetSelectedUnitName: string;
		procedure ClickMenu(Sender: TObject);
	public
		function GetIDString: string;
		function GetName: string;
		function GetState: TWizardState;
		procedure Execute;
		constructor Create;
		destructor Destroy; override;
	end;

procedure Register;

implementation

uses
	Winapi.Messages,
	Vcl.Controls;

procedure Register;
begin
	(BorlandIDEServices as IOTAWizardServices).AddWizard(TModelWizard.Create);
end;

function TModelWizard.GetIDString: string;
begin
	Result := 'Delphi.ModelWizard';
end;

function TModelWizard.GetName: string;
begin
	Result := 'Gerador de Model Controller';
end;

function TModelWizard.GetState: TWizardState;
begin
	Result := [wsEnabled];
end;

function TModelWizard.LoadTemplate: string;
var
	CodUnit: TStringList;
begin
	CodUnit := TStringList.Create;
	try
		CodUnit.Add('unit Unit|modelName|.Controller;');
		CodUnit.Add('');
		CodUnit.Add('interface');
		CodUnit.Add('uses');
		CodUnit.Add('  Horse,');
		CodUnit.Add('  Horse.Commons,');
    CodUnit.Add('  Horse.GBSwagger,');    
		CodUnit.Add('  Classes,');
		CodUnit.Add('  SysUtils,');
		CodUnit.Add('  System.Json;');
		CodUnit.Add('');
		CodUnit.Add('type');
		CodUnit.Add('  T|modelName|Controller = class');
		CodUnit.Add('    class procedure Router;');
		CodUnit.Add('    class procedure Get(Req: THorseRequest; Res: THorseResponse);');
		CodUnit.Add('    class procedure GetForID(Req: THorseRequest; Res: THorseResponse);');
		CodUnit.Add('    class procedure Post(Req: THorseRequest; Res: THorseResponse);');
		CodUnit.Add('    class procedure Put(Req: THorseRequest; Res: THorseResponse);');
		CodUnit.Add('    class procedure Delete(Req: THorseRequest; Res: THorseResponse);');
		CodUnit.Add('  end;');
		CodUnit.Add('');
		CodUnit.Add('implementation');
		CodUnit.Add('');
		CodUnit.Add('{ T|modelName|Controller }');
		CodUnit.Add('');
		CodUnit.Add('uses');
		CodUnit.Add('  UnitConnection.Model.Interfaces,');
		CodUnit.Add('  UnitDatabase,');
		CodUnit.Add('  UnitFunctions,');
		CodUnit.Add('  Unit|modelName|.Model,');
    CodUnit.Add('  UnitConstants,');    
		CodUnit.Add('  UnitTabela.Helpers;');
		CodUnit.Add('');
		CodUnit.Add('class procedure T|modelName|Controller.Delete(Req: THorseRequest; Res: THorseResponse);');
		CodUnit.Add('var |modelName|: T|modelName|;');
		CodUnit.Add('  id: Integer;');
		CodUnit.Add('begin');
		CodUnit.Add('  try');
		CodUnit.Add('    id := Req.Params.Items[''id''].ToInteger();');
		CodUnit.Add('    |modelName| := T|modelName|.Create(TDatabase.Connection);');
		CodUnit.Add('    |modelName|.Apagar(id);');
		CodUnit.Add('    Res.Send('''').Status(THTTPStatus.NoContent);');
		CodUnit.Add('  finally');
		CodUnit.Add('    |modelName|.DisposeOf;');
		CodUnit.Add('  end;');
		CodUnit.Add('end;');
		CodUnit.Add('');
		CodUnit.Add('class procedure T|modelName|Controller.Get(Req: THorseRequest; Res: THorseResponse);');
    CodUnit.Add('var');
    CodUnit.Add('  |modelName|: T|modelName|;');
    CodUnit.Add('  aJson: TJSONArray;');
    CodUnit.Add('  Query: iQuery;');
    CodUnit.Add('  Filtros: TStringList;');
    CodUnit.Add('  ParamName, ParamValue, QueryParams: string;');
    CodUnit.Add('  i: Integer;');
    CodUnit.Add('  Limite: Integer;');
    CodUnit.Add('  Pagina: Integer;');
    CodUnit.Add('  Pular: Integer;');
    CodUnit.Add('  SQLBase: string;');
    CodUnit.Add('  WhereClause: string;');
    CodUnit.Add('begin');
    CodUnit.Add('  aJson := TJSONArray.Create;');
    CodUnit.Add('  Query := TDatabase.Query;');
    CodUnit.Add('  |modelName| := T|modelName|.Create(TDatabase.Connection);');
    CodUnit.Add('  |modelName|.CriaTabela;');
    CodUnit.Add('  Filtros := TStringList.Create;');
    CodUnit.Add('  try');
    CodUnit.Add('    // Obtem parametros de paginacao (page e limit)');
    CodUnit.Add('    Limite := 10; // Valor padrao');
    CodUnit.Add('    Pagina := 1;  // Valor padrao (primeira pagina)');
    CodUnit.Add('    ');
    CodUnit.Add('    if Req.Query.ContainsKey(''limit'') then');
    CodUnit.Add('      Limite := Req.Query.Items[''limit''].ToInteger();');
    CodUnit.Add('    if Req.Query.ContainsKey(''page'') then');
    CodUnit.Add('      Pagina := Req.Query.Items[''page''].ToInteger();');
    CodUnit.Add('    ');
    CodUnit.Add('    // Calcula o SKIP baseado na pagina e limite');
    CodUnit.Add('    if Pagina < 1 then');
    CodUnit.Add('      Pagina := 1;');
    CodUnit.Add('    Pular := (Pagina - 1) * Limite;');
    CodUnit.Add('    ');
    CodUnit.Add('    // Monta SELECT com paginacao');
    CodUnit.Add('    if Limite > 0 then');
    CodUnit.Add('      SQLBase := Format(''SELECT FIRST %d SKIP %d DISTINCT |prefix|_CODIGO FROM |Tabela|'', [Limite, Pular])');
    CodUnit.Add('    else');
    CodUnit.Add('      SQLBase := ''SELECT DISTINCT |prefix|_CODIGO FROM |Tabela|'';');
    CodUnit.Add('');
    CodUnit.Add('    // Monta filtros dinamicos');
    CodUnit.Add('    for QueryParams in Req.Query.Dictionary.Keys do');
    CodUnit.Add('    begin');
    CodUnit.Add('    	ParamName := QueryParams.ToUpper;');
    CodUnit.Add('      ParamValue := Req.Query.Items[ParamName].Replace('''''''', '''');');
    CodUnit.Add('');
    CodUnit.Add('      // Ignora par metros de controle');
    CodUnit.Add('      if (ParamName = ''LIMIT'') or (ParamName = ''PAGE'') then');
    CodUnit.Add('        Continue;');
    CodUnit.Add('');
    CodUnit.Add('      // Adiciona filtro com LIKE para texto');
    CodUnit.Add('      if not ParamValue.IsEmpty then');
    CodUnit.Add('        Filtros.Add(Format(''%s LIKE %s'', [ParamName, QuotedStr(''%'' + ParamValue + ''%'')]));');
    CodUnit.Add('    end;');
    CodUnit.Add('');
    CodUnit.Add('    // Monta SQL final');
    CodUnit.Add('    Query.Add(SQLBase);');
    CodUnit.Add('    if Filtros.Count > 0 then');
    CodUnit.Add('    begin');
    CodUnit.Add('      WhereClause := ''WHERE '' + String.Join('' OR '', Filtros.ToStringArray);');
    CodUnit.Add('      Query.Add(WhereClause);');
    CodUnit.Add('    end;');
    CodUnit.Add('    Query.Add(''ORDER BY |prefix|_CODIGO'');');
    CodUnit.Add('    Query.Open;');
    CodUnit.Add('');
    CodUnit.Add('    // Monta JSON de retorno');
    CodUnit.Add('    Query.Dataset.First;');
    CodUnit.Add('    while not Query.Dataset.Eof do');
    CodUnit.Add('    begin');
    CodUnit.Add('      |modelName|.BuscaDadosTabela(Query.Dataset.FieldByName(''|prefix|_CODIGO'').AsInteger);');
    CodUnit.Add('      aJson.Add(TJSONObject.ParseJSONValue(|modelName|.ToJson) as TJSONObject);');
    CodUnit.Add('      Query.Dataset.Next;');
    CodUnit.Add('    end;');
    CodUnit.Add('');
    CodUnit.Add('    Res.Send<TJSONArray>(aJson);');
    CodUnit.Add('  finally');
    CodUnit.Add('    Filtros.Free;');
    CodUnit.Add('    |modelName|.DisposeOf;');
    CodUnit.Add('  end;');    
    CodUnit.Add('end;');
    CodUnit.Add('');
		CodUnit.Add('class procedure T|modelName|Controller.GetForID(Req: THorseRequest; Res: THorseResponse);');
		CodUnit.Add('var |modelName|: T|modelName|;');
		CodUnit.Add('    aJson: TJSONArray;');
		CodUnit.Add('    id: Integer;');
		CodUnit.Add('begin');
		CodUnit.Add('  aJson := TJSONArray.Create;');
		CodUnit.Add('  id := Req.Params.Items[''id''].ToInteger();');
		CodUnit.Add('  try');
		CodUnit.Add('    |modelName| := T|modelName|.Create(TDatabase.Connection);');
		CodUnit.Add('    |modelName|.CriaTabela;');
		CodUnit.Add('    |modelName|.BuscaDadosTabela(id);');
		CodUnit.Add('    Res.Send<TJSONObject>(|modelName|.ToJsonObject);');
		CodUnit.Add('  finally');
		CodUnit.Add('    |modelName|.DisposeOf;');
		CodUnit.Add('  end;');
		CodUnit.Add('end;');
		CodUnit.Add('');
		CodUnit.Add('class procedure T|modelName|Controller.Post(Req: THorseRequest; Res: THorseResponse);');
		CodUnit.Add('var |modelName|: T|modelName|;');
		CodUnit.Add('begin');
		CodUnit.Add('  try');
		CodUnit.Add('    |modelName| := T|modelName|.Create(TDatabase.Connection).fromJson<T|modelName|>(Req.Body);');
		CodUnit.Add('    |modelName|.CriaTabela;');
		CodUnit.Add('    if |modelName|.Codigo = 0 then');
		CodUnit.Add('        |modelName|.Codigo := GeraCodigo(''|Tabela|'', ''|prefix|_CODIGO'');');
		CodUnit.Add('    |modelName|.SalvaNoBanco(1);');
		CodUnit.Add('    Res.Send<TJSONObject>(|modelName|.ToJsonObject);');
		CodUnit.Add('  finally');
		CodUnit.Add('    |modelName|.DisposeOf;');
		CodUnit.Add('  end;');
		CodUnit.Add('end;');
		CodUnit.Add('');
		CodUnit.Add('class procedure T|modelName|Controller.Put(Req: THorseRequest; Res: THorseResponse);');
		CodUnit.Add('var |modelName|: T|modelName|;');
		CodUnit.Add('begin');
		CodUnit.Add('  try');
		CodUnit.Add('    |modelName| := T|modelName|.Create(TDatabase.Connection).fromJson<T|modelName|>(Req.Body);');
		CodUnit.Add('    |modelName|.CriaTabela;');
		CodUnit.Add('    |modelName|.SalvaNoBanco(1);');
		CodUnit.Add('    Res.Send<TJSONObject>(|modelName|.ToJsonObject);');
		CodUnit.Add('  finally');
		CodUnit.Add('    |modelName|.DisposeOf;');
		CodUnit.Add('  end;');
		CodUnit.Add('end;');
		CodUnit.Add('');
		CodUnit.Add('class procedure T|modelName|Controller.Router;');
		CodUnit.Add('begin');
		CodUnit.Add('  THorse.Group');
		CodUnit.Add('        .Prefix(''/v1'')');
		CodUnit.Add('        .Route(''/|route|'')');
		CodUnit.Add('          .Get(Get)');
		CodUnit.Add('          .Post(Post)');
		CodUnit.Add('          .Put(Put)');
		CodUnit.Add('        .&End');
		CodUnit.Add('        .Group');
		CodUnit.Add('        .Prefix(''/v1'')');
		CodUnit.Add('        .Route(''/|route|/:id'')');
		CodUnit.Add('          .Get(GetForID)');
		CodUnit.Add('          .Delete(Delete)');
		CodUnit.Add('        .&End');
		CodUnit.Add('end;');
		CodUnit.Add('');
    CodUnit.Add('initialization');
    CodUnit.Add('    Swagger');
    CodUnit.Add('	.BasePath(''v1'')');
    CodUnit.Add('    .Path(''|route|'')');
    CodUnit.Add('      .Tag(''|modelName|'')');
    CodUnit.Add('      .GET(''Lista Todos(as)'', ''Lista todos(as) os(as) |modelName|s'')');
    CodUnit.Add('        .AddResponse(200, ''Operação bem Sucedida'')');
    CodUnit.Add('          .Schema(T|modelName|)');
    CodUnit.Add('          .IsArray(True)');
    CodUnit.Add('        .&End');
    CodUnit.Add('        .AddResponse(400).&End');
    CodUnit.Add('        .AddResponse(500).&End');
    CodUnit.Add('      .&End');
    CodUnit.Add('      .POST(''Criar |modelName|'', ''Cria um(a) novo(a) |modelName|'')');
    CodUnit.Add('        .AddParamBody(''Dados do(a) |modelName|'', ''|modelName|'')');
    CodUnit.Add('          .Required(True)');
    CodUnit.Add('          .Schema(T|modelName|)');
    CodUnit.Add('        .&End');
    CodUnit.Add('        .AddResponse(201, ''Created'')');
    CodUnit.Add('          .Schema(T|modelName|)');
    CodUnit.Add('        .&End');
    CodUnit.Add('        .AddResponse(400, ''BadRequest'')');
    CodUnit.Add('          .Schema(TAPIError)');
    CodUnit.Add('        .&End');
    CodUnit.Add('        .AddResponse(500).&End');
    CodUnit.Add('      .&End');
    CodUnit.Add('      .PUT(''Atualiza |modelName|'', ''Atualiza os dados de um(a) |modelName|'')');
    CodUnit.Add('        .AddParamBody(''Dados do(a) |modelName|'', ''|modelName|'')');
    CodUnit.Add('          .Required(True)');
    CodUnit.Add('          .Schema(T|modelName|)');
    CodUnit.Add('        .&End');
    CodUnit.Add('        .AddResponse(200, ''Ok'')');
    CodUnit.Add('          .Schema(T|modelName|)');
    CodUnit.Add('        .&End');
    CodUnit.Add('        .AddResponse(400, ''BadRequest'')');
    CodUnit.Add('          .Schema(TAPIError)');
    CodUnit.Add('        .&End');
    CodUnit.Add('        .AddResponse(500).&End');
    CodUnit.Add('      .&End');
    CodUnit.Add('    .&End');
    CodUnit.Add('  .&End');
    CodUnit.Add('  .BasePath(''v1'')');
    CodUnit.Add('    .Path(''|route|/{id}'')');
    CodUnit.Add('      .Tag(''|modelName|'')');
    CodUnit.Add('      .GET(''Obtem um(a) |modelName|'')');
    CodUnit.Add('        .AddParamPath(''id'', ''Id do(a) |modelName| para buscar'')');
    CodUnit.Add('          .Required(True)');
    CodUnit.Add('          .Schema(SWAG_INTEGER)');
    CodUnit.Add('        .&End');
    CodUnit.Add('        .AddResponse(200, ''Operação bem Sucedida'')');
    CodUnit.Add('          .Schema(T|modelName|)');
    CodUnit.Add('        .&End');
    CodUnit.Add('        .AddResponse(404, ''|modelName| não encontrado(a)'').&End');
    CodUnit.Add('        .AddResponse(400, ''BadRequest'')');
    CodUnit.Add('          .Schema(TAPIError)');
    CodUnit.Add('        .&End');
    CodUnit.Add('        .AddResponse(500).&End');
    CodUnit.Add('      .&End');
    CodUnit.Add('      .DELETE(''Apagar um(a) |modelName|'')');
    CodUnit.Add('        .AddParamPath(''id'', ''id do(a) |modelName| para deletar'')');
    CodUnit.Add('          .Required(True)');
    CodUnit.Add('          .Schema(SWAG_INTEGER)');
    CodUnit.Add('        .&End');
    CodUnit.Add('        .AddResponse(404, ''|modelName| não encontrado(a)'').&End');
    CodUnit.Add('        .AddResponse(400, ''BadRequest'')');
    CodUnit.Add('          .Schema(TAPIError)');
    CodUnit.Add('        .&End');
    CodUnit.Add('        .AddResponse(500).&End');
    CodUnit.Add('      .&End');
    CodUnit.Add('    .&End');
    CodUnit.Add('  .&End');
    CodUnit.Add('');
		CodUnit.Add('end.');
    Result := CodUnit.Text;
	finally
		CodUnit.DisposeOf;
	end;
end;

procedure TModelWizard.ClickMenu(Sender: TObject);
var
	ModuleServices: IOTAModuleServices;
	Module        : IOTAModule;
	Editor        : IOTAEditor;
begin
	ModuleServices := BorlandIDEServices as IOTAModuleServices;
	Module         := ModuleServices.CurrentModule;
	if Assigned(Module) then
	begin
		Editor := Module.GetModuleFileEditor(0) as IOTASourceEditor;
		if Assigned(Editor) then
			ShowMessage('Unit atual: ' + Editor.FileName);
    Execute;
	end;
end;

constructor TModelWizard.Create;
begin
	FMainMenu := (BorlandIDEServices as INTAServices).MainMenu;

	NewItem         := TMenuItem.Create(FMainMenu);
  NewItem.Name 		:= 'NewMenuItem1';
	NewItem.Caption := 'Criar Controller REST';
	NewItem.OnClick := ClickMenu;

	FMainMenu.Items.Add(NewItem);
end;

destructor TModelWizard.Destroy;
begin
	NewItem.DisposeOf;
	inherited;
end;

procedure TModelWizard.Execute;
var
	ModelName, Prefix, Tabela, Rota: string;
	OutputPath, NewUnit            : string;
	Template                       : TStringList;
begin
	// Entrada individual para cada campo
	if not InputQuery('Model Controller - ModelName', 'Informe o nome do model:', ModelName) then
		Exit;

	if not InputQuery('Model Controller - Prefix', 'Informe o prefixo:', Prefix) then
		Exit;

	if not InputQuery('Model Controller - Tabela', 'Informe o nome da tabela:', Tabela) then
		Exit;

	if not InputQuery('Model Controller - Route', 'Informe a rota:', Rota) then
		Exit;

	Template := TStringList.Create;
	try
		// Aqui você pode carregar o template como arquivo externo ou hardcoded
		Template.Text := LoadTemplate; // Suponha que você criou essa função

		// Substituir os campos
		Template.Text := StringReplace(Template.Text, '|modelName|', ModelName, [rfReplaceAll]);
		Template.Text := StringReplace(Template.Text, '|prefix|', Prefix, [rfReplaceAll]);
		Template.Text := StringReplace(Template.Text, '|Tabela|', Tabela, [rfReplaceAll]);
		Template.Text := StringReplace(Template.Text, '|route|', Rota, [rfReplaceAll]);

		// Caminho de saída: mesma pasta da unit selecionada
		OutputPath := GetSelectedUnitPath; // função que você pode criar usando OTA

		NewUnit := OutputPath + 'Unit' + ModelName + '.Controller.pas';
		Template.SaveToFile(NewUnit); // função que salva o arquivo

		ShowMessage('Unit criada com sucesso: ' + NewUnit);
	finally
		Template.DisposeOf;
	end;
end;

function TModelWizard.GetSelectedUnitPath: string;
var
	ModuleServices: IOTAModuleServices;
	Module        : IOTAModule;
	Editor        : IOTAEditor;
	i             : Integer;
begin
	Result         := '';
	ModuleServices := BorlandIDEServices as IOTAModuleServices;

	// Obtém o módulo ativo
	Module := ModuleServices.CurrentModule;
	if Assigned(Module) then
	begin
		// Itera pelos editores do módulo
		for i := 0 to Module.GetModuleFileCount - 1 do
		begin
			Editor := Module.GetModuleFileEditor(i);
			if Assigned(Editor) then
			begin
				Result := ExtractFilePath(Editor.FileName);
				Exit;
			end;
		end;
	end;
end;

function TModelWizard.GetSelectedUnitName: string;
var
	Project: IOTAProject;
	Module : IOTAModule;
begin
	Project := GetActiveProject;
	if Assigned(Project) then
		Module := GetActiveProject; // ou iterar pelos módulos
	if Assigned(Module) then
		Result := ExtractFileName(Module.FileName);
end;

end.
