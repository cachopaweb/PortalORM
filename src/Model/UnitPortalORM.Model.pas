unit UnitPortalORM.Model;

interface

uses
	System.Generics.Collections, System.Classes, System.SysUtils,
	FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error,
	FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.FB, FireDAC.Phys.FBDef,
	FireDAC.ConsoleUI.Wait, FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt, FireDAC.Phys.IBBase, FireDAC.Comp.DataSet, FireDAC.Comp.Client,
	System.Rtti, UnitInsereTabela.Model, REST.Json.Types, UnitConnection.Model.Interfaces;

type
	TTabela                    = class;
	TTabelaRelacionamento      = class of TTabela;
	TTipoRelacionamento        = (UmPraUm, UmPraMuitos);

	THelperDateTime = record helper for TDateTime
		function IsValid: Boolean;
	end;

	TCampo = class(TCustomAttribute)
	private
		FNome: string;
		FTipo: string;
		procedure SetNome(const Value: string);
		procedure SetTipo(const Value: string);
		{ private declarations }
	protected
		{ protected declarations }
	public
		{ public declarations }
		property Nome: string read FNome write SetNome;
		property Tipo: string read FTipo write SetTipo;
		constructor Create(_Nome, _Tipo: string);
	published
		{ published declarations }
	end;

	TRelacionamento = class(TCustomAttribute)
	private
		FTabela            : string;
		FCampoReferencia   : string;
		FClasse            : TTabelaRelacionamento;
		FCampoBusca        : string;
		FTipoRelacionamento: TTipoRelacionamento;
		{ private declarations }
	protected
		{ protected declarations }
	public
		{ public declarations }
		property Tabela            : string read FTabela write FTabela;
		property CampoReferencia   : string read FCampoReferencia write FCampoReferencia;
		property CampoBusca        : string read FCampoBusca write FCampoBusca;
		property Classe            : TTabelaRelacionamento read FClasse write FClasse;
		property TipoRelacionamento: TTipoRelacionamento read FTipoRelacionamento write FTipoRelacionamento;
		constructor Create(_Tabela, _CampoBusca, _CampoReferencia: string; _Classe: TTabelaRelacionamento; _TipoRelacionamento: TTipoRelacionamento);
	published
		{ published declarations }
	end;

	TNomeTabela = class(TCustomAttribute)
	private
		FNome      : string;
		FCampoBusca: string;
	public
		property Nome      : string read FNome write FNome;
		property CampoBusca: string read FCampoBusca write FCampoBusca;
		constructor Create(_Nome, _CampoBusca: string);
	end;

	TRecursoServidor = class(TCustomAttribute)
	private
		FRecurso: string;
	public
		property Recurso: string read FRecurso write FRecurso;
		constructor Create(_Recurso: string);
	end;

	TTabela = class
	private
		{ private declarations }
		[JSONMarshalledAttribute(false)]
		Nome: string;
		[JSONMarshalledAttribute(false)]
		CampoBusca: string;
		[JSONMarshalledAttribute(false)]
		function BuscaValorDataSet(Propriedade: TRttiProperty): TValue;
    procedure CriaTabela;
	protected
		{ protected declarations }
		[JSONMarshalledAttribute(false)]
		Contexto: TRttiContext;
		[JSONMarshalledAttribute(false)]
		IBQR: TFDQuery;
		[JSONMarshalledAttribute(false)]
		Campos: TList<TCampo>;
		[JSONMarshalledAttribute(false)]
		Propriedades: TDictionary<TRttiProperty, TCampo>;
		[JSONMarshalledAttribute(false)]
		Relacionamentos: TDictionary<TRttiProperty, TRelacionamento>;
		[JSONMarshalledAttribute(false)]
		FBancoDeDados: iConnection;
		[JSONMarshalledAttribute(false)]
		FIndiceConexao: integer;
		procedure VarreCampos;
		function BuscaValorCampoReferencia(Propriedades: TDictionary<TRttiProperty, TCampo>; CampoReferencia: string): integer;
	public
		{ public declarations }
		procedure BuscaDadosTabela(ValorBusca: string; Tentativa: smallint = 0; ClausulaWhere: string = ''); overload;
		procedure BuscaDadosTabela(ValorBusca: integer; Tentativa: smallint = 0; ClausulaWhere: string = ''); overload;
		function BuscaListaFilhos(ValorBusca: integer; Tentativa: smallint; Relacionamento: TRelacionamento): TArray<TTabela>;
		procedure SalvaNoBanco(Tentativa: smallint = 0);
		procedure Apagar(Codigo: integer);
		constructor Create(BancoDeDados: TFDConnection; TransacaoExterna: TFDTransaction = nil); overload;
		constructor Create(BancoDeDados: iConnection); overload;
		constructor Create; overload;
		destructor Destroy; override;
	published
		{ published declarations }
	end;

	TBanco = class
	private
		{ private declarations }
		FListaTabelas: TStringList;
		function BuscaListaTabelas: TStringList;
		function BuscaListaCampos(Tabela: TTabela): TStringList;
		procedure AnalisaTabela(Tabela: TTabela);
		procedure AnalisaCampos(Tabela: TTabela);
		procedure CriaTabela(Tabela: TTabela);
	protected
		{ protected declarations }
		IBQR: TFDQuery;
	public
		{ public declarations }
		Tabelas: TList<TTabela>;
		procedure Analisa;
		constructor Create(BancoDeDados: TFDConnection);
		destructor Destroy; override;
	published
		{ published declarations }
	end;

implementation

{ TBanco }

procedure TBanco.Analisa;
var
	Tabela: TTabela;
begin
	FListaTabelas := BuscaListaTabelas;
	for Tabela in Tabelas do
	begin
		AnalisaTabela(Tabela);
	end;
end;

procedure TBanco.AnalisaCampos(Tabela: TTabela);
var
	SQL        : string;
	Campos     : string;
	Campo      : TCampo;
	ListaCampos: TStringList;
begin
	try
		Campos      := '';
		ListaCampos := BuscaListaCampos(Tabela);
		try
			// Faz a varredura dos campos
			for Campo in Tabela.Campos do
			begin
				// Se o campo nao estiver na lista, ele sera criado
				if ListaCampos.IndexOf(Campo.Nome) < 0 then
					Campos := Campos + 'ADD ' + Campo.Nome + ' ' + Campo.Tipo + ', ';
			end;
			if Campos <> '' then
			begin
				// Tira os dois ultimos caracteres
				Campos := Campos.Substring(0, Campos.Length - 2);
				SQL    := 'ALTER TABLE ' + Tabela.Nome + ' ' + Campos;
				IBQR.Close;
				IBQR.SQL.Clear;
				IBQR.SQL.Add(SQL);
				IBQR.ExecSQL;
			end;
		finally
			ListaCampos.DisposeOf;
		end;
	except
		on E: Exception do
		begin
			raise Exception.Create('Houve erro ao criar a Tabela ' + Tabela.Nome + #13#13 + E.Message);
		end;
	end;
end;

procedure TBanco.AnalisaTabela(Tabela: TTabela);
begin
	if FListaTabelas.IndexOf(Tabela.Nome) < 0 then
	begin
		CriaTabela(Tabela);
	end
	else
	begin
		AnalisaCampos(Tabela);
	end;
end;

function TBanco.BuscaListaCampos(Tabela: TTabela): TStringList;
begin
	Result := TStringList.Create;
	IBQR.Close;
	IBQR.SQL.Clear;
	IBQR.SQL.Add('SELECT C.RDB$FIELD_NAME CAMPO, CASE T.RDB$FIELD_TYPE WHEN 7 THEN ''SMALLINT'' ');
	IBQR.SQL.Add('WHEN 8 THEN ''INTEGER'' WHEN 9 THEN ''QUAD'' WHEN 10 THEN ''FLOAT'' WHEN 11 THEN ''D_FLOAT'' ');
	IBQR.SQL.Add('WHEN 12 THEN ''DATE'' WHEN 13 THEN ''TIME'' WHEN 14 THEN ''CHAR'' WHEN 16 THEN ''INT64'' ');
	IBQR.SQL.Add('WHEN 27 THEN ''DOUBLE'' WHEN 35 THEN ''TIMESTAMP'' WHEN 37 THEN ''VARCHAR'' WHEN 40 THEN ''CSTRING'' ');
	IBQR.SQL.Add('WHEN 261 THEN ''BLOB'' ELSE ''UNKNOWN'' END AS TIPO, T.RDB$FIELD_LENGTH TAMANHO ');
	IBQR.SQL.Add('FROM RDB$RELATION_FIELDS C INNER JOIN RDB$FIELDS T ON C.RDB$FIELD_SOURCE = T.RDB$FIELD_NAME ');
	IBQR.SQL.Add('WHERE C.RDB$RELATION_NAME = :TABELA ');
	IBQR.SQL.Add('ORDER BY C.RDB$FIELD_POSITION ');
	IBQR.ParamByName('TABELA').Value := Tabela.Nome;
	IBQR.Open;
	IBQR.First;
	while not IBQR.Eof do
	begin
		Result.Add(Trim(IBQR.FieldByName('CAMPO').AsString));
		IBQR.Next;
	end;
end;

function TBanco.BuscaListaTabelas: TStringList;
begin
	Result := TStringList.Create;
	IBQR.Close;
	IBQR.SQL.Clear;
	IBQR.SQL.Add('SELECT RDB$RELATION_NAME TABELA ');
	IBQR.SQL.Add('FROM RDB$RELATIONS T ');
	IBQR.SQL.Add('WHERE RDB$VIEW_BLR IS NULL and (RDB$SYSTEM_FLAG = 0 OR RDB$SYSTEM_FLAG IS NULL) ');
	IBQR.SQL.Add('ORDER BY TABELA');
	IBQR.Open;
	IBQR.First;
	while not IBQR.Eof do
	begin
		Result.Add(Trim(IBQR.FieldByName('TABELA').AsString));
		IBQR.Next;
	end;
end;

constructor TBanco.Create(BancoDeDados: TFDConnection);
begin
	IBQR            := TFDQuery.Create(BancoDeDados);
	IBQR.Connection := BancoDeDados;
	Tabelas         := TList<TTabela>.Create;
end;

procedure TBanco.CriaTabela(Tabela: TTabela);
var
	SQL   : string;
	Campos: string;
	Campo : TCampo;
begin
	try
		// Faz a varredura dos campos
		Campos := '';
		for Campo in Tabela.Campos do
		begin
			Campos := Campos + Campo.Nome + ' ' + Campo.Tipo + ', ';
		end;
		// Tira os dois ultimos caracteres
		Campos := Campos.Substring(0, Campos.Length - 2);
		SQL    := 'CREATE TABLE ' + Tabela.Nome + '(' + Campos + ')';
		IBQR.Close;
		IBQR.SQL.Clear;
		IBQR.SQL.Add(SQL);
		IBQR.ExecSQL;
	except
		on E: Exception do
		begin
			raise Exception.Create('Houve erro ao criar a Tabela ' + Tabela.Nome + #13#13 + E.Message);
		end;
	end;
end;

destructor TBanco.Destroy;
begin
	IBQR.DisposeOf;
	if Assigned(Tabelas) then
		Tabelas.DisposeOf;
	if Assigned(FListaTabelas) then
		FListaTabelas.DisposeOf;
	inherited;
end;

{ TCampo }

constructor TCampo.Create(_Nome, _Tipo: string);
begin
	FNome := _Nome;
	FTipo := _Tipo;
end;

procedure TCampo.SetNome(const Value: string);
begin
	FNome := Value;
end;

procedure TCampo.SetTipo(const Value: string);
begin
	FTipo := Value;
end;

{ TTabela }

procedure TTabela.BuscaDadosTabela(ValorBusca: string; Tentativa: smallint = 0; ClausulaWhere: string = '');
var
	Propriedade         : TRttiProperty;
	Banco               : TBanco;
	TabelaFilha         : TTabela;
	Relacionamento      : TRelacionamento;
	Value               : TValue;
	ValorCampoReferencia: integer;
begin
	try
		IBQR.Close;
		IBQR.SQL.Clear;
		IBQR.SQL.Add('SELECT * FROM ' + Self.Nome + ' WHERE ' + Self.CampoBusca + ' = ' + ValorBusca.QuotedString);
		if ClausulaWhere <> '' then
			IBQR.SQL.Add(' ' + ClausulaWhere);
		IBQR.Open;
		// preenche o valor das propriedades
		for Propriedade in Propriedades.Keys do
		begin
			Value := BuscaValorDataSet(Propriedade);
			Propriedade.SetValue(Self, Value);
		end;
		// busca relacionamentos e preeche-os
		for Propriedade in Relacionamentos.Keys do
		begin
			if Relacionamentos.TryGetValue(Propriedade, Relacionamento) then
			begin
				TabelaFilha := Relacionamento.Classe.Create(TFDConnection(IBQR.Connection));
				case Relacionamento.TipoRelacionamento of
					UmPraUm:
						begin
							ValorCampoReferencia := BuscaValorCampoReferencia(Propriedades, Relacionamento.CampoReferencia);
							TabelaFilha.BuscaDadosTabela(ValorCampoReferencia);
							Value := TabelaFilha;
							Propriedade.SetValue(Self, Value);
						end;
					UmPraMuitos:
						begin
							TabelaFilha.CampoBusca := Relacionamento.CampoBusca;
							Value                  := TValue.From(TabelaFilha.BuscaListaFilhos(ValorBusca.ToInteger, Tentativa, Relacionamento));
							Propriedade.SetValue(Self, Value);
						end;
				end;
			end;
		end;
	except
		on E: Exception do
		begin
			if ((Pos('FIELD', UpperCase(E.Message)) > 0) and (Pos('NOT FOUND', UpperCase(E.Message)) > 0) or (Pos('TABLE UNKNOWN', UpperCase(E.Message)) > 0)) and (Tentativa < 5) then
			begin
				// Caso alguma coluna não exista na tabela, ela será criada
				Banco := TBanco.Create(TFDConnection(IBQR.Connection));
				VarreCampos;
				Banco.Tabelas.Add(Self);
				Banco.Analisa;
				Banco.DisposeOf;
				BuscaDadosTabela(ValorBusca, Tentativa + 1, ClausulaWhere);
			end
			else
				raise Exception.Create('Erro ao ler o valor de ' + Propriedades.Items[Propriedade].Nome + ' da Tabela ' + Self.Nome + '.' + #13#13 + E.Message);
		end;
	end;
	//
	IBQR.Close;
end;

procedure TTabela.Apagar(Codigo: integer);
var
	Propriedade: TRttiProperty;
	Banco      : TBanco;
	Value      : TValue;
begin
	try
		IBQR.Close;
		IBQR.SQL.Clear;
		IBQR.SQL.Add('DELETE FROM ' + Self.Nome + ' WHERE ' + Self.CampoBusca + ' = ' + Codigo.ToString);
		IBQR.ExecSQL;
	except
		on E: Exception do
		begin
			raise Exception.Create('Erro ao deletar registro da tabela de ' + Self.Nome + '.' + #13#13 + E.Message);
		end;
	end;
	//
end;

procedure TTabela.BuscaDadosTabela(ValorBusca: integer; Tentativa: smallint = 0; ClausulaWhere: string = '');
begin
	BuscaDadosTabela(ValorBusca.ToString, Tentativa, ClausulaWhere);
end;

function TTabela.BuscaListaFilhos(ValorBusca: integer; Tentativa: smallint; Relacionamento: TRelacionamento): TArray<TTabela>;
var
	Propriedade: TRttiProperty;
	Banco      : TBanco;
	Value      : TValue;
	IBQRFilhos : TFDQuery;
	i          : integer;
begin
	IBQRFilhos            := TFDQuery.Create(nil);
	IBQRFilhos.Connection := IBQR.Connection;
	try
		try
			IBQRFilhos.Close;
			IBQRFilhos.SQL.Clear;
			IBQRFilhos.SQL.Add('SELECT * FROM ' + Self.Nome + ' WHERE ' + Relacionamento.CampoReferencia + ' = ' + ValorBusca.ToString);
			IBQRFilhos.Open;
			//
			i := 0;
			IBQRFilhos.First;
			while not IBQRFilhos.Eof do
			begin
				SetLength(Result, i + 1);
				Result[i] := Relacionamento.Classe.Create(TFDConnection(IBQR.Connection));
				Result[i].BuscaDadosTabela(IBQRFilhos.FieldByName(Self.CampoBusca).AsInteger, 1);
				Inc(i);
				IBQRFilhos.Next;
			end;
		except
			on E: Exception do
			begin
				if ((Pos('FIELD', UpperCase(E.Message)) > 0) and (Pos('NOT FOUND', UpperCase(E.Message)) > 0) or (Pos('TABLE UNKNOWN', UpperCase(E.Message)) > 0)) and (Tentativa < 5) then
				begin
					// Caso alguma coluna não exista na tabela, ela será criada
					Banco := TBanco.Create(TFDConnection(IBQRFilhos.Connection));
					VarreCampos;
					Banco.Tabelas.Add(Self);
					Banco.Analisa;
					Banco.DisposeOf;
					BuscaDadosTabela(ValorBusca, Tentativa + 1);
				end
				else
					raise Exception.Create('Erro ao ler o valor de ' + Propriedades.Items[Propriedade].Nome + ' da Tabela ' + Self.Nome + '.' + #13#13 + E.Message);
			end;
		end;
		//
		IBQRFilhos.Close;
	finally
		IBQRFilhos.DisposeOf
	end;
end;

function TTabela.BuscaValorCampoReferencia(Propriedades: TDictionary<TRttiProperty, TCampo>; CampoReferencia: string): integer;
var
	Propriedade: TRttiProperty;
begin
	for Propriedade in Propriedades.Keys do
	begin
		if Propriedades.Items[Propriedade].Nome = CampoReferencia then
		begin
			Result := Propriedade.GetValue(Self).AsInteger;
			Break;
		end;
	end;
end;

constructor TTabela.Create;
begin

end;

constructor TTabela.Create(BancoDeDados: TFDConnection; TransacaoExterna: TFDTransaction = nil);
begin
	{ Create a new Rtti context }
	Contexto        := TRttiContext.Create;
	Campos          := TList<TCampo>.Create;
	Propriedades    := TDictionary<TRttiProperty, TCampo>.Create;
	Relacionamentos := TDictionary<TRttiProperty, TRelacionamento>.Create;
	VarreCampos;
	//
	IBQR            := TFDQuery.Create(nil);
	IBQR.Connection := BancoDeDados;
	if TransacaoExterna <> nil then
		IBQR.Transaction := TransacaoExterna
end;

destructor TTabela.Destroy;
var
	i: integer;
begin
	Contexto.Free;
	if Assigned(Propriedades) then
		Propriedades.DisposeOf;
	if Assigned(Relacionamentos) then
		Relacionamentos.DisposeOf;
	if Assigned(IBQR) then
		IBQR.DisposeOf;
	if Assigned(Campos) then
		Campos.DisposeOf;
	if Assigned(FBancoDeDados) then
		FBancoDeDados.Disconnected(FIndiceConexao);
	inherited;
end;

function TTabela.BuscaValorDataSet(Propriedade: TRttiProperty): TValue;
begin
	// Joga o valor do banco de dados para a Propriedaded
	case Propriedade.PropertyType.TypeKind of
		tkInteger:
			Result := IBQR.FieldByName(Propriedades.Items[Propriedade].Nome).AsInteger;
		tkFloat:
			Result := IBQR.FieldByName(Propriedades.Items[Propriedade].Nome).AsFloat;
		tkChar, tkString, tkWChar, tkLString, tkUString:
			Result := IBQR.FieldByName(Propriedades.Items[Propriedade].Nome).AsString;
		tkWString:
			Result := IBQR.FieldByName(Propriedades.Items[Propriedade].Nome).AsWideString;
		tkVariant:
			Result := IBQR.FieldByName(Propriedades.Items[Propriedade].Nome).AsString;
		tkInt64:
			Result := IBQR.FieldByName(Propriedades.Items[Propriedade].Nome).AsLargeInt;
	else
		Result := IBQR.FieldByName(Propriedades.Items[Propriedade].Nome).AsString;
	end;
end;

constructor TTabela.Create(BancoDeDados: iConnection);
begin
	FIndiceConexao := BancoDeDados.Connected;
	Create(TFDConnection(BancoDeDados.GetListaConexoes[FIndiceConexao]));
end;

procedure TTabela.SalvaNoBanco(Tentativa: smallint = 0);
var
	Propriedade : TRttiProperty;
	Campo       : TCampo;
	InsereTabela: iInsereTabela;
	Banco       : TBanco;
begin
	try
		InsereTabela := TModelInsereTabela.New(IBQR);
		InsereTabela.SetNomeTabela(Self.Nome);
		InsereTabela.SetCampoCodigo(Self.CampoBusca);
		// Busca a lista de Propriedades da Classe
		for Propriedade in Propriedades.Keys do
		begin
			Campo := Propriedades.Items[Propriedade];
			case Propriedade.PropertyType.TypeKind of
				tkInteger:
					InsereTabela.AddCamposEValores(Campo.Nome, Propriedade.GetValue(Self).AsInteger);
				tkFloat:
					begin
						if (Propriedade.GetValue(Self).TypeInfo = TypeInfo(TDate)) or (Propriedade.GetValue(Self).TypeInfo = TypeInfo(TTime)) or (Propriedade.GetValue(Self).TypeInfo = TypeInfo(TDateTime)) then
						begin
							if TDateTime(Propriedade.GetValue(Self).AsVariant).IsValid then
								InsereTabela.AddCamposEValores(Campo.Nome, TDateTime(Propriedade.GetValue(Self).AsVariant))
						end
						else
							InsereTabela.AddCamposEValores(Campo.Nome, Propriedade.GetValue(Self).AsVariant)
					end;
				tkString, tkWChar, tkLString, tkWString, tkVariant, tkUString:
					InsereTabela.AddCamposEValores(Campo.Nome, Propriedade.GetValue(Self).AsString);
				tkInt64:
					InsereTabela.AddCamposEValores(Campo.Nome, Propriedade.GetValue(Self).AsInt64);
			else
				InsereTabela.AddCamposEValores(Campo.Nome, Propriedade.GetValue(Self).AsVariant)
			end;
		end;
		InsereTabela.Command.Execute;
	except
		on E: Exception do
		begin
			if ((Pos('COLUMN UNKNOWN', UpperCase(E.Message)) > 0) or (Pos('TABLE UNKNOWN', UpperCase(E.Message)) > 0)) and (Tentativa < 5) then
			begin
				// Caso alguma coluna não exista na tabela, ela será criada
				Banco := TBanco.Create(TFDConnection(IBQR.Connection));
				VarreCampos;
				Banco.Tabelas.Add(Self);
				Banco.Analisa;
				Banco.DisposeOf;
				SalvaNoBanco(Tentativa + 1);
			end
			else
				raise Exception.Create('Erro ao gravar o valor de ' + Campo.Nome + ' da Tabela ' + Self.Nome + '.' + #13#13 + E.Message);
		end;
	end;
end;

procedure TTabela.VarreCampos;
var
	Tipo       : TRttiType;
	Atributo   : TCustomAttribute;
	Propriedade: TRttiProperty;
begin
	Campos.Clear;
	Propriedades.Clear;
	Relacionamentos.Clear;
	{ Extract type information for TSomeType type }
	Tipo := Contexto.GetType(Self.ClassType);
	try
		// busca dados da tabela
		for Atributo in Tipo.GetAttributes do
		begin
			if Atributo is TNomeTabela then
			begin
				Nome       := TNomeTabela(Atributo).Nome;
				CampoBusca := TNomeTabela(Atributo).CampoBusca;
			end;
		end;
		// busca dados das propriedades
		for Propriedade in Tipo.GetProperties do
		begin
			{ Search for the custom attribute and do some custom processing }
			for Atributo in Propriedade.GetAttributes do
			begin
				if Atributo is TCampo then
				begin
					Campos.Add(TCampo(Atributo));
					Propriedades.Add(Propriedade, TCampo(Atributo));
				end;
				if Atributo is TRelacionamento then
					Relacionamentos.Add(Propriedade, TRelacionamento(Atributo));
			end;
		end;
	finally
		Tipo.DisposeOf;
	end;
end;

procedure TTabela.CriaTabela;
var
	Banco: TBanco;
begin
	Banco := TBanco.Create(TFDConnection(IBQR.Connection));
	VarreCampos;
	Banco.Tabelas.Add(Self);
	Banco.Analisa;
	Banco.DisposeOf;
end;


{ TRelacionamento }

constructor TRelacionamento.Create(_Tabela, _CampoBusca, _CampoReferencia: string; _Classe: TTabelaRelacionamento; _TipoRelacionamento: TTipoRelacionamento);
begin
	FTabela         := _Tabela;
	CampoBusca      := _CampoBusca;
	CampoReferencia := _CampoReferencia;
	Classe := _Classe;
	TipoRelacionamento := _TipoRelacionamento;
end;

{ TNomeTabela }

constructor TNomeTabela.Create(_Nome, _CampoBusca: string);
begin
	Nome       := _Nome;
	CampoBusca := _CampoBusca;
end;

{ TBaseURL }

constructor TRecursoServidor.Create(_Recurso: string);
begin
	FRecurso := _Recurso;
end;

{ THelperDateTime }

function THelperDateTime.IsValid: Boolean;
begin
	Result := (FormatDateTime('dd/mm/yy', Self) <> '30/12/99');
end;

end.
