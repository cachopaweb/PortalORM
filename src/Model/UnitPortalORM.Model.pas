unit UnitPortalORM.Model;

interface

uses
	System.Generics.Collections,
	System.Classes,
	System.SysUtils,
	FireDAC.Stan.Intf,
	FireDAC.Stan.Option,
	FireDAC.Stan.Error,
	FireDAC.UI.Intf,
	FireDAC.Phys.Intf,
	FireDAC.Stan.Def,
	FireDAC.Stan.Pool,
	FireDAC.Stan.Async,
	FireDAC.Phys,
	FireDAC.Phys.FB,
	FireDAC.Phys.FBDef,
	FireDAC.ConsoleUI.Wait,
	FireDAC.Stan.Param,
	FireDAC.DatS,
	FireDAC.DApt.Intf,
	FireDAC.DApt,
	FireDAC.Phys.IBBase,
	FireDAC.Comp.DataSet,
	FireDAC.Comp.Client,
	System.Rtti,
	UnitInsereTabela.Model,
	REST.Json.Types,
	UnitConnection.Model.Interfaces,
	Data.DB;

type
	TTabela               = class;
	TTabelaRelacionamento = class of TTabela;
	TTipoRelacionamento   = (UmPraUm, UmPraMuitos);

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
		FTabelaFilha: TObject;
		function BuscaValorDataSet(Query: TDataset; Propriedade: TRttiProperty): TValue;
	protected
		{ protected declarations }
		[JSONMarshalled(false)]
		Nome: string;
		[JSONMarshalled(false)]
		CampoBusca: string;
		[JSONMarshalled(false)]
		Contexto: TRttiContext;
		[JSONMarshalled(false)]
		IBQR: TFDQuery;
		[JSONMarshalled(false)]
		Campos: TList<TCampo>;
		[JSONMarshalled(false)]
		Propriedades: TDictionary<TRttiProperty, TCampo>;
		[JSONMarshalled(false)]
		Relacionamentos: TDictionary<TRttiProperty, TRelacionamento>;
		[JSONMarshalled(false)]
		FBancoDeDados: iConnection;
		[JSONMarshalled(false)]
		FIndiceConexao: integer;
		[JSONMarshalled(false)]
		TransacaoBusca: TFDTransaction;
		[JSONMarshalled(false)]
		IBQRBusca: TFDQuery;
		procedure VarreCampos;
		function BuscaValorCampoReferencia(Propriedades: TDictionary<TRttiProperty, TCampo>; CampoReferencia: string): integer;
	public
		{ public declarations }
		procedure BuscaDadosTabela(ValorBusca: string; Tentativa: smallint = 0; ClausulaWhere: string = ''; CamposEspecificos: string = ''); overload;
		procedure BuscaDadosTabela(ValorBusca: integer; Tentativa: smallint = 0; ClausulaWhere: string = ''; CamposEspecificos: string = ''); overload;
		procedure BuscaPorCampo(_CampoBusca, ValorBusca: string; Tentativa: smallint = 0); overload;
		procedure BuscaPorCampo(_CampoBusca: string; ValorBusca: integer; Tentativa: smallint = 0); overload;
		procedure BuscaPorCampos(_CamposBusca, ValoresBusca: array of string; Tentativa: smallint = 0); overload;
		function BuscaListaFilhos(ValorBusca: integer; Tentativa: smallint; Relacionamento: TRelacionamento): TList<TTabela>;
		function GeraCodigo(CampoCodigo: string; Tentativa: smallint = 0): integer;
		function IncrementaGenerator(NomeGenerator: string): integer;
		procedure SalvaNoBanco(Tentativa: smallint = 0; Comitar: Boolean = True);
		procedure Apagar(Codigo: integer; Comitar: Boolean = True);
		procedure Commit;
		procedure CriaTabela;
		[JSONMarshalled(false)]
		property TabelaFilha: TObject read FTabelaFilha write FTabelaFilha;
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
		IBQR     : TFDQuery;
		Transacao: TFDTransaction;
	public
		{ public declarations }
		Tabelas: TList<TTabela>;
		procedure Analisa;
		constructor Create(BancoDeDados: TFDConnection); overload;
		constructor Create; overload;
		destructor Destroy; override;
	published
		{ published declarations }
	end;

implementation

{ TBanco }
{$IFDEF REST}

uses
	UnitTabela.Helpers,
	UnitClientREST.Model.Interfaces;
{$ENDIF}

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
	Transacao        := TFDTransaction.Create(BancoDeDados);
	IBQR             := TFDQuery.Create(BancoDeDados);
	IBQR.Connection  := BancoDeDados;
	IBQR.Transaction := Transacao;
	Tabelas          := TList<TTabela>.Create;
end;

constructor TBanco.Create;
begin
	Tabelas := TList<TTabela>.Create;
end;

procedure TBanco.CriaTabela(Tabela: TTabela);
var
	SQL         : string;
	Campos      : string;
	Campo       : TCampo;
	IBQRAux     : TFDQuery;
	TransacaoAux: TFDTransaction;
begin
	TransacaoAux            := TFDTransaction.Create(nil);
	TransacaoAux.Connection := IBQR.Connection;
	IBQRAux                 := TFDQuery.Create(nil);
	IBQRAux.Connection      := TransacaoAux.Connection;
	IBQRAux.Transaction     := TransacaoAux;
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
		IBQRAux.Close;
		IBQRAux.SQL.Clear;
		IBQRAux.SQL.Add(SQL);
		IBQRAux.ExecSQL;
		if IBQRAux.Transaction.Active then
			IBQRAux.Transaction.Commit;
	except
		on E: Exception do
		begin
			if IBQRAux.Transaction.Active then
				IBQRAux.Transaction.Rollback;
			raise Exception.Create('Houve erro ao criar a Tabela ' + Tabela.Nome + #13#13 + E.Message);
		end;
	end;
	IBQRAux.DisposeOf;
	TransacaoAux.DisposeOf;
end;

destructor TBanco.Destroy;
begin
	IBQR.DisposeOf;
	Transacao.DisposeOf;
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

procedure TTabela.BuscaDadosTabela(ValorBusca: string; Tentativa: smallint = 0; ClausulaWhere: string = ''; CamposEspecificos: string = '');
var
	Propriedade         : TRttiProperty;
	Banco               : TBanco;
	TabelaFilha         : TTabela;
	Relacionamento      : TRelacionamento;
	Value               : TValue;
	ValorCampoReferencia: integer;
	Campos              : string;
begin
	try
		// Poderá buscar todos os campos ou apenas os campos informados
		if CamposEspecificos = '' then
			Campos := '*'
		else
			Campos := CamposEspecificos;
		if IBQRBusca.Transaction.Active then
			IBQRBusca.Transaction.Rollback;
		IBQRBusca.Close;
		IBQRBusca.SQL.Clear;
		IBQRBusca.SQL.Add('SELECT ' + Campos + ' FROM ' + Self.Nome + ' WHERE ' + Self.Nome + '.' + Self.CampoBusca + ' = ' + ValorBusca.QuotedString);
		if ClausulaWhere <> '' then
			IBQRBusca.SQL.Add(' ' + ClausulaWhere);
		IBQRBusca.Open;
		// preenche o valor das propriedades
		for Propriedade in Propriedades.Keys do
		begin
			// filtra apenas pelos campos existentes no Dataset
			if CamposEspecificos.IsEmpty or Assigned(IBQRBusca.FindField(Propriedades.Items[Propriedade].Nome)) then
			begin
				Value := BuscaValorDataSet(IBQRBusca, Propriedade);
				Propriedade.SetValue(Self, Value);
			end;
		end;
		// busca relacionamentos e preenche-os
		for Propriedade in Relacionamentos.Keys do
		begin
			if Relacionamentos.TryGetValue(Propriedade, Relacionamento) then
			begin
				TabelaFilha := Relacionamento.Classe.Create(TFDConnection(IBQR.Connection));
				case Relacionamento.TipoRelacionamento of
					UmPraUm:
						begin
							ValorCampoReferencia := BuscaValorCampoReferencia(Propriedades, Relacionamento.CampoReferencia);
							TabelaFilha.BuscaPorCampo(Relacionamento.CampoBusca, ValorCampoReferencia);
							Value := TabelaFilha;
						end;
					UmPraMuitos:
						begin
							TabelaFilha.CampoBusca := Relacionamento.CampoBusca;
							Value                  := TabelaFilha.BuscaListaFilhos(ValorBusca.ToInteger, 0, Relacionamento);
						end;
				end;
				Propriedade.SetValue(Self, Value);
			end;
		end;
	except
		on E: Exception do
		begin
			if ((Pos('FIELD', UpperCase(E.Message)) > 0) and (Pos('NOT FOUND', UpperCase(E.Message)) > 0) or (Pos('TABLE UNKNOWN', UpperCase(E.Message)) > 0) or (Pos('COLUMN UNKNOWN', UpperCase(E.Message)) > 0)) and (Tentativa < 5) then
			begin
				// Caso alguma coluna não exista na tabela, ela será criada
				CriaTabela;
				BuscaDadosTabela(ValorBusca, Tentativa + 1, ClausulaWhere, CamposEspecificos);
			end
			else
			begin
      	try
          if Propriedades.ContainsKey(Propriedade) then
            raise Exception.Create('Erro ao ler o valor de ' + Propriedades.Items[Propriedade].Nome + ' da Tabela ' + Self.Nome + '.' + #13#13 + E.Message)
          else
            raise Exception.Create('Erro ao buscar valores da Tabela ' + Self.Nome + '.' + #13#13 + E.Message);
      	except
          raise Exception.Create(E.Message);    
        end;
			end;
		end;
	end;
	//
	IBQRBusca.Close;
	if IBQRBusca.Transaction.Active then
		IBQRBusca.Transaction.Rollback;
end;

procedure TTabela.Apagar(Codigo: integer; Comitar: Boolean = True);
begin
	try
		if Comitar then
		begin
			if IBQR.Transaction.Active then
				IBQR.Transaction.Rollback;
		end;
		IBQR.Close;
		IBQR.SQL.Clear;
		IBQR.SQL.Add('DELETE FROM ' + Self.Nome + ' WHERE ' + Self.CampoBusca + ' = ' + Codigo.ToString);
		IBQR.ExecSQL;
		if Comitar then
			if IBQR.Transaction.Active then
				IBQR.Transaction.Commit;
	except
		on E: Exception do
		begin
			if Comitar then
				if IBQR.Transaction.Active then
					IBQR.Transaction.Rollback;
			raise Exception.Create('Erro ao deletar registro da tabela ' + Self.Nome + '.' + #13#13 + E.Message);
		end;
	end;
	//
	if Comitar then
	begin
		IBQR.Close;
		if IBQR.Transaction.Active then
			IBQR.Transaction.Rollback;
	end;
end;

procedure TTabela.BuscaDadosTabela(ValorBusca: integer; Tentativa: smallint = 0; ClausulaWhere: string = ''; CamposEspecificos: string = '');
begin
	BuscaDadosTabela(ValorBusca.ToString, Tentativa, ClausulaWhere, CamposEspecificos);
end;

function TTabela.BuscaListaFilhos(ValorBusca: integer; Tentativa: smallint; Relacionamento: TRelacionamento): TList<TTabela>;
var
	Propriedade: TRttiProperty;
	IBQRFilhos : TFDQuery;
begin
	IBQRFilhos             := TFDQuery.Create(nil);
	IBQRFilhos.Connection  := IBQR.Connection;
	IBQRFilhos.Transaction := IBQR.Transaction;
	try
		try
			if IBQRFilhos.Transaction.Active then
				IBQRFilhos.Transaction.Rollback;
			IBQRFilhos.Close;
			IBQRFilhos.SQL.Clear;
			IBQRFilhos.SQL.Add('SELECT * FROM ' + Self.Nome + ' WHERE ' + Relacionamento.CampoReferencia + ' = ' + ValorBusca.ToString);
			IBQRFilhos.Open;
			//
			IBQRFilhos.First;
			Result := TList<TTabela>.Create;
			while not IBQRFilhos.Eof do
			begin
				Result.Add(TTabela(Relacionamento.Classe.Create(TFDConnection(Self.IBQR.Connection), TFDTransaction(Self.IBQR.Transaction))));
				Result[Pred(Result.Count)].BuscaDadosTabela(IBQRFilhos.FieldByName(Self.CampoBusca).AsInteger, 1);
				IBQRFilhos.Next;
			end;
		except
			on E: Exception do
			begin
				if ((Pos('FIELD', UpperCase(E.Message)) > 0) and (Pos('NOT FOUND', UpperCase(E.Message)) > 0) or (Pos('TABLE UNKNOWN', UpperCase(E.Message)) > 0)) and (Tentativa < 5) then
				begin
					// Caso alguma coluna não exista na tabela, ela será criada
					CriaTabela;
					BuscaListaFilhos(ValorBusca, Tentativa + 1, Relacionamento);
				end
				else
					raise Exception.Create('Erro ao ler o valor de ' + Propriedades.Items[Propriedade].Nome + ' da Tabela ' + Self.Nome + '.' + #13#13 + E.Message);
			end;
		end;
		//
		IBQRFilhos.Close;
		if IBQRBusca.Transaction.Active then
			IBQRBusca.Transaction.Rollback;
	finally
		IBQRFilhos.DisposeOf
	end;
end;

procedure TTabela.BuscaPorCampo(_CampoBusca: string; ValorBusca: integer; Tentativa: smallint);
begin
	BuscaPorCampo(_CampoBusca, ValorBusca.ToString, Tentativa);
end;

procedure TTabela.BuscaPorCampo(_CampoBusca, ValorBusca: string; Tentativa: smallint);
begin
	try
		if IBQRBusca.Transaction.Active then
			IBQRBusca.Transaction.Rollback;
		IBQRBusca.Close;
		IBQRBusca.SQL.Clear;
		IBQRBusca.SQL.Add('SELECT ' + Self.CampoBusca + ' FROM ' + Self.Nome + ' WHERE ' + _CampoBusca + ' = ' + '''' + ValorBusca + '''');
		IBQRBusca.Open;
		if not IBQRBusca.IsEmpty then
			BuscaDadosTabela(IBQRBusca.FieldByName(Self.CampoBusca).AsString)
		else
			BuscaDadosTabela('0'); // Faz a busca com valor Zero só para zerar as propriedades
	except
		on E: Exception do
		begin
			if ((Pos('FIELD', UpperCase(E.Message)) > 0) and (Pos('NOT FOUND', UpperCase(E.Message)) > 0) or (Pos('TABLE UNKNOWN', UpperCase(E.Message)) > 0) or (Pos('COLUMN UNKNOWN', UpperCase(E.Message)) > 0)) and (Tentativa < 5) then
			begin
				// Caso alguma coluna não exista na tabela, ela será criada
				CriaTabela;
				BuscaPorCampo(_CampoBusca, ValorBusca, Tentativa + 1);
			end
			else
			begin
				raise Exception.Create('Erro ao buscar valores da Tabela ' + Self.Nome + '.' + #13#13 + E.Message);
			end;
		end;
	end;
end;

procedure TTabela.BuscaPorCampos(_CamposBusca, ValoresBusca: array of string; Tentativa: smallint);
var
	Campos: string;
	i     : integer;
begin
	try
		Campos := '';
		for i  := Low(_CamposBusca) to High(_CamposBusca) do
		begin
			Campos := Campos + _CamposBusca[i] + ' = ''' + ValoresBusca[i] + ''' AND ';
		end;
		Campos := Campos.Substring(0, Campos.Length - 5);
		if IBQRBusca.Transaction.Active then
			IBQRBusca.Transaction.Rollback;
		IBQRBusca.Close;
		IBQRBusca.SQL.Clear;
		IBQRBusca.SQL.Add('SELECT ' + Self.CampoBusca + ' FROM ' + Self.Nome + ' WHERE ' + Campos);
		IBQRBusca.Open;
		if not IBQRBusca.IsEmpty then
			BuscaDadosTabela(IBQRBusca.FieldByName(Self.CampoBusca).AsString)
		else
			BuscaDadosTabela('0'); // Faz a busca com valor Zero só para zerar as propriedades
	except
		on E: Exception do
		begin
			if ((Pos('FIELD', UpperCase(E.Message)) > 0) and (Pos('NOT FOUND', UpperCase(E.Message)) > 0) or (Pos('TABLE UNKNOWN', UpperCase(E.Message)) > 0) or (Pos('COLUMN UNKNOWN', UpperCase(E.Message)) > 0)) and (Tentativa < 5) then
			begin
				// Caso alguma coluna não exista na tabela, ela será criada
				CriaTabela;
				BuscaPorCampos(_CamposBusca, ValoresBusca, Tentativa + 1);
			end
			else
			begin
				raise Exception.Create('Erro ao buscar valores da Tabela ' + Self.Nome + '.' + #13#13 + E.Message);
			end;
		end;
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

constructor TTabela.Create(BancoDeDados: TFDConnection; TransacaoExterna: TFDTransaction = nil);
var
	Propriedade   : TRttiProperty;
	Relacionamento: TRelacionamento;
	Transacao     : TFDTransaction;
begin
	{ Create a new Rtti context }
	Contexto        := TRttiContext.Create;
	Campos          := TList<TCampo>.Create;
	Propriedades    := TDictionary<TRttiProperty, TCampo>.Create;
	Relacionamentos := TDictionary<TRttiProperty, TRelacionamento>.Create;
	VarreCampos;
	//
	Transacao            := TFDTransaction.Create(nil);
	Transacao.Connection := BancoDeDados;
	IBQR                 := TFDQuery.Create(nil);
	IBQR.Connection      := BancoDeDados;
	if TransacaoExterna <> nil then
		IBQR.Transaction := TransacaoExterna
	else
		IBQR.Transaction := Transacao;
	// Foram criados estes dois componentes exclusivos para busca, para permitir o Rollback antes da busca.
	// Se não tiver componentes dedicados para busca, o rollback cancela os dados já inseridos por outras classes
	TransacaoBusca            := TFDTransaction.Create(nil);
	TransacaoBusca.Connection := BancoDeDados;
	IBQRBusca                 := TFDQuery.Create(nil);
	IBQRBusca.Connection      := BancoDeDados;
	IBQRBusca.Transaction     := TransacaoBusca;
	// busca relacionamentos e cria-os
	for Propriedade in Relacionamentos.Keys do
	begin
		if Relacionamentos.TryGetValue(Propriedade, Relacionamento) then
		begin
			case Relacionamento.TipoRelacionamento of
				UmPraUm:
					begin
						// TabelaFilha := Relacionamento.Classe.Create(IBQR.Database);
						Propriedade.SetValue(Self, Relacionamento.Classe.Create(TFDConnection(IBQR.Connection), TransacaoExterna));
					end;
				UmPraMuitos:
					begin

					end;
			end;
		end;
	end;
	CriaTabela;
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
	if Assigned(TransacaoBusca) then
		TransacaoBusca.DisposeOf;
	if Assigned(IBQRBusca) then
		IBQRBusca.DisposeOf;
	if Assigned(Campos) then
		Campos.DisposeOf;
	if Assigned(FBancoDeDados) then
		FBancoDeDados.Disconnected(FIndiceConexao);
	inherited;
end;

function TTabela.GeraCodigo(CampoCodigo: string; Tentativa: smallint): integer;
var
	IBQRAux     : TFDQuery;
	TransacaoAux: TFDTransaction;
begin
	TransacaoAux            := TFDTransaction.Create(nil);
	TransacaoAux.Connection := IBQR.Connection;
	IBQRAux                 := TFDQuery.Create(nil);
	IBQRAux.Connection      := TransacaoAux.Connection;
	IBQRAux.Transaction     := TransacaoAux;
	try
		IBQRAux.Close;
		IBQRAux.SQL.Clear;
		IBQRAux.SQL.Add('SELECT COALESCE(MAX(' + CampoCodigo + '), 0)+1 CODIGO FROM ' + Self.Nome);
		IBQRAux.Open;
		Result := IBQRAux.FieldByName('CODIGO').AsInteger;
	except
		on E: Exception do
		begin
			if ((Pos('FIELD', UpperCase(E.Message)) > 0) and (Pos('NOT FOUND', UpperCase(E.Message)) > 0) or (Pos('TABLE UNKNOWN', UpperCase(E.Message)) > 0) or (Pos('COLUMN UNKNOWN', UpperCase(E.Message)) > 0)) and (Tentativa < 5) then
			begin
				// Caso alguma coluna não exista na tabela, ela será criada
				CriaTabela;
				if IBQRAux.Transaction.Active then
					IBQRAux.Transaction.Rollback;
				Result := GeraCodigo(CampoCodigo, Tentativa + 1);
			end
			else
				raise Exception.Create('Erro ao gerar o Código da Tabela ' + Self.Nome + '.' + #13#13 + E.Message);
		end;
	end;
	TransacaoAux.DisposeOf;
	IBQRAux.DisposeOf;
end;

function TTabela.IncrementaGenerator(NomeGenerator: string): integer;
var
	IBQRAux     : TFDQuery;
	TransacaoAux: TFDTransaction;
begin
	TransacaoAux            := TFDTransaction.Create(nil);
	TransacaoAux.Connection := IBQR.Connection;
	IBQRAux                 := TFDQuery.Create(nil);
	IBQRAux.Connection      := TransacaoAux.Connection;
	IBQRAux.Transaction     := TransacaoAux;
	try
		IBQRAux.Close;
		IBQRAux.SQL.Clear;
		IBQRAux.SQL.Add('SELECT GEN_ID(' + NomeGenerator + ', 1) CODIGO FROM RDB$DATABASE');
		IBQRAux.Open;
		Result := IBQRAux.FieldByName('CODIGO').AsInteger;
	except
		on E: Exception do
		begin
			raise Exception.Create('Erro ao obter o valor do Generator ' + NomeGenerator + '!' + #13#13 + E.Message);
		end;
	end;
	TransacaoAux.DisposeOf;
	IBQRAux.DisposeOf;
end;

function TTabela.BuscaValorDataSet(Query: TDataset; Propriedade: TRttiProperty): TValue;
begin
	// Joga o valor do banco de dados para a Propriedade
	case Propriedade.PropertyType.TypeKind of
		tkInteger:
			Result := Query.FieldByName(Propriedades.Items[Propriedade].Nome).AsInteger;
		tkFloat:
			Result := Query.FieldByName(Propriedades.Items[Propriedade].Nome).AsFloat;
		tkChar, tkString, tkWChar, tkLString, tkUString:
			Result := Query.FieldByName(Propriedades.Items[Propriedade].Nome).AsString;
		tkWString:
			Result := Query.FieldByName(Propriedades.Items[Propriedade].Nome).AsWideString;
		tkVariant:
			Result := Query.FieldByName(Propriedades.Items[Propriedade].Nome).AsString;
		tkInt64:
			Result := Query.FieldByName(Propriedades.Items[Propriedade].Nome).AsLargeInt;
	else
		Result := Query.FieldByName(Propriedades.Items[Propriedade].Nome).AsString;
	end;
end;

constructor TTabela.Create(BancoDeDados: iConnection);
begin
	FIndiceConexao := BancoDeDados.Connected;
	Create(TFDConnection(BancoDeDados.GetListaConexoes[FIndiceConexao]));
end;

procedure TTabela.SalvaNoBanco(Tentativa: smallint = 0; Comitar: Boolean = True);
var
	Propriedade : TRttiProperty;
	Campo       : TCampo;
	InsereTabela: iInsereTabela;
	Banco       : TBanco;
	{$IFDEF REST}
	Response: TClientResult;
	{$ENDIF}
begin
	{$IFDEF REST}
	Response := Self.Post;
	if not(Response.StatusCode in [200, 201]) then
		raise Exception.Create('Erro ao enviar dados para o servidor!' + sLineBreak + 'Erro: ' + Response.Content + '. ' + Response.Error + sLineBreak + 'Rota API: ' + Response.Route);
	{$ELSE}
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
						if Propriedade.GetValue(Self).TypeInfo = TypeInfo(TDate) then
						begin
							if (TDateTime(Propriedade.GetValue(Self).AsVariant).IsValid) then
								InsereTabela.AddCamposEValores(Campo.Nome, FormatDateTime('dd/mm/yyyy', Propriedade.GetValue(Self).AsVariant))
						end
						else if Propriedade.GetValue(Self).TypeInfo = TypeInfo(TDateTime) then
						begin
							if (TDateTime(Propriedade.GetValue(Self).AsVariant).IsValid) then
								InsereTabela.AddCamposEValores(Campo.Nome, FormatDateTime('dd/mm/yyyy hh:mm:ss', Propriedade.GetValue(Self).AsVariant))
						end
						else if (Propriedade.GetValue(Self).TypeInfo = TypeInfo(TTime)) then
							InsereTabela.AddCamposEValores(Campo.Nome, FormatDateTime('hh:mm:ss', Propriedade.GetValue(Self).AsVariant))
						else
							InsereTabela.AddCamposEValores(Campo.Nome, Propriedade.GetValue(Self).AsVariant);
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
		if Comitar then
			if IBQR.Transaction.Active then
				IBQR.Transaction.Commit;
	except
		on E: Exception do
		begin
			if Comitar then
			begin
				if IBQR.Transaction.Active then
					IBQR.Transaction.Rollback;
			end;
			if ((Pos('COLUMN UNKNOWN', UpperCase(E.Message)) > 0) or (Pos('TABLE UNKNOWN', UpperCase(E.Message)) > 0)) and (Tentativa < 5) then
			begin
				// Caso alguma coluna não exista na tabela, ela será criada
				CriaTabela;
				SalvaNoBanco(Tentativa + 1, Comitar);
			end
			else
				raise Exception.Create('Erro ao gravar o valor de ' + Campo.Nome + ' da Tabela ' + Self.Nome + '.' + #13#13 + E.Message);
		end;
	end;
	//
	if Comitar then
	begin
		IBQR.Close;
		if IBQR.Transaction.Active then
			IBQR.Transaction.Rollback;
	end;
	{$ENDIF}
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

procedure TTabela.Commit;
begin
	if IBQR.Transaction.Active then
		IBQR.Transaction.Commit;
end;

{ procedure TTabela.BuscaPorCampos(_CamposBusca, ValoresBusca: array of string;
	Tentativa: smallint);
	begin

	end;

	TRelacionamento }

constructor TRelacionamento.Create(_Tabela, _CampoBusca, _CampoReferencia: string; _Classe: TTabelaRelacionamento; _TipoRelacionamento: TTipoRelacionamento);
begin
	FTabela            := _Tabela;
	CampoBusca         := _CampoBusca;
	CampoReferencia    := _CampoReferencia;
	Classe             := _Classe;
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
	Result := FormatDateTime('dd/mm/yyyy', Self) <> '30/12/1899';
end;

end.
