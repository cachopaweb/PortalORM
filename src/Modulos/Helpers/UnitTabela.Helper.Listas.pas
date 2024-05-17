unit UnitTabela.Helper.Listas;

interface

uses
	System.SysUtils,
	UnitPortalORM.Model,
	System.Generics.Collections,
	IBX.IBQuery,
	System.Rtti, FireDAC.Comp.Client;

type
	THelperTTabelaListas = class helper for TTabela
  private
  	function Clone<T: TTabela>(Tabela: T): T;
  public
  	function PreencheLista<T: TTabela, constructor>(CampoReferencia: string; ValorBusca: integer): TList<T>; overload;
    function PreencheLista<T: TTabela, constructor>(CampoReferencia: string; ValorBusca: string): TList<T>; overload;
    function PreencheLista<T: TTabela, constructor>(_CamposBusca, ValoresBusca: array of string; OrderBy: string = ''): TList<T>; overload;
    function PreencheListaWhere<T: TTabela, constructor>(CondicaoWhere: string; OrderBy: string = ''): TList<T>;
  end;

implementation

{ THelperListaTabela<T> }

function THelperTTabelaListas.Clone<T>(Tabela: T): T;
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
			if Propriedade.IsWritable then
				ListaPropriedades.Add(Propriedade)
		end;
		// clonando os dados da tabela
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

function THelperTTabelaListas.PreencheLista<T>(CampoReferencia: string; ValorBusca: string): TList<T>;
var
	Query: TFDQuery;
begin
	Query             := TFDQuery.Create(nil);
	Query.Connection  := IBQR.Connection;
	Result := TList<T>.Create;
	try
		Query.Close;
		Query.SQL.Clear;
		Query.SQL.Add('SELECT * FROM ' + Self.Nome + ' WHERE ' + CampoReferencia + ' = ' + '''' + ValorBusca + '''');
		Query.Open;
		Query.First;
		while not Query.Eof do
		begin
			Result.Add(TTabela(T.Create).Create(TFDConnection(Self.IBQR.Connection)));
			TTabela(Result[Pred(Result.Count)]).BuscaDadosTabela(Query.FieldByName(Self.CampoBusca).AsInteger);
			Query.Next;
		end;
	finally
		Query.DisposeOf;
	end;
end;

function THelperTTabelaListas.PreencheLista<T>(CampoReferencia: string; ValorBusca: integer): TList<T>;
begin
  Result := PreencheLista<T>(CampoReferencia, ValorBusca.ToString);
end;

function THelperTTabelaListas.PreencheLista<T>(_CamposBusca, ValoresBusca: array of string; OrderBy: string = ''): TList<T>;
var 
	Query: TFDQuery;
	CamposAux: string;
  i: Integer;
begin
	CamposAux := '';
  for i := Low(_CamposBusca) to High(_CamposBusca) do
  begin
    CamposAux := CamposAux + _CamposBusca[i]+' = '''+ValoresBusca[i]+''' AND ';
  end;
  CamposAux := CamposAux.Substring(0, CamposAux.Length - 5);
  //
  Query             := TFDQuery.Create(nil);
	Query.Connection  := IBQR.Connection;
	Result := TList<T>.Create;
  try
    Query.Close;
    Query.SQL.Clear;
    Query.SQL.Add('SELECT '+Self.CampoBusca+' FROM ' + Self.Nome + ' WHERE ' + CamposAux);
    if OrderBy <> '' then
      Query.SQL.Add(' ORDER BY '+OrderBy);
    Query.Open;
    Query.First;
    while not Query.Eof do
    begin
      Result.Add(TTabela(T.Create).Create(TFDConnection(Self.IBQR.Connection)));
      TTabela(Result[Pred(Result.Count)]).BuscaDadosTabela(Query.FieldByName(Self.CampoBusca).AsInteger);
      Query.Next;
    end;
  finally
    Query.DisposeOf;
  end;
end;

function THelperTTabelaListas.PreencheListaWhere<T>(CondicaoWhere: string; OrderBy: string = ''): TList<T>;
var 
	Query: TFDQuery;	
begin
  Query             := TFDQuery.Create(nil);
	Query.Connection    := IBQR.Connection;
	Result := TList<T>.Create;
  try
    Query.Close;
    Query.SQL.Clear;
    Query.SQL.Add('SELECT '+Self.CampoBusca+' FROM ' + Self.Nome + ' WHERE ' + CondicaoWhere);
    if OrderBy <> '' then
      Query.SQL.Add(' ORDER BY '+OrderBy);
    Query.Open;
    Query.First;
    while not Query.Eof do
    begin
      Result.Add(TTabela(T.Create).Create(TFDConnection(Self.IBQR.Connection)));
      TTabela(Result[Pred(Result.Count)]).BuscaDadosTabela(Query.FieldByName(Self.CampoBusca).AsInteger);
      Query.Next;
    end;
  finally
    Query.DisposeOf;
  end;
end;

end.
