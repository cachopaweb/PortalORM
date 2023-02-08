unit UnitInsereTabela.Model;

interface

uses
  System.Generics.Collections,
  UnitCommand.Interfaces,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error,
  FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.FB, FireDAC.Phys.FBDef,
  FireDAC.ConsoleUI.Wait, FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt, FireDAC.Phys.IBBase, FireDAC.Comp.DataSet, FireDAC.Comp.Client,
  Data.DB;

type
  iInsereTabela = interface
    ['{160BF1E8-74FE-4E5C-94CB-2E73EB57182C}']
    function SetNomeTabela(Value: string): iInsereTabela;
    function SetCampoCodigo(Value: string): iInsereTabela;
    function AddCamposEValores(Campo: string; Valores: Variant): iInsereTabela;
    function Command: iCommand;
    function Execute: iCommand;
  end;

  TModelInsereTabela = class(TInterfacedObject, iInsereTabela, iCommand)
  private
    FNomeTabela: string;
    FCampoCodigo: string;
    FListaCamposEValores: TDictionary<string, Variant>;
    FQuery: TFDQuery;
    function InsereTabela(NomeTabela, CampoCodigo: string; CamposEValores: TDictionary<string, Variant>; var IBQR: TFDQuery): Boolean;
  public
    constructor Create(Query: TFDQuery);
    destructor Destroy; override;
    class function New(Query: TFDQuery): iInsereTabela;
    function SetNomeTabela(Value: string): iInsereTabela;
    function SetCampoCodigo(Value: string): iInsereTabela;
    function AddCamposEValores(Campo: string; Valores: Variant): iInsereTabela;
    function Command: iCommand;
    function Execute: iCommand;
  end;

implementation

uses
  System.SysUtils;

{ TModelInsereTabela }

function TModelInsereTabela.AddCamposEValores(Campo: string; Valores: Variant): iInsereTabela;
begin
  Result := Self;
  FListaCamposEValores.Add(Campo, Valores);
end;

function TModelInsereTabela.Command: iCommand;
begin
  Result := Self;
end;

constructor TModelInsereTabela.Create(Query: TFDQuery);
begin
  FQuery               := Query;
  FListaCamposEValores := TDictionary<string, Variant>.Create;
end;

destructor TModelInsereTabela.Destroy;
begin
  FreeAndNil(FListaCamposEValores);
  inherited;
end;

function TModelInsereTabela.Execute: iCommand;
begin
  Result := Self;
  if FNomeTabela = '' then
    raise Exception.Create('Nome da tabela é obrigatório!');
  if FCampoCodigo = '' then
    raise Exception.Create('Nome do Campo do código da tabela é obrigatório!');
  InsereTabela(FNomeTabela, FCampoCodigo, FListaCamposEValores, FQuery);
end;

class function TModelInsereTabela.New(Query: TFDQuery): iInsereTabela;
begin
  Result := Self.Create(Query);
end;

function TModelInsereTabela.SetCampoCodigo(Value: string): iInsereTabela;
begin
  Result       := Self;
  FCampoCodigo := Value;
end;

function TModelInsereTabela.SetNomeTabela(Value: string): iInsereTabela;
begin
  Result      := Self;
  FNomeTabela := Value;
end;

function TModelInsereTabela.InsereTabela(NomeTabela, CampoCodigo: string; CamposEValores: TDictionary<string, Variant>; var IBQR: TFDQuery): Boolean;
var
  ListaCampos, ListaParametros, Campo: string;
  Valor: Variant;
begin
  try
    ListaCampos     := '';
    ListaParametros := '';
    for Campo in CamposEValores.Keys do
    begin
      ListaCampos     := ListaCampos + Campo + ', ';
      ListaParametros := ListaParametros + ':' + Campo + ', ';
    end;
    ListaCampos     := ListaCampos.Substring(0, ListaCampos.Length - 2);
    ListaParametros := ListaParametros.Substring(0, ListaParametros.Length - 2);
    IBQR.Close;
    IBQR.SQL.Clear;
    IBQR.SQL.Add('UPDATE OR INSERT INTO ' + NomeTabela + '(' + ListaCampos + ') ');
    IBQR.SQL.Add('VALUES(' + ListaParametros + ') ');
    IBQR.SQL.Add('MATCHING(' + CampoCodigo + ')');
    for Campo in CamposEValores.Keys do
    begin
      CamposEValores.TryGetValue(Campo, Valor);
      case TVarData(Valor).vType of
        varString, varUString:
          IBQR.ParamByName(Campo).AsString := string(Valor);
        varSmallint, varInteger, varInt64:
          IBQR.ParamByName(Campo).AsInteger := integer(Valor);
        varDouble:
          IBQR.ParamByName(Campo).AsFloat := Double(Valor);
        varCurrency:
          IBQR.ParamByName(Campo).AsFloat := Currency(Valor);
        varDate:
          IBQR.ParamByName(Campo).AsDateTime := Valor;
      else
        IBQR.ParamByName(Campo).Value := Valor;
      end;
    end;
    IBQR.ExecSQL;
  except
    on E: EDatabaseError do
    begin
      raise EDatabaseError.Create(Format('Erro ao Inserir ou Atualizar a Tabela: %s, Campo: %s.' + sLineBreak + 'Erro: %s', [NomeTabela, Campo, E.Message]));
    end;
    on E: Exception do
    begin
      raise Exception.Create(Format('Erro ao Inserir ou Atualizar a Tabela: %s, Campo: %s.' + sLineBreak + 'Erro: %s', [NomeTabela, Campo, E.Message]));
    end;
  end;
end;

end.
