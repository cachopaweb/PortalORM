unit UnitPrincipal;

interface

uses
	Winapi.Windows,
	Winapi.Messages,
	System.SysUtils,
	System.Variants,
	System.Classes,
	Vcl.Graphics,
	Vcl.Controls,
	Vcl.Forms,
	Vcl.Dialogs,
	Vcl.ExtCtrls,
	Vcl.StdCtrls,
	Vcl.Buttons,
	Data.DB,
	Vcl.Grids,
	Vcl.DBGrids,
	Vcl.WinXCtrls, FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
	FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
	FireDAC.Stan.Async, FireDAC.DApt, FireDAC.UI.Intf, FireDAC.Stan.Def,
	FireDAC.Stan.Pool, FireDAC.Phys, FireDAC.Phys.FB, FireDAC.Phys.FBDef,
	FireDAC.VCLUI.Wait, FireDAC.Phys.IBBase, FireDAC.Comp.Client,
	FireDAC.Comp.DataSet, System.Actions, Vcl.ActnList, Vcl.ComCtrls, 
  Vcl.ToolWin,
  FileCtrl;

type
	TFrmPrincipal = class(TForm)
		panLayoutPrincipal: TPanel;
		panLayoutDireita: TPanel;
		panLayoutEsquerda: TPanel;
		panDireita: TPanel;
		MemoResultado: TMemo;
		panLeft: TPanel;
		panLeftTop: TPanel;
		panLeftGridFields: TPanel;
		panLeftListaTabelas: TPanel;
		Panel2: TPanel;
		panLeftGridTabelas: TPanel;
		panTopo: TPanel;
		lblProjeto: TLabel;
		EdtBancoDeDados: TSearchBox;
		DBGrid1: TDBGrid;
		DBGrid2: TDBGrid;
		DSTabelas: TDataSource;
		DSCampos: TDataSource;
		FDMetaInfoTabelas: TFDMetaInfoQuery;
		FDConnection1: TFDConnection;
		FDPhysFBDriverLink1: TFDPhysFBDriverLink;
		FileOpenDialog1: TFileOpenDialog;
		FDMetaInfoCampos: TFDMetaInfoQuery;
		Panel1: TPanel;
		EdtTabela: TEdit;
		Label1: TLabel;
		EdtCampoBusca: TEdit;
		Label2: TLabel;
		FDMetaInfoTabelasTABLE_NAME: TWideStringField;
		FDMetaInfoCamposRECNO: TIntegerField;
		FDMetaInfoCamposCATALOG_NAME: TWideStringField;
		FDMetaInfoCamposSCHEMA_NAME: TWideStringField;
		FDMetaInfoCamposTABLE_NAME: TWideStringField;
		FDMetaInfoCamposCOLUMN_NAME: TWideStringField;
		FDMetaInfoCamposCOLUMN_POSITION: TIntegerField;
		FDMetaInfoCamposCOLUMN_DATATYPE: TIntegerField;
		FDMetaInfoCamposCOLUMN_TYPENAME: TWideStringField;
		FDMetaInfoCamposCOLUMN_ATTRIBUTES: TLongWordField;
		FDMetaInfoCamposCOLUMN_PRECISION: TIntegerField;
		FDMetaInfoCamposCOLUMN_SCALE: TIntegerField;
		FDMetaInfoCamposCOLUMN_LENGTH: TIntegerField;
		panBotaoDireita: TPanel;
		spbSelecionar: TSpeedButton;
		ActionList1: TActionList;
		actSair: TAction;
		ToolBar1: TToolBar;
		ToolButton1: TToolButton;
		ToolButton2: TToolButton;
		actGerarClasse: TAction;
		ToolButton3: TToolButton;
		ToolButton4: TToolButton;
    Panel3: TPanel;
    EdtPesquisaTabela: TSearchBox;
    Panel4: TPanel;
    TimerPesquisa: TTimer;
    FDMetaInfoChavePrimaria: TFDMetaInfoQuery;
    FDMetaInfoChavePrimariaRECNO: TIntegerField;
    FDMetaInfoChavePrimariaCATALOG_NAME: TWideStringField;
    FDMetaInfoChavePrimariaSCHEMA_NAME: TWideStringField;
    FDMetaInfoChavePrimariaTABLE_NAME: TWideStringField;
    FDMetaInfoChavePrimariaINDEX_NAME: TWideStringField;
    FDMetaInfoChavePrimariaCOLUMN_NAME: TWideStringField;
    FDMetaInfoChavePrimariaCOLUMN_POSITION: TIntegerField;
    FDMetaInfoChavePrimariaSORT_ORDER: TWideStringField;
    FDMetaInfoChavePrimariaFILTER: TWideStringField;
    btnSalvar: TSpeedButton;
    FileSaveDialog1: TFileSaveDialog;
    BtnConectar: TBitBtn;
		procedure DBGrid1CellClick(Column: TColumn);
		procedure spbSelecionarClick(Sender: TObject);
		procedure DSTabelasDataChange(Sender: TObject; Field: TField);
		procedure DSCamposDataChange(Sender: TObject; Field: TField);
		procedure FormShow(Sender: TObject);
		procedure FormDestroy(Sender: TObject);
		procedure FormClose(Sender: TObject; var Action: TCloseAction);
		procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
		procedure actSairExecute(Sender: TObject);
		procedure actGerarClasseExecute(Sender: TObject);
    procedure EdtBancoDeDadosInvokeSearch(Sender: TObject);
    procedure TimerPesquisaTimer(Sender: TObject);
    procedure EdtPesquisaTabelaChange(Sender: TObject);
    procedure btnSalvarClick(Sender: TObject);
    procedure BtnConectarClick(Sender: TObject);
	private
		{ Private declarations }
		procedure DesmembraCampoTipo(Texto: string; var Campo, Tipo: string);
		procedure AjustaNomeTipoDelphi(CampoBD, TipoBD: string; var CampoDelphi, TipoDelphi: string);
		procedure ConectaBancoDeDados(BD: string);
    function ConvertToCamelCase(const input: string): string;
    function ConvertToPascalCase(const input: string): string;
	public
		{ Public declarations }
	end;

var
	FrmPrincipal: TFrmPrincipal;

implementation

{$R *.dfm}

uses UnitArquivosIni.Model;

procedure TFrmPrincipal.DBGrid1CellClick(Column: TColumn);
begin
	FDMetaInfoCampos.Active       := False;
	FDMetaInfoCampos.MetaInfoKind := mkTableFields;
	FDMetaInfoCampos.ObjectName   := FDMetaInfoTabelas.FieldByName('TABLE_NAME').AsString;
	FDMetaInfoCampos.Active       := True;
end;

procedure TFrmPrincipal.FormClose(Sender: TObject; var Action: TCloseAction);
begin
	Application.Terminate;
end;

procedure TFrmPrincipal.FormDestroy(Sender: TObject);
begin
	FrmPrincipal := nil;
	TArquivosIni.New.SetSecao('GERADOR_CLASSES').GravarValor('BD', EdtBancoDeDados.Text);
end;

procedure TFrmPrincipal.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
	if Key = VK_ESCAPE then
		actSair.Execute;
end;

Procedure TFrmPrincipal.FormShow(Sender: TObject);
begin
	EdtBancoDeDados.Text := TArquivosIni.New.SetSecao('GERADOR_CLASSES').LerValor('BD', '');
	if not string(EdtBancoDeDados.Text).IsEmpty then
		ConectaBancoDeDados(EdtBancoDeDados.Text);
end;

procedure TFrmPrincipal.spbSelecionarClick(Sender: TObject);
begin
	actGerarClasse.Execute;
end;

procedure TFrmPrincipal.TimerPesquisaTimer(Sender: TObject);
begin
	TimerPesquisa.Enabled := False;
  if not string(EdtPesquisaTabela.Text).IsEmpty then
  begin
    FDMetaInfoTabelas.Filtered := False;
    FDMetaInfoTabelas.Filter   := 'TABLE_NAME LIKE '+QuotedStr('%'+String(EdtPesquisaTabela.Text).ToUpper+'%');
    FDMetaInfoTabelas.Filtered := True;
  end else
  begin
  	FDMetaInfoTabelas.Filtered := False;      
  end;
end;

procedure TFrmPrincipal.DesmembraCampoTipo(Texto: string; var Campo, Tipo: string);
var
	CampoAux, TipoAux: string;
	i, Inicio, Fim   : Integer;
begin
	Campo  := '';
	Tipo   := '';
	Inicio := 0;
	// Busca o Início
	while Texto.Chars[Inicio] = ' ' do
	begin
		Inc(Inicio, 1);
	end;
	Fim := Inicio;
	// Busca o fim do nome do campo
	while Texto.Chars[Fim] <> ' ' do
	begin
		Inc(Fim, 1);
	end;
	// Copia o nome do campo
	Campo  := Texto.Substring(Inicio, Fim - Inicio);
	Inicio := Fim;
	// Busca o início do tipo
	while Texto.Chars[Inicio] = ' ' do
	begin
		Inc(Inicio, 1);
	end;
	// Verifica se tem virgula no final do texto
	if (Texto.Chars[Texto.Length - 1] = ',') then
		Fim := Texto.Length - 1
	else
		Fim := Texto.Length;
	Tipo  := Texto.Substring(Inicio, Fim - Inicio);
end;

procedure TFrmPrincipal.DSCamposDataChange(Sender: TObject; Field: TField);
begin
	EdtCampoBusca.Text := FDMetaInfoCamposCOLUMN_NAME.AsString;
end;

procedure TFrmPrincipal.DSTabelasDataChange(Sender: TObject; Field: TField);
begin
	EdtTabela.Text := FDMetaInfoTabelasTABLE_NAME.AsString;
end;

procedure TFrmPrincipal.EdtBancoDeDadosInvokeSearch(Sender: TObject);
begin
	if FileOpenDialog1.Execute then
	begin
		EdtBancoDeDados.Text := FileOpenDialog1.FileName;
		ConectaBancoDeDados(EdtBancoDeDados.Text);
	end;
end;

procedure TFrmPrincipal.EdtPesquisaTabelaChange(Sender: TObject);
begin
	TimerPesquisa.Enabled := True;
end;

procedure TFrmPrincipal.actGerarClasseExecute(Sender: TObject);
var
	i                               : Integer;
	Campo, Tipo                     : string;
	NomeCampoDelphi, TipoCampoDelphi: string;
begin
	if EdtTabela.Text = '' then
	begin
		ShowMessage('Informe o Nome da Tabela!');
		EdtTabela.SetFocus;
		Exit;
	end;
	if EdtCampoBusca.Text = '' then
	begin
		ShowMessage('Informe o nome do Campo de Busca!');
		EdtCampoBusca.SetFocus;
		Exit;
	end;
	MemoResultado.Lines.Clear;
  MemoResultado.Lines.Add(Format('unit Unit%s.Model;', [ConvertToCamelCase(String(EdtTabela.Text))]));
  MemoResultado.Lines.Add('');
  MemoResultado.Lines.Add('interface');
  MemoResultado.Lines.Add('');
  MemoResultado.Lines.Add('uses');
  MemoResultado.Lines.Add('  {$IFDEF PORTALORM}');
  MemoResultado.Lines.Add('  UnitPortalORM.Model;');
  MemoResultado.Lines.Add('  {$ELSE}');
  MemoResultado.Lines.Add('  UnitBancoDeDados.Model;');
  MemoResultado.Lines.Add('  {$ENDIF}');
  MemoResultado.Lines.Add('');
  MemoResultado.Lines.Add('type');  
  MemoResultado.Lines.Add('  [TRecursoServidor(''/'+ConvertToPascalCase(String(EdtTabela.Text))+''')]');
	MemoResultado.Lines.Add('  [TNomeTabela(''' + EdtTabela.Text + ''', ''' + EdtCampoBusca.Text + ''')]');
	MemoResultado.Lines.Add(Format('  T%s = class(TTabela)', [ConvertToCamelCase(String(EdtTabela.Text))]));
	MemoResultado.Lines.Add('  private');
	MemoResultado.Lines.Add('    { private declarations }');
	FDMetaInfoCampos.First;
	while not FDMetaInfoCampos.Eof do
	begin
		Campo := FDMetaInfoCamposCOLUMN_NAME.AsString;
		Tipo  := FDMetaInfoCamposCOLUMN_TYPENAME.AsString;
    AjustaNomeTipoDelphi(Campo, Tipo, NomeCampoDelphi, TipoCampoDelphi);
		MemoResultado.Lines.Add('    F' + NomeCampoDelphi + ': ' + TipoCampoDelphi+';');
		FDMetaInfoCampos.Next;
	end;
  MemoResultado.Lines.Add('  public');
	MemoResultado.Lines.Add('    { public declarations }');    
	FDMetaInfoCampos.First;
	while not FDMetaInfoCampos.Eof do
	begin
		Campo := FDMetaInfoCamposCOLUMN_NAME.AsString;
		Tipo  := FDMetaInfoCamposCOLUMN_TYPENAME.AsString;
    //Chave primaria
    FDMetaInfoChavePrimaria.Active := False;
    FDMetaInfoChavePrimaria.MetaInfoKind   := mkPrimaryKeyFields;  
    FDMetaInfoChavePrimaria.BaseObjectName := FDMetaInfoTabelas.FieldByName('TABLE_NAME').AsString;
    FDMetaInfoChavePrimaria.Active         := True;
    /////
		if Tipo.Contains('CHAR') or Tipo.Contains('VARCHAR') then
			MemoResultado.Lines.Add(Format('    [TCampo(''%s'', ''%s'')]', [Campo, Tipo + '(' + FDMetaInfoCamposCOLUMN_LENGTH.AsString + ')']))
		else if Tipo.Contains('DECIMAL') or Tipo.Contains('NUMERIC') then
			MemoResultado.Lines.Add(Format('    [TCampo(''%s'', ''%s'')]', [Campo, 'NUMERIC(' + FDMetaInfoCamposCOLUMN_PRECISION.AsString + ',' + FDMetaInfoCamposCOLUMN_SCALE.AsString + ')']))
		else if FDMetaInfoChavePrimariaCOLUMN_NAME.AsString = FDMetaInfoCamposCOLUMN_NAME.AsString then    	        
    	MemoResultado.Lines.Add(Format('    [TCampo(''%s'', ''%s'')]', [Campo, 'INTEGER NOT NULL PRIMARY KEY']))
    else
			MemoResultado.Lines.Add(Format('    [TCampo(''%s'', ''%s'')]', [Campo, Tipo]));
		AjustaNomeTipoDelphi(Campo, Tipo, NomeCampoDelphi, TipoCampoDelphi);
		MemoResultado.Lines.Add('    property ' + NomeCampoDelphi + ': ' + TipoCampoDelphi + ' read F' + NomeCampoDelphi + ' write F' + NomeCampoDelphi + ';');
		FDMetaInfoCampos.Next;
	end;
	MemoResultado.Lines.Add('  end;');
  MemoResultado.Lines.Add('');
  MemoResultado.Lines.Add('implementation');
  MemoResultado.Lines.Add('');
  MemoResultado.Lines.Add('end.');
	FDMetaInfoCampos.First;
end;

function TFrmPrincipal.ConvertToPascalCase(const input: string): string;
var
  i: Integer;
  words: TStringList;
begin
  // Divide a entrada em palavras usando o caractere de sublinhado como delimitador
  words := TStringList.Create;
  try
    words.Delimiter := '_';
    words.DelimitedText := input;

    // Transforma a primeira letra de cada palavra em maiúscula
    for i := 0 to words.Count - 1 do
    begin
      words[i] := AnsiUpperCase(words[i][1]) + AnsiLowerCase(Copy(words[i], 2, MaxInt));
    end;

    // Concatena as palavras para formar o resultado final    
    Result := words.Text.Replace(sLineBreak, '');
    Result := Result.Substring(0, 1).ToLower+(Result.Substring(1, Result.Length))
  finally
    words.Free;
  end;
end;

function TFrmPrincipal.ConvertToCamelCase(const input: string): string;
var
  i: Integer;
  words: TStringList;
begin
  // Divide a entrada em palavras usando o caractere de sublinhado como delimitador
  words := TStringList.Create;
  try
    words.Delimiter := '_';
    words.DelimitedText := input;

    // Transforma a primeira letra de cada palavra em maiúscula
    for i := 0 to words.Count - 1 do
    begin
      words[i] := AnsiUpperCase(words[i][1]) + AnsiLowerCase(Copy(words[i], 2, MaxInt));
    end;

    // Concatena as palavras para formar o resultado final    
    Result := words.Text.Replace(sLineBreak, '');
  finally
    words.Free;
  end;
end;

procedure TFrmPrincipal.actSairExecute(Sender: TObject);
begin
	if Application.MessageBox('Deseja realmente sair da aplicação?', 'Confirmar', MB_YESNO) = IDYes then
	begin
		Self.Close;
  end;
end;

procedure TFrmPrincipal.AjustaNomeTipoDelphi(CampoBD, TipoBD: string; var CampoDelphi, TipoDelphi: string);
var
	Inicio: Integer;
begin
	Inicio      := CampoBD.IndexOf('_');
	CampoDelphi := CampoBD.Substring(Inicio + 1, CampoBD.Length - Inicio);
	CampoDelphi := CampoDelphi.Chars[0] + CampoDelphi.LowerCase(CampoDelphi.Substring(1, CampoDelphi.Length - 1));
	if TipoBD.Contains('VARCHAR') then
		TipoDelphi := 'string';
	if (TipoBD.Contains('NUMERIC')) or (TipoBD.Contains('FLOAT')) or (TipoBD.Contains('DECIMAL')) then
		TipoDelphi := 'double';
	if TipoBD.Contains('INTEGER') then
		TipoDelphi := 'integer';
	if TipoBD.Contains('DATE') then
		TipoDelphi := 'TDate';
	if TipoBD.Contains('TIME') then
		TipoDelphi := 'TTime';
	if TipoBD.Contains('TIMESTAMP') then
		TipoDelphi := 'TDateTime';
	if TipoBD.Contains('SMALLINT') then
		TipoDelphi := 'integer';
	if TipoBD.Contains('CHAR') then
		TipoDelphi := 'string';
end;

procedure TFrmPrincipal.BtnConectarClick(Sender: TObject);
begin
	ConectaBancoDeDados(EdtBancoDeDados.Text);
end;

procedure TFrmPrincipal.btnSalvarClick(Sender: TObject);
var
  DiretorioRaiz: string;
  NomeArquivo: string;
  Diretorio: string;
  DirInicial: string;
begin
	DirInicial := String(EdtBancoDeDados.Text).Split(['Dados'])[0];
  if SelectDirectory(DirInicial, [sdAllowCreate, sdPerformCreate, sdPrompt], 0) then
  begin   
  	DiretorioRaiz := ExtractFileDir(DirInicial.Substring(0, DirInicial.Length)+'\');
    Diretorio := DiretorioRaiz+'\'+ConvertToCamelCase(String(EdtTabela.Text));
    if not DirectoryExists(Diretorio) then
    	ForceDirectories(Diretorio);
    NomeArquivo := Diretorio+Format('\Unit%s.Model.pas', [ConvertToCamelCase(String(EdtTabela.Text))]);
    MemoResultado.Lines.SaveToFile(ChangeFileExt(NomeArquivo, '.pas'));
    Application.MessageBox('Unit salva com sucesso!', 'Salvando unit', MB_OK+MB_ICONINFORMATION);
  end;
end;

procedure TFrmPrincipal.ConectaBancoDeDados(BD: string);
begin
	FDConnection1.Close;
	FDConnection1.Params.Database := BD;
	FDConnection1.Connected       := True;
	FDMetaInfoTabelas.Active      := True;
  EdtPesquisaTabela.SetFocus;
end;

end.
