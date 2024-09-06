unit UnitArquivosIni.Model;

interface

uses
  System.SysUtils,
  IniFiles,
  System.Variants;

type
  iArquivosIni = interface
    ['{97DD7FC9-74B6-494C-9CFE-A690AFC81F8E}']
    function SetSecao(Value: string): iArquivosIni;
    function GravarValor(Identificador: string; Value: variant): iArquivosIni;
    function Apagar(Indentificador: string): iArquivosIni;
    function LerValor(Identificador: string; Default: integer): integer; overload;
    function LerValor(Identificador: string; Default: boolean): boolean; overload;
    function LerValor(Identificador: string; Default: string): string; overload;
    function LerValor(Identificador: string; Default: double): double; overload;
    function LerValor(Identificador: string; Default: TDateTime): TDateTime; overload;
  end;

  TArquivosIni = class(TInterfacedObject, iArquivosIni)
  private
    FSecao: string;
    FIniFile: TIniFile;
  public
    constructor Create();
    destructor Destroy; override;
    class function New(): iArquivosIni;
    function SetSecao(Value: string): iArquivosIni;
    function GravarValor(Identificador: string; Value: variant): iArquivosIni;
    function Apagar(Indentificador: string): iArquivosIni;
    function LerValor(Identificador: string; Default: integer): integer; overload;
    function LerValor(Identificador: string; Default: boolean): boolean; overload;
    function LerValor(Identificador: string; Default: string): string; overload;
    function LerValor(Identificador: string; Default: double): double; overload;
    function LerValor(Identificador: string; Default: TDateTime): TDateTime; overload;
  end;

implementation

{ TArquivosIni }

function TArquivosIni.Apagar(Indentificador: string): iArquivosIni;
begin
	Result := Self;
	FIniFile.DeleteKey(FSecao, Indentificador);
end;

constructor TArquivosIni.Create();
var
  CaminhoIni: string;
begin
  CaminhoIni := GetEnvironmentVariable('LOCALAPPDATA') + '\PORTAL\' + ExtractFileName(ParamStr(0));
  if not DirectoryExists(ExtractFileDir(CaminhoIni)) then
    ForceDirectories(ExtractFileDir(CaminhoIni));
  FIniFile := TIniFile.Create(ChangeFileExt(CaminhoIni, '.ini'))
end;

destructor TArquivosIni.Destroy;
begin
  FIniFile.Free;
  inherited;
end;

function TArquivosIni.GravarValor(Identificador: string; Value: variant): iArquivosIni;
begin
  case TVarData(Value).vType of
    varString, varUString:
      FIniFile.WriteString(FSecao, Identificador, string(Value));
    varSmallint, varInteger:
      FIniFile.WriteInteger(FSecao, Identificador, integer(Value));
    varDouble, varCurrency:
      FIniFile.WriteFloat(FSecao, Identificador, double(Value));
    varDate:
      FIniFile.WriteFloat(FSecao, Identificador, TDateTime(Value));
    varBoolean:
    	FIniFile.WriteBool(FSecao, Identificador, boolean(Value));
  end;
end;

function TArquivosIni.LerValor(Identificador: string; Default: integer): integer;
begin
  Result := FIniFile.ReadInteger(FSecao, Identificador, Default);
end;

function TArquivosIni.LerValor(Identificador: string; Default: boolean): boolean;
begin
  Result := FIniFile.ReadBool(FSecao, Identificador, Default);
end;

function TArquivosIni.LerValor(Identificador, Default: string): string;
begin
  Result := FIniFile.ReadString(FSecao, Identificador, Default)
end;

function TArquivosIni.LerValor(Identificador: string; Default: double): double;
begin
  Result := FIniFile.ReadFloat(FSecao, Identificador, Default);
end;

function TArquivosIni.LerValor(Identificador: string; Default: TDateTime): TDateTime;
begin
  Result := FIniFile.ReadDateTime(FSecao, Identificador, Default);
end;

class function TArquivosIni.New(): iArquivosIni;
begin
  Result := Self.Create();
end;

function TArquivosIni.SetSecao(Value: string): iArquivosIni;
begin
  Result := Self;
  FSecao := Value;
end;

end.
