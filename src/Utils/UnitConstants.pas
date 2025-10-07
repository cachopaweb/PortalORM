unit UnitConstants;

interface

uses System.SysUtils;

type
	TAPIError = class
  private
    Ferror: string;
  public
    property error: string read Ferror write Ferror;
  end;
  
  TConstants = class
    class function BancoDados: string;
  end;

implementation

{ TConstants }

uses System.StrUtils;

class function TConstants.BancoDados: string;
begin
  Result := GetEnvironmentVariable('CAMINHO_BD');
end;

end.
