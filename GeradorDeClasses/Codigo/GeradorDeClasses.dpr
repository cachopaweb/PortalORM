program GeradorDeClasses;

uses
  Vcl.Forms,
  UnitPrincipal in 'UnitPrincipal.pas' {FrmPrincipal},
  UnitArquivosIni.Model in '..\..\FormsComuns\Classes\ArquivosIni\Model\UnitArquivosIni.Model.pas';

{$R *.res}

begin
  Application.Initialize;
	Application.MainFormOnTaskbar := True;
	Application.CreateForm(TFrmPrincipal, FrmPrincipal);
  Application.Run;
end.
