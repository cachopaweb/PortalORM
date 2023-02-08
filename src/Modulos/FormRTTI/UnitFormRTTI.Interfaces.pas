unit UnitFormRTTI.Interfaces;

interface

uses
  UnitPortalORM.Model,
  Vcl.Forms;

type
  TLigarCampos = class(TCustomAttribute)
  private
    FCampo: string;
    FApenasLeitura: boolean;
  public
    property Campo: string read FCampo write FCampo;
    property ApenasLeitura: boolean read FApenasLeitura write FApenasLeitura;
    constructor Create(aCampo: string; aApenasLeitura: boolean = false);
  end;

{$SCOPEDENUMS ON}

  TTipoOperacao = (Insercao, Edicao, Confirmar, Cancelar, Inicio);
{$SCOPEDENUMS OFF}
  TEventoTipoOperacao = procedure(TipoOperacao: TTipoOperacao) of object;

type
  iFormRTTI = interface
    ['{A46CA0C1-2FD1-457A-BD5A-B3384446129B}']
    function Inserir: iFormRTTI;
    function Excluir(id: integer): iFormRTTI;
    function Confirmar: iFormRTTI;
    function Cancelar: iFormRTTI;
    function Editar: iFormRTTI;
    function BindForm(FForm: TForm): iFormRTTI;
    function SetEventoTipoOperacao(Value: TEventoTipoOperacao): iFormRTTI;
    function SetTabela(Tabela: TTabela): iFormRTTI;
  end;

implementation

{ TFormParaClass }

constructor TLigarCampos.Create(aCampo: string; aApenasLeitura: boolean = false);
begin
  FCampo         := aCampo;
  FApenasLeitura := aApenasLeitura;
end;

end.
