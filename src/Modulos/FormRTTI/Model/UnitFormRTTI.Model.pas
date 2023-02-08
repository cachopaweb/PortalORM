unit UnitFormRTTI.Model;

interface

uses
  System.Generics.Collections,
  System.RTTI,
  Vcl.Forms,
  Data.DB,
  TypInfo,
  System.Classes,
  System.SysUtils,
  System.Variants,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  Vcl.Graphics,
  JvValidateEdit,
  UnitFormRTTI.Interfaces,
  UnitBancoDeDados.Model,
  UnitRTTI.Helper,
  JvBaseEdits,
  UnitClienteREST.Model.Interfaces, Vcl.Mask, Vcl.Dialogs, Winapi.Windows;

type
  TFormRTTI = class(TInterfacedObject, iFormRTTI)
  private
    FForm: TForm;
    FTabela: TTabela;
    FTipoOperacao: TTipoOperacao;
    FEventoTipoOperacao: TEventoTipoOperacao;
    procedure LimparCampos(Tabela: TTabela);
    procedure LimpaPropriedades(Tabela: TTabela);
    procedure HabilitarComponents(Value: Boolean);
    procedure ValorParaComponent(Component: TComponent; Valor: Variant);
    procedure ValorParaProperty(Tabela: TTabela; Propriedade: TRttiProperty; Valor: TValue);
    function ObtemValorComponent(Component: TComponent): TValue;
    function ObtemRTTIPropertyValue(Tabela: TTabela; NomePropriedade: String): Variant;
    function ObtemRTTIProperty(Tabela: TTabela; NomePropriedade: String): TRttiProperty;
    procedure ClasseParaForm;
    procedure FormParaClasse;
  public
    constructor Create;
    destructor Destroy; override;
    class function New : iFormRTTI;
    function Inserir: iFormRTTI;
    function Excluir(id: integer): iFormRTTI;
    function Confirmar: iFormRTTI;
    function Cancelar: iFormRTTI;
    function Editar: iFormRTTI;
    function BindForm(Form: TForm): iFormRTTI;
    function SetEventoTipoOperacao(Value: TEventoTipoOperacao): iFormRTTI;
    function SetTabela(Tabela: TTabela): iFormRTTI;
  end;

implementation



{ TFormRTTI }

uses UnitTabela.Helper.Json;

function TFormRTTI.Editar: iFormRTTI;
begin
  Result := Self;
  HabilitarComponents(True);
  FTipoOperacao := TTipoOperacao.Edicao;
  if Assigned(FEventoTipoOperacao) then
    FEventoTipoOperacao(FTipoOperacao);
end;

procedure TFormRTTI.ClasseParaForm;
var
  ctxRtti : TRttiContext;
  typRtti : TRttiType;
  prpRtti : TRttiField;
begin
  ctxRtti := TRttiContext.Create;
  try
    typRtti := ctxRtti.GetType(FForm.ClassInfo);
    for prpRtti in typRtti.GetFields do
    begin
      if prpRtti.Tem<TLigarCampos> then
      begin
        ValorParaComponent(
          FForm.FindComponent(prpRtti.Name),
          ObtemRTTIPropertyValue(FTabela, prpRtti.GetAttribute<TLigarCampos>.Campo)
        );
      end;
    end;
    HabilitarComponents(False);
  finally
    ctxRtti.Free;
  end;
end;

function TFormRTTI.BindForm(Form: TForm): iFormRTTI;
begin
  Result := Self;
  FForm := Form;
  //inicializa a ligacao do form com a classe
  ClasseParaForm;
end;

function TFormRTTI.Cancelar: iFormRTTI;
var
  Response: TClientResult;
begin
  Result := Self;
  if not FTabela.TemBaseURL then
    FTabela.BuscaDadosTabela(1);
  FTipoOperacao := TTipoOperacao.Cancelar;
  if Assigned(FEventoTipoOperacao) then
    FEventoTipoOperacao(FTipoOperacao);
  BindForm(FForm);
end;

function TFormRTTI.Confirmar: iFormRTTI;
var
  Response: TClientResult;
begin
  Result := Self;
  try
    FormParaClasse;
    //se tem a base URL então usa implementações de metodos REST
    //atraves dos helpers json
    if FTabela.TemBaseURL then
    begin
      if FTipoOperacao = TTipoOperacao.Insercao then
        Response := FTabela.Post
      else
        Response := FTabela.Put;
      if not Response.StatusCode in [200, 201] then
        raise Exception.Create('Erro ao confirmar dados no servidor!'+sLineBreak
                              +Response.Content+sLineBreak
                              +'Erro:'+Response.Error);
    end else
      FTabela.SalvaNoBanco();
    Application.MessageBox('Dados confirmados com sucesso!', 'Sucesso', MB_OK+MB_ICONINFORMATION);
    if Assigned(FEventoTipoOperacao) then
      FEventoTipoOperacao(TTipoOperacao.Confirmar);
  except on E: Exception do
    begin
      raise Exception.Create('Erro ao inserir confirmar dados!'+sLineBreak+E.Message);
    end;
  end;
end;

constructor TFormRTTI.Create;
begin

end;

function TFormRTTI.Excluir(id: integer): iFormRTTI;
begin
  Result := Self;
  try
    if FTabela.TemBaseURL then
    begin
      FTabela.Delete(id);
    end else
    begin
      FTabela.Apagar(id);
      FTabela.BuscaDadosTabela(id-1);
    end;
    Application.MessageBox('Dados Excluidos com sucesso!', 'Sucesso', MB_OK+MB_ICONINFORMATION);
    BindForm(FForm);
  except on E: Exception do
    begin
      raise Exception.Create('Erro tentar excluir registro!'+sLineBreak+E.Message);
    end;
  end;
end;

destructor TFormRTTI.Destroy;
begin

  inherited;
end;

procedure TFormRTTI.HabilitarComponents(Value: Boolean);
var
  ctxRtti : TRttiContext;
  typRtti : TRttiType;
  prpRtti : TRttiField;
  Component: TComponent;
begin
  ctxRtti := TRttiContext.Create;
  try
    typRtti := ctxRtti.GetType(FForm.ClassInfo);
    for prpRtti in typRtti.GetFields do
    begin
      if prpRtti.Tem<TLigarCampos> then
      begin
        Component := FForm.FindComponent(prpRtti.Name);
        if Assigned(Component) then
        begin
          if Component is TLabel then
            (Component as TLabel).Enabled := Value;

          if Component is TEdit then
            (Component as TEdit).Enabled := Value;

          if Component is TJvValidateEdit then
            (Component as TJvValidateEdit).Enabled := Value;

          if Component is TJvCalcEdit then
            (Component as TJvCalcEdit).Enabled := Value;

          if Component is TComboBox then
            (Component as TComboBox).Enabled := Value;

          if Component is TRadioGroup then
            (Component as TRadioGroup).Enabled := Value;

          if Component is TShape then
            (Component as TShape).Brush.Color := clGray;

          if Component is TDateTimePicker then
            (Component as TDateTimePicker).Enabled := Value;

          if Component is TCheckBox then
            (Component as TCheckBox).Enabled := Value;

          if Component is TTrackBar then
            (Component as TTrackBar).Enabled := Value;

          if Component is TMemo then
            (Component as TMemo).Enabled := Value;

          if Component is TMaskEdit then
            (Component as TMaskEdit).Enabled := Value;
        end;
      end;
    end;
  finally
    ctxRtti.Free;
  end;

end;

procedure TFormRTTI.FormParaClasse;
var
  ctxRtti : TRttiContext;
  typRtti : TRttiType;
  prpRtti : TRttiField;
begin
  ctxRtti := TRttiContext.Create;
  try
    typRtti := ctxRtti.GetType(FForm.ClassInfo);
    for prpRtti in typRtti.GetFields do
    begin
      if prpRtti.Tem<TLigarCampos> and (not prpRtti.GetAttribute<TLigarCampos>.ApenasLeitura) then
      begin
        ValorParaProperty(
          FTabela,
          ObtemRTTIProperty(FTabela, prpRtti.GetAttribute<TLigarCampos>.Campo),
          ObtemValorComponent(FForm.FindComponent(prpRtti.Name))
        );
      end;
    end;
  finally
    ctxRtti.Free;
  end;
end;

function TFormRTTI.Inserir: iFormRTTI;
begin
  Result := Self;
  LimparCampos(FTabela);
  FTipoOperacao := TTipoOperacao.Insercao;
  if Assigned(FEventoTipoOperacao) then
    FEventoTipoOperacao(FTipoOperacao);
  HabilitarComponents(True);
end;

procedure TFormRTTI.LimpaPropriedades(Tabela: TTabela);
var
  ctxRtti : TRttiContext;
  typRtti : TRttiType;
  prpRttiProp: TRttiProperty;
begin
  typRtti := ctxRtti.GetType(Tabela.ClassInfo);
  try
    for prpRttiProp in typRtti.GetProperties do
    begin
       case prpRttiProp.PropertyType.TypeKind of
        tkUnknown: ;
        tkInteger: prpRttiProp.SetValue(Pointer(Tabela), 0);
        tkChar: ;
        tkEnumeration: ;
        tkFloat: prpRttiProp.SetValue(Pointer(Tabela), 0);
        tkSet: ;
        tkClass: ;
        tkMethod: ;
        tkString, tkWChar, tkLString, tkWString, tkVariant, tkUString:
          prpRttiProp.SetValue(Pointer(Tabela), '');
        tkArray: ;
        tkRecord: ;
        tkInterface: ;
        tkInt64: prpRttiProp.SetValue(Pointer(Tabela), 0);
        tkDynArray: ;
        tkClassRef: ;
        tkPointer: ;
        tkProcedure: ;
        else
          prpRttiProp.SetValue(Pointer(Tabela), '');
      end;
    end;
  finally

  end;
end;

procedure TFormRTTI.LimparCampos(Tabela: TTabela);
var
  ctxRtti : TRttiContext;
  typRtti : TRttiType;
  prpRtti : TRttiField;
  prpRttiProp: TRttiProperty;
  Component: TComponent;
begin
  ctxRtti := TRttiContext.Create;
  try
    ////
    LimpaPropriedades(Tabela);
    ////
    typRtti := ctxRtti.GetType(FForm.ClassInfo);
    for prpRtti in typRtti.GetFields do
    begin
      if prpRtti.Tem<TLigarCampos> then
      begin
        if not prpRtti.GetAttribute<TLigarCampos>.Campo.ToUpper.Contains('CODIGO') then
        begin
          Component := FForm.FindComponent(prpRtti.Name);
          if Assigned(Component) then
          begin
            if Component is TLabel then
              (Component as TLabel).Caption := '';

            if Component is TEdit then
              (Component as TEdit).Text := '';

            if Component is TJvValidateEdit then
              (Component as TJvValidateEdit).Value := 0;

            if Component is TJvCalcEdit then
              (Component as TJvCalcEdit).Value := 0;

            if Component is TComboBox then
              (Component as TComboBox).ItemIndex := -1;

            if Component is TRadioGroup then
              (Component as TRadioGroup).ItemIndex := -1;

            if Component is TShape then
              (Component as TShape).Brush.Color := clBtnFace;

            if (Component is TDateTimePicker) then
              (Component as TDateTimePicker).Date := Now;

            if Component is TCheckBox then
              (Component as TCheckBox).Checked := False;

            if Component is TTrackBar then
              (Component as TTrackBar).Position := 0;

            if Component is TMemo then
              (Component as TMemo).Lines.Clear;

            if Component is TMaskEdit then
              (Component as TMaskEdit).Text := '';
          end;
        end;
      end;
    end;
  finally
    ctxRtti.Free;
  end;
end;

class function TFormRTTI.New: iFormRTTI;
begin
  Result := Self.Create;
end;

function TFormRTTI.ObtemRTTIProperty(Tabela: TTabela; NomePropriedade: String): TRttiProperty;
var
  ctxRttiEntity : TRttiContext;
  typRttiEntity : TRttiType;
begin
  ctxRttiEntity := TRttiContext.Create;
  try
    typRttiEntity := ctxRttiEntity.GetType(Tabela.ClassInfo);
    Result := typRttiEntity.GetProperty(NomePropriedade);
    if not Assigned(Result) then
      Result := typRttiEntity.GetPropertyFromAttribute<TCampo>(NomePropriedade);

    if not Assigned(Result) then
      raise Exception.Create('Property ' + NomePropriedade + ' not found!');
  finally
    ctxRttiEntity.Free;
  end;  
end;

function TFormRTTI.ObtemRTTIPropertyValue(Tabela: TTabela; NomePropriedade: String): Variant;
begin
  Result := ObtemRTTIProperty(Tabela, NomePropriedade).GetValue(Pointer(Tabela)).AsVariant;
end;

function TFormRTTI.ObtemValorComponent(Component: TComponent): TValue;
begin
  if Component is TLabel then
    Result := TValue.FromVariant((Component as TLabel).Caption);
    
  if Component is TEdit then
    Result := TValue.FromVariant((Component as TEdit).Text);

  if Component is TJvValidateEdit then
    Result := TValue.FromVariant((Component as TJvValidateEdit).Value);

  if Component is TJvCalcEdit then
    Result := TValue.FromVariant((Component as TJvCalcEdit).Value);

  if Component is TComboBox then
    Result := TValue.FromVariant((Component as TComboBox).Items[(Component as TComboBox).ItemIndex]);

  if Component is TRadioGroup then
    Result := TValue.FromVariant((Component as TRadioGroup).Items[(Component as TRadioGroup).ItemIndex]);

  if Component is TShape then
    Result := TValue.FromVariant((Component as TShape).Brush.Color);
  
  if Component is TCheckBox then
    Result := TValue.FromVariant((Component as TCheckBox).Checked);
  
  if Component is TTrackBar then
    Result := TValue.FromVariant((Component as TTrackBar).Position);
  
  if Component is TDateTimePicker then
    Result := TValue.FromVariant((Component as TDateTimePicker).DateTime);

  if Component is TMemo then
    Result := TValue.FromVariant((Component as TMemo).Text);

  if Component is TMaskEdit then
    Result := TValue.FromVariant((Component as TMaskEdit).Text);
end;

function TFormRTTI.SetEventoTipoOperacao(Value: TEventoTipoOperacao): iFormRTTI;
begin
  Result := Self;
  FEventoTipoOperacao := Value;
end;

function TFormRTTI.SetTabela(Tabela: TTabela): iFormRTTI;
begin
  Result := Self;
  FTabela := Tabela;
end;

procedure TFormRTTI.ValorParaComponent(Component: TComponent; Valor: Variant);
begin
  if VarIsNull(Valor) then exit;

  if Component is TLabel then
  begin
    if True then

    (Component as TLabel).Caption := Valor;
  end;

  if Component is TEdit then
    (Component as TEdit).Text := Valor;

  if Component is TJvValidateEdit then
    (Component as TJvValidateEdit).Value := Valor;

  if Component is TJvCalcEdit then
    (Component as TJvCalcEdit).Value := Valor;

  if Component is TComboBox then
    (Component as TComboBox).ItemIndex := (Component as TComboBox).Items.IndexOf(Valor);

  if Component is TRadioGroup then
    (Component as TRadioGroup).ItemIndex := (Component as TRadioGroup).Items.IndexOf(Valor);

  if Component is TShape then
    (Component as TShape).Brush.Color := Valor;

  if (Component is TDateTimePicker) then
    (Component as TDateTimePicker).Date := Valor;

  if Component is TCheckBox then
    (Component as TCheckBox).Checked := Valor;

  if Component is TTrackBar then
    (Component as TTrackBar).Position := Valor;

  if Component is TMemo then
    (Component as TMemo).Text := Valor;

  if Component is TMaskEdit then
    (Component as TMaskEdit).Text := Valor;
end;

procedure TFormRTTI.ValorParaProperty(Tabela: TTabela; Propriedade: TRttiProperty; Valor: TValue);
begin
  case Propriedade.PropertyType.TypeKind of
    tkUnknown: ;
    tkInteger: Propriedade.SetValue(Pointer(Tabela), StrToInt(Valor.ToString));
    tkChar: ;
    tkEnumeration: ;
    tkFloat:
    begin
      if (Valor.TypeInfo = TypeInfo(TDate))
        or (Valor.TypeInfo = TypeInfo(TTime))
        or (Valor.TypeInfo = TypeInfo(TDateTime)) then
        Propriedade.SetValue(Pointer(Tabela), StrToDateTime(Valor.ToString))
      else
        Propriedade.SetValue(Pointer(Tabela), StrToFloat(Valor.ToString));
    end;
    tkSet: ;
    tkClass: ;
    tkMethod: ;
    tkString, tkWChar, tkLString, tkWString, tkVariant, tkUString:
      Propriedade.SetValue(Pointer(Tabela), Valor);
    tkArray: ;
    tkRecord: ;
    tkInterface: ;
    tkInt64: Propriedade.SetValue(Pointer(Tabela), Valor.Cast<Int64>);
    tkDynArray: ;
    tkClassRef: ;
    tkPointer: ;
    tkProcedure: ;
    else
      Propriedade.SetValue(Pointer(Tabela), Valor);
  end;
end;

end.
