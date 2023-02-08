unit UnitRTTI.Helper;

interface
uses
  System.RTTI,
  UnitBancoDeDados.Model;

type
  TRttiFieldHelper = class helper for TRttiField
  public
    function Tem<T: TCustomAttribute>: Boolean;
    function GetAttribute<T: TCustomAttribute>: T;
  end;

  TRttiTypeHelper = class helper for TRttiType
  public
    function Tem<T: TCustomAttribute>: Boolean;
    function GetAttribute<T: TCustomAttribute>: T;
    function GetPropertyFromAttribute<T: TCustomAttribute>
      : TRttiProperty; overload;
    function GetPropertyFromAttribute<T: TCampo>(const aFieldName: string)
      : TRttiProperty; overload;
  end;

  TRttiPropertyHelper = class helper for TRttiProperty
  public
    function Tem<T: TCustomAttribute>: Boolean;
    function GetAttribute<T: TCustomAttribute>: T;
    function EhCampo: Boolean;
    function FieldName: string;
  end;

implementation

{ TRttiFieldHelper }

function TRttiFieldHelper.GetAttribute<T>: T;
var
  oAtributo: TCustomAttribute;
begin
  Result := nil;
  for oAtributo in GetAttributes do
    if oAtributo is T then
      Exit((oAtributo as T));
end;

function TRttiFieldHelper.Tem<T>: Boolean;
begin
  Result := GetAttribute<T> <> nil
end;


{ TRttiTypeHelper }

function TRttiTypeHelper.GetAttribute<T>: T;
var
  oAtributo: TCustomAttribute;
begin
  Result := nil;
  for oAtributo in GetAttributes do
    if oAtributo is T then
      Exit((oAtributo as T));
end;

function TRttiTypeHelper.GetPropertyFromAttribute<T>(const aFieldName: string): TRttiProperty;
var
  RttiProp: TRttiProperty;
begin
  Result := nil;
  for RttiProp in GetProperties do
  begin
    if RttiProp.GetAttribute<T> = nil then
      Continue;

    if RttiProp.GetAttribute<TCampo>.Nome = aFieldName then
      Exit(RttiProp);
  end;
end;

function TRttiTypeHelper.GetPropertyFromAttribute<T>: TRttiProperty;
var
  RttiProp: TRttiProperty;
begin
  Result := nil;
  for RttiProp in GetProperties do
    if RttiProp.GetAttribute<T> <> nil then
      Exit(RttiProp);
end;

function TRttiTypeHelper.Tem<T>: Boolean;
begin
  Result := GetAttribute<T> <> nil
end;

{ TRttiPropertyHelper }

function TRttiPropertyHelper.GetAttribute<T>: T;
var
  oAtributo: TCustomAttribute;
begin
  Result := nil;
  for oAtributo in GetAttributes do
    if oAtributo is T then
      Exit((oAtributo as T));
end;

function TRttiPropertyHelper.EhCampo: Boolean;
begin
  Result := Tem<TCampo>
end;

function TRttiPropertyHelper.FieldName: string;
begin
  Result := Name;
  if EhCampo then
    Result := GetAttribute<TCampo>.Nome;
end;

function TRttiPropertyHelper.Tem<T>: Boolean;
begin
  Result := GetAttribute<T> <> nil
end;


end.
