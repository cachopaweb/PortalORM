# PortalORM

A simple ORM for Delphi and Firedac

### For install in your project using [boss](https://github.com/HashLoad/boss):
``` sh
$ boss install https://github.com/CachopaWeb/PortalORM
```

## Sample PortalORM

```delphi

interface

uses
  UnitPortalORM.Model;

type
  [TRecursoServidor('/usuarios')]
  [TNomeTabela('USUARIOS', 'USU_CODIGO')]
  TUsuarios = class(TTabela)
  private
    FCodigo: integer;
    FLogin: string;
    Ffun_codigo: integer;
    FSenha: string;
    { private declarations }
  public
    { public declarations }
    [TCampo('USU_CODIGO', 'INTEGER NOT NULL PRIMARY KEY')]
    property Codigo: integer read FCodigo write FCodigo;
    [TCampo('USU_LOGIN', 'VARCHAR(20)')]
    property Login: string read FLogin write FLogin;
    [TCampo('USU_FUN', 'INTEGER')]
    property fun_codigo: integer read Ffun_codigo write Ffun_codigo;
    [TCampo('USU_SENHA', 'VARCHAR(30)')]
    property Senha: string read FSenha write FSenha;
  end;

implementation

end.

```


## Establishing the database connection

```delphi

unit UnitDatabase;

interface
uses
  UnitConnection.Model.Interfaces;

type
  TDatabase = class
    class function Connection: iConnection;
    class function Query: iQuery;
  end;

implementation

{ TDatabase }

uses
  UnitConstantes,
  UnitConnection.Firedac.Model,
  UnitFactory.Connection.Firedac;

class function TDatabase.Connection: iConnection;
begin
  Result := TConnectionFiredac.New(TConstants.BancoDados);
end;

class function TDatabase.Query: iQuery;
begin
  Result := TFactoryConnectionFiredac.New(TConstants.BancoDados).Query;
end;

end.
´´´

## Fetching data and saving to the database

```delphi
var
  Usuario: TUsuarios;
begin
  Usuario := TUsuarios.Create(TDatabase.Connection);
  Usuario.BuscaDadosTabela(id);
end;

var
  Usuario: TUsuarios;
begin
  Usuario := TUsuarios.Create(TDatabase.Connection);
  Usuario.SalvaNoBanco(1);
end;

´´´
