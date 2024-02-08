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
  UnitPortalORM.Model,
  UnitFuncionarios.Model;//for the relationship

type
  [TRecursoServidor('/usuarios')]
  [TNomeTabela('USUARIOS', 'USU_CODIGO')]
  TUsuarios = class(TTabela)
  private
    FCodigo: integer;
    FLogin: string;
    Ffun_codigo: integer;
    FSenha: string;
    FFuncionarios: TFuncionarios;
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
    [TRelacionamento('FUNCIONARIOS', 'FUN_CODIGO', 'USU_FUN', TFuncionarios, TTipoRelacionamento.UmPraUm)]
    property Funcionarios: TFuncionarios read FFuncionarios write FFuncionarios;
  end;

implementation

end.

```

## Sample Class Constants

```delphi
unit UnitConstants;

interface

uses System.SysUtils;

type
  TConstants = class
    class function BancoDados: string;
  end;

implementation

{ TConstants }

uses System.StrUtils;

class function TConstants.BancoDados: string;
begin
  Result := GetEnvironmentVariable('DB_HOST');
end;

end.
´´´


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
  UnitConstants,
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
//Fetching user for id
var
  Usuario: TUsuarios;
begin
  Usuario := TUsuarios.Create(TDatabase.Connection);
  Usuario.BuscaDadosTabela(id);
end;

//save user 
var
  Usuario: TUsuarios;
begin
  Usuario := TUsuarios.Create(TDatabase.Connection);
  Usuario.SalvaNoBanco(1);//trying one time
end;

//delete user
var
  Usuario: TUsuarios;
begin
  Usuario := TUsuarios.Create(TDatabase.Connection);
  Usuario.Apagar(id);
end;

//change user
var
  Usuario: TUsuarios;
begin
  Usuario := TUsuarios.Create(TDatabase.Connection);
  Usuario.BuscaDadosTabela(id);
  Usuario.Login := 'New Login';
  Usuario.SalvaNoBanco(3);//trying three times
end;

´´´
