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
