unit UnitCommand.Interfaces;

interface

type
  iCommand = interface
    ['{260DAF48-4AD1-4A8D-AB6E-998328A8BC80}']
    function Execute: iCommand;
  end;

  iInvoker = interface
    ['{6ED009B8-018F-4B2B-8ECC-E2DE91B01400}']
    function Add(Value: iCommand): iInvoker;
    function Execute: iInvoker;
  end;

implementation

end.
