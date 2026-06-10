unit UnitMenuContexto;

interface

uses
	ToolsAPI,
	System.Classes,
	Vcl.Dialogs;

type
	TUnitMenuNotifier = class(TNotifierObject, IOTAProjectMenuItemCreatorNotifier)
	public
		procedure AddMenuItems(const Project: IOTAProject; const IdentList: TStrings; const ProjectManagerMenuList: IInterfaceList; IsMultiSelect: Boolean);
	end;

implementation

procedure Register;
begin
	(BorlandIDEServices as IOTAProjectManager).AddMenuItemCreatorNotifier(TUnitMenuNotifier.Create);
end;

end.
