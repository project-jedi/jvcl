unit DataPluginU;


interface


uses

   Windows, Messages, SysUtils, Classes, Dialogs, Forms,

   JvPlugin, Db, DBTables, DBGrids;


type


  TuilPlugin1 = class(TJvPlugin)

    Table1: TTable;

    DataSource1: TDataSource;

    procedure uilPlugin1Create(Sender: TObject);

    procedure uilPlugin1Initialize(Sender: TObject;
      var AllowLoad: Boolean);

  private

    { Private declarations }

  public

    { Public declarations }

  end;


function RegisterPlugin : TuilPlugin1; stdcall;


implementation


{$R *.DFM}


// IMPORTANT NOTE: If you change the name of the Plugin container,

// you must set the type below to the same type. (Delphi changes

// the declaration, but not the procedure itself. Both the return

// type and the type created must be the same as the declared type above.

function RegisterPlugin : TuilPlugin1;

begin

  Result := TuilPlugin1.Create(nil);

end;


procedure TuilPlugin1.uilPlugin1Create(Sender: TObject);
begin
   Table1.Open;
end;

procedure TuilPlugin1.uilPlugin1Initialize(Sender: TObject;
  var AllowLoad: Boolean);
var
   Comp : TComponent;
begin
   Comp := HostApplication.MainForm.FindComponent('dbgrid1');
   if assigned(Comp) then
      TDBGrid(Comp).DataSource := DataSource1;
end;

end.
