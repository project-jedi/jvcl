unit PluginTest;

interface

uses
   Windows,
   Messages,
   SysUtils,
   Classes,
   Dialogs,
   Forms,
   JvPlugin,
   JvPlgIntf;

type
  TTest = class(TJvPlugin, IMyPluginInterface)
    procedure JvPluginDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Procedure ShowPlug(Sender : TObject);
  end;

function RegisterPlugin : TTest; stdcall;

exports
  RegisterPlugin;

implementation

uses ufrmPluginForm;

{$R *.DFM}

// IMPORTANT NOTE: If you change the name of the Plugin container,
// you must set the type below to the same type. (Delphi changes
// the declaration, but not the procedure itself. Both the return
// type and the type created must be the same as the declared type above.
function RegisterPlugin : TTest;
begin
  Result := TTest.Create(nil);
end;

{ TTest }

procedure TTest.ShowPlug(Sender: TObject);
begin
     if frmPluginForm=nil then
        frmPluginForm:=TfrmPluginForm.Create(Application);

     frmPluginForm.Show;
     //ShowMessage('ShowPlug was called');
end;

procedure TTest.JvPluginDestroy(Sender: TObject);
begin
     FreeAndNil(frmPluginForm);
end;

end.
