unit MDIPluginU;


interface


uses

   Windows,
   Messages,
   SysUtils,
   Classes,
   Dialogs,
   Forms,
   JvPlugin,
   MDIPluginFormU;


type


  TuilMDIPluginSample = class( TJvPlugin )

    procedure uilMDIPluginSampleInitialize(Sender: TObject;
      var AllowLoad: Boolean);
    procedure uilMDIPluginSampleDestroy(Sender: TObject);
    procedure uilMDIPluginSampleCommands0Execute(Sender: TObject);
  private
    FMDIChild : TfrmMDIChild;
  protected
    OldApplication : TApplication;
  public
    { Public declarations }
  end;

var
   uilMDIPluginSample: TJvPlugin;


// IMPORTANT: This function should return the same
// type as above. For instance, the default is TuilPlugin1, so
// the declaration below would read:
// function RegisterPlugin : TuilPlugin1; stdcall;
// Hopefully I'll figure out a better way before release!
function RegisterPlugin : TuilMDIPluginSample; stdcall;


implementation




{$R *.DFM}


function RegisterPlugin : TuilMDIPluginSample;

begin

  Result := TuilMDIPluginSample.Create(nil);

end;


procedure TuilMDIPluginSample.uilMDIPluginSampleInitialize(Sender: TObject;
  var AllowLoad: Boolean);
begin
   OldApplication := Application;
   Application := HostApplication;
   FMDIChild := TfrmMDIChild.Create(nil);
end;

procedure TuilMDIPluginSample.uilMDIPluginSampleDestroy(Sender: TObject);
begin
   Application := OldApplication;
end;

procedure TuilMDIPluginSample.uilMDIPluginSampleCommands0Execute(Sender: TObject);
begin
   FMDIChild.WindowState := wsMaximized;
end;

end.
