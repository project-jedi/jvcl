unit JvProperyStoreEditorMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, JvProgramVersionCheck, JvComponent, JvAppStorage, JvAppIniStorage,
  StdCtrls, JvUrlListGrabber, JvUrlGrabbers, ImgList, JvPropertyStore,JvAppxmlStorage,
  JvComponentBase, ExtCtrls, JvPropertyStoreEditor, JvExControls, JvInspector;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Button2: TButton;
    Panel1: TPanel;
    procedure Button2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FJvPropertyStoreEditorControl: TJvPropertyStoreEditorControl;
    FVersionHistory: TJvProgramVersionHistory;
  protected
    property VersionHistory: TJvProgramVersionHistory read FVersionHistory write
        FVersionHistory;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

{.$DEFINE USE_DEVEXPRESS}

Uses JclFileUtils,

  {$IFDEF USE_DEVEXPRESS}
  JvDynControlEngineDevExpCx,
  {$ELSE}
  JvDynControlEngineJVCLInspector,
  JvDynControlEngineJVCL,
  {$ENDIF}
  JvTypes;

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FVersionHistory := TJvProgramVersionHistory.Create(self);

end;

destructor TForm1.Destroy;
begin
  FreeAndNil(FVersionHistory);
  inherited Destroy;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  EditPropertyStore(FVersionHistory) ;
  FJvPropertyStoreEditorControl.PropertyStore := FVersionHistory;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  FJvPropertyStoreEditorControl:= TJvPropertyStoreEditorControl.Create(self);
  FJvPropertyStoreEditorControl.Parent := Panel1;
  FJvPropertyStoreEditorControl.Align := alClient;
  FJvPropertyStoreEditorControl.PropertyStore := FVersionHistory;
end;

begin
  {$IFDEF USE_DEVEXPRESS}
  {$ELSE}
  RegisterJvDynControlRTTIInspectorControl(DynControlEngineJVCL);
  {$ENDIF}
end.
