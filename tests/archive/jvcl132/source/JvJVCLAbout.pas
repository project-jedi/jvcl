unit JvJVCLAbout;
{$I JEDI.INC}
interface

uses
  Windows, Messages, SysUtils, {$IFDEF DELPHI6} Variants,{$ENDIF}
  Classes, Graphics, Controls, Forms,
  Dialogs, JvHotLink, StdCtrls, ExtCtrls, jpeg, JvLabel,
  {$IFDEF DELPHI5} DsgnIntf, {$ENDIF} {$IFDEF DELPHI6} DesignEditors, DesignIntf, {$ENDIF}
  JVCLVer;

type

  TJVCLAboutDialogProperty = class(TPropertyEditor)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
  end;

  TJvJVCLAboutForm = class(TForm)
    Bevel1: TBevel;
    lblVersion: TLabel;
    pnlImage: TPanel;
    Image1: TImage;
    btnOK: TButton;
    JvHotLink1: TJvHotLink;
    JvHotLink4: TJvHotLink;
    lblNews: TJvHotLink;
    Label1: TLabel;
    Label2: TLabel;
    lblCopyRight: TLabel;
    lblRights: TLabel;
    Image3: TImage;
    MainPanel: TPanel;
    Bevel2: TBevel;
    lblVisitJedi: TLabel;
    lblMailingList: TLabel;
    lblNewsgroup: TLabel;
    lblJvHotLink2: TJvHotLink;
    lblBugs: TLabel;
    lblBugsURL: TJvHotLink;
    procedure btnOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Panel1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    protected
      procedure CreateParams(var Params: TCreateParams); override;

  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  JvJVCLAboutForm: TJvJVCLAboutForm;

implementation

{$R *.dfm}

{ TJVCLAboutDialogProperty }

procedure TJVCLAboutDialogProperty.Edit;
var
  Dialog: TJvJVCLAboutForm;
begin
  Dialog := TJvJVCLAboutForm.Create(nil);
  try
    Dialog.ShowModal;
  finally
    Dialog.Free;
  end;

end;

function TJVCLAboutDialogProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

function TJVCLAboutDialogProperty.GetValue: string;
begin
  Result := JVCL_VERSIONSTRING;
end;

{ TJvJVCLAboutForm }

procedure TJvJVCLAboutForm.btnOKClick(Sender: TObject);
begin
Close;
end;

procedure TJvJVCLAboutForm.CreateParams(var Params: TCreateParams);
begin
	inherited CreateParams(Params);
  with Params do
  begin
    Style := (Style or WS_POPUP) and (not WS_DLGFRAME);
  end;

end;

procedure TJvJVCLAboutForm.FormShow(Sender: TObject);
begin
  lblVersion.Caption := JVCL_VERSIONSTRING;
  lblCopyRight.Caption := 'Copyright© Project JEDI, 1999 - ' + FormatDateTime('yyyy', Now);
end;

procedure TJvJVCLAboutForm.Panel1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ReleaseCapture;
  Perform(WM_SYSCOMMAND, SC_MOVE + 2, 0);
end;

end.
