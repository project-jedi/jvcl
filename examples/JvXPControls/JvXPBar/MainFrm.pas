unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, JvXPCore, JvXPBar, JvXPContainer, ImgList, ActnList, ExtCtrls,
  StdCtrls;

resourcestring
  SClickEvent =
    'You have clicked the action "%s"...';

type
  TfrmMain = class(TForm)
    acConnectAdministrator: TAction;
    acConnectLocalServer: TAction;
    acConnectRemoteServer: TAction;
    aclWinXPBar: TActionList;
    acSettingsDatabase: TAction;
    acSettingsDownloads: TAction;
    acSettingsStatistics: TAction;
    acSettingsUsers: TAction;
    acSynchronizeUnknown: TAction;
    acSynchronizeWeb: TAction;
    btnCollpaseAll: TButton;
    btnExpandAll: TButton;
    cntDetails: TJvXPContainer;
    cntWinXPBar: TJvXPContainer;
    cntWinXPBarPanel1: TJvXPContainer;
    dxContainer1: TJvXPContainer;
    dxContainer2: TJvXPContainer;
    dxWinXPBar1: TJvXPBar;
    dxWinXPBar2: TJvXPBar;
    dxWinXPBar3: TJvXPBar;
    imlWinXPBar: TImageList;
    lbWelcome: TLabel;
    sbxWinXPBar: TScrollBox;
    spltMain: TSplitter;
    btnToogleEnableMode: TButton;
    btnToggleVisibleMode: TButton;
    procedure acConnectRemoteServerExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnCollpaseAllClick(Sender: TObject);
    procedure btnExpandAllClick(Sender: TObject);
    procedure btnToogleEnableModeClick(Sender: TObject);
    procedure btnToggleVisibleModeClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

{-----------------------------------------------------------------------------
  Procedure: TfrmMain.FormCreate
  Author:    M. Hoffmann
  Date:      06-Feb-2003
  Arguments: Sender: TObject
  Result:    None
-----------------------------------------------------------------------------}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  cntDetails.Align := alClient;
end;

{-----------------------------------------------------------------------------
  Procedure: TfrmMain.acConnectRemoteServerExecute
  Author:    M. Hoffmann
  Date:      06-Feb-2003
  Arguments: Sender: TObject
  Result:    None
-----------------------------------------------------------------------------}

procedure TfrmMain.acConnectRemoteServerExecute(Sender: TObject);
begin
  with TAction(Sender) do
    MessageBox(Application.Handle, PChar(Format(SClickEvent, [Name])),
      'Click', MB_ICONINFORMATION);
end;

{-----------------------------------------------------------------------------
  Procedure: TfrmMain.btnCollpaseAllClick
  Author:    M. Hoffmann
  Date:      06-Feb-2003
  Arguments: Sender: TObject
  Result:    None
-----------------------------------------------------------------------------}

procedure TfrmMain.btnCollpaseAllClick(Sender: TObject);
begin
  dxWinXPBar1.Collapsed := True;
  dxWinXPBar2.Collapsed := True;
  dxWinXPBar3.Collapsed := True;
end;

{-----------------------------------------------------------------------------
  Procedure: TfrmMain.btnExpandAllClick
  Author:    M. Hoffmann
  Date:      06-Feb-2003
  Arguments: Sender: TObject
  Result:    None
-----------------------------------------------------------------------------}

procedure TfrmMain.btnExpandAllClick(Sender: TObject);
begin
  dxWinXPBar1.Collapsed := False;
  dxWinXPBar2.Collapsed := False;
  dxWinXPBar3.Collapsed := False;
end;

{-----------------------------------------------------------------------------
  Procedure: TfrmMain.btnToogleEnableModeClick
  Author:    M. Hoffmann
  Date:      06-Feb-2003
  Arguments: Sender: TObject
  Result:    None
-----------------------------------------------------------------------------}

procedure TfrmMain.btnToogleEnableModeClick(Sender: TObject);
begin
  dxWinXPBar1.Items[0].Enabled := not dxWinXPBar1.Items[0].Enabled;
end;

{-----------------------------------------------------------------------------
  Procedure: TfrmMain.btnToggleVisibleModeClick
  Author:    M. Hoffmann
  Date:      06-Feb-2003
  Arguments: Sender: TObject
  Result:    None
-----------------------------------------------------------------------------}

procedure TfrmMain.btnToggleVisibleModeClick(Sender: TObject);
begin
  dxWinXPBar2.Items[1].Visible := not dxWinXPBar2.Items[1].Visible;
end;

end.

