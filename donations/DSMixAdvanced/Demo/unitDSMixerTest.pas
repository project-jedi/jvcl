unit unitDSMixerTest;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  cbDSmixer, unitChannel, Menus, StdCtrls, mmsystem, XPMan, ComCtrls;

const
  MAX_CHANNELS = 4;

type
  TFormDSMixer = class(TForm)
    MainMenu1: TMainMenu;
    miDevices: TMenuItem;
    File1: TMenuItem;
    Addchannel1: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    StatusBar1: TStatusBar;
    procedure FormCreate(Sender: TObject);
    procedure miCriarCanalClick(Sender: TObject);

    procedure miDeviceClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure About1Click(Sender: TObject);
  public
    dsm: TcbDSMixer;
    procedure UpdateStatus;
    { Public declarations }
  end;

var
  FormDSMixer: TFormDSMixer;

implementation

{$R *.DFM}

procedure TFormDSMixer.FormCreate(Sender: TObject);
var
  NewItem: TMenuItem;
  Cnt: Integer;
begin
  dsm := TcbDSMixer.Create(self);

  for Cnt := 0 to dsm.DeviceCount-1 do
  begin
    NewItem := TMenuItem.Create(Self);
    NewItem.Caption := dsm.Devices[Cnt];
    NewItem.Tag := Cnt;
    NewItem.RadioItem := True;
    NewItem.OnClick := miDeviceClick;
    miDevices.Add(NewItem);
  end;
  miDevices.Items[0].Checked := True;
end;

procedure TFormDSMixer.miCriarCanalClick(Sender: TObject);
begin
  TFormChannel.Create(self, DSM);
  UpdateStatus;
end;

procedure TFormDSMixer.miDeviceClick(Sender: TObject);
begin
  dsm.DeviceIndex := TComponent(Sender).Tag;
  dsm.DestroyDirectSound;
  TMenuItem(Sender).Checked := True;
end;

procedure TFormDSMixer.FormDestroy(Sender: TObject);
var
  Cnt: Integer;
begin
  for Cnt := MDIChildCount-1 downto 0 do
      MDIChildren[Cnt].Free;
  dsm.Free;
end;

procedure TFormDSMixer.About1Click(Sender: TObject);
begin
MessageBox(formdsmixer.handle,pchar('DSMixer Demo application'+#13+
  'By Pavel Bibergal cyberkm@barak-online.net'),pchar('DSMIX Demo'),0);
end;

procedure TFormDSMixer.UpdateStatus;
begin
StatusBar1.Panels[0].Text:='Channels: '+inttostr(dsm.ChannelCount);
end;

end.
