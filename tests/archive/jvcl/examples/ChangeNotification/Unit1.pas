unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvChangeNotify, Buttons, StdCtrls, ComCtrls, ExtCtrls, JvComponent;

type
  TForm1 = class(TForm)
    CN1: TJvChangeNotify;
    ListBox2: TListBox;
    Label3: TLabel;
    ListView1: TListView;
    Panel1: TPanel;
    SpeedButton1: TSpeedButton;
    Label2: TLabel;
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    udInterval: TUpDown;
    Button3: TButton;
    Label4: TLabel;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure CN1ChangeNotify(Sender: TObject; Dir: String;
      Actions: TJvChangeActions);
  private
    { Private declarations }
    procedure ResetCaptions(Invert:boolean);
    procedure WMGetMinMaxInfo(var Msg:TWMGetMinMaxInfo);message WM_GETMINMAXINFO;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses Unit2;

{$R *.DFM}

procedure TForm1.ResetCaptions(Invert:boolean);
const
  aCap:array[boolean] of string=('TJvChangeNotification demo','Checking...');
begin
  if Invert then
    Caption := aCap[not CN1.Active]
  else
    Caption := aCap[CN1.Active];
  Application.Title := Caption;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CN1.Active := false;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if ListView1.Selected <> nil then
  begin
    CN1.Notifications[ListView1.Selected.Index].Free;
    ListView1.Items.Delete(ListView1.Selected.Index);
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var Options:TJvChangeActions; aItem:TListItem;S:string;
begin
  if Form2.ShowModal = mrOK then
  begin
    aItem := ListView1.Items.Add;
    Options := [];
    S := '';
    with Form2 do
    begin
      if cbFileNames.Checked then
      begin
        Include(Options,caChangeFileName);
        S := 'Files,';
      end;
      if cbDirNames.Checked then
      begin
        Include(Options,caChangeDirName);
        S := S + 'Directories,';
      end;
      if cbAttributes.Checked then
      begin
        Include(Options,caChangeAttributes);
        S := S + 'Attributes,';
      end;
      if cbSize.Checked then
      begin
        Include(Options,caChangeSize);
        S := S + 'Size,';
      end;
      if cbWrite.Checked then
      begin
        Include(Options,caChangeLastWrite);
        S := S + 'Last Write,';
      end;
      S := '(' + Copy(S,1,Length(S) - 1) + ')';
      aItem.Caption := Form2.Edit1.Text;
      if Form2.cbSubTrees.Checked and (Win32Platform = VER_PLATFORM_WIN32_NT) then
        aItem.SubItems.Add('Yes')
      else
        aItem.SubItems.Add('No');
      aItem.SubItems.Add(S);
      with CN1.Notifications.Add do
      begin
        IncludeSubTrees := Form2.cbSubTrees.Checked and (Win32Platform = VER_PLATFORM_WIN32_NT);
        Directory := Form2.Edit1.Text;
        Actions := Options;
      end;
  end;
  end;
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
var b:boolean;
begin
  if CN1.Notifications.Count = 0 then
  begin
    ShowMessage('No notifications to monitor!');
    SpeedButton1.Down := false;
    Exit;
  end;

  b := SpeedButton1.Down;
  Button1.Enabled := not b;
  Button2.Enabled := not b;
  ResetCaptions(true);
  { do this *after* setting buttons }
  CN1.Active := b;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  ListBox2.Clear;
  ResetCaptions(false);
end;

procedure TForm1.CN1ChangeNotify(Sender: TObject; Dir: String;
  Actions: TJvChangeActions);
begin
  Application.Title := Format ('Change in %s (%s)',[Dir,ActionsToString(Actions)]);
  ListBox2.Items.Add(Application.Title);
  FlashWindow(Form1.Handle,true);
  MessageBeep(DWORD(-1));
end;

procedure TForm1.WMGetMinMaxINfo(var Msg:TWMGetMinMaxInfo);
begin
  with Msg.MinMaxInfo^.ptMinTrackSize do
  begin
    X := 392;
    Y := 295;
  end;
  Msg.Result := 0;
end;
end.
