unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, JvComponent, JvAppHotKey, StdCtrls, ComCtrls;

type
  TfrmMain = class(TForm)
    HotKey1: THotKey;
    Label1: TLabel;
    btnAdd: TButton;
    lbHotKeys: TListBox;
    procedure btnAddClick(Sender: TObject);
  private
    procedure DoHotKey(Sender: TObject);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation
uses
  Menus;
{$R *.dfm}

procedure TfrmMain.DoHotKey(Sender:TObject);
begin
  Application.BringToFront;
  ShowMessage(Format('HotKey "%s" pressed!',[ShortCutToText((Sender as TJvApplicationHotKey).HotKey)]));
end;

procedure TfrmMain.btnAddClick(Sender: TObject);
var S:string;
begin
  S := ShortCutToText(HotKey1.HotKey);
  if lbHotKeys.Items.IndexOf(S) > -1 then
  begin
    ShowMessage('Hot key already assigned!');
    Exit;
  end;
  with TJvApplicationHotKey.Create(self) do
  begin
    HotKey := HotKey1.HotKey;
    Active := true;
    OnHotKey := DoHotKey;
    lbHotKeys.Items.Add(S);
  end;
end;

end.
