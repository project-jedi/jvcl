unit JvAppHotKeyDemoMainFormU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, JvComponent, JvAppHotKey, StdCtrls, ComCtrls;

type
  TJvAppHotKeyDemoMainForm = class(TForm)
    HotKey1: THotKey;
    Label1: TLabel;
    btnAdd: TButton;
    lbHotKeys: TListBox;
    Label2: TLabel;
    procedure btnAddClick(Sender: TObject);
  private
    procedure DoHotKey(Sender: TObject);
  end;

var
  JvAppHotKeyDemoMainForm: TJvAppHotKeyDemoMainForm;

implementation

uses
  Menus;

{$R *.dfm}

procedure TJvAppHotKeyDemoMainForm.DoHotKey(Sender:TObject);
begin
  Application.BringToFront;
  ShowMessage(Format('HotKey "%s" pressed!',[ShortCutToText((Sender as TJvApplicationHotKey).HotKey)]));
end;

procedure TJvAppHotKeyDemoMainForm.btnAddClick(Sender: TObject);
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
