unit JvHotKeyFormU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, JvComponent, JvAppHotKey, StdCtrls, ComCtrls, ExtCtrls,
  JvCaptionPanel;

type
  TJvHotKeyForm = class(TForm)
    JvCaptionPanel1: TJvCaptionPanel;
    Label1: TLabel;
    HotKey1: THotKey;
    btnAdd: TButton;
    lbHotKeys: TListBox;
    Label2: TLabel;
    procedure btnAddClick(Sender: TObject);
  private
    procedure DoHotKey(Sender: TObject);
    { Private declarations }
  public
    { Public declarations }
  end;

implementation
uses
  Menus;
{$R *.dfm}

procedure TJvHotKeyForm.DoHotKey(Sender:TObject);
begin
  Application.BringToFront;
  ShowMessage(Format('HotKey "%s" pressed!',[ShortCutToText((Sender as TJvApplicationHotKey).HotKey)]));
end;

procedure TJvHotKeyForm.btnAddClick(Sender: TObject);
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
