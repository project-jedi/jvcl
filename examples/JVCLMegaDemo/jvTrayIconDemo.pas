unit jvTrayIconDemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Menus, JvComponent, JvTrayIcon;

type
  TfrTrayicon = class(TForm)
    JvTrayIcon1: TJvTrayIcon;
    PopupMenu1: TPopupMenu;
    ShowTray1: TMenuItem;
    ShowForm1: TMenuItem;
    Close1: TMenuItem;
    Label1: TLabel;
    procedure ShowTray1Click(Sender: TObject);
    procedure ShowForm1Click(Sender: TObject);
    procedure Close1Click(Sender: TObject);
  end;

implementation

{$R *.DFM}

procedure TfrTrayicon.ShowTray1Click(Sender: TObject);
begin
  JvTrayIcon1.Active := true;
end;

procedure TfrTrayicon.ShowForm1Click(Sender: TObject);
begin
  JvTrayIcon1.active := false;
end;

procedure TfrTrayicon.Close1Click(Sender: TObject);
begin
  close;
end;

end.
