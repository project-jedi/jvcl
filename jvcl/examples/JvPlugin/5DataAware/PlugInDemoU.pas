unit PlugInDemoU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvPluginMan, StdCtrls, Buttons, Menus, ExtCtrls, ImgList, Grids, DBGrids,
  JvComponent;

type
  TForm1 = class(TForm)
    uilPluginManager: TJvPluginManager;
    MainMenu1: TMainMenu;
    Plugin1: TMenuItem;
    File1: TMenuItem;
    Exit1: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    DBGrid1: TDBGrid;
    procedure Exit1Click(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure SendMessagetoPlugins1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    NumButtons : integer;
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Exit1Click(Sender: TObject);
begin
   Close;
end;

procedure TForm1.About1Click(Sender: TObject);
begin
   ShowMessage('A simple host application for demoing JEDI plug-ins.'#13#10#13#10'(c) 1999, Unlimited Intelligence Limited.');
end;

procedure TForm1.SendMessagetoPlugins1Click(Sender: TObject);
begin
   uilPluginManager.SendMessage(1000, InputBox('Enter message to send to plugin', 'Message', 'Your message here'));
end;

procedure TForm1.FormShow(Sender: TObject);
begin
   uilPluginManager.LoadPlugins;
end;

end.
