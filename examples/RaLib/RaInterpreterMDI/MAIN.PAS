unit Main;

interface

uses Sysutils, Forms, Controls, Menus, Classes, ComCtrls,
     JvInterpreter, JvInterpreterFm, JvInterpreter_all, JvComponent;

type
  TMainForm   = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    FileNewItem: TMenuItem;
    Window1: TMenuItem;
    Help1: TMenuItem;
    N1: TMenuItem;
    FileExitItem: TMenuItem;
    HelpAboutItem: TMenuItem;
    StatusBar: TStatusBar;
    N2: TMenuItem;
    JvInterpreterFm1: TJvInterpreterFm;
    procedure FileNew1Execute(Sender: TObject);
    procedure HelpAbout1Execute(Sender: TObject);
    procedure FileExit1Execute(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm  ;

implementation

{$R *.DFM}

uses ChildWin, aboutRaInterpreter;

procedure TMainForm  .FileNew1Execute(Sender: TObject);
begin
  JvInterpreterFm1.RunForm(ExtractFilePath(paramstr(0))+'childwin.pas');
end;

procedure TMainForm  .HelpAbout1Execute(Sender: TObject);
begin
  JvInterpreterRunFormModal(ExtractFilePath(paramstr(0))+'aboutRAInterpreter.pas');
end;

procedure TMainForm  .FileExit1Execute(Sender: TObject);
begin
  Close;
end;

end.
