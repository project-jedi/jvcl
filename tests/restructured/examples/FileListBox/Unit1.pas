unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, JvDriveCtrls, FileCtrl, JvCtrls,
  JvComponent, JvListBox, JvCombobox;

type
  TForm1 = class(TForm)
    DriveCombo1: TJvDriveCombo;
    JvDirectoryListBox1: TJvDirectoryListBox;
    JvFileListBox1: TJvFileListBox;
    StatusBar1: TStatusBar;
    procedure JvDirectoryListBox1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.JvDirectoryListBox1Change(Sender: TObject);
begin
  StatusBar1.Panels[0].Text := JvDirectoryListBox1.Directory;
end;


end.
