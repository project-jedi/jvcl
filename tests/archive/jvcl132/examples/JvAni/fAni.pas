unit fAni;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, FileCtrl, ExtCtrls, JvAni;

type
  TForm1 = class(TForm)
    Image1: TImage;
    FileListBox1: TFileListBox;
    DirectoryListBox1: TDirectoryListBox;
    DriveComboBox1: TDriveComboBox;
    Memo1: TMemo;
    procedure FileListBox1Click(Sender: TObject);
  private
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.FileListBox1Click(Sender: TObject);
begin
  self.Image1.Picture.LoadFromFile(self.FileListBox1.FileName);

  with TJvAni(TJvAni(self.Image1.Picture.Graphic)) do
  begin
    Animated:=true;
    self.Memo1.Clear;
    self.Memo1.Lines.Add('Author : '+Author);
    self.Memo1.Lines.Add('Title : '+Title);
    self.Memo1.Lines.Add('Frames : '+IntToStr(FramesCount));
  end;
end;

end.
