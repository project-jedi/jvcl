unit JvAniMainFormU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, FileCtrl, ExtCtrls, JvAni;

type
  TJvAniMainForm = class(TForm)
    Image1: TImage;
    FileListBox1: TFileListBox;
    DirectoryListBox1: TDirectoryListBox;
    DriveComboBox1: TDriveComboBox;
    Memo1: TMemo;
    procedure FileListBox1Click(Sender: TObject);
  end;

var
  JvAniMainForm: TJvAniMainForm;

implementation

{$R *.DFM}

procedure TJvAniMainForm.FileListBox1Click(Sender: TObject);
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
