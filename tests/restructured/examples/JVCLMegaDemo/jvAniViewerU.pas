unit jvAniViewerU;

interface

uses 
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, FileCtrl, ExtCtrls, JvAni;

type
  TjvAniViewer = class(TFrame)
    Image1: TImage;
    FileListBox1: TFileListBox;
    DirectoryListBox1: TDirectoryListBox;
    DriveComboBox1: TDriveComboBox;
    Memo1: TMemo;
    Label1: TLabel;
    procedure FileListBox1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.DFM}

procedure TjvAniViewer.FileListBox1Click(Sender: TObject);
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
