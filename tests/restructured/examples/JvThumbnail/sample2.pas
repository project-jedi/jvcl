unit sample2;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, FileCtrl, ComCtrls, JvBaseThumbnail, JvThumbImage, JvThumbNails ;

type
  TForm2 = class(TForm)
    Splitter2: TSplitter;
    Panel6: TPanel;
    Splitter4: TSplitter;
    DirectoryListBox2: TDirectoryListBox;
    FileListBox1: TFileListBox;
    Panel8: TPanel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    REDBar: TTrackBar;
    GreenBar: TTrackBar;
    BlueBar: TTrackBar;
    contrastBar: TTrackBar;
    Button2: TButton;
    Panel9: TPanel;
    DriveComboBox2: TDriveComboBox;
    Panel10: TPanel;
    FilterComboBox1: TFilterComboBox;
    Panel7: TPanel;
    Panel5: TPanel;
    Label5: TLabel;
    Bevel1: TBevel;
    asbuttoncb: TCheckBox;
    autoloadcb: TCheckBox;
    minmemcb: TCheckBox;
    titleplacegr: TRadioGroup;
    Edit1: TEdit;
    GroupBox1: TGroupBox;
    Button4: TButton;
    Button5: TButton;
    Label1: TLabel;
    LightnessBar: TTrackBar;
    Button1: TButton;
    AngleGr: TRadioGroup;
    ThumbNail1: TJVThumbNail;
    ThumbImage1: TJvThumbImage;
    procedure Button2Click(Sender: TObject);
    procedure Panel6Resize(Sender: TObject);
    procedure FileListBox1Change(Sender: TObject);
    procedure asbuttoncbClick(Sender: TObject);
    procedure autoloadcbClick(Sender: TObject);
    procedure minmemcbClick(Sender: TObject);
    procedure titleplacegrClick(Sender: TObject);
    procedure Panel8Resize(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure thumbnail1Click(Sender: TObject);
    procedure Panel10Resize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure AngleGrClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.DFM}

procedure TForm2.Button2Click(Sender: TObject);
begin
  thumbimage1.ChangeRGB(redbar.Position,greenbar.Position,bluebar.Position);
  thumbimage1.Contrast(contrastbar.Position);
  THumbImage1.Lightness(LightnessBar.Position);
  redbar.Position:=0;
  greenbar.Position :=0;
  bluebar.Position:=0;
  contrastbar.position:=0;
  lightnessbar.Position:=0;
end;

procedure TForm2.Panel6Resize(Sender: TObject);
begin
  DriveComboBox2.Height:= panel9.ClientHeight;
  DriveComboBox2.Width := panel9.ClientWidth;
end;

procedure TForm2.FileListBox1Change(Sender: TObject);
begin
  if filelistbox1.FileName<>'' then
    thumbnail1.FileName := filelistbox1.FileName;
end;

procedure TForm2.asbuttoncbClick(Sender: TObject);
begin
  THumbnail1.Asbutton := asbuttoncb.Checked;
end;

procedure TForm2.autoloadcbClick(Sender: TObject);
begin
  thumbnail1.autoload := autoloadcb.Checked;
end;

procedure TForm2.minmemcbClick(Sender: TObject);
begin
  thumbnail1.minimizememory:=minmemcb.Checked;
end;

procedure TForm2.titleplacegrClick(Sender: TObject);
begin
  thumbnail1.TitlePlacement := ttitlepos(titleplacegr.ItemIndex);
end;

procedure TForm2.Panel8Resize(Sender: TObject);
begin
  redbar.Width := panel8.ClientWidth;
end;

procedure TForm2.Button4Click(Sender: TObject);
begin
  thumbimage1.invert;
end;

procedure TForm2.Button5Click(Sender: TObject);
begin
  thumbimage1.grayscale;
end;

procedure TForm2.thumbnail1Click(Sender: TObject);
begin
  if thumbnail1.FileName<>'' then
    thumbimage1.Loadfromfile(thumbnail1.FileName);
end;

procedure TForm2.Panel10Resize(Sender: TObject);
begin
  filtercombobox1.Width := panel10.ClientWidth;
  filtercombobox1.Height:= panel10.ClientHeight;
end;

procedure TForm2.FormShow(Sender: TObject);
begin
  //thumbimage1.Picture.Free;
  TitlePlacegr.ItemIndex := integer(thumbnail1.titlePlacement);
  Anglegr.ItemIndex := integer(thumbimage1.angle);
end;

procedure TForm2.AngleGrClick(Sender: TObject);
begin
  thumbimage1.angle := TAngle(anglegr.ItemIndex)
end;

end.
