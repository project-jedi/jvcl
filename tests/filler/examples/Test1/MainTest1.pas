unit MainTest1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvFillFontList, StdCtrls, JvListBox, JvFillerControls, JvLabel,
  JvComponent, JvFillBasicImpl, JvFillStringList;

type
  TForm1 = class(TForm)
    JvFillListBox1: TJvFillListBox;
    JvFontFiller1: TJvFontFiller;
    JvFillLabel1: TJvFillLabel;
    JvStringsFiller1: TJvStringsFiller;
    JvFillListBox2: TJvFillListBox;
    btnEditStrings: TButton;
    btnEditTree: TButton;
    procedure btnEditStringsClick(Sender: TObject);
    procedure btnEditTreeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    JvTreeFiller1: TJvTreeFiller;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

uses
  JvFillerEditor;

procedure TForm1.btnEditStringsClick(Sender: TObject);
begin
  EditFiller(JvStringsFiller1);
end;

procedure TForm1.btnEditTreeClick(Sender: TObject);
begin
  EditFiller(JvTreeFiller1);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  JvTreeFiller1 := TJvTreeFiller.Create(Self);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  JvTreeFiller1.Free;
end;

end.
