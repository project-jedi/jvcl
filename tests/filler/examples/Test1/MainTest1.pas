unit MainTest1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvFillFontList, StdCtrls, JvListBox, JvFillerControls, JvLabel,
  JvComponent, JvFillBasicImpl, JvFillStringList, JvxCtrls;

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
  JvFillIntf,
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
var
  FilItemsIntf: IFillerItems;
  FilManIntf: IFillerItemManagment;
begin
  JvTreeFiller1 := TJvTreeFiller.Create(Self);
  if not JvTreeFiller1.GetInterface(IFillerItemManagment, FilManIntf) then
    raise Exception.Create('No managment interface');
  with FilManIntf do
    (New as IFillerItemText).Caption := 'Item 1';
  with FilManIntf do
    (New as IFillerItemText).Caption := 'Item 2';
  with FilManIntf do
    (New as IFillerItemText).Caption := 'Item 3';
  with FilManIntf do
    (New as IFillerItemText).Caption := 'Item 4';
  if not Supports(FilManIntf, IFillerItems, FilItemsIntf) then
    raise Exception.Create('No items interface');
  with FilItemsIntf do
  begin
    with getItem(0) as IFillerItemManagment do
      (New as IFillerItemText).Caption := 'Item 1.1';
    with (getItem(0) as IFillerItems) as IFillerItemManagment do
      (New as IFillerItemText).Caption := 'Item 1.2';
    with (getItem(0) as IFillerItems) as IFillerItemManagment do
      (New as IFillerItemText).Caption := 'Item 1.3';
    with (getItem(2) as IFillerItems) as IFillerItemManagment do
      (New as IFillerItemText).Caption := 'Item 2.1';
    with (getItem(2) as IFillerItems) as IFillerItemManagment do
      (New as IFillerItemText).Caption := 'Item 2.2';
    with (getItem(2) as IFillerItems) as IFillerItemManagment do
      (New as IFillerItemText).Caption := 'Item 2.3';
    with (getItem(2) as IFillerItems) as IFillerItemManagment do
      (New as IFillerItemText).Caption := 'Item 2.4';
    with (getItem(2) as IFillerItems) as IFillerItemManagment do
      (New as IFillerItemText).Caption := 'Item 2.5';
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  JvTreeFiller1.Free;
end;

end.
