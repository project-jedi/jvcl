unit JvDbMaskEditDemoFm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Mask, JvMaskEdit, JvDBControls, ExtCtrls, DBCtrls,
  dbcgrids, DB, JvCsvData, JvEdit, JvValidateEdit;

type
  TForm1 = class(TForm)
    JvCsvDataSet1: TJvCsvDataSet;
    DataSource1: TDataSource;
    JvCsvDataSet1NAME: TStringField;
    JvCsvDataSet1PHONE: TStringField;
    DBCtrlGrid1: TDBCtrlGrid;
    DBNavigator1: TDBNavigator;
    Label3: TLabel;
    Label4: TLabel;
    Label1: TLabel;
    EditNAME: TJvDBMaskEdit;
    EditPHONE: TJvDBMaskEdit;
    procedure EditNAMEAcceptNewValue(Sender: TObject;
      oldValue: String; var newValue: String; var Accept, Post: Boolean);
    procedure EditNAMEKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure EditNAMEExit(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.EditNAMEAcceptNewValue(Sender: TObject;
  oldValue: String; var newValue: String; var Accept, Post: Boolean);
begin
 if UpperCase(newValue) = 'BOB' then begin
    // you would probably not put up a modal dialog box EVER in a real application, because
    // modal dialog boxes are annoying, but it does make this demo more fun...
    Application.MessageBox('But you hate Bob, you can''t put him in your phone book. I insist. I''m not going to let you do that, Dave.','Your Computer Loves You and Takes Care of You', MB_OK);
    // newValue := 'Dave'; // you could also accept a value, but substitute another value here, via lookup of a code or something.
    Accept := false;
    exit;
 end;
 Post := true; // A valid name is posted automatically, no need to click Post button.
end;

procedure TForm1.EditNAMEKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  OutputDebugString(PChar(IntToStr(Key)));
end;

procedure TForm1.EditNAMEExit(Sender: TObject);
begin
  OutputDebugString('OnExit');
end;

end.
