unit fJvHLEdPropDlgTestMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvHLEdPropDlg, StdCtrls, Buttons, JvButtons, JvEditor, JvHLEditor,
  fJvHLEdPropDlgTestParams, JvRegAuto;

type
  TForm1 = class(TForm)
    RAHLEditor1: TJvHLEditor;
    RAhtButton1: TJvHTButton;
    RegAuto1: TJvRegAuto;
    RAHLEdPropDlg1: TJvHLEdPropDlg;
    procedure RAhtButton1Click(Sender: TObject);
    procedure RAHLEdPropDlg1DialogPopup(Sender: TObject; Form: TForm);
    procedure RAHLEdPropDlg1DialogClosed(Sender: TObject; Form: TForm;
      Apply: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    MyParamsForm: TMyParams ;
    procedure ApplyParams(Form: TMyParams );
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.RAhtButton1Click(Sender: TObject);
begin
  RAHLEdPropDlg1.Execute;
end;

procedure TForm1.RAHLEdPropDlg1DialogPopup(Sender: TObject; Form: TForm);
begin
  MyParamsForm := TMyParams .Create(Self);
  MyParamsForm.Load;
  MyParamsForm.TabSheet1.PageControl := TJvHLEditorParamsForm(Form).Pages;
  MyParamsForm.TabSheet2.PageControl := TJvHLEditorParamsForm(Form).Pages;
  MyParamsForm.TabSheet1.PageIndex := 0;
  MyParamsForm.TabSheet2.PageIndex := 1;
  Form.Caption := 'Properties';
end;

procedure TForm1.RAHLEdPropDlg1DialogClosed(Sender: TObject; Form: TForm;
  Apply: Boolean);
begin
  if Apply then
  begin
    MyParamsForm.Save;
    ApplyParams(MyParamsForm);
  end;
  MyParamsForm.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  if RAHLEdPropDlg1.RegAuto <> nil then
  begin
    RAHLEdPropDlg1.Restore;
    RAHLEdPropDlg1.LoadCurrentHighlighterColors;
  end;

  MyParamsForm := TMyParams .Create(Self);
  try
    MyParamsForm.Load;
    ApplyParams(MyParamsForm);
  finally
    MyParamsForm.Free;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if RAHLEdPropDlg1.RegAuto <> nil then
    RAHLEdPropDlg1.Save;
end;

procedure TForm1.ApplyParams(Form: TMyParams );
begin
  Caption := Form.GetAppCaption;
end;


end.
