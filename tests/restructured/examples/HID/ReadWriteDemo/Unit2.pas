unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TInfoForm = class(TForm)
    DevStrings: TListBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Vid: TLabel;
    Pid: TLabel;
    Vers: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    InputLen: TLabel;
    OutputLen: TLabel;
    FeatureLen: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    VendorName: TLabel;
    ProductName: TLabel;
    Label11: TLabel;
    SerialNo: TLabel;
    LangStrings: TListBox;
    Label12: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  InfoForm: TInfoForm;

implementation

uses Unit1;

{$R *.DFM}

procedure TInfoForm.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  VendorName.Caption := TheDev.VendorName;
  ProductName.Caption := TheDev.ProductName;
  SerialNo.Caption := TheDev.SerialNumber;
  Vid.Caption  := IntToHex(TheDev.Attributes.VendorID, 4);
  Pid.Caption  := IntToHex(TheDev.Attributes.ProductID, 4);
  Vers.Caption := IntToHex(TheDev.Attributes.VersionNumber, 4);
  if TheDev.Caps.InputReportByteLength > 0 then
    InputLen.Caption   := IntToHex(TheDev.Caps.InputReportByteLength-1, 1);
  if TheDev.Caps.OutputReportByteLength > 0 then
    OutputLen.Caption  := IntToHex(TheDev.Caps.OutputReportByteLength-1, 1);
  if TheDev.Caps.FeatureReportByteLength > 0 then
    FeatureLen.Caption := IntToHex(TheDev.Caps.FeatureReportByteLength-1, 1);
  for I := 1 to 255 do
    if TheDev.DeviceStrings[I] <> '' then
      DevStrings.Items.Add(Format('%3d) %s',[I+1,TheDev.DeviceStrings[I]]));
  for I := 0 to TheDev.LanguageStrings.Count - 1 do
    LangStrings.Items.Add(TheDev.LanguageStrings[I]);
end;

end.
